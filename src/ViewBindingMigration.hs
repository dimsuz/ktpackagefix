{-# LANGUAGE OverloadedStrings #-}
module ViewBindingMigration where

import Turtle hiding (fp)
import Prelude hiding (FilePath)
import Data.Maybe (listToMaybe, isJust, fromJust)
import Data.Foldable (for_)
import Data.Ord (comparing)
import qualified Data.List as DL
import Control.Monad (filterM)
import System.IO (hClose)
import qualified Control.Foldl as Fold
import qualified Data.Text as T

-- * Find viewLayoutResource, derive binding name
-- * Gather ids from layout file and replace them with binding.id in controller file
-- * Rename import for ScopedMviController
-- * Add binding name to ScopedMviController template params
-- * Add binding name to Config template params
-- * Remove viewLayoutResource, replace with binding inflate reference
-- * Remove imports: synthetics, ".R$"
-- * Add imports: ViewBinding, derived binding name
data Module = Module
  { moduleManifest :: FilePath
  , moduleSrcMainPath :: FilePath
  , moduleDir :: FilePath
  , modulePackage :: Text
  , moduleName :: Text
  }
  deriving Show

data Controller = Controller
  { controllerName :: Text
  , controllerFilePath :: FilePath
  , controllerBindingName :: Text
  , controllerChildViewIds :: [Text]
  , controllerModule :: Module
  }
  deriving Show

data ContentType = ControllerClass | ViewClass

inplaceFilter :: Pattern () -> FilePath -> IO ()
inplaceFilter ptrn file = liftIO $ runManaged $ do
  here <- pwd
  (tmpfile, handle) <- mktemp here "inplaceFilter"
  let matches line = (not . null) (match ptrn (lineToText line))
  let filtered = mfilter matches (input file)
  outhandle handle filtered
  liftIO (hClose handle)
  copymod file tmpfile
  mv tmpfile file

extractModuleName :: FilePath -> Text
extractModuleName = T.pack . encodeString . dirname . extractModuleDir

extractModuleDir :: FilePath -> FilePath
extractModuleDir = parent . parent . directory

extractRootPath :: FilePath -> FilePath
extractRootPath manifest = directory manifest </> "kotlin"

extractPackage :: FilePath -> IO (Maybe Text)
extractPackage manifest = do
  let matches = listToMaybe . match packagePattern . lineToText <$> input manifest
  join <$> fold matches (Fold.find isJust)
  where packagePattern = has ("package=" *> char '"' *> chars1 <* char '"')

buildModule :: FilePath -> IO Module
buildModule mf = do
  package <- fromJust <$> extractPackage mf
  return (Module mf (extractRootPath mf) (extractModuleDir mf) package (extractModuleName mf))

findModules :: FilePath -> IO [Module]
findModules dir = do
  files <- fold manifests Fold.list
  mapM buildModule files
  where
    manifests = findtree (invert (has "build")) (find (ends "src/main/AndroidManifest.xml") dir)

grepFind :: Text -> FilePath -> IO Bool
grepFind t file = not . null <$> fold lines Fold.list
  where lines = grep (has (text t)) (input file)

extractText :: FilePath -> Pattern Text -> IO (Maybe Text)
extractText file p = do
  let matches = listToMaybe . match p . lineToText <$> input file
  join <$> fold matches (Fold.find isJust)

snakeToPascal :: Text -> Text
snakeToPascal s = T.concat (map T.toTitle parts)
  where parts = T.splitOn "_" s

snakeToCamel :: Text -> Text
snakeToCamel s = head parts <> T.concat (map T.toTitle (drop 1 parts))
  where parts = T.splitOn "_" s

toBindingName :: Text -> Text
toBindingName = (<> "Binding") . snakeToPascal

findChildViewIds :: Module -> Text -> IO [Text]
findChildViewIds m layoutRes = do
  let filename = moduleDir m </> "src" </> "main" </> "res" </> "layout" </> fromText layoutRes <.> "xml"
  let matches = match viewIdPattern . lineToText <$> input filename
  join <$> fold matches Fold.list
  where viewIdPattern = has (("android:id=\"@+id/" <|> "android:id=\"@id/") *> chars1 <* char '"')

buildController :: Module -> ContentType -> FilePath -> IO Controller
buildController m ct file = do
  resource <- fromJust <$> extractText file layoutResourcePattern
  name <- fromJust <$> extractText file namePattern
  viewIds <- findChildViewIds m resource
  return Controller
    { controllerName = name
    , controllerFilePath = file
    , controllerBindingName = toBindingName resource
    , controllerChildViewIds = viewIds
    , controllerModule = m
    }
  where namePattern = case ct of
          ControllerClass -> has ("class " *> chars1 <* "Controller :")
          ViewClass -> has ("class " *> chars1 <* "View @JvmOverloads")
        layoutResourcePattern = case ct of
          ControllerClass -> has ("viewLayoutResource = R.layout." *> chars1)
          ViewClass -> has ("View.inflate(context, R.layout." *> chars1 <* ", this)")

findControllers :: ContentType -> Module -> IO [Controller]
findControllers ct m = do
  files <- fold controllers Fold.list
  filtered <- filterM filterPred files
  mapM (buildController m ct) filtered
  where
    controllers = find filePattern (moduleDir m)
    filePattern = case ct of
      ControllerClass -> ends "Controller.kt"
      ViewClass -> ends "View.kt"
    filterPred = case ct of
      ControllerClass -> isScopedController
      ViewClass -> isCustomView
    isScopedController file = if filename file == "ScopedMviController.kt"
      then return False
      else grepFind "ScopedMviController<" file
    isCustomView file = do
      isView <- grepFind "View @JvmOverloads" file
      isAlreadyMigrated <- grepFind "databinding" file
      hasLayout <- grepFind "View.inflate" file
      return $ not isAlreadyMigrated && isView && hasLayout

removeKotlinXImport :: Controller -> IO ()
removeKotlinXImport c = inplaceFilter removePattern (controllerFilePath c)
  where removePattern = invert (has "kotlinx.android.synthetic")

updateConfigObject :: Controller -> IO ()
updateConfigObject c = do
  inplace templateParamPattern (controllerFilePath c)
  inplace layoutResourcePattern (controllerFilePath c)
  where
    bindingName = controllerBindingName c
    templateParamPattern = "Config<ViewState> {" *> pure ("Config<ViewState, " <> bindingName <> "> {")
    layoutResourcePattern = "val viewLayoutResource" *> chars1 *> pure ("val inflater: ViewInflater<" <> bindingName <> "> = " <> bindingName <> "::inflate")

updateInheritedClass :: Controller -> IO ()
updateInheritedClass c = do
  inplace classPattern (controllerFilePath c)
  where
    bindingName = controllerBindingName c
    classPattern = "ScopedMviController<ViewState, " *> pure ("ScopedMviController<ViewState, " <> bindingName <> ", ")

sortDescending :: [Text] -> [Text]
sortDescending = DL.sortBy (flip $ comparing T.length)

replaceViewIdsWithBindingIds :: Controller -> IO ()
replaceViewIdsWithBindingIds c = for_ (sortDescending $ controllerChildViewIds c) $ \viewId -> do
  let idPattern = text viewId *> pure ("binding." <> snakeToCamel viewId)
  inplace idPattern (controllerFilePath c)

removeUnusedRImports :: Controller -> IO ()
removeUnusedRImports c = do
  hasRUsage <- grepFind "R." (controllerFilePath c)
  unless hasRUsage $ inplaceFilter (invert importPattern) (controllerFilePath c)
  where importPattern = text ("import " <> modulePackage m <> ".R")
        m = controllerModule c

removeViewInflation :: Controller -> IO ()
removeViewInflation c = inplaceFilter (invert inflatePattern) (controllerFilePath c)
  where inflatePattern = begins (spaces1 *> "View.inflate")

insertBindingVal :: Controller -> IO ()
insertBindingVal c = inplace bindingPattern (controllerFilePath c)
  where bindingPattern = do
          initLine <- begins (spaces1 <> "init {")
          return $ "  private val binding = " <> controllerBindingName c <> ".inflate(LayoutInflater.from(context), this)" <> "\n\n" <> initLine

insertRequiredImportsC :: Controller -> IO ()
insertRequiredImportsC c = inplace insertPattern (controllerFilePath c)
  where insertPattern = do
          packageLine <- begins "package "
          return $ packageLine <> "\n" <> bindingImport <> "\n" <> inflaterImport
        inflaterImport = "import ru.appkode.base.ui.mvi.core.ViewInflater"
        bindingImport = "import " <> modulePackage m <> ".databinding." <> controllerBindingName c
        m = controllerModule c

insertRequiredImportsV :: Controller -> IO ()
insertRequiredImportsV c = inplace insertPattern (controllerFilePath c)
  where insertPattern = do
          packageLine <- begins "package "
          return $ packageLine <> "\n" <> bindingImport <> "\n" <> inflaterImport
        bindingImport = "import " <> modulePackage m <> ".databinding." <> controllerBindingName c
        inflaterImport = "import android.view.LayoutInflater"
        m = controllerModule c

refactorToViewBinding :: IO ()
refactorToViewBinding = do
  workDir <- pwd
  modules <- findModules (collapse workDir)
  putStrLn $ "Found " <> show (length modules) <> " modules"
  controllers <- join <$> mapM (findControllers ControllerClass) modules
  putStrLn $ "Found " <> show (length controllers) <> " controllers"
  views <- join <$> mapM (findControllers ViewClass) modules
  putStrLn $ "Found " <> show (length views) <> " views"
  for_ controllers $ \c -> do
    removeKotlinXImport c
    updateInheritedClass c
    updateConfigObject c
    replaceViewIdsWithBindingIds c
    insertRequiredImportsC c
    removeUnusedRImports c
  putStrLn $ "Migrated " <> show (length controllers) <> " controllers"
  for_ views $ \v -> do
    removeKotlinXImport v
    replaceViewIdsWithBindingIds v
    insertRequiredImportsV v
    removeUnusedRImports v
    insertBindingVal v
    removeViewInflation v
  putStrLn $ "Migrated " <> show (length views) <> " views"
