{-# LANGUAGE OverloadedStrings #-}
module ViewBindingMigration where

import Turtle hiding (fp)
import Prelude hiding (FilePath)
import Data.Maybe (listToMaybe, isJust, fromJust)
import Data.Foldable (for_)
import Control.Monad (filterM)
import System.IO (hClose)
import qualified Control.Foldl as Fold
import qualified Data.Text as T

-- * Find viewLayoutResource, derive binding name
-- * Gather ids from layout file and replace them with binding.id in controller file
-- * Rename ScopedMviControllerOld -> ScopedMviController
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

extractControllerName :: FilePath -> IO (Maybe Text)
extractControllerName file = do
  let matches = listToMaybe . match packagePattern . lineToText <$> input file
  join <$> fold matches (Fold.find isJust)
  where packagePattern = has ("class " *> chars1 <* "Controller :")

extractLayoutResource :: FilePath -> IO (Maybe Text)
extractLayoutResource file = do
  let matches = listToMaybe . match packagePattern . lineToText <$> input file
  join <$> fold matches (Fold.find isJust)
  where packagePattern = has ("viewLayoutResource = R.layout." *> chars1)

snakeToPascal :: Text -> Text
snakeToPascal s = T.concat (map T.toTitle parts)
  where parts = T.splitOn "_" s

snakeToCamel :: Text -> Text
snakeToCamel s = head parts <> T.concat (map T.toTitle (drop 1 parts))
  where parts = T.splitOn "_" s

extractBindingName :: FilePath -> IO (Maybe Text)
extractBindingName file = do
  resource <- extractLayoutResource file
  return ((<> "Binding") . snakeToPascal <$> resource)

findChildViewIds :: Module -> Text -> IO [Text]
findChildViewIds m layoutRes = do
  let filename = moduleDir m </> "src" </> "main" </> "res" </> "layout" </> fromText layoutRes <.> "xml"
  let matches = match viewIdPattern . lineToText <$> input filename
  join <$> fold matches Fold.list
  where viewIdPattern = has (("android:id=\"@+id/" <|> "android:id=\"@id/") *> chars1 <* char '"')

buildController :: Module -> FilePath -> IO Controller
buildController m file = do
  name <- fromJust <$> extractControllerName file
  bindingName <- fromJust <$> extractBindingName file
  resource <- fromJust<$> extractLayoutResource file
  viewIds <- findChildViewIds m resource
  return Controller
    { controllerName = name
    , controllerFilePath = file
    , controllerBindingName = bindingName
    , controllerChildViewIds = viewIds
    , controllerModule = m
    }

findControllers :: Module -> IO [Controller]
findControllers m = do
  files <- fold controllers Fold.list
  scopedControllers <- filterM isScopedController files
  mapM (buildController m) scopedControllers
  where
    controllers = find (ends "Controller.kt") (moduleDir m)
    isScopedController = grepFind "ScopedMviControllerOld<"

renameControllerOldToNew :: Controller -> IO ()
renameControllerOldToNew c = inplace renamePattern (controllerFilePath c)
  where renamePattern = "ScopedMviControllerOld" *> pure "ScopedMviController"

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

replaceViewIdsWithBindingIds :: Controller -> IO ()
replaceViewIdsWithBindingIds c = for_ (controllerChildViewIds c) $ \viewId -> do
  let idPattern = text viewId *> pure ("binding." <> snakeToCamel viewId)
  inplace idPattern (controllerFilePath c)

removeUnusedRImports :: Controller -> IO ()
removeUnusedRImports c = do
  hasRUsage <- grepFind "R." (controllerFilePath c)
  unless hasRUsage $ inplaceFilter (invert importPattern) (controllerFilePath c)
  where importPattern = text ("import " <> modulePackage m <> ".R")
        m = controllerModule c

insertRequiredImports :: Controller -> IO ()
insertRequiredImports c = do
  inplace insertPattern (controllerFilePath c)
  where insertPattern = do
          packageLine <- begins "package "
          return $ packageLine <> "\n" <> bindingImport <> "\n" <> inflaterImport
        inflaterImport = "import ru.appkode.base.ui.mvi.core.ViewInflater"
        bindingImport = "import " <> (modulePackage m) <> ".databinding." <> controllerBindingName c
        m = controllerModule c

refactorToViewBinding :: IO ()
refactorToViewBinding = do
  -- workDir <- pwd
  let workDir = "../casino-android/casino-ui-profile"
  modules <- findModules (collapse workDir)
  controllers <- join <$> mapM findControllers modules
  for_ controllers $ \c -> do
    renameControllerOldToNew c
    removeKotlinXImport c
    updateInheritedClass c
    updateConfigObject c
    replaceViewIdsWithBindingIds c
    insertRequiredImports c
    removeUnusedRImports c
