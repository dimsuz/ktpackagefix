{-# LANGUAGE OverloadedStrings #-}
module ViewBindingMigration where

import Turtle hiding (fp)
import Prelude hiding (FilePath)
import Data.Maybe (listToMaybe, isJust, fromJust)
import Control.Monad (filterM)
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
  , modulePackage :: Maybe Text
  , moduleName :: Text
  }
  deriving Show

data Controller = Controller
  { controllerName :: Text
  , controllerFilePath :: FilePath
  , controllerBindingName :: Text
  , controllerChildViewIds :: [Text]
  }
  deriving Show

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
  package <- extractPackage mf
  return (Module mf (extractRootPath mf) (extractModuleDir mf) package (extractModuleName mf))

findModules :: FilePath -> IO [Module]
findModules dir = do
  files <- fold manifests Fold.list
  mapM buildModule files
  where
    manifests = findtree (invert (has "build")) (find (ends "src/main/AndroidManifest.xml") dir)

isScopedController :: FilePath -> IO Bool
isScopedController file = not . null <$> fold lines Fold.list
  where lines = grep (has "ScopedMviControllerOld<") (input file)

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

buildController :: FilePath -> IO Controller
buildController file = do
  name <- fromJust <$> extractControllerName file
  bindingName <- fromJust <$> extractBindingName file
  return Controller
    { controllerName = name
    , controllerFilePath = file
    , controllerBindingName = bindingName
    , controllerChildViewIds = []
    }

findControllers :: Module -> IO [Controller]
findControllers m = do
  files <- fold controllers Fold.list
  scopedControllers <- filterM isScopedController files
  mapM buildController scopedControllers
  where
    controllers = find (ends "Controller.kt") (moduleDir m)

refactorToViewBinding :: IO ()
refactorToViewBinding = do
  -- workDir <- pwd
  let workDir = "../casino-android/casino-ui-profile"
  modules <- findModules (collapse workDir)
  controllers <- join <$> mapM findControllers modules
  print controllers
