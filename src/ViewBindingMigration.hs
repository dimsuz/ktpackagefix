{-# LANGUAGE OverloadedStrings #-}
module ViewBindingMigration where

import Turtle hiding (fp)
import Prelude hiding (FilePath)
import Data.Maybe (listToMaybe, isJust)
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
  , controllerBindingName :: FilePath
  , controllerChildViewIds :: [Text]
  }
  deriving Show

extractModuleName :: FilePath -> Text
extractModuleName manifest = T.pack $ encodeString $ dirname $ parent $ parent $ directory manifest

extractModuleDir :: FilePath -> FilePath
extractModuleDir manifest = parent $ parent $ directory manifest

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

extractControllerName :: FilePath -> IO (Maybe)

buildController :: FilePath -> IO Controller
buildController file = do
  return Controller
    { controllerName = "hello"
    , controllerFilePath = file
    , controllerBindingName = file
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
