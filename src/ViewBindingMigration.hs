{-# LANGUAGE OverloadedStrings #-}
module ViewBindingMigration where

import Turtle hiding (fp)
import Prelude hiding (FilePath)
import Data.Maybe (listToMaybe, isJust)
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
  return (Module mf (extractRootPath mf) package (extractModuleName mf))

findModules :: FilePath -> IO [Module]
findModules dir = do
  files <- fold manifests Fold.list
  mapM buildModule files
  where
    manifests = findtree (invert (has "build")) (find (ends "src/main/AndroidManifest.xml") dir)

findControllers :: Module -> IO [Controller]
findControllers _ = return []

refactorToViewBinding :: IO ()
refactorToViewBinding = do
  -- workDir <- pwd
  let workDir = "../casino-android"
  modules <- findModules $ collapse workDir
  controllers <- join <$> mapM findControllers modules
  print controllers
