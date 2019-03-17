{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle
import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import Data.Maybe (listToMaybe, isJust)
import Data.Either (lefts)
import qualified Data.Text as T

data Module = Module
  { moduleManifest :: FilePath
  , moduleRootPath :: FilePath
  , modulePackage :: Maybe Text
  , moduleName :: Text
  }
  deriving Show

extractModuleName :: FilePath -> Text
extractModuleName manifest = T.pack $ encodeString $ dirname $ parent $ parent $ directory manifest

extractRootPath :: FilePath -> FilePath
extractRootPath manifest = directory manifest </> "kotlin"

packagePattern :: Pattern Text
packagePattern = has ("package=" *> char '"' *> chars1 <* char '"')

extractPackage :: FilePath -> IO (Maybe Text)
extractPackage manifest = do
  let matches = listToMaybe . match packagePattern . lineToText <$> input manifest
  join <$> fold matches (Fold.find isJust)

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

validateModule :: Module -> IO (Either Text Module)
validateModule m = do
  manifestExists <- testfile (moduleManifest m)
  rootPathExists <- testdir (moduleRootPath m)
  let hasPackage = isJust (modulePackage m)
  let name = moduleName m
  return (
    boolToEither manifestExists ("Module:  " <> name <> ". No manifest found") m >>
      boolToEither rootPathExists ("Module:  " <> name <> ". root path does not exist, expected 'src/kotlin'") m >>
      boolToEither hasPackage ("Module:  " <> name <> ". Package not found") m
    )

boolToEither :: Bool -> a -> b -> Either a b
boolToEither test left right = if test then Right right else Left left

printErrorList :: [Text] -> IO ()
printErrorList errs = do
  putStrLn "Found errors: "
  mapM_ (putStrLn . T.unpack . ("  • " <>)) errs

main :: IO ()
main = do
  modules <- findModules "../tbi-android"
  validationErrors <- lefts <$> traverse validateModule modules
  unless (null validationErrors) (printErrorList validationErrors)
