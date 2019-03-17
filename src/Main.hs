{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle
import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import Data.Maybe (listToMaybe)

data Module = Module
  { moduleManifest :: FilePath
  , moduleRootPath :: FilePath
  , modulePackage :: Text
  }
  deriving Show

extractRootPath :: FilePath -> FilePath
extractRootPath manifest = manifest </> "src" </> "main"

extractPackage :: FilePath -> Text
extractPackage manifest = "com.example"

packagePattern :: Pattern Text
packagePattern = has ("package=" *> char '"' *> chars1 <* char '"')

extractPackage' :: FilePath -> IO (Maybe Text)
extractPackage' manifest = do
  let matches = listToMaybe . match packagePattern . lineToText <$> input manifest
  join <$> fold matches Fold.head

findModules :: FilePath -> IO [Module]
findModules dir = do
  files <- fold manifests Fold.list
  return $ map (\mf -> Module mf (extractRootPath mf) (extractPackage mf)) files
  where
    manifests = findtree (invert (has "build")) (find (ends "AndroidManifest.xml") dir)

main :: IO ()
main = do
  modules <- findModules "../tbi-android"
  print modules
  putStrLn "hello world"
