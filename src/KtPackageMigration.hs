{-# LANGUAGE OverloadedStrings #-}

module KtPackageMigration where

import qualified Control.Foldl as Fold
import Data.Bifunctor
import Data.Either (lefts, rights)
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe)
import qualified Data.Text as T
import Turtle hiding (f, fp)
import Prelude hiding (FilePath)

dryRun = False

data Module
  = Module
      { moduleManifest :: FilePath,
        moduleSrcMainPath :: FilePath,
        modulePackage :: Maybe Text,
        moduleName :: Text
      }
  deriving (Show)

extractModuleName :: FilePath -> Text
extractModuleName manifest = T.pack $ encodeString $ dirname $ parent $ parent $ directory manifest

extractRootPath :: FilePath -> FilePath
extractRootPath manifest = directory manifest </> "kotlin"

extractPackage :: FilePath -> IO (Maybe Text)
extractPackage manifest = do
  let matches = listToMaybe . match packagePattern . lineToText <$> input manifest
  join <$> fold matches (Fold.find isJust)
  where
    packagePattern = has ("package=" *> char '"' *> chars1 <* char '"')

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
  rootPathExists <- testdir (moduleSrcMainPath m)
  let hasPackage = isJust (modulePackage m)
  let name = moduleName m
  return
    ( boolToEither manifestExists ("Module:  " <> name <> ". No manifest found") m
        >> boolToEither rootPathExists ("Module:  " <> name <> ". source path does not exist, expected 'src/kotlin'") m
        >> boolToEither hasPackage ("Module:  " <> name <> ". Package not found") m
    )

validateKotlinPackage :: Module -> FilePath -> IO (Either FilePath ())
validateKotlinPackage m fp = do
  let expectedPackage = derivePackageName m fp
  filePackage <- extractPackageFromKt fp
  let isExpected = filePackage == Just expectedPackage
  return $ if isExpected then Right () else Left fp

convertValidationResult :: Module -> Either FilePath () -> Either Text Module
convertValidationResult m = bimap (\errfp -> "Module: " <> name <> "\n\tfile " <> fname errfp <> " has invalid package") (const m)
  where
    name = moduleName m
    fname = stripModulePrefix m

validateKotlinPackages :: Module -> IO (Either Text ())
validateKotlinPackages m = do
  files <- fold (findKotlinFiles (moduleSrcMainPath m)) Fold.list
  validated <- mapM (validateKotlinPackage m) files
  return $ mapM_ (convertValidationResult m) validated

findKotlinFiles :: FilePath -> Shell FilePath
findKotlinFiles = find (ends ".kt")

extractPackageFromKt :: FilePath -> IO (Maybe Text)
extractPackageFromKt file = do
  let matches = listToMaybe . match packagePattern . lineToText <$> input file
  join <$> fold matches (Fold.find isJust)
  where
    packagePattern = has ("package " *> chars1)

toPackageName :: Text -> Text
toPackageName = T.map (\c -> if c == '/' then '.' else c)

packageToFileName :: Text -> FilePath
packageToFileName t = decodeString $ T.unpack $ T.map (\c -> if c == '.' then '/' else c) t

stripModulePrefix :: Module -> FilePath -> Text
stripModulePrefix m fp =
  let -- no easy way to append '/' in the end, have to do this dance...
      srcMainPath = decodeString (encodeString (moduleSrcMainPath m) <> "/")
      relativeDir = directory . fromJust $ stripPrefix srcMainPath fp
   in T.pack $ encodeString (relativeDir </> filename fp)

derivePackageName :: Module -> FilePath -> Text
derivePackageName m fp =
  let rdirname = T.dropWhileEnd (\c -> c == '/' || c == '.') $ stripModulePrefix m (directory fp)
      basePackage = fromJust $ modulePackage m
      relativePackageNameDraft = toPackageName rdirname
      relativePackageName =
        if relativePackageNameDraft == basePackage
          then ""
          else fromMaybe relativePackageNameDraft (T.stripPrefix (basePackage <> ".") relativePackageNameDraft)
   in if T.null relativePackageName
        then basePackage
        else basePackage <> "." <> relativePackageName

isAlreadyMigrated :: Module -> Bool
isAlreadyMigrated _ = False

repackageSourceFiles :: Module -> IO ()
repackageSourceFiles m = do
  topLevelFiles <- fold (ls (moduleSrcMainPath m)) Fold.list
  mapM_
    ( \f -> do
        let relativeFileName = decodeString $ T.unpack $ stripModulePrefix m f
        let targetFile = collapse (moduleSrcMainPath m </> packageToFileName (fromJust $ modulePackage m) </> relativeFileName)
        mktree $ directory targetFile
        mv f targetFile
    )
    topLevelFiles

boolToEither :: Bool -> a -> b -> Either a b
boolToEither test left right = if test then Right right else Left left

printErrorList :: Text -> [Text] -> IO ()
printErrorList title errs = do
  putStrLn ""
  putStrLn $ T.unpack title
  mapM_ (putStrLn . T.unpack . ("  • " <>)) errs

refactorKotlinPackages :: IO ()
refactorKotlinPackages = do
  workDir <- pwd
  modules <- findModules $ collapse workDir
  validatedModules <- traverse validateModule modules
  let validationErrors = lefts validatedModules
  let validModules = rights validatedModules
  packageValidationErrors <- lefts <$> traverse validateKotlinPackages validModules
  unless (null validationErrors) (printErrorList "Found errors, some modules will be skipped: " validationErrors)
  if null packageValidationErrors
    then do
      putStrLn "\nPackage validation: OK"
      unless dryRun $ mapM_ repackageSourceFiles (filter (not . isAlreadyMigrated) validModules)
    else printErrorList "Package errors: " packageValidationErrors
