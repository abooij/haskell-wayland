import Data.Functor
import Data.Text (unwords, pack, splitOn)
import Data.Text.IO (writeFile)
import Data.List (unionBy)
import System.FilePath
import System.Directory
import System.Process

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Verbosity (silent)
import Distribution.Simple.Build
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Simple.Utils
import Distribution.Simple.Setup


main = defaultMainWithHooks
  simpleUserHooks { buildHook = unstaticGenerate } where
    -- unstaticGenerate :: Args -> BuildFlags -> IO HookedBuildInfo
    -- unstaticGenerate :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    -- unstaticGenerate :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    unstaticGenerate :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
    -- unstaticGenerate args cf pd lbi = do
    unstaticGenerate pd lbi uh bf = do
      -- let outfile = combine (buildDir lbi) "cbits/wayland.c"
      let outfile = "cbits/wayland.c"
      -- let outfile = "dist/build/cbits/wayland.c"
      putStrLn ("Removing \"static inline\" from wayland header files to generate " ++ outfile)
      -- getDirectoryContents (buildDir lbi) >>= print
      -- let newdb = addKnownProgram cppProgram defaultProgramDb
      newdb <- configureProgram silent cppProgram (addKnownProgram cppProgram defaultProgramDb)
      c_preprocessed <- getDbProgramOutput silent cppProgram newdb [outfile++".in"]
      let unstaticed = Data.Text.unwords $ splitOn (pack "static inline") $ pack c_preprocessed
      Data.Text.IO.writeFile outfile unstaticed      --createDirectory "dist/build"

      defaultBuildHook pd lbi uh bf
      -- return emptyHookedBuildInfo


                                    -- PreProcessor {
                                    --   platformIndependent = False,
                                    --   runPreProcessor = mkSimplePreProcessor $ \infile outfile verbosity ->
                                    --     do info verbosity ("Removing \"static inline\" from " ++ infile ++ " to generate " ++ outfile)
                                    --
                                    --        let unstaticed = Data.Text.unwords $ splitOn (pack "static inline") $ pack c_preprocessed
                                    --        Data.Text.IO.writeFile outfile unstaticed
                                    --   }

-- simpleUserHooks { postConf = unstaticGenerate } where
--   unstaticGenerate :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
--   unstaticGenerate args cf pd lbi = do

defaultBuildHook :: PackageDescription -> LocalBuildInfo
        -> UserHooks -> BuildFlags -> IO ()
defaultBuildHook pkg_descr localbuildinfo hooks flags =
  build pkg_descr localbuildinfo flags (allSuffixHandlers hooks)


allSuffixHandlers :: UserHooks
                  -> [PPSuffixHandler]
allSuffixHandlers hooks
    = overridesPP (hookedPreProcessors hooks) knownSuffixHandlers
    where
      overridesPP :: [PPSuffixHandler] -> [PPSuffixHandler] -> [PPSuffixHandler]
      overridesPP = unionBy (\x y -> fst x == fst y)
