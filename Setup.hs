import Data.Text (words, unwords, pack, split, splitOn, unpack, intercalate)
import Data.List (unionBy, nub, zip4, isPrefixOf, isInfixOf)
import System.FilePath (combine)
import System.Directory (createDirectoryIfMissing)

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Verbosity (silent)
import Distribution.Simple.Build (build)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, buildDir)
import Distribution.Simple.PreProcess (PPSuffixHandler, knownSuffixHandlers)
import Distribution.Simple.Program (cppProgram, getDbProgramOutput)
import Distribution.Simple.Program.Db (configureProgram, addKnownProgram, defaultProgramDb)
import Distribution.Simple.Setup


main = defaultMainWithHooks
  simpleUserHooks { buildHook = unstaticGenerate } where
    unstaticGenerate :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
    unstaticGenerate pd lbi uh bf = do
      let
        -- outdir = combine (buildDir lbi) "cbits"  -- dunno, this doesn't seem to work
        outdir = "cbits" -- just write to the source dir instead... bah
        outfile = combine outdir "wayland.c"
        infile = "cbits/wayland.c.in"
      -- let outfile = "cbits/wayland.c"
      -- let outfile = "dist/build/cbits/wayland.c"
      putStrLn ("Removing \"static inline\" from wayland header files to generate " ++ outfile)
      newdb <- configureProgram silent cppProgram (addKnownProgram cppProgram defaultProgramDb)
      inputFile <- readFile infile
      c_preprocessed <- getDbProgramOutput silent cppProgram newdb [infile]

      let pre_funcs = tail $ splitOn (pack "static inline ") $ pack c_preprocessed
          funcs = map (head.splitOn (pack "{")) pre_funcs
          decls = map (head.splitOn (pack "(")) funcs
          returnTypes = map (unpack . Data.Text.unwords . init . Data.Text.words) decls
          names = map (unpack . last . Data.Text.words) decls
          arguments = map (unpack . head.splitOn (pack ")") .head.tail.splitOn (pack "(")) funcs
          argumentNames = map (unpack . intercalate (pack ",") . map (last . split (\x -> x=='*' || x==' ')) . splitOn (pack ",") . pack) arguments

          newFuncs = zip4 returnTypes names arguments argumentNames
          newCFuncs = map (\ (returnType, name, args, argNames) ->
                              returnType ++ " x_"++name++"("++args++")\n" ++
                              "{\n"++
                              "\t"++(if returnType=="void" then "" else "return ")++name++"("++argNames++");\n"++
                              "}\n") newFuncs

          interfaces = filter (isInfixOf "extern const struct wl_interface") (lines c_preprocessed)
          newInterfaces = map (drop (length "extern ")) interfaces

          newCCode = inputFile ++ unlines newInterfaces ++ unlines newCFuncs

      createDirectoryIfMissing True outdir
      writeFile outfile newCCode

      defaultBuildHook pd lbi uh bf -- this could be the start of some hip-hop song


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
