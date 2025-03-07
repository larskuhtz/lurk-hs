import Control.Monad
import Data.Maybe
import qualified Distribution.PackageDescription as PD
import Distribution.Simple
  ( Args
  , UserHooks (confHook, preConf)
  , defaultMainWithHooks
  , simpleUserHooks
  )
import Distribution.Simple.BuildPaths
  ( mkGenericSharedBundledLibName
  , mkGenericSharedLibName
  , mkGenericStaticLibName
  , dllExtension
  )
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo (localPkgDescr)
  , buildDir
  , compiler
  )
import Distribution.Simple.Program
  ( Program
  , ProgramDb
  , addKnownPrograms
  , configureAllKnownPrograms
  , defaultProgramDb
  , ghcProgram
  , runDbProgram
  , simpleProgram
  )
import Distribution.Simple.Setup
  ( BuildFlags (buildVerbosity)
  , ConfigFlags (configVerbosity)
  , CleanFlags(..)
  , fromFlag
  )
import Distribution.Simple.UserHooks
  ( UserHooks (buildHook, confHook, cleanHook)
  )
import Distribution.Simple.Utils
  ( notice
  , createDirectoryIfMissingVerbose
  , info
  , installExecutableFile
  )
import Distribution.System (buildPlatform, buildOS, OS(..))
import Distribution.Text (display)
import System.Directory (getCurrentDirectory)

-- -------------------------------------------------------------------------- --
-- main

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks { buildHook = rustBuildHook }

-- -------------------------------------------------------------------------- --
-- Build Tools

cargoProgram :: Program
cargoProgram = simpleProgram "cargo"

install_name_toolProgram :: Program
install_name_toolProgram = simpleProgram "install_name_tool"

programDb :: ProgramDb
programDb = addKnownPrograms
    [cargoProgram, install_name_toolProgram]
    defaultProgramDb

-- -------------------------------------------------------------------------- --

rustBuildHook
  :: PD.PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> BuildFlags
  -> IO ()
rustBuildHook description localBuildInfo hooks flags = do
  pdb <- configureAllKnownPrograms verbosity programDb

  -- run Rust build
  -- FIXME: add `--target $TARGET` flag to support cross-compiling to $TARGET
  notice verbosity "Call `cargo build --release` to build a dependency written in Rust"
  runDbProgram verbosity cargoProgram pdb
    [ "build"
    , "--release"
    , "--target-dir", rustTargetDir
    ]

  -- Install build results into cabal build directory
  createDirectoryIfMissingVerbose verbosity True artifactsDir
  installExecutableFile verbosity staticSource staticTarget
  installExecutableFile verbosity staticSource staticTarget_
  installExecutableFile verbosity dynSource dynTarget
  installExecutableFile verbosity dynSource dynTarget_

  when (buildOS == OSX) $ do
    runDbProgram verbosity install_name_toolProgram pdb
        [ "-id"
        , "@rpath" </> dynTargetFileName
        , dynTarget
        ]
    runDbProgram verbosity install_name_toolProgram pdb
        [ "-id"
        , "@rpath" </> dynTargetFileName_
        , dynTarget_
        ]

  info verbosity "rustc compilation succeeded"
  buildHook simpleUserHooks description localBuildInfo hooks flags
 where
  verbosity = fromFlag $ buildVerbosity flags
  cid = compilerId $ compiler localBuildInfo

  -- base builddir for this component within dist-newstyle
  localBuildDir = buildDir localBuildInfo

  -- directory where build artifacts are placed
  artifactsDir = localBuildDir

  -- library name stem
  sourceLibname = "plonk_verify"
  targetLibname = "C" <> sourceLibname

  -- The target directory for rust builds
  rustTargetDir = localBuildDir </> "rust-target"
  -- directory where Rust builds place build artificts
  rustArtifactDir = rustTargetDir <> "/release"

  -- Static library names
  staticSource = rustArtifactDir </> mkGenericStaticLibName sourceLibname
  staticTarget = artifactsDir </> mkGenericStaticLibName targetLibname
  staticTarget_ = artifactsDir </> mkGenericStaticLibName targetLibname

  -- Dynamic library names
  dynSource = rustArtifactDir </> "lib" <> sourceLibname <.> dllExtension buildPlatform
  dynTargetFileName = mkGenericSharedBundledLibName buildPlatform cid targetLibname
  dynTarget = artifactsDir </> dynTargetFileName

  -- work around for cabal picking the wrong name during local builds with
  -- template Haskell and in the repl
  dynTargetFileName_ = mkGenericSharedLibName buildPlatform cid targetLibname
  dynTarget_ = artifactsDir </> dynTargetFileName_

-- -------------------------------------------------------------------------- --
-- Utils

(</>) :: FilePath -> FilePath -> FilePath
a </> b = a <> "/" <> b

(<.>) :: FilePath -> FilePath -> FilePath
a <.> b = a <> "." <> b

