-- This file was generated by `cargo-cabal`, its goal is to define few hooks to
-- call `cargo` on the fly and link correctly the generated library.
--
-- While it's an acceptable hack as this project is currently a prototype, this
-- should be removed before `cargo-cabal` stable release.

import Data.Maybe
import qualified Distribution.PackageDescription as PD
import Distribution.Simple
  ( Args
  , UserHooks (confHook, preConf)
  , defaultMainWithHooks
  , simpleUserHooks
  )
import Distribution.Simple.Program
  ( ghcProgram
  )
import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo (localPkgDescr)
  , buildDir
  , compiler
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
import Distribution.Simple.Utils (rawSystemExit)
import System.Directory (getCurrentDirectory)
import Distribution.Text (display)
import Distribution.Simple.Compiler
import Distribution.System (buildPlatform)
import Distribution.Simple.BuildPaths
  ( mkGenericSharedBundledLibName
  , mkGenericStaticLibName
  , dllExtension
  )

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { buildHook = rustBuildHook
      , cleanHook = rustCleanHook
      }

rustCleanHook
  :: PD.PackageDescription
  -> ()
  -> UserHooks
  -> CleanFlags
  -> IO ()
rustCleanHook desc () hooks flags = do
  pwd <- getCurrentDirectory
  putStrLn $ "cleanup rust build artifacts: delete " <> pwd <> "/target"
  rawSystemExit (fromFlag $ cleanVerbosity flags) "rm" ["-r", "./target"]
  cleanHook simpleUserHooks desc () hooks flags

rustBuildHook
  :: PD.PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> BuildFlags
  -> IO ()
rustBuildHook description localBuildInfo hooks flags = do
  -- run Rust build
  -- FIXME add cargo and rust to program db during configure step
  -- FIXME: add `--target $TARGET` flag to support cross-compiling to $TARGET
  putStrLn "Call `cargo build --release` to build a dependency written in Rust"
  rawSystemExit (fromFlag $ buildVerbosity flags) "cargo" ["build","--release"]

  -- copy static lib
  putStrLn $ "Copy libplonk_verify.a to " <> staticTarget
  rawSystemExit (fromFlag $ buildVerbosity flags) "cp" [staticSource, staticTarget]

  -- copy dyn lib
  putStrLn $ "Copy libplonk_verify.dylib to " <> dynTarget
  rawSystemExit (fromFlag $ buildVerbosity flags) "cp" [dynSource, dynTarget]

  putStrLn "rustc compilation succeeded"
  buildHook simpleUserHooks description localBuildInfo hooks flags
 where
  c = compiler localBuildInfo
  sourceLibname = "plonk_verify"
  targetLibname = "Cplonk_verify"
  sourceBuildDir = "target/release"
  targetBuildDir = buildDir localBuildInfo
  staticSource = sourceBuildDir <> "/" <> mkGenericStaticLibName sourceLibname
  staticTarget = targetBuildDir <> "/" <> mkGenericStaticLibName targetLibname
  dynSource = sourceBuildDir <> "/" <> "lib" <> sourceLibname <> "." <> dllExtension buildPlatform
  dynTarget = targetBuildDir <> "/" <> mkGenericSharedBundledLibName buildPlatform (compilerId c) targetLibname

