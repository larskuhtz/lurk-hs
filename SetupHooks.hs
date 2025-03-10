{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE NoFieldSelectors #-}

module SetupHooks
( setupHooks
) where

import Control.Monad.IO.Class

import Data.ByteString.Builder qualified as BB
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M

import Distribution.Simple.SetupHooks
    ( BuildHooks (preBuildComponentRules)
    , ConfigFlags (configVerbosity)
    , ConfigureHooks (preConfPackageHook)
    , ConfiguredProgram
    , Dependency (..)
    , Dict (Dict)
    , Location (..)
    , PreBuildComponentInputs (buildingWhat, localBuildInfo, targetInfo)
    , PreConfPackageInputs (configFlags, localBuildConfig)
    , PreConfPackageOutputs (extraConfiguredProgs)
    , ProgramDb
    , RuleOutput (..)
    , RulesM
    , SetupHooks(configureHooks, buildHooks)
    , TargetInfo (targetComponent, targetCLBI)
    , Verbosity
    , addRuleMonitors
    , autogenComponentModulesDir
    , buildingWhatVerbosity
    , buildingWhatVerbosity
    , compiler
    , configureUnconfiguredProgram
    , location
    , mkCommand
    , monitorDirectory
    , noBuildHooks
    , noConfigureHooks
    , noPreConfPackageOutputs
    , noSetupHooks
    , registerRule
    , registerRule_
    , rules
    , simpleProgram
    , staticRule
    )
import Distribution.Utils.Path
    ( (<.>)
    , (</>)
    , Build
    , CWD
    , FileOrDir(Dir)
    , Pkg
    , SymbolicPath
    , absoluteWorkingDir
    , getSymbolicPath
    , interpretSymbolicPath
    , interpretSymbolicPathAbsolute
    , makeRelativePathEx
    , makeSymbolicPath
    , moduleNameSymbolicPath
    , unsafeCoerceSymbolicPath
    )
import Distribution.Simple.Flag (fromFlag)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program (runProgramCwd)
import Distribution.Simple.Program.Db (lookupProgramByName)
import Distribution.Types.LocalBuildConfig
import Distribution.Simple.BuildPaths
    ( dllExtension
    , mkGenericSharedBundledLibName
    , mkGenericSharedLibName
    , mkGenericStaticLibName
    )
import Distribution.System (buildPlatform)
import System.Directory (makeRelativeToCurrentDirectory)
import Distribution.Simple.GHC (installLib)
import Distribution.Simple.Utils (installExecutableFile)
import Distribution.Simple
    ( Compiler(..)
    , CompilerId(..)
    )
import Distribution.Simple.Utils
    ( notice
    , die'
    )
import Distribution.ModuleName
import System.FilePath qualified as FP (takeFileName)

-- -------------------------------------------------------------------------- --
-- Settings

rustProjectDir :: FilePath
rustProjectDir = "rust"

-- | the module name of the generated Haskell module for the library. The value
-- is the Haskell module name without file extension.
--
-- This module must exist as a depedency to trigger the rust build. It can be
-- empty or it can be used, for instance, to export FFI calls.
--
triggerModuleName :: ModuleName
triggerModuleName = "Rust"

rustLibName :: String
rustLibName = "plonk_verify"

cargoName :: String
cargoName = "cargo"

install_name_toolName :: String
install_name_toolName = "install_name_tool"

-- -------------------------------------------------------------------------- --
-- Hooks

setupHooks :: SetupHooks
setupHooks = noSetupHooks
    { configureHooks = noConfigureHooks
        { preConfPackageHook = Just configureCargo
        }
    , buildHooks = noBuildHooks
        { preBuildComponentRules = Just $ rules (static ()) cargoBuildRule
        }
    }

-- -------------------------------------------------------------------------- --
-- Configure Rust Build Tools

configureCargo :: PreConfPackageInputs -> IO PreConfPackageOutputs
configureCargo pcpi = do
    extraPrograms <- traverse (configureProgram verbosity progDb)
        [ cargoName
        , install_name_toolName
        ]
    return $ (noPreConfPackageOutputs pcpi)
        { extraConfiguredProgs = M.fromList extraPrograms
        }
  where
    cfg = pcpi.configFlags

    lbc :: LocalBuildConfig
    lbc = pcpi.localBuildConfig
    verbosity = fromFlag $ configVerbosity cfg
    progDb = lbc.withPrograms

configureProgram
    :: Verbosity
    -> ProgramDb
    -> String
    -> IO (String, ConfiguredProgram)
configureProgram verbosity progDb name =
    configureUnconfiguredProgram verbosity (simpleProgram name) progDb >>= \case
        Nothing -> die' verbosity "program not found"
        Just x -> return (name, x)

-- -------------------------------------------------------------------------- --
-- Build-Hook Rules
--
-- Rules currently don't fire on extra bundled libraries, but only on Haskell
-- sources and rule outputs.
--
-- We work around this be requireing that an module must be generated for each
-- external library. Conveniently, this module can provide FFI calls for that
-- library.
--
-- We also recommend that the library itself is build as a public internal
-- library, so that it can be used by different components without the need to
-- build and install it again for each component.
--

cargoBuildRule :: PreBuildComponentInputs -> RulesM ()
cargoBuildRule pbci = do

    cargoProg <- case lookupProgramByName cargoName progDb of
        Nothing -> liftIO $
            die' verbosity "The program cargo is not configured for use by Cabal. This is a bug in the SetupHooks script"
        Just x -> return x

    install_name_toolProg <- case lookupProgramByName install_name_toolName progDb of
        Nothing -> liftIO $
            die' verbosity "The program install_name_tool is not configured for use by Cabal. This is a bug in the SetupHooks script"
        Just x -> return x

    addRuleMonitors [ monitorDirectory "src" ]

    r0 <- registerRule "rust:cargo" $
        staticRule (cargoCmd cargoProg)
            []
            (staticSource NE.:| [ dynSource ])

    r1 <- registerRule "rust:install:static" $
        staticRule (installStaticCmd staticSource staticTarget)
            [RuleDependency (RuleOutput r0 0)]
            (NE.singleton staticTarget)

    r2 <- registerRule "rust:install:dynamic" $
        staticRule (installDynCmd install_name_toolProg dynSource dynTarget)
            [RuleDependency (RuleOutput r0 1)]
            (NE.singleton dynTarget)

    r3 <- registerRule "rust:install:dynamic_" $
        staticRule (installDynCmd install_name_toolProg dynSource dynTarget_)
            [RuleDependency (RuleOutput r0 1)]
            (NE.singleton dynTarget_)

    registerRule_ "rust:trigger" $
        staticRule (triggerCmd triggerModuleLoc)
            [ RuleDependency (RuleOutput r1 0)
            , RuleDependency (RuleOutput r2 0)
            , RuleDependency (RuleOutput r3 0)
            ]
            (NE.singleton triggerModuleLoc)

  where
    verbosity = buildingWhatVerbosity $ pbci.buildingWhat
    progDb = pbci.localBuildInfo.localBuildConfig.withPrograms

    cid = compilerId $ Distribution.Simple.LocalBuildInfo.compiler $ pbci.localBuildInfo

    -- Do we need to consider this? When would this be not just 'Nothing'?
    mbWorkDir = mbWorkDirLBI $ pbci.localBuildInfo

    cargoCmd prog = mkCommand (static Dict) (static runCargo)
        ( prog
        , verbosity
        , rustSourceDir
        , rustBuildDir
        )
    installStaticCmd s d = mkCommand
        (static Dict)
        (static installStatic)
        (verbosity, s, d)
    installDynCmd p s d = mkCommand
        (static Dict)
        (static installDyn)
        (p, verbosity, s, d)
    triggerCmd m = mkCommand
        (static Dict)
        (static triggerModule)
        (verbosity, m)

    -- -------------- --
    -- Paths

    autoGenPath = autogenComponentModulesDir pbci.localBuildInfo pbci.targetInfo.targetCLBI

    rustSourceDir = makeSymbolicPath @Pkg rustProjectDir

    -- base builddir for this component within dist-newstyle
    localBuildPath = buildDir $ pbci.localBuildInfo

    -- extra bundled libraries are per component, so we should build them in the
    -- scope of a component?
    -- Or should we rather treat them like an component on their own?
    compBuildPath = componentBuildDir pbci.localBuildInfo pbci.targetInfo.targetCLBI

    -- directory where build artifacts are placed
    artifactsDir = compBuildPath

    -- The target directory for rust builds (within dist-newstyle)
    rustBuildDir = artifactsDir </> makeRelativePathEx "rust-target"

    -- directory where cargo places build artificts
    rustArtifactDir = rustBuildDir </> makeRelativePathEx "release"

    -- -------------- --
    -- Autogenerated Haskell Modules

    triggerModuleLoc = Location autoGenPath $
        moduleNameSymbolicPath triggerModuleName <.> "hs"

    -- -------------- --
    -- Rust Artifacts

    -- We need to ensure that the following works:
    --
    -- * static linking of application binaries and
    -- * dynamic linking for repl and template Haskell.
    --
    -- (note, that GHC requires all libraries to be be present in TH
    -- computations even when the splice does not depend on it)
    --
    -- This needs to work for
    -- * the package itself,
    -- * package that directly depend on this package via Hackage, local path,
    --   or source-repository-package stanzas, and
    -- * packages that depend on this package indirectly.

    targetLibName = "C" <> rustLibName
    rustArtifact n = Location rustArtifactDir $ makeRelativePathEx n
    rustInstall n = Location compBuildPath $ makeRelativePathEx n

    -- Static library names
    staticSource = rustArtifact $ mkGenericStaticLibName rustLibName
    staticTarget = rustInstall $ mkGenericStaticLibName targetLibName

    -- Dynamic library names
    dynSource = rustArtifact $ "lib" <> rustLibName <.> dllExtension buildPlatform
    dynTargetFileName = mkGenericSharedBundledLibName buildPlatform cid targetLibName
    dynTarget = rustInstall $ dynTargetFileName

    -- work around for cabal picking the wrong name during local builds with
    -- template Haskell and in the repl
    dynTargetFileName_ = mkGenericSharedLibName buildPlatform cid targetLibName
    dynTarget_ = rustInstall $ dynTargetFileName_

data RustProj
type RustProjectDir = SymbolicPath Pkg ('Dir RustProj)
data RustBuild
type RustBuildDir = SymbolicPath Pkg ('Dir RustBuild)

-- -------------------------------------------------------------------------- --
-- Build Commands

-- | Build the Rust project
--
runCargo
    :: (ConfiguredProgram, Verbosity, RustProjectDir, RustBuildDir)
    -> IO ()
runCargo (prog, verbosity, sourceDir, targetDir) = do
    pkgDir <- absoluteWorkingDir Nothing
    let targetDir' = interpretSymbolicPathAbsolute pkgDir targetDir
    runProgramCwd verbosity (Just sourceDir') prog
        [ "build"
        , "--release"
        , "--target-dir", targetDir'
        ]
  where
    -- I think, it is fair to assume that the current CWD is the the package
    -- directory.
    sourceDir' = unsafeCoerceSymbolicPath sourceDir

-- | Move static Rust library to the location where Cabal expects them
--
installStatic
    :: (Verbosity, Location, Location)
    -> IO ()
installStatic (verbosity, source, dest) = do
    notice verbosity $ "install " <> show source' <> " to " <> show dest'
    installExecutableFile verbosity source' dest'
  where
    libname = FP.takeFileName source'
    source' = interpretSymbolicPath Nothing $ location source
    dest' = interpretSymbolicPath Nothing $ location dest

-- | Move Rust libraries to the location where Cabal expects them
--
installDyn
    :: (ConfiguredProgram, Verbosity, Location, Location)
    -> IO ()
installDyn (prog, verbosity, source, dest) = do
    notice verbosity $ "install " <> show source' <> " to " <> show dest'
    installExecutableFile verbosity source' dest'
    runProgramCwd verbosity Nothing prog
        [ "-id"
        , "@rpath" </> libname
        , dest'
        ]
  where
    libname = FP.takeFileName dest'
    source' = interpretSymbolicPath Nothing $ location source
    dest' = interpretSymbolicPath Nothing $ location dest

-- | In order for the build to trigger there must a rule that declares the
-- dependency on an autogenerate module. By default that module is empty. But
-- one could also use it to expose FII calls.
--
triggerModule
    :: (Verbosity, Location)
    -> IO ()
triggerModule (verbosity, l) = do
    notice verbosity "create trigger module"
    writeFile path
        $ "module " <> fromString modname <> "\n"
        <> "(abc\n"
        <> ") where\n"
        <> "\n"
        <> "abc :: Int\n"
        <> "abc = 2"
        -- TODOD add FFI calls
  where
    path = interpretSymbolicPath Nothing $ location l

    modname = L.intercalate "." (components triggerModuleName)

