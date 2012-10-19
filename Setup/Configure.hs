-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Configure
-- Copyright   :  Isaac Jones 2003-2005
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This deals with the /configure/ phase. It provides the 'configure' action
-- which is given the package description and configure flags. It then tries
-- to: configure the compiler; resolves any conditionals in the package
-- description; resolve the package dependencies; check if all the extensions
-- used by this package are supported by the compiler; check that all the build
-- tools are available (including version checks if appropriate); checks for
-- any required @pkg-config@ packages (updating the 'BuildInfo' with the
-- results)
--
-- Then based on all this it saves the info in the 'LocalBuildInfo' and writes
-- it out to the @dist\/setup-config@ file. It also displays various details to
-- the user, the amount of information displayed depending on the verbosity
-- level.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Setup.Configure
( sharedPath
) where

import Control.Applicative ((<$>))
import System.Exit
import qualified System.Info
import qualified Control.Exception as Exception

import Distribution.Simple.Compiler
    ( CompilerFlavor(..), Compiler(compilerId), compilerFlavor, compilerVersion
    , showCompilerId, unsupportedLanguages, unsupportedExtensions
    , PackageDB(..), PackageDBStack )
import Distribution.Package
    ( PackageName(PackageName), PackageIdentifier(..), PackageId
    , packageName, packageVersion, Package(..)
    , Dependency(Dependency), simplifyDependency
    , InstalledPackageId(..) )
import Distribution.InstalledPackageInfo as Installed
    ( InstalledPackageInfo, InstalledPackageInfo_(..)
    , emptyInstalledPackageInfo )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.PackageDescription as PD
    ( PackageDescription(..), specVersion, GenericPackageDescription(..)
    , Library(..), hasLibs, Executable(..), BuildInfo(..), allExtensions
    , HookedBuildInfo, updatePackageDescription, allBuildInfo
    , FlagName(..), TestSuite(..), Benchmark(..), flagName )
import Distribution.PackageDescription.Configuration
    ( finalizePackageDescription, mapTreeData )
import Distribution.PackageDescription.Check
    ( PackageCheck(..), checkPackage, checkPackageFiles )
import Distribution.Simple.Hpc ( enableCoverage )
import Distribution.Simple.Program
    ( Program(..), ProgramLocation(..), ConfiguredProgram(..)
    , ProgramConfiguration, defaultProgramConfiguration
    , configureAllKnownPrograms, knownPrograms, lookupKnownProgram
    , userSpecifyArgss, userSpecifyPaths
    , requireProgram, requireProgramVersion
    , pkgConfigProgram, gccProgram, rawSystemProgramStdoutConf )
import Distribution.Simple.Setup
    ( ConfigFlags(..), CopyDest(..), fromFlag, fromFlagOrDefault, flagToMaybe )
import Distribution.Simple.InstallDirs
    ( InstallDirs(..), defaultInstallDirs, combineInstallDirs )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo(..), ComponentLocalBuildInfo(..)
    , absoluteInstallDirs, prefixRelativeInstallDirs, inplacePackageId
    , allComponentsBy, Component(..), foldComponent, ComponentName(..) )
import Distribution.Simple.BuildPaths
    ( autogenModulesDir )
import Distribution.Simple.Utils
    ( die, warn, info, setupMessage, createDirectoryIfMissingVerbose
    , intercalate, cabalVersion
    , withFileContents, writeFileAtomic
    , withTempFile )
import Distribution.System
    ( OS(..), buildOS, buildPlatform )
import Distribution.Version
         ( Version(..), anyVersion, orLaterVersion, withinRange, isAnyVersion )
import Distribution.Verbosity
    ( Verbosity, lessVerbose )

import qualified Distribution.Simple.GHC  as GHC
import qualified Distribution.Simple.JHC  as JHC
import qualified Distribution.Simple.LHC  as LHC
import qualified Distribution.Simple.NHC  as NHC
import qualified Distribution.Simple.Hugs as Hugs
import qualified Distribution.Simple.UHC  as UHC

import Control.Monad
    ( when, unless, foldM, filterM, forM )
import Data.List
    ( nub, partition, isPrefixOf, inits, find )
import Data.Maybe
    ( isNothing, catMaybes, mapMaybe )
import Data.Monoid
    ( Monoid(..) )
import Data.Graph
    ( SCC(..), graphFromEdges, transposeG, vertices, stronglyConnCompR )
import System.Directory
    ( doesFileExist, getModificationTime, createDirectoryIfMissing, getTemporaryDirectory )
import System.Exit
    ( ExitCode(..), exitWith )
import System.FilePath
    ( (</>), isAbsolute )
import qualified System.Info
    ( compilerName, compilerVersion )
import System.IO
    ( hPutStrLn, stderr, hClose )
import Distribution.Text
    ( Text(disp), display, simpleParse )
import Text.PrettyPrint
    ( comma, punctuate, render, nest, sep )

import Prelude hiding (catch)

sharedPath
    :: (GenericPackageDescription, HookedBuildInfo)
    -> ConfigFlags -> IO FilePath
sharedPath (pkg_descr0, pbi) cfg
  = do  let distPref = fromFlag (configDistPref cfg)
            buildDir' = distPref </> "build"
            verbosity = fromFlag (configVerbosity cfg)

        let programsConfig = userSpecifyArgss (configProgramArgs cfg)
                           . userSpecifyPaths (configProgramPaths cfg)
                           $ configPrograms cfg
            userInstall = fromFlag (configUserInstall cfg)
            packageDbs = implicitPackageDbStack userInstall
                           (flagToMaybe $ configPackageDB cfg)

        -- detect compiler
        (comp, programsConfig') <- configCompiler
          (flagToMaybe $ configHcFlavor cfg)
          (flagToMaybe $ configHcPath cfg) (flagToMaybe $ configHcPkg cfg)
          programsConfig (lessVerbose verbosity)
        let version = compilerVersion comp
            flavor  = compilerFlavor comp

        -- Create a PackageIndex that makes *any libraries that might be*
        -- defined internally to this package look like installed packages, in
        -- case an executable should refer to any of them as dependencies.
        --
        -- It must be *any libraries that might be* defined rather than the
        -- actual definitions, because these depend on conditionals in the .cabal
        -- file, and we haven't resolved them yet.  finalizePackageDescription
        -- does the resolution of conditionals, and it takes internalPackageSet
        -- as part of its input.
        --
        -- Currently a package can define no more than one library (which has
        -- the same name as the package) but we could extend this later.
        -- If we later allowed private internal libraries, then here we would
        -- need to pre-scan the conditional data to make a list of all private
        -- libraries that could possibly be defined by the .cabal file.
        let pid = packageId pkg_descr0
            internalPackage = emptyInstalledPackageInfo {
                --TODO: should use a per-compiler method to map the source
                --      package ID into an installed package id we can use
                --      for the internal package set. The open-codes use of
                --      InstalledPackageId . display here is a hack.
                Installed.installedPackageId = InstalledPackageId $ display $ pid,
                Installed.sourcePackageId = pid
              }
            internalPackageSet = PackageIndex.fromList [internalPackage]
        installedPackageSet <- getInstalledPackages (lessVerbose verbosity) comp
                                      packageDbs programsConfig'

        let -- Constraint test function for the solver
            dependencySatisfiable =
                not . null . PackageIndex.lookupDependency pkgs'
              where
                pkgs' = PackageIndex.insert internalPackage installedPackageSet
            enableTest t = t { testEnabled = fromFlag (configTests cfg) }
            flaggedTests = map (\(n, t) -> (n, mapTreeData enableTest t))
                               (condTestSuites pkg_descr0)
            enableBenchmark bm = bm { benchmarkEnabled = fromFlag (configBenchmarks cfg) }
            flaggedBenchmarks = map (\(n, bm) -> (n, mapTreeData enableBenchmark bm))
                               (condBenchmarks pkg_descr0)
            pkg_descr0'' = pkg_descr0 { condTestSuites = flaggedTests
                                      , condBenchmarks = flaggedBenchmarks }

        (pkg_descr0', flags) <-
                case finalizePackageDescription
                       (configConfigurationsFlags cfg)
                       dependencySatisfiable
                       Distribution.System.buildPlatform
                       (compilerId comp)
                       (configConstraints cfg)
                       pkg_descr0''
                of Right r -> return r
                   Left missing ->
                       die $ "At least the following dependencies are missing:\n"
                         ++ (render . nest 4 . sep . punctuate comma
                                    . map (disp . simplifyDependency)
                                    $ missing)

        -- add extra include/lib dirs as specified in cfg
        -- we do it here so that those get checked too
        let pkg_descr =
                enableCoverage (fromFlag (configLibCoverage cfg)) distPref
                $ addExtraIncludeLibDirs pkg_descr0'

        -- installation directories
        defaultDirs <- defaultInstallDirs flavor userInstall (hasLibs pkg_descr)
        let installDirs = combineInstallDirs fromFlagOrDefault
                            defaultDirs (configInstallDirs cfg)

        let dirs = InstallDirs.absoluteInstallDirs
                (packageId pkg_descr)
                (compilerId comp)
                NoCopyDest
                installDirs
        return $ datadir dirs
    where
      addExtraIncludeLibDirs pkg_descr =
          let extraBi = mempty { extraLibDirs = configExtraLibDirs cfg
                               , PD.includeDirs = configExtraIncludeDirs cfg}
              modifyLib l        = l{ libBuildInfo = libBuildInfo l `mappend` extraBi }
              modifyExecutable e = e{ buildInfo    = buildInfo e    `mappend` extraBi}
          in pkg_descr{ library     = modifyLib        `fmap` library pkg_descr
                      , executables = modifyExecutable  `map` executables pkg_descr}

-- -----------------------------------------------------------------------------
-- Configuring package dependencies

getInstalledPackages :: Verbosity -> Compiler
                     -> PackageDBStack -> ProgramConfiguration
                     -> IO PackageIndex
getInstalledPackages verbosity comp packageDBs progconf = do
  info verbosity "Reading installed packages..."
  case compilerFlavor comp of
    GHC -> GHC.getInstalledPackages verbosity packageDBs progconf
    Hugs->Hugs.getInstalledPackages verbosity packageDBs progconf
    JHC -> JHC.getInstalledPackages verbosity packageDBs progconf
    LHC -> LHC.getInstalledPackages verbosity packageDBs progconf
    NHC -> NHC.getInstalledPackages verbosity packageDBs progconf
    UHC -> UHC.getInstalledPackages verbosity comp packageDBs progconf
    flv -> die $ "don't know how to find the installed packages for "
              ++ display flv

-- | Currently the user interface specifies the package dbs to use with just a
-- single valued option, a 'PackageDB'. However internally we represent the
-- stack of 'PackageDB's explictly as a list. This function converts encodes
-- the package db stack implicit in a single packagedb.
--
implicitPackageDbStack :: Bool -> Maybe PackageDB -> PackageDBStack
implicitPackageDbStack userInstall maybePackageDB
  | userInstall = GlobalPackageDB : UserPackageDB : extra
  | otherwise   = GlobalPackageDB : extra
  where
    extra = case maybePackageDB of
      Just (SpecificPackageDB db) -> [SpecificPackageDB db]
      _                           -> []

-- -----------------------------------------------------------------------------
-- Determining the compiler details

configCompiler :: Maybe CompilerFlavor -> Maybe FilePath -> Maybe FilePath
               -> ProgramConfiguration -> Verbosity
               -> IO (Compiler, ProgramConfiguration)
configCompiler Nothing _ _ _ _ = die "Unknown compiler"
configCompiler (Just hcFlavor) hcPath hcPkg conf verbosity = do
  case hcFlavor of
      GHC  -> GHC.configure  verbosity hcPath hcPkg conf
      JHC  -> JHC.configure  verbosity hcPath hcPkg conf
      LHC  -> do (_,ghcConf) <- GHC.configure  verbosity Nothing hcPkg conf
                 LHC.configure  verbosity hcPath Nothing ghcConf
      Hugs -> Hugs.configure verbosity hcPath hcPkg conf
      NHC  -> NHC.configure  verbosity hcPath hcPkg conf
      UHC  -> UHC.configure  verbosity hcPath hcPkg conf
      _    -> die "Unknown compiler"
