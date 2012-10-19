module Setup.Configure
( sharedPath
) where

import Distribution.Simple.Compiler
    ( CompilerFlavor(..), Compiler(compilerId), compilerFlavor )
import Distribution.Package
    ( Package(..) )
import Distribution.PackageDescription as PD
    ( GenericPackageDescription(..), HookedBuildInfo )
import Distribution.Simple.Program
    ( ProgramConfiguration, userSpecifyArgss, userSpecifyPaths )
import Distribution.Simple.Setup
    ( ConfigFlags(..), CopyDest(..), fromFlag, fromFlagOrDefault, flagToMaybe )
import Distribution.Simple.InstallDirs
    ( InstallDirs(..), defaultInstallDirs, combineInstallDirs )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.Utils ( die )
import Distribution.Verbosity
    ( Verbosity, lessVerbose )

import qualified Distribution.Simple.GHC  as GHC
import qualified Distribution.Simple.JHC  as JHC
import qualified Distribution.Simple.LHC  as LHC
import qualified Distribution.Simple.NHC  as NHC
import qualified Distribution.Simple.Hugs as Hugs
import qualified Distribution.Simple.UHC  as UHC

sharedPath
    :: (GenericPackageDescription, HookedBuildInfo)
    -> ConfigFlags -> IO FilePath
sharedPath (pkg_descr0, _) cfg
  = do  let verbosity = fromFlag (configVerbosity cfg)
            programsConfig = userSpecifyArgss (configProgramArgs cfg)
                           . userSpecifyPaths (configProgramPaths cfg)
                           $ configPrograms cfg
            userInstall = fromFlag (configUserInstall cfg)

        -- detect compiler
        (comp, _) <- configCompiler
          (flagToMaybe $ configHcFlavor cfg)
          (flagToMaybe $ configHcPath cfg) (flagToMaybe $ configHcPkg cfg)
          programsConfig (lessVerbose verbosity)
        let flavor  = compilerFlavor comp

        -- installation directories
        defaultDirs <- defaultInstallDirs flavor userInstall True
        let installDirs = combineInstallDirs fromFlagOrDefault
                            defaultDirs (configInstallDirs cfg)

        let dirs = InstallDirs.absoluteInstallDirs
                (packageId pkg_descr0)
                (compilerId comp)
                NoCopyDest
                installDirs
        return $ datadir dirs

-- ----------------------------------------------------------------------------
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
