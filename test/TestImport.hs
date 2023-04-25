{-# LANGUAGE CPP #-}

module TestImport where

import StaticLS.StaticEnv as StaticEnv
import StaticLS.StaticEnv.Options as StaticEnv
import System.Directory (makeAbsolute)
import System.FilePath ((</>))
import qualified TestImport.TestData as Test

initStaticEnv :: IO StaticEnv
initStaticEnv = do
    initStaticEnvOpts defaultTestStaticEnvOptions

#if __GLASGOW_HASKELL__ >= 906
ghcVerDir :: FilePath
ghcVerDir =
        "ghc961/"
#else
ghcVerDir :: FilePath
ghcVerDir =
        "ghc944/"
#endif

-- | For testing errors on different versions of ghc
#if __GLASGOW_HASKELL__ == 904
notGhcVerDir :: FilePath
notGhcVerDir =
        "ghc961/"
#else
notGhcVerDir :: FilePath
notGhcVerDir =
        "ghc944/"
#endif

defaultTestStaticEnvOptions :: StaticEnvOptions
defaultTestStaticEnvOptions =
    staticEnvOptionsGhcVer ghcVerDir

badGhcTestStaticEnvOptions :: StaticEnvOptions
badGhcTestStaticEnvOptions =
    staticEnvOptionsGhcVer notGhcVerDir

staticEnvOptionsGhcVer :: FilePath -> StaticEnvOptions
staticEnvOptionsGhcVer ghcDir =
    StaticEnvOptions
        { optionHieDbPath = Just (hiedbDirGhcVer ghcDir)
        , optionHieFilesPath = Just (hieDirGhcVer ghcDir)
        }

hiedbDirGhcVer :: FilePath -> FilePath
hiedbDirGhcVer ghcDir = Test.testDataRoot </> ghcDir </> ".hiedb/"

hieDirGhcVer :: FilePath -> FilePath
hieDirGhcVer ghcDir = Test.testDataRoot </> ghcDir </> ".hiefiles/"

initStaticEnvOpts :: StaticEnvOptions -> IO StaticEnv
initStaticEnvOpts options = do
    wsRoot <- makeAbsolute "."
    StaticEnv.initStaticEnv wsRoot options
