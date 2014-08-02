module Paths_lambda_calculator (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/greghale/Programming/lambda/haskell/.cabal-sandbox/bin"
libdir     = "/home/greghale/Programming/lambda/haskell/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/lambda-calculator-0.1.0.0"
datadir    = "/home/greghale/Programming/lambda/haskell/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/lambda-calculator-0.1.0.0"
libexecdir = "/home/greghale/Programming/lambda/haskell/.cabal-sandbox/libexec"
sysconfdir = "/home/greghale/Programming/lambda/haskell/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lambda_calculator_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lambda_calculator_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lambda_calculator_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lambda_calculator_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lambda_calculator_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
