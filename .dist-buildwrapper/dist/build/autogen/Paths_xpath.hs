module Paths_xpath (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/odr/.cabal/bin"
libdir     = "/home/odr/.cabal/lib/xpath-0.1/ghc-7.4.1"
datadir    = "/home/odr/.cabal/share/xpath-0.1"
libexecdir = "/home/odr/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "xpath_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "xpath_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "xpath_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xpath_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
