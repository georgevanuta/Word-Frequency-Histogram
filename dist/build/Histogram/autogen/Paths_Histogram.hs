{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Histogram (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/george/.cabal/bin"
libdir     = "/home/george/.cabal/lib/x86_64-linux-ghc-8.6.5/Histogram-0.1.0.0-DpWs4xx1wZC7P1xrVhFkM3-Histogram"
dynlibdir  = "/home/george/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/george/.cabal/share/x86_64-linux-ghc-8.6.5/Histogram-0.1.0.0"
libexecdir = "/home/george/.cabal/libexec/x86_64-linux-ghc-8.6.5/Histogram-0.1.0.0"
sysconfdir = "/home/george/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Histogram_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Histogram_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Histogram_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Histogram_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Histogram_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Histogram_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
