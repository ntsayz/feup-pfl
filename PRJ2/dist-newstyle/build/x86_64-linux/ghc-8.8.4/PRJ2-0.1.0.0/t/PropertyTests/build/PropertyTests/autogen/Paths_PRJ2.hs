{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_PRJ2 (
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

bindir     = "/home/ntsayz/.cabal/bin"
libdir     = "/home/ntsayz/.cabal/lib/x86_64-linux-ghc-8.8.4/PRJ2-0.1.0.0-inplace-PropertyTests"
dynlibdir  = "/home/ntsayz/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/ntsayz/.cabal/share/x86_64-linux-ghc-8.8.4/PRJ2-0.1.0.0"
libexecdir = "/home/ntsayz/.cabal/libexec/x86_64-linux-ghc-8.8.4/PRJ2-0.1.0.0"
sysconfdir = "/home/ntsayz/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PRJ2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PRJ2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "PRJ2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "PRJ2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PRJ2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PRJ2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
