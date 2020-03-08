{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_1JC3_Assign1 (
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

bindir     = "/Users/kanwarsandhu/Downloads/1JC3-Assign1-2/.stack-work/install/x86_64-osx/d973ddee07bd4c81d4605d7c5424f8dd27b327e78520a61bf074565917221957/8.6.5/bin"
libdir     = "/Users/kanwarsandhu/Downloads/1JC3-Assign1-2/.stack-work/install/x86_64-osx/d973ddee07bd4c81d4605d7c5424f8dd27b327e78520a61bf074565917221957/8.6.5/lib/x86_64-osx-ghc-8.6.5/1JC3-Assign1-0.1.0.0-8lHWDVeKzPf85BX0VTYQRH"
dynlibdir  = "/Users/kanwarsandhu/Downloads/1JC3-Assign1-2/.stack-work/install/x86_64-osx/d973ddee07bd4c81d4605d7c5424f8dd27b327e78520a61bf074565917221957/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/kanwarsandhu/Downloads/1JC3-Assign1-2/.stack-work/install/x86_64-osx/d973ddee07bd4c81d4605d7c5424f8dd27b327e78520a61bf074565917221957/8.6.5/share/x86_64-osx-ghc-8.6.5/1JC3-Assign1-0.1.0.0"
libexecdir = "/Users/kanwarsandhu/Downloads/1JC3-Assign1-2/.stack-work/install/x86_64-osx/d973ddee07bd4c81d4605d7c5424f8dd27b327e78520a61bf074565917221957/8.6.5/libexec/x86_64-osx-ghc-8.6.5/1JC3-Assign1-0.1.0.0"
sysconfdir = "/Users/kanwarsandhu/Downloads/1JC3-Assign1-2/.stack-work/install/x86_64-osx/d973ddee07bd4c81d4605d7c5424f8dd27b327e78520a61bf074565917221957/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "1JC3_Assign1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "1JC3_Assign1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "1JC3_Assign1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "1JC3_Assign1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "1JC3_Assign1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "1JC3_Assign1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
