{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_amm_sim (
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

bindir     = "/home/hatake/.cabal/bin"
libdir     = "/home/hatake/.cabal/lib/x86_64-linux-ghc-8.10.4/amm-sim-0.1.0.0-inplace-amm-sim"
dynlibdir  = "/home/hatake/.cabal/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/hatake/.cabal/share/x86_64-linux-ghc-8.10.4/amm-sim-0.1.0.0"
libexecdir = "/home/hatake/.cabal/libexec/x86_64-linux-ghc-8.10.4/amm-sim-0.1.0.0"
sysconfdir = "/home/hatake/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "amm_sim_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "amm_sim_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "amm_sim_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "amm_sim_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "amm_sim_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "amm_sim_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
