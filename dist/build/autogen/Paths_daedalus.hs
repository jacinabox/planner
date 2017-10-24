{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_daedalus (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\james\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\james\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.3\\daedalus-0.1.0.0-F3OKWTFeFimLTFTQsoGmU3"
datadir    = "C:\\Users\\james\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.3\\daedalus-0.1.0.0"
libexecdir = "C:\\Users\\james\\AppData\\Roaming\\cabal\\daedalus-0.1.0.0-F3OKWTFeFimLTFTQsoGmU3"
sysconfdir = "C:\\Users\\james\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "daedalus_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "daedalus_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "daedalus_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "daedalus_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "daedalus_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
