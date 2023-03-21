{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_VigierFRL (
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

bindir     = "/mnt/c/Users/usuario/Desktop/Vallejos Vigier V2952-1 ALP/.stack-work/install/x86_64-linux-tinfo6/e672ace75d5b12841ead5bbbf0977b313129a403009a8b7e780236561b3d1997/9.0.2/bin"
libdir     = "/mnt/c/Users/usuario/Desktop/Vallejos Vigier V2952-1 ALP/.stack-work/install/x86_64-linux-tinfo6/e672ace75d5b12841ead5bbbf0977b313129a403009a8b7e780236561b3d1997/9.0.2/lib/x86_64-linux-ghc-9.0.2/VigierFRL-0.1.0.0-4pdkmmYtiTjGq1Wmdq4wx6"
dynlibdir  = "/mnt/c/Users/usuario/Desktop/Vallejos Vigier V2952-1 ALP/.stack-work/install/x86_64-linux-tinfo6/e672ace75d5b12841ead5bbbf0977b313129a403009a8b7e780236561b3d1997/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/mnt/c/Users/usuario/Desktop/Vallejos Vigier V2952-1 ALP/.stack-work/install/x86_64-linux-tinfo6/e672ace75d5b12841ead5bbbf0977b313129a403009a8b7e780236561b3d1997/9.0.2/share/x86_64-linux-ghc-9.0.2/VigierFRL-0.1.0.0"
libexecdir = "/mnt/c/Users/usuario/Desktop/Vallejos Vigier V2952-1 ALP/.stack-work/install/x86_64-linux-tinfo6/e672ace75d5b12841ead5bbbf0977b313129a403009a8b7e780236561b3d1997/9.0.2/libexec/x86_64-linux-ghc-9.0.2/VigierFRL-0.1.0.0"
sysconfdir = "/mnt/c/Users/usuario/Desktop/Vallejos Vigier V2952-1 ALP/.stack-work/install/x86_64-linux-tinfo6/e672ace75d5b12841ead5bbbf0977b313129a403009a8b7e780236561b3d1997/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "VigierFRL_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "VigierFRL_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "VigierFRL_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "VigierFRL_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "VigierFRL_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "VigierFRL_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
