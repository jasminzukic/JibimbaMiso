{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Jibimba (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/saaffa/Haskell/JibimbaMiso/.stack-work/install/x86_64-linux/lts-7.1/ghcjs-0.2.1.9007001_ghc-8.0.1/bin"
libdir     = "/home/saaffa/Haskell/JibimbaMiso/.stack-work/install/x86_64-linux/lts-7.1/ghcjs-0.2.1.9007001_ghc-8.0.1/lib/x86_64-linux-ghcjs-0.2.1.9007001-ghc8_0_1/Jibimba-0.1.0.0"
datadir    = "/home/saaffa/Haskell/JibimbaMiso/.stack-work/install/x86_64-linux/lts-7.1/ghcjs-0.2.1.9007001_ghc-8.0.1/share/x86_64-linux-ghcjs-0.2.1.9007001-ghc8_0_1/Jibimba-0.1.0.0"
libexecdir = "/home/saaffa/Haskell/JibimbaMiso/.stack-work/install/x86_64-linux/lts-7.1/ghcjs-0.2.1.9007001_ghc-8.0.1/libexec"
sysconfdir = "/home/saaffa/Haskell/JibimbaMiso/.stack-work/install/x86_64-linux/lts-7.1/ghcjs-0.2.1.9007001_ghc-8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Jibimba_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Jibimba_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Jibimba_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Jibimba_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Jibimba_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
