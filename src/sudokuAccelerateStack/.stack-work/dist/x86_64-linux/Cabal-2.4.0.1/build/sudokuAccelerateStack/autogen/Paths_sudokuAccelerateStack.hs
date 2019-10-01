{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_sudokuAccelerateStack (
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

bindir     = "/home/david/Documents/haskell/hoi/sudokus/src/sudokuAccelerateStack/.stack-work/install/x86_64-linux/f3674ca6fedc37ce939407406a507a9f4bd6f285faf357f678adc5e1a7aa4ea9/8.6.5/bin"
libdir     = "/home/david/Documents/haskell/hoi/sudokus/src/sudokuAccelerateStack/.stack-work/install/x86_64-linux/f3674ca6fedc37ce939407406a507a9f4bd6f285faf357f678adc5e1a7aa4ea9/8.6.5/lib/x86_64-linux-ghc-8.6.5/sudokuAccelerateStack-0.1.0.0-FJBRyYl4bh2KEeHpHr9wCp-sudokuAccelerateStack"
dynlibdir  = "/home/david/Documents/haskell/hoi/sudokus/src/sudokuAccelerateStack/.stack-work/install/x86_64-linux/f3674ca6fedc37ce939407406a507a9f4bd6f285faf357f678adc5e1a7aa4ea9/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/david/Documents/haskell/hoi/sudokus/src/sudokuAccelerateStack/.stack-work/install/x86_64-linux/f3674ca6fedc37ce939407406a507a9f4bd6f285faf357f678adc5e1a7aa4ea9/8.6.5/share/x86_64-linux-ghc-8.6.5/sudokuAccelerateStack-0.1.0.0"
libexecdir = "/home/david/Documents/haskell/hoi/sudokus/src/sudokuAccelerateStack/.stack-work/install/x86_64-linux/f3674ca6fedc37ce939407406a507a9f4bd6f285faf357f678adc5e1a7aa4ea9/8.6.5/libexec/x86_64-linux-ghc-8.6.5/sudokuAccelerateStack-0.1.0.0"
sysconfdir = "/home/david/Documents/haskell/hoi/sudokus/src/sudokuAccelerateStack/.stack-work/install/x86_64-linux/f3674ca6fedc37ce939407406a507a9f4bd6f285faf357f678adc5e1a7aa4ea9/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sudokuAccelerateStack_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sudokuAccelerateStack_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sudokuAccelerateStack_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sudokuAccelerateStack_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sudokuAccelerateStack_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sudokuAccelerateStack_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
