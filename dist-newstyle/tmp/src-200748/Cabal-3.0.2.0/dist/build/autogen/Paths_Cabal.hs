{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Cabal (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [3,0,2,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ryan/.cabal/store/ghc-8.6.5/Cabal-3.0.2.0-b8550634597a8e937a50e21f04fdf39ab34ebdd0442db0caca9fe4722f1dfaab/bin"
libdir     = "/home/ryan/.cabal/store/ghc-8.6.5/Cabal-3.0.2.0-b8550634597a8e937a50e21f04fdf39ab34ebdd0442db0caca9fe4722f1dfaab/lib"
dynlibdir  = "/home/ryan/.cabal/store/ghc-8.6.5/Cabal-3.0.2.0-b8550634597a8e937a50e21f04fdf39ab34ebdd0442db0caca9fe4722f1dfaab/lib"
datadir    = "/home/ryan/.cabal/store/ghc-8.6.5/Cabal-3.0.2.0-b8550634597a8e937a50e21f04fdf39ab34ebdd0442db0caca9fe4722f1dfaab/share"
libexecdir = "/home/ryan/.cabal/store/ghc-8.6.5/Cabal-3.0.2.0-b8550634597a8e937a50e21f04fdf39ab34ebdd0442db0caca9fe4722f1dfaab/libexec"
sysconfdir = "/home/ryan/.cabal/store/ghc-8.6.5/Cabal-3.0.2.0-b8550634597a8e937a50e21f04fdf39ab34ebdd0442db0caca9fe4722f1dfaab/etc"

getBinDir     = catchIO (getEnv "Cabal_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Cabal_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Cabal_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Cabal_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Cabal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Cabal_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
