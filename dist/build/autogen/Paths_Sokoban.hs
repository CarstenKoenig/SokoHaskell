module Paths_Sokoban (
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

bindir     = "/home/carsten/.cabal/bin"
libdir     = "/home/carsten/.cabal/lib/Sokoban-0.1/ghc-7.4.1"
datadir    = "/home/carsten/.cabal/share/Sokoban-0.1"
libexecdir = "/home/carsten/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Sokoban_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Sokoban_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Sokoban_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Sokoban_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
