module Paths_assembly_interpreter_haskell (
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

bindir     = "/home/balintkiss/.cabal/bin"
libdir     = "/home/balintkiss/.cabal/lib/x86_64-linux-ghc-7.10.3/assembly-interpreter-haskell-0.1.0.0-HqNzKU7eyKm4ZjcnAHbnEx"
datadir    = "/home/balintkiss/.cabal/share/x86_64-linux-ghc-7.10.3/assembly-interpreter-haskell-0.1.0.0"
libexecdir = "/home/balintkiss/.cabal/libexec"
sysconfdir = "/home/balintkiss/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "assembly_interpreter_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "assembly_interpreter_haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "assembly_interpreter_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "assembly_interpreter_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "assembly_interpreter_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
