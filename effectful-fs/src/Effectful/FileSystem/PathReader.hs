{-# LANGUAGE CPP #-}

-- | Provides an effect for reading paths.
--
-- @since 0.1
module Effectful.FileSystem.PathReader
  ( -- * Effect
    PathReaderEffect (..),
    Path,
    listDirectory,
    getDirectoryContents,
    getCurrentDirectory,
    getHomeDirectory,
    getXdgDirectory,
    getXdgDirectoryList,
    getAppUserDataDirectory,
    getUserDocumentsDirectory,
    getTemporaryDirectory,
    getFileSize,
    canonicalizePath,
    makeAbsolute,
    makeRelativeToCurrentDirectory,
    doesPathExist,
    doesFileExist,
    doesDirectoryExist,
    findExecutable,
    findExecutables,
    findExecutablesInDirectories,
    findFile,
    findFiles,
    findFileWith,
    findFilesWith,
    pathIsSymbolicLink,
    getSymbolicLinkTarget,
    getPermissions,
    getAccessTime,
    getModificationTime,

    -- ** Handlers
    runPathReaderIO,

    -- * Xdg Utils
    getXdgConfig,

    -- * Re-exports
    XdgDirectory (..),
    XdgDirectoryList (..),
    Permissions (..),
    UTCTime (..),
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time (UTCTime (..))
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import Effectful.FileSystem.Path (Path)
import System.Directory
  ( Permissions (..),
    XdgDirectory (..),
    XdgDirectoryList (..),
  )
#if MIN_VERSION_filepath(1,4,100) && MIN_VERSION_directory(1,3,8)
import System.Directory.OsPath qualified as Dir
#else
import System.Directory qualified as Dir
#endif

-- | Effect for reading paths.
--
-- @since 0.1
data PathReaderEffect :: Effect where
  ListDirectory :: Path -> PathReaderEffect m [Path]
  GetDirectoryContents :: Path -> PathReaderEffect m [Path]
  GetCurrentDirectory :: PathReaderEffect m Path
  GetHomeDirectory :: PathReaderEffect m Path
  GetXdgDirectory :: XdgDirectory -> Path -> PathReaderEffect m Path
  GetXdgDirectoryList :: XdgDirectoryList -> PathReaderEffect m [Path]
  GetAppUserDataDirectory :: Path -> PathReaderEffect m Path
  GetUserDocumentsDirectory :: PathReaderEffect m Path
  GetTemporaryDirectory :: PathReaderEffect m Path
  GetFileSize :: Path -> PathReaderEffect m Integer
  CanonicalizePath :: Path -> PathReaderEffect m Path
  MakeAbsolute :: Path -> PathReaderEffect m Path
  MakeRelativeToCurrentDirectory :: Path -> PathReaderEffect m Path
  DoesPathExist :: Path -> PathReaderEffect m Bool
  DoesFileExist :: Path -> PathReaderEffect m Bool
  DoesDirectoryExist :: Path -> PathReaderEffect m Bool
  FindExecutable :: String -> PathReaderEffect m (Maybe Path)
  FindExecutables :: String -> PathReaderEffect m [Path]
  FindExecutablesInDirectories :: [Path] -> String -> PathReaderEffect m [Path]
  FindFile :: [Path] -> String -> PathReaderEffect m (Maybe Path)
  FindFiles :: [Path] -> String -> PathReaderEffect m [Path]
  FindFileWith :: (Path -> m Bool) -> [Path] -> String -> PathReaderEffect m (Maybe Path)
  FindFilesWith :: (Path -> m Bool) -> [Path] -> String -> PathReaderEffect m [Path]
  PathIsSymbolicLink :: Path -> PathReaderEffect m Bool
  GetSymbolicLinkTarget :: Path -> PathReaderEffect m Path
  GetPermissions :: Path -> PathReaderEffect m Permissions
  GetAccessTime :: Path -> PathReaderEffect m UTCTime
  GetModificationTime :: Path -> PathReaderEffect m UTCTime

-- | @since 0.1
type instance DispatchOf PathReaderEffect = Dynamic

-- | Runs 'PathReaderEffect' in 'IO'.
--
-- @since 0.1
runPathReaderIO ::
  ( IOE :> es
  ) =>
  Eff (PathReaderEffect : es) a ->
  Eff es a
runPathReaderIO = interpret $ \env -> \case
  ListDirectory p -> liftIO $ Dir.listDirectory p
  GetDirectoryContents p -> liftIO $ Dir.getDirectoryContents p
  GetCurrentDirectory -> liftIO Dir.getCurrentDirectory
  GetHomeDirectory -> liftIO Dir.getHomeDirectory
  GetXdgDirectory xdg p -> liftIO $ Dir.getXdgDirectory xdg p
  GetXdgDirectoryList xdg -> liftIO $ Dir.getXdgDirectoryList xdg
  GetAppUserDataDirectory p -> liftIO $ Dir.getAppUserDataDirectory p
  GetUserDocumentsDirectory -> liftIO Dir.getUserDocumentsDirectory
  GetTemporaryDirectory -> liftIO Dir.getTemporaryDirectory
  GetFileSize p -> liftIO $ Dir.getFileSize p
  CanonicalizePath p -> liftIO $ Dir.canonicalizePath p
  MakeAbsolute p -> liftIO $ Dir.makeAbsolute p
  MakeRelativeToCurrentDirectory p -> liftIO $ Dir.makeRelativeToCurrentDirectory p
  DoesPathExist p -> liftIO $ Dir.doesPathExist p
  DoesFileExist p -> liftIO $ Dir.doesFileExist p
  DoesDirectoryExist p -> liftIO $ Dir.doesDirectoryExist p
  FindExecutable p -> liftIO $ Dir.findExecutable p
  FindExecutables p -> liftIO $ Dir.findExecutables p
  FindExecutablesInDirectories ps str -> liftIO $ Dir.findExecutablesInDirectories ps str
  FindFile ps str -> liftIO $ Dir.findFile ps str
  FindFiles ps str -> liftIO $ Dir.findFiles ps str
  FindFileWith f ps str -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ Dir.findFileWith (runInIO . f) ps str
  FindFilesWith f ps str -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ Dir.findFilesWith (runInIO . f) ps str
  PathIsSymbolicLink p -> liftIO $ Dir.pathIsSymbolicLink p
  GetSymbolicLinkTarget p -> liftIO $ Dir.getSymbolicLinkTarget p
  GetPermissions p -> liftIO $ Dir.getPermissions p
  GetAccessTime p -> liftIO $ Dir.getAccessTime p
  GetModificationTime p -> liftIO $ Dir.getModificationTime p

-- | @since 0.1
listDirectory ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es [Path]
listDirectory = send . ListDirectory

-- | @since 0.1
getDirectoryContents ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es [Path]
getDirectoryContents = send . GetDirectoryContents

-- | @since 0.1
getCurrentDirectory ::
  ( PathReaderEffect :> es
  ) =>
  Eff es Path
getCurrentDirectory = send GetCurrentDirectory

-- | @since 0.1
getHomeDirectory ::
  ( PathReaderEffect :> es
  ) =>
  Eff es Path
getHomeDirectory = send GetHomeDirectory

-- | @since 0.1
getXdgDirectory ::
  ( PathReaderEffect :> es
  ) =>
  XdgDirectory ->
  Path ->
  Eff es Path
getXdgDirectory xdg = send . GetXdgDirectory xdg

-- | @since 0.1
getXdgDirectoryList ::
  ( PathReaderEffect :> es
  ) =>
  XdgDirectoryList ->
  Eff es [Path]
getXdgDirectoryList = send . GetXdgDirectoryList

-- | @since 0.1
getAppUserDataDirectory ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Path
getAppUserDataDirectory = send . GetAppUserDataDirectory

-- | @since 0.1
getUserDocumentsDirectory ::
  ( PathReaderEffect :> es
  ) =>
  Eff es Path
getUserDocumentsDirectory = send GetUserDocumentsDirectory

-- | @since 0.1
getTemporaryDirectory ::
  ( PathReaderEffect :> es
  ) =>
  Eff es Path
getTemporaryDirectory = send GetTemporaryDirectory

-- | @since 0.1
getFileSize ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Integer
getFileSize = send . GetFileSize

-- | @since 0.1
canonicalizePath ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Path
canonicalizePath = send . CanonicalizePath

-- | @since 0.1
makeAbsolute ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Path
makeAbsolute = send . MakeAbsolute

-- | @since 0.1
makeRelativeToCurrentDirectory ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Path
makeRelativeToCurrentDirectory = send . MakeRelativeToCurrentDirectory

-- | @since 0.1
doesPathExist ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Bool
doesPathExist = send . DoesPathExist

-- | @since 0.1
doesFileExist ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Bool
doesFileExist = send . DoesFileExist

-- | @since 0.1
doesDirectoryExist ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Bool
doesDirectoryExist = send . DoesDirectoryExist

-- | @since 0.1
findExecutable ::
  ( PathReaderEffect :> es
  ) =>
  String ->
  Eff es (Maybe Path)
findExecutable = send . FindExecutable

-- | @since 0.1
findExecutables ::
  ( PathReaderEffect :> es
  ) =>
  String ->
  Eff es [Path]
findExecutables = send . FindExecutables

-- | @since 0.1
findExecutablesInDirectories ::
  ( PathReaderEffect :> es
  ) =>
  [Path] ->
  String ->
  Eff es [Path]
findExecutablesInDirectories ps = send . FindExecutablesInDirectories ps

-- | @since 0.1
findFile ::
  ( PathReaderEffect :> es
  ) =>
  [Path] ->
  String ->
  Eff es (Maybe Path)
findFile ps = send . FindFile ps

-- | @since 0.1
findFiles ::
  ( PathReaderEffect :> es
  ) =>
  [Path] ->
  String ->
  Eff es [Path]
findFiles ps = send . FindFiles ps

-- | @since 0.1
findFileWith ::
  ( PathReaderEffect :> es
  ) =>
  (Path -> Eff es Bool) ->
  [Path] ->
  String ->
  Eff es (Maybe Path)
findFileWith f ps = send . FindFileWith f ps

-- | @since 0.1
findFilesWith ::
  ( PathReaderEffect :> es
  ) =>
  (Path -> Eff es Bool) ->
  [Path] ->
  String ->
  Eff es [Path]
findFilesWith f ps = send . FindFilesWith f ps

-- | @since 0.1
pathIsSymbolicLink ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Bool
pathIsSymbolicLink = send . PathIsSymbolicLink

-- | @since 0.1
getSymbolicLinkTarget ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Path
getSymbolicLinkTarget = send . GetSymbolicLinkTarget

-- | @since 0.1
getPermissions ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Permissions
getPermissions = send . GetPermissions

-- | @since 0.1
getAccessTime ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es UTCTime
getAccessTime = send . GetAccessTime

-- | @since 0.1
getModificationTime ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es UTCTime
getModificationTime = send . GetModificationTime

-- | Retrieves the Xdg Config directory.
--
-- @since 0.1
getXdgConfig ::
  ( PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Path
getXdgConfig = getXdgDirectory XdgConfig
