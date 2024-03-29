-- | Provides a dynamic effect for the readable portion of "System.Directory"'s
-- interface.
--
-- @since 0.1
module Effectful.FileSystem.PathReader.Dynamic
  ( -- * Effect
    PathReaderDynamic (..),

    -- ** Functions
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
    findFileWith,
    findFilesWith,
    pathIsSymbolicLink,
    getSymbolicLinkTarget,
    getPermissions,
    getAccessTime,
    getModificationTime,

    -- ** Handlers
    runPathReaderDynamicIO,

    -- * Functions
    findFile,
    findFiles,

    -- ** XDG Utils
    getXdgData,
    getXdgConfig,
    getXdgCache,
    getXdgState,

    -- * Path Types
    PathType (..),

    -- ** Functions
    PR.Utils.displayPathType,
    getPathType,
    isPathType,
    throwIfWrongPathType,

    -- ** Optics
    PR.Utils._PathTypeFile,
    PR.Utils._PathTypeDirectory,
    PR.Utils._PathTypeSymbolicLink,
    PR.Utils._PathTypeOther,

    -- * Misc
    listDirectoryRecursive,
    listDirectoryRecursiveSymbolicLink,
    doesSymbolicLinkExist,
    pathIsSymbolicDirectoryLink,
    pathIsSymbolicFileLink,

    -- * Re-exports
    OsPath,
    Permissions,
    UTCTime (..),
    XdgDirectory (..),
    XdgDirectoryList (..),
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time (UTCTime (UTCTime, utctDay, utctDayTime))
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import Effectful.Exception (catchIOError)
import Effectful.FileSystem.PathReader.Utils
  ( PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeOther,
        PathTypeSymbolicLink
      ),
  )
import Effectful.FileSystem.PathReader.Utils qualified as PR.Utils
import Effectful.FileSystem.Utils (OsPath, (</>))
import Effectful.FileSystem.Utils qualified as Utils
import GHC.IO.Exception (IOErrorType (InappropriateType))
import System.Directory
  ( Permissions,
    XdgDirectory (XdgCache, XdgConfig, XdgData, XdgState),
    XdgDirectoryList (XdgConfigDirs, XdgDataDirs),
  )
import System.Directory.OsPath qualified as Dir
import System.IO.Error qualified as IO.Error

-- | Dynamic effect for reading paths.
--
-- @since 0.1
data PathReaderDynamic :: Effect where
  ListDirectory :: OsPath -> PathReaderDynamic m [OsPath]
  GetDirectoryContents :: OsPath -> PathReaderDynamic m [OsPath]
  GetCurrentDirectory :: PathReaderDynamic m OsPath
  GetHomeDirectory :: PathReaderDynamic m OsPath
  GetXdgDirectory :: XdgDirectory -> OsPath -> PathReaderDynamic m OsPath
  GetXdgDirectoryList :: XdgDirectoryList -> PathReaderDynamic m [OsPath]
  GetAppUserDataDirectory :: OsPath -> PathReaderDynamic m OsPath
  GetUserDocumentsDirectory :: PathReaderDynamic m OsPath
  GetTemporaryDirectory :: PathReaderDynamic m OsPath
  GetFileSize :: OsPath -> PathReaderDynamic m Integer
  CanonicalizePath :: OsPath -> PathReaderDynamic m OsPath
  MakeAbsolute :: OsPath -> PathReaderDynamic m OsPath
  MakeRelativeToCurrentDirectory :: OsPath -> PathReaderDynamic m OsPath
  DoesPathExist :: OsPath -> PathReaderDynamic m Bool
  DoesFileExist :: OsPath -> PathReaderDynamic m Bool
  DoesDirectoryExist :: OsPath -> PathReaderDynamic m Bool
  FindExecutable :: OsPath -> PathReaderDynamic m (Maybe OsPath)
  FindExecutables :: OsPath -> PathReaderDynamic m [OsPath]
  FindExecutablesInDirectories ::
    [OsPath] ->
    OsPath ->
    PathReaderDynamic m [OsPath]
  FindFileWith ::
    (OsPath -> m Bool) ->
    [OsPath] ->
    OsPath ->
    PathReaderDynamic m (Maybe OsPath)
  FindFilesWith ::
    (OsPath -> m Bool) ->
    [OsPath] ->
    OsPath ->
    PathReaderDynamic m [OsPath]
  PathIsSymbolicLink :: OsPath -> PathReaderDynamic m Bool
  GetSymbolicLinkTarget :: OsPath -> PathReaderDynamic m OsPath
  GetPermissions :: OsPath -> PathReaderDynamic m Permissions
  GetAccessTime :: OsPath -> PathReaderDynamic m UTCTime
  GetModificationTime :: OsPath -> PathReaderDynamic m UTCTime

-- | @since 0.1
type instance DispatchOf PathReaderDynamic = Dynamic

-- | Runs 'PathReaderDynamic' in 'IO'.
--
-- @since 0.1
runPathReaderDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (PathReaderDynamic : es) a ->
  Eff es a
runPathReaderDynamicIO = interpret $ \env -> \case
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
  FindExecutablesInDirectories ps str ->
    liftIO $ Dir.findExecutablesInDirectories ps str
  FindFileWith f ps str -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ Dir.findFileWith (runInIO . f) ps str
  FindFilesWith f ps str -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ Dir.findFilesWith (runInIO . f) ps str
  PathIsSymbolicLink p -> liftIO $ Dir.pathIsSymbolicLink p
  GetSymbolicLinkTarget p -> liftIO $ Dir.getSymbolicLinkTarget p
  GetPermissions p -> liftIO $ Dir.getPermissions p
  GetAccessTime p -> liftIO $ Dir.getAccessTime p
  GetModificationTime p -> liftIO $ Dir.getModificationTime p

-- | Search through the given list of directories for the given file.
--
-- The behavior is equivalent to 'findFileWith', returning only the first
-- occurrence. Details can be found in the documentation of 'findFileWith'.
--
-- @since 0.1
findFile :: (PathReaderDynamic :> es) => [OsPath] -> OsPath -> Eff es (Maybe OsPath)
findFile = findFileWith (\_ -> pure True)

-- | Search through the given list of directories for the given file and
-- returns all paths where the given file exists.
--
-- The behavior is equivalent to 'findFilesWith'. Details can be found in the
-- documentation of 'findFilesWith'.
--
-- @since 0.1
findFiles :: (PathReaderDynamic :> es) => [OsPath] -> OsPath -> Eff es [OsPath]
findFiles = findFilesWith (\_ -> pure True)

-- | Lifted 'Dir.listDirectory'.
--
-- @since 0.1
listDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es [OsPath]
listDirectory = send . ListDirectory

-- | Lifted 'Dir.getDirectoryContents'.
--
-- @since 0.1
getDirectoryContents ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es [OsPath]
getDirectoryContents = send . GetDirectoryContents

-- | Lifted 'Dir.getCurrentDirectory'.
--
-- @since 0.1
getCurrentDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  Eff es OsPath
getCurrentDirectory = send GetCurrentDirectory

-- | Lifted 'Dir.getHomeDirectory'.
--
-- @since 0.1
getHomeDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  Eff es OsPath
getHomeDirectory = send GetHomeDirectory

-- | Lifted 'Dir.getXdgDirectory'.
--
-- @since 0.1
getXdgDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  XdgDirectory ->
  OsPath ->
  Eff es OsPath
getXdgDirectory xdg = send . GetXdgDirectory xdg

-- | Lifted 'Dir.getXdgDirectoryList'.
--
-- @since 0.1
getXdgDirectoryList ::
  ( PathReaderDynamic :> es
  ) =>
  XdgDirectoryList ->
  Eff es [OsPath]
getXdgDirectoryList = send . GetXdgDirectoryList

-- | Lifted 'Dir.getAppUserDataDirectory'.
--
-- @since 0.1
getAppUserDataDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es OsPath
getAppUserDataDirectory = send . GetAppUserDataDirectory

-- | Lifted 'Dir.getUserDocumentsDirectory'.
--
-- @since 0.1
getUserDocumentsDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  Eff es OsPath
getUserDocumentsDirectory = send GetUserDocumentsDirectory

-- | Lifted 'Dir.getTemporaryDirectory'.
--
-- @since 0.1
getTemporaryDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  Eff es OsPath
getTemporaryDirectory = send GetTemporaryDirectory

-- | Lifted 'Dir.getFileSize'.
--
-- @since 0.1
getFileSize ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es Integer
getFileSize = send . GetFileSize

-- | Lifted 'Dir.canonicalizePath'.
--
-- @since 0.1
canonicalizePath ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es OsPath
canonicalizePath = send . CanonicalizePath

-- | Lifted 'Dir.makeAbsolute'.
--
-- @since 0.1
makeAbsolute ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es OsPath
makeAbsolute = send . MakeAbsolute

-- | Lifted 'Dir.makeRelativeToCurrentDirectory'.
--
-- @since 0.1
makeRelativeToCurrentDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es OsPath
makeRelativeToCurrentDirectory = send . MakeRelativeToCurrentDirectory

-- | Lifted 'Dir.doesPathExist'.
--
-- @since 0.1
doesPathExist ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es Bool
doesPathExist = send . DoesPathExist

-- | Lifted 'Dir.doesFileExist'.
--
-- @since 0.1
doesFileExist ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es Bool
doesFileExist = send . DoesFileExist

-- | Lifted 'Dir.doesDirectoryExist'.
--
-- @since 0.1
doesDirectoryExist ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es Bool
doesDirectoryExist = send . DoesDirectoryExist

-- | Lifted 'Dir.findExecutable'.
--
-- @since 0.1
findExecutable ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es (Maybe OsPath)
findExecutable = send . FindExecutable

-- | Lifted 'Dir.findExecutables'.
--
-- @since 0.1
findExecutables ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es [OsPath]
findExecutables = send . FindExecutables

-- | Lifted 'Dir.findExecutablesInDirectories'.
--
-- @since 0.1
findExecutablesInDirectories ::
  ( PathReaderDynamic :> es
  ) =>
  [OsPath] ->
  OsPath ->
  Eff es [OsPath]
findExecutablesInDirectories ps = send . FindExecutablesInDirectories ps

-- | Lifted 'Dir.findFileWith'.
--
-- @since 0.1
findFileWith ::
  ( PathReaderDynamic :> es
  ) =>
  (OsPath -> Eff es Bool) ->
  [OsPath] ->
  OsPath ->
  Eff es (Maybe OsPath)
findFileWith f ps = send . FindFileWith f ps

-- | Lifted 'Dir.findFilesWith'.
--
-- @since 0.1
findFilesWith ::
  ( PathReaderDynamic :> es
  ) =>
  (OsPath -> Eff es Bool) ->
  [OsPath] ->
  OsPath ->
  Eff es [OsPath]
findFilesWith f ps = send . FindFilesWith f ps

-- | Lifted 'Dir.pathIsSymbolicLink'.
--
-- @since 0.1
pathIsSymbolicLink ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es Bool
pathIsSymbolicLink = send . PathIsSymbolicLink

-- | Lifted 'Dir.getSymbolicLinkTarget'.
--
-- @since 0.1
getSymbolicLinkTarget ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es OsPath
getSymbolicLinkTarget = send . GetSymbolicLinkTarget

-- | Lifted 'Dir.getPermissions'.
--
-- @since 0.1
getPermissions ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es Permissions
getPermissions = send . GetPermissions

-- | Lifted 'Dir.getAccessTime'.
--
-- @since 0.1
getAccessTime ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es UTCTime
getAccessTime = send . GetAccessTime

-- | Lifted 'Dir.getModificationTime'.
--
-- @since 0.1
getModificationTime ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es UTCTime
getModificationTime = send . GetModificationTime

-- | Retrieves the XDG data directory e.g. @~/.local\/share@.
--
-- @since 0.1
getXdgData ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es OsPath
getXdgData = getXdgDirectory XdgData

-- | Retrieves the XDG config directory e.g. @~/.config@.
--
-- @since 0.1
getXdgConfig ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es OsPath
getXdgConfig = getXdgDirectory XdgConfig

-- | Retrieves the XDG cache directory e.g. @~/.cache@.
--
-- @since 0.1
getXdgCache ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es OsPath
getXdgCache = getXdgDirectory XdgCache

-- | Retrieves the XDG state directory e.g. @~/.local\/state@.
--
-- @since 0.1
getXdgState ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es OsPath
getXdgState = getXdgDirectory XdgState

-- | Returns true if the path is a symbolic link. Does not traverse the link.
--
-- @since 0.1
doesSymbolicLinkExist ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es Bool
doesSymbolicLinkExist p =
  -- pathIsSymbolicLink throws an exception if the path does not exist,
  -- so we need to handle this. Note that the obvious alternative, prefacing
  -- the call with doesPathExist does not work, as that operates on the link
  -- target. doesFileExist also behaves this way.
  pathIsSymbolicLink p `catchIOError` \_ -> pure False

-- | Retrieves the recursive directory contents; splits the sub folders and
-- directories apart.
--
-- @since 0.1
listDirectoryRecursive ::
  forall es.
  ( PathReaderDynamic :> es
  ) =>
  -- | Root path.
  OsPath ->
  -- | (files, directories)
  Eff es ([OsPath], [OsPath])
listDirectoryRecursive root = recurseDirs [emptyPath]
  where
    recurseDirs :: [OsPath] -> Eff es ([OsPath], [OsPath])
    recurseDirs [] = pure ([], [])
    recurseDirs (d : ds) = do
      (files, dirs) <- splitPaths root d [] [] =<< listDirectory (root </> d)
      (files', dirs') <- recurseDirs (dirs ++ ds)
      pure (files ++ files', dirs ++ dirs')
    emptyPath = mempty

-- | Like 'listDirectoryRecursive' except symbolic links are not traversed
-- i.e. they are returned separately.
--
-- @since 0.1
listDirectoryRecursiveSymbolicLink ::
  forall es.
  ( PathReaderDynamic :> es
  ) =>
  -- | Root path.
  OsPath ->
  -- | (files, directories, symbolic links)
  Eff es ([OsPath], [OsPath], [OsPath])
listDirectoryRecursiveSymbolicLink root = recurseDirs [emptyPath]
  where
    recurseDirs :: [OsPath] -> Eff es ([OsPath], [OsPath], [OsPath])
    recurseDirs [] = pure ([], [], [])
    recurseDirs (d : ds) = do
      (files, dirs, symlinks) <-
        splitPathsSymboliclink root d [] [] [] =<< listDirectory (root </> d)
      (files', dirs', symlinks') <- recurseDirs (dirs ++ ds)
      pure (files ++ files', dirs ++ dirs', symlinks ++ symlinks')
    emptyPath = mempty

splitPaths ::
  forall es.
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  OsPath ->
  [OsPath] ->
  [OsPath] ->
  [OsPath] ->
  Eff es ([OsPath], [OsPath])
splitPaths root d = go
  where
    go :: [OsPath] -> [OsPath] -> [OsPath] -> Eff es ([OsPath], [OsPath])
    go files dirs [] = pure (reverse files, reverse dirs)
    go files dirs (p : ps) = do
      let dirEntry = d </> p
      isDir <- doesDirectoryExist (root </> dirEntry)
      if isDir
        then go files (dirEntry : dirs) ps
        else go (dirEntry : files) dirs ps

splitPathsSymboliclink ::
  forall es.
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  OsPath ->
  [OsPath] ->
  [OsPath] ->
  [OsPath] ->
  [OsPath] ->
  Eff es ([OsPath], [OsPath], [OsPath])
splitPathsSymboliclink root d = go
  where
    go :: [OsPath] -> [OsPath] -> [OsPath] -> [OsPath] -> Eff es ([OsPath], [OsPath], [OsPath])
    go files dirs symlinks [] = pure (reverse files, reverse dirs, symlinks)
    go files dirs symlinks (p : ps) = do
      let dirEntry = d </> p
          fullPath = root </> dirEntry

      isSymlink <- doesSymbolicLinkExist fullPath
      if isSymlink
        then go files dirs (dirEntry : symlinks) ps
        else do
          isDir <- doesDirectoryExist fullPath
          if isDir
            then go files (dirEntry : dirs) symlinks ps
            else go (dirEntry : files) dirs symlinks ps

-- | Like 'pathIsSymbolicDirectoryLink' but for files.
--
-- @since 0.1
pathIsSymbolicFileLink ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es Bool
pathIsSymbolicFileLink = pathIsSymbolicLinkType doesFileExist

-- | Returns true if @p@ is a symbolic link and it points to an extant
-- directory.
--
-- This function and 'pathIsSymbolicFileLink' are intended to distinguish file
-- and directory links on Windows. This matters for knowing when to use:
--
--     - @createFileLink@ vs. @createDirectoryLink@
--     - @removeFile@ vs. @removeDirectoryLink@
--
-- Suppose we want to copy an arbitrary path @p@. We first determine that
-- @p@ is a symlink via 'doesSymbolicLinkExist'. If
-- 'pathIsSymbolicDirectoryLink' returns true then we know we should use
-- "Effects.FileSystem.PathWriter"'s @createDirectoryLink@. Otherwise we can
-- fall back to @createFileLink@.
--
-- Because this relies on the symlink's target, this is best effort, and it is
-- possible 'pathIsSymbolicDirectoryLink' and 'pathIsSymbolicFileLink' both
-- return false.
--
-- Note that Posix makes no distinction between file and directory symbolic
-- links. Thus if your system only has to work on Posix, you probably don't
-- need this function.
--
-- @since 0.1
pathIsSymbolicDirectoryLink ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es Bool
pathIsSymbolicDirectoryLink = pathIsSymbolicLinkType doesDirectoryExist

pathIsSymbolicLinkType ::
  ( PathReaderDynamic :> es
  ) =>
  (OsPath -> Eff es Bool) ->
  OsPath ->
  Eff es Bool
pathIsSymbolicLinkType predicate p = do
  isSymLink <- doesSymbolicLinkExist p
  if not isSymLink
    then pure False
    else do
      mtarget <-
        (Just <$> getSymbolicLinkTarget p)
          `catchIOError` \_ -> pure Nothing

      case mtarget of
        Nothing -> pure False
        Just target -> predicate target

-- | Throws 'IOException' if the path does not exist or the expected path type
-- does not match actual.
--
-- @since 0.1
throwIfWrongPathType ::
  ( PathReaderDynamic :> es
  ) =>
  -- | The location for the thrown exception (e.g. function name)
  String ->
  -- | Expected path type
  PathType ->
  -- | Path
  OsPath ->
  Eff es ()
throwIfWrongPathType location expected path = do
  actual <- getPathType path

  let err =
        mconcat
          [ "Expected path '",
            Utils.decodeOsToFpShow path,
            "' to have type ",
            PR.Utils.displayPathType expected,
            ", but detected ",
            PR.Utils.displayPathType actual
          ]

  unless (expected == actual) $
    Utils.throwPathIOError
      path
      location
      InappropriateType
      err

-- | Checks that the path type matches the expectation. Throws
-- 'IOException' if the path does not exist or the type cannot be detected.
--
-- @since 0.1
isPathType ::
  ( PathReaderDynamic :> es
  ) =>
  -- | Expected path type.
  PathType ->
  -- Path.
  OsPath ->
  Eff es Bool
isPathType expected = fmap (== expected) . getPathType

-- | Returns the type for a given path without following symlinks.
-- Throws 'IOException' if the path does not exist or the type cannot be
-- detected.
--
-- @since 0.1
getPathType ::
  ( PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es PathType
getPathType path = do
  -- This needs to be first as does(Directory|File|Path)Exist acts on the target.
  symlinkExists <- doesSymbolicLinkExist path
  if symlinkExists
    then pure PathTypeSymbolicLink
    else do
      dirExists <- doesDirectoryExist path
      if dirExists
        then pure PathTypeDirectory
        else do
          fileExists <- doesFileExist path
          if fileExists
            then pure PathTypeFile
            else do
              pathExists <- doesPathExist path
              if pathExists
                then pure PathTypeOther
                else
                  Utils.throwPathIOError
                    path
                    "getPathType"
                    IO.Error.doesNotExistErrorType
                    "path does not exist"
