-- | Provides a dynamic effect for "System.Posix.Files".
--
-- @since 0.1
module Effectful.Posix.Dynamic
  ( -- * Effect
    Posix (..),
    setFileMode,
    setFdMode,
    setFileCreationMask,
    fileAccess,
    fileExist,
    getFileStatus,
    getFdStatus,
    getSymbolicLinkStatus,
    createNamedPipe,
    createDevice,
    createLink,
    removeLink,
    createSymbolicLink,
    readSymbolicLink,
    rename,
    setOwnerAndGroup,
    setFdOwnerAndGroup,
    setSymbolicLinkOwnerAndGroup,
    setFileTimes,
    touchFile,
    setFileSize,
    setFdSize,
    getPathVar,
    getFdPathVar,

    -- ** Handler
    runPosix,

    -- * PathType
    PathType (..),
    displayPathType,

    -- ** Functions
    throwIfWrongPathType,
    isPathType,
    getPathType,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor ((<&>))
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (HasCallStack, interpret, send)
import FileSystem.IO qualified as FS.IO
import FileSystem.PathType
  ( PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeOther,
        PathTypeSymbolicLink
      ),
    displayPathType,
  )
import GHC.IO.Exception (IOErrorType (InappropriateType))
import System.OsString.Internal.Types (OsString (OsString))
import System.Posix.Files.PosixString (FileStatus, PathVar)
import System.Posix.Files.PosixString qualified as PFiles
import System.Posix.PosixString (PosixPath)
import System.Posix.Types
  ( DeviceID,
    EpochTime,
    Fd,
    FileMode,
    FileOffset,
    GroupID,
    Limit,
    UserID,
  )

-- | Dynamic effect for "System.Posix.Files".
--
-- @since 0.1
data Posix :: Effect where
  SetFileMode :: PosixPath -> FileMode -> Posix m ()
  SetFdMode :: Fd -> FileMode -> Posix m ()
  SetFileCreationMask :: FileMode -> Posix m FileMode
  FileAccess :: PosixPath -> Bool -> Bool -> Bool -> Posix m Bool
  FileExist :: PosixPath -> Posix m Bool
  GetFileStatus :: PosixPath -> Posix m FileStatus
  GetFdStatus :: Fd -> Posix m FileStatus
  GetSymbolicLinkStatus :: PosixPath -> Posix m FileStatus
  CreateNamedPipe :: PosixPath -> FileMode -> Posix m ()
  CreateDevice :: PosixPath -> FileMode -> DeviceID -> Posix m ()
  CreateLink :: PosixPath -> PosixPath -> Posix m ()
  RemoveLink :: PosixPath -> Posix m ()
  CreateSymbolicLink :: PosixPath -> PosixPath -> Posix m ()
  ReadSymbolicLink :: PosixPath -> Posix m PosixPath
  Rename :: PosixPath -> PosixPath -> Posix m ()
  SetOwnerAndGroup :: PosixPath -> UserID -> GroupID -> Posix m ()
  SetFdOwnerAndGroup :: Fd -> UserID -> GroupID -> Posix m ()
  SetSymbolicLinkOwnerAndGroup ::
    PosixPath -> UserID -> GroupID -> Posix m ()
  SetFileTimes :: PosixPath -> EpochTime -> EpochTime -> Posix m ()
  TouchFile :: PosixPath -> Posix m ()
  SetFileSize :: PosixPath -> FileOffset -> Posix m ()
  SetFdSize :: Fd -> FileOffset -> Posix m ()
  GetPathVar :: PosixPath -> PathVar -> Posix m Limit
  GetFdPathVar :: Fd -> PathVar -> Posix m Limit

-- | @since 0.1
type instance DispatchOf Posix = Dynamic

-- | Runs 'Posix' in 'IO'.
--
-- @since 0.1
runPosix ::
  (HasCallStack, IOE :> es) =>
  Eff (Posix : es) a ->
  Eff es a
runPosix = interpret $ \_ -> \case
  SetFileMode p m -> liftIO $ PFiles.setFileMode p m
  SetFdMode fd m -> liftIO $ PFiles.setFdMode fd m
  SetFileCreationMask m -> liftIO $ PFiles.setFileCreationMask m
  FileAccess p b c d -> liftIO $ PFiles.fileAccess p b c d
  FileExist p -> liftIO $ PFiles.fileExist p
  GetFileStatus p -> liftIO $ PFiles.getFileStatus p
  GetFdStatus fd -> liftIO $ PFiles.getFdStatus fd
  GetSymbolicLinkStatus p -> liftIO $ PFiles.getSymbolicLinkStatus p
  CreateNamedPipe p m -> liftIO $ PFiles.createNamedPipe p m
  CreateDevice p m did -> liftIO $ PFiles.createDevice p m did
  CreateLink p1 p2 -> liftIO $ PFiles.createLink p1 p2
  RemoveLink p -> liftIO $ PFiles.removeLink p
  CreateSymbolicLink p1 p2 -> liftIO $ PFiles.createSymbolicLink p1 p2
  ReadSymbolicLink p -> liftIO $ PFiles.readSymbolicLink p
  Rename p1 p2 -> liftIO $ PFiles.rename p1 p2
  SetOwnerAndGroup p uid gid -> liftIO $ PFiles.setOwnerAndGroup p uid gid
  SetFdOwnerAndGroup fd uid gid ->
    liftIO $ PFiles.setFdOwnerAndGroup fd uid gid
  SetSymbolicLinkOwnerAndGroup p uid gid ->
    liftIO $ PFiles.setSymbolicLinkOwnerAndGroup p uid gid
  SetFileTimes p t1 t2 -> liftIO $ PFiles.setFileTimes p t1 t2
  TouchFile p -> liftIO $ PFiles.touchFile p
  SetFileSize p oset -> liftIO $ PFiles.setFileSize p oset
  SetFdSize fd oset -> liftIO $ PFiles.setFdSize fd oset
  GetPathVar p m -> liftIO $ PFiles.getPathVar p m
  GetFdPathVar fd m -> liftIO $ PFiles.getFdPathVar fd m

-- | Lifted 'PFiles.setFileMode'.
--
-- @since 0.1
setFileMode ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  FileMode ->
  Eff es ()
setFileMode p = send . SetFileMode p

-- | Lifted 'PFiles.setFdMode'.
--
-- @since 0.1
setFdMode ::
  ( HasCallStack,
    Posix :> es
  ) =>
  Fd ->
  FileMode ->
  Eff es ()
setFdMode p = send . SetFdMode p

-- | Lifted 'PFiles.setFileCreationMask'.
--
-- @since 0.1
setFileCreationMask ::
  ( HasCallStack,
    Posix :> es
  ) =>
  FileMode ->
  Eff es FileMode
setFileCreationMask = send . SetFileCreationMask

-- | Lifted 'PFiles.fileAccess'.
--
-- @since 0.1
fileAccess ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Bool ->
  Bool ->
  Bool ->
  Eff es Bool
fileAccess p b c = send . FileAccess p b c

-- | Lifted 'PFiles.fileExist'.
--
-- @since 0.1
fileExist ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Eff es Bool
fileExist = send . FileExist

-- | Lifted 'PFiles.getFileStatus'.
--
-- @since 0.1
getFileStatus ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Eff es FileStatus
getFileStatus = send . GetFileStatus

-- | Lifted 'PFiles.getFdStatus'.
--
-- @since 0.1
getFdStatus ::
  ( HasCallStack,
    Posix :> es
  ) =>
  Fd ->
  Eff es FileStatus
getFdStatus = send . GetFdStatus

-- | Lifted 'PFiles.getSymbolicLinkStatus'.
--
-- @since 0.1
getSymbolicLinkStatus ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Eff es FileStatus
getSymbolicLinkStatus = send . GetSymbolicLinkStatus

-- | Lifted 'PFiles.createNamedPipe'.
--
-- @since 0.1
createNamedPipe ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  FileMode ->
  Eff es ()
createNamedPipe p = send . CreateNamedPipe p

-- | Lifted 'PFiles.createDevice'.
--
-- @since 0.1
createDevice ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  FileMode ->
  DeviceID ->
  Eff es ()
createDevice p m = send . CreateDevice p m

-- | Lifted 'PFiles.createLink'.
--
-- @since 0.1
createLink ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  PosixPath ->
  Eff es ()
createLink p = send . CreateLink p

-- | Lifted 'PFiles.removeLink'.
--
-- @since 0.1
removeLink ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Eff es ()
removeLink = send . RemoveLink

-- | Lifted 'PFiles.createSymbolicLink'.
--
-- @since 0.1
createSymbolicLink ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  PosixPath ->
  Eff es ()
createSymbolicLink p = send . CreateSymbolicLink p

-- | Lifted 'PFiles.readSymbolicLink'.
--
-- @since 0.1
readSymbolicLink ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Eff es PosixPath
readSymbolicLink = send . ReadSymbolicLink

-- | Lifted 'PFiles.rename'.
--
-- @since 0.1
rename ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  PosixPath ->
  Eff es ()
rename p = send . Rename p

-- | Lifted 'PFiles.setOwnerAndGroup'.
--
-- @since 0.1
setOwnerAndGroup ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  UserID ->
  GroupID ->
  Eff es ()
setOwnerAndGroup p uid = send . SetOwnerAndGroup p uid

-- | Lifted 'PFiles.setFdOwnerAndGroup'.
--
-- @since 0.1
setFdOwnerAndGroup ::
  ( HasCallStack,
    Posix :> es
  ) =>
  Fd ->
  UserID ->
  GroupID ->
  Eff es ()
setFdOwnerAndGroup fd uid = send . SetFdOwnerAndGroup fd uid

-- | Lifted 'PFiles.setSymbolicLinkOwnerAndGroup'.
--
-- @since 0.1
setSymbolicLinkOwnerAndGroup ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  UserID ->
  GroupID ->
  Eff es ()
setSymbolicLinkOwnerAndGroup p uid = send . SetSymbolicLinkOwnerAndGroup p uid

-- | Lifted 'PFiles.setFileTimes'.
--
-- @since 0.1
setFileTimes ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  EpochTime ->
  EpochTime ->
  Eff es ()
setFileTimes p t = send . SetFileTimes p t

-- | Lifted 'PFiles.touchFile'.
--
-- @since 0.1
touchFile ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Eff es ()
touchFile = send . TouchFile

-- | Lifted 'PFiles.setFileSize'.
--
-- @since 0.1
setFileSize ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  FileOffset ->
  Eff es ()
setFileSize p = send . SetFileSize p

-- | Lifted 'PFiles.setFdSize'.
--
-- @since 0.1
setFdSize ::
  ( HasCallStack,
    Posix :> es
  ) =>
  Fd ->
  FileOffset ->
  Eff es ()
setFdSize fd = send . SetFdSize fd

-- | Lifted 'PFiles.getPathVar'.
--
-- @since 0.1
getPathVar ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  PathVar ->
  Eff es Limit
getPathVar p = send . GetPathVar p

-- | Lifted 'PFiles.getFdPathVar'.
--
-- @since 0.1
getFdPathVar ::
  ( HasCallStack,
    Posix :> es
  ) =>
  Fd ->
  PathVar ->
  Eff es Limit
getFdPathVar fd = send . GetFdPathVar fd

-- | Throws 'IOException' if the path does not exist or the expected path type
-- does not match actual.
--
-- @since 0.1
throwIfWrongPathType ::
  ( HasCallStack,
    Posix :> es
  ) =>
  String ->
  PathType ->
  PosixPath ->
  Eff es ()
throwIfWrongPathType location expected path = do
  actual <- getPathType path

  let err =
        mconcat
          [ "Expected path to have type ",
            displayPathType expected,
            ", but detected ",
            displayPathType actual
          ]

  unless (expected == actual) $
    FS.IO.throwPathIOError
      (OsString path)
      location
      InappropriateType
      err

-- | Checks that the path type matches the expectation. Throws
-- 'IOException' if the path does not exist or the type cannot be detected.
--
-- @since 0.1
isPathType ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PathType ->
  PosixPath ->
  Eff es Bool
isPathType expected = fmap (== expected) . getPathType

-- | Returns the type for a given path without following symlinks.
-- Throws 'IOException' if the path does not exist or the type cannot be
-- detected.
--
-- @since 0.1
getPathType ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Eff es PathType
getPathType path =
  getSymbolicLinkStatus path <&> \status ->
    if
      | PFiles.isSymbolicLink status -> PathTypeSymbolicLink
      | PFiles.isDirectory status -> PathTypeDirectory
      | PFiles.isRegularFile status -> PathTypeFile
      | otherwise -> PathTypeOther