{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for reading a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleReader.Static
  ( -- * Effect
    HandleReader,
    hIsEOF,
    hGetBuffering,
    hIsOpen,
    hIsClosed,
    hIsReadable,
    hIsWritable,
    hIsSeekable,
    hIsTerminalDevice,
    hGetEcho,
    hWaitForInput,
    hReady,
    hGetChar,
    hGetLine,
    hGetContents,
    hGet,
    hGetSome,
    hGetNonBlocking,

    -- ** Handlers
    runHandleReader,

    -- * UTF-8 Utils

    -- ** GetLine
    hGetLineUtf8,
    hGetLineUtf8Lenient,
    hGetLineUtf8ThrowM,

    -- ** GetContents
    hGetContentsUtf8,
    hGetContentsUtf8Lenient,
    hGetContentsUtf8ThrowM,

    -- ** Get
    hGetUtf8,
    hGetUtf8Lenient,
    hGetUtf8ThrowM,

    -- ** GetSome
    hGetSomeUtf8,
    hGetSomeUtf8Lenient,
    hGetSomeUtf8ThrowM,

    -- ** GetNonBlocking
    hGetNonBlockingUtf8,
    hGetNonBlockingUtf8Lenient,
    hGetNonBlockingUtf8ThrowM,

    -- * Re-exports
    ByteString,
    Handle,
    OsPath,
    Text,
    UnicodeException,
  )
where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Static
  ( HasCallStack,
    SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    unsafeEff_,
  )
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8
import System.IO (BufferMode, Handle)
import System.IO qualified as IO

-- | Static effect for reading a handle.
--
-- @since 0.1
data HandleReader :: Effect

type instance DispatchOf HandleReader = Static WithSideEffects

data instance StaticRep HandleReader = MkHandleReader

-- | Runs 'HandleReader' in 'IO'.
--
-- @since 0.1
runHandleReader ::
  (HasCallStack, IOE :> es) =>
  Eff (HandleReader : es) a ->
  Eff es a
runHandleReader = evalStaticRep MkHandleReader

-- | Lifted 'IO.hIsEof'.
--
-- @since 0.1
hIsEOF :: (HandleReader :> es, HasCallStack) => Handle -> Eff es Bool
hIsEOF = unsafeEff_ . IO.hIsEOF

-- | Lifted 'IO.hGetBuffering'.
--
-- @since 0.1
hGetBuffering ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es BufferMode
hGetBuffering = unsafeEff_ . IO.hGetBuffering

-- | Lifted 'IO.hIsOpen'.
--
-- @since 0.1
hIsOpen ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsOpen = unsafeEff_ . IO.hIsOpen

-- | Lifted 'IO.hIsClosed'.
--
-- @since 0.1
hIsClosed ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsClosed = unsafeEff_ . IO.hIsClosed

-- | Lifted 'IO.hIsReadable'.
--
-- @since 0.1
hIsReadable ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsReadable = unsafeEff_ . IO.hIsReadable

-- | Lifted 'IO.hIsWritable'.
--
-- @since 0.1
hIsWritable ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsWritable = unsafeEff_ . IO.hIsWritable

-- | Lifted 'IO.hIsSeekable'.
--
-- @since 0.1
hIsSeekable ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsSeekable = unsafeEff_ . IO.hIsSeekable

-- | Lifted 'IO.hIsTerminalDevice'.
--
-- @since 0.1
hIsTerminalDevice ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsTerminalDevice = unsafeEff_ . IO.hIsTerminalDevice

-- | Lifted 'IO.hGetEcho'.
--
-- @since 0.1
hGetEcho ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hGetEcho = unsafeEff_ . IO.hGetEcho

-- | Lifted 'IO.hWaitForInput'.
--
-- @since 0.1
hWaitForInput ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Bool
hWaitForInput h = unsafeEff_ . IO.hWaitForInput h

-- | Lifted 'IO.hReady'.
--
-- @since 0.1
hReady ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hReady = unsafeEff_ . IO.hReady

-- | Lifted 'IO.hGetChar'.
--
-- @since 0.1
hGetChar ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Char
hGetChar = unsafeEff_ . IO.hGetChar

-- | Lifted 'BS.hGetLine'.
--
-- @since 0.1
hGetLine ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es ByteString
hGetLine = unsafeEff_ . C8.hGetLine

-- | Lifted 'BS.hGetContents'.
--
-- @since 0.1
hGetContents ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es ByteString
hGetContents = unsafeEff_ . C8.hGetContents

-- | Lifted 'BS.hGet'.
--
-- @since 0.1
hGet ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es ByteString
hGet h = unsafeEff_ . C8.hGet h

-- | Lifted 'BS.hGetSome'.
--
-- @since 0.1
hGetSome ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es ByteString
hGetSome h = unsafeEff_ . C8.hGetSome h

-- | Lifted 'BS.hGetNonBlocking'.
--
-- @since 0.1
hGetNonBlocking ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es ByteString
hGetNonBlocking h = unsafeEff_ . C8.hGetNonBlocking h

-- | 'hGetLine' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetLineUtf8 ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es (Either UnicodeException Text)
hGetLineUtf8 = fmap FS.UTF8.decodeUtf8 . hGetLine

-- | 'hGetLine' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetLineUtf8Lenient ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Text
hGetLineUtf8Lenient = fmap FS.UTF8.decodeUtf8Lenient . hGetLine

-- | 'hGetLine' and 'FS.UTF8.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetLineUtf8ThrowM ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Text
hGetLineUtf8ThrowM = hGetLine >=> FS.UTF8.decodeUtf8ThrowM

-- | 'hGetContents' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetContentsUtf8 ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es (Either UnicodeException Text)
hGetContentsUtf8 = fmap FS.UTF8.decodeUtf8 . hGetContents

-- | 'hGetContents' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetContentsUtf8Lenient ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Text
hGetContentsUtf8Lenient = fmap FS.UTF8.decodeUtf8Lenient . hGetContents

-- | 'hGetContents' and 'FS.UTF8.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetContentsUtf8ThrowM ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Text
hGetContentsUtf8ThrowM = hGetContents >=> FS.UTF8.decodeUtf8ThrowM

-- | 'hGet' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetUtf8 ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetUtf8 h = fmap FS.UTF8.decodeUtf8 . hGet h

-- | 'hGet' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetUtf8Lenient ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetUtf8Lenient h = fmap FS.UTF8.decodeUtf8Lenient . hGet h

-- | 'hGet' and 'FS.UTF8.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetUtf8ThrowM ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetUtf8ThrowM h = hGet h >=> FS.UTF8.decodeUtf8ThrowM

-- | 'hGetSome' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetSomeUtf8 ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetSomeUtf8 h = fmap FS.UTF8.decodeUtf8 . hGetSome h

-- | 'hGetSome' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetSomeUtf8Lenient ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetSomeUtf8Lenient h = fmap FS.UTF8.decodeUtf8Lenient . hGetSome h

-- | 'hGetSome' and 'FS.UTF8.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetSomeUtf8ThrowM ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetSomeUtf8ThrowM h = hGetSome h >=> FS.UTF8.decodeUtf8ThrowM

-- | 'hGetNonBlocking' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetNonBlockingUtf8 ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetNonBlockingUtf8 h = fmap FS.UTF8.decodeUtf8 . hGetNonBlocking h

-- | 'hGetNonBlocking' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetNonBlockingUtf8Lenient ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetNonBlockingUtf8Lenient h = fmap FS.UTF8.decodeUtf8Lenient . hGetNonBlocking h

-- | 'hGetNonBlocking' and 'FS.UTF8.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetNonBlockingUtf8ThrowM ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetNonBlockingUtf8ThrowM h = hGetNonBlocking h >=> FS.UTF8.decodeUtf8ThrowM
