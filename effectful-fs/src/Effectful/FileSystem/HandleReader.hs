-- | Provides an effect for reading a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleReader
  ( -- * Effect
    HandleReaderEffect (..),

    -- ** Functions
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
    runHandleReaderIO,

    -- * UTF-8 Utils
    hGetLineUtf8,
    hGetLineUtf8Lenient,
    hGetLineUtf8ThrowM,
    hGetContentsUtf8,
    hGetContentsUtf8Lenient,
    hGetContentsUtf8ThrowM,
    hGetUtf8,
    hGetUtf8Lenient,
    hGetUtf8ThrowM,
    hGetSomeUtf8,
    hGetSomeUtf8Lenient,
    hGetSomeUtf8ThrowM,
    hGetNonBlockingUtf8,
    hGetNonBlockingUtf8Lenient,
    hGetNonBlockingUtf8ThrowM,

    -- * Reexports
    ByteString,
    Handle,
    Text,
    UnicodeException,
  )
where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Exception
  ( CallStackEffect,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.FileSystem.FileReader
  ( decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,
  )
import GHC.Stack (HasCallStack)
import System.IO (BufferMode, Handle)
import System.IO qualified as IO

-- | Effect for reading a handle.
--
-- @since 0.1
data HandleReaderEffect :: Effect where
  HIsEOF :: (HasCallStack) => Handle -> HandleReaderEffect m Bool
  HGetBuffering :: (HasCallStack) => Handle -> HandleReaderEffect m BufferMode
  HIsOpen :: (HasCallStack) => Handle -> HandleReaderEffect m Bool
  HIsClosed :: (HasCallStack) => Handle -> HandleReaderEffect m Bool
  HIsReadable :: (HasCallStack) => Handle -> HandleReaderEffect m Bool
  HIsWritable :: (HasCallStack) => Handle -> HandleReaderEffect m Bool
  HIsSeekable :: (HasCallStack) => Handle -> HandleReaderEffect m Bool
  HIsTerminalDevice :: (HasCallStack) => Handle -> HandleReaderEffect m Bool
  HGetEcho :: (HasCallStack) => Handle -> HandleReaderEffect m Bool
  HWaitForInput :: (HasCallStack) => Handle -> Int -> HandleReaderEffect m Bool
  HReady :: (HasCallStack) => Handle -> HandleReaderEffect m Bool
  HGetChar :: (HasCallStack) => Handle -> HandleReaderEffect m Char
  HGetLine :: (HasCallStack) => Handle -> HandleReaderEffect m ByteString
  HGetContents :: (HasCallStack) => Handle -> HandleReaderEffect m ByteString
  HGet :: (HasCallStack) => Handle -> Int -> HandleReaderEffect m ByteString
  HGetSome :: (HasCallStack) => Handle -> Int -> HandleReaderEffect m ByteString
  HGetNonBlocking :: (HasCallStack) => Handle -> Int -> HandleReaderEffect m ByteString

-- | @since 0.1
type instance DispatchOf HandleReaderEffect = Dynamic

-- | Runs 'HandleReaderEffect' in 'IO'.
--
-- @since 0.1
runHandleReaderIO ::
  ( CallStackEffect :> es,
    IOE :> es
  ) =>
  Eff (HandleReaderEffect : es) a ->
  Eff es a
runHandleReaderIO = interpret $ \_ -> \case
  HIsEOF h -> addCallStack $ liftIO $ IO.hIsEOF h
  HGetBuffering h -> addCallStack $ liftIO $ IO.hGetBuffering h
  HIsOpen h -> addCallStack $ liftIO $ IO.hIsOpen h
  HIsClosed h -> addCallStack $ liftIO $ IO.hIsClosed h
  HIsReadable h -> addCallStack $ liftIO $ IO.hIsReadable h
  HIsWritable h -> addCallStack $ liftIO $ IO.hIsWritable h
  HIsSeekable h -> addCallStack $ liftIO $ IO.hIsSeekable h
  HIsTerminalDevice h -> addCallStack $ liftIO $ IO.hIsTerminalDevice h
  HGetEcho h -> addCallStack $ liftIO $ IO.hGetEcho h
  HWaitForInput h i -> addCallStack $ liftIO $ IO.hWaitForInput h i
  HReady h -> addCallStack $ liftIO $ IO.hReady h
  HGetChar h -> addCallStack $ liftIO $ IO.hGetChar h
  HGetLine h -> addCallStack $ liftIO $ BS.hGetLine h
  HGetContents h -> addCallStack $ liftIO $ BS.hGetContents h
  HGet h i -> addCallStack $ liftIO $ BS.hGet h i
  HGetSome h i -> addCallStack $ liftIO $ BS.hGetSome h i
  HGetNonBlocking h i -> addCallStack $ liftIO $ BS.hGetNonBlocking h i

-- | @since 0.1
hIsEOF :: (HandleReaderEffect :> es) => Handle -> Eff es Bool
hIsEOF = send . HIsEOF

-- | @since 0.1
hGetBuffering ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es BufferMode
hGetBuffering = send . HGetBuffering

-- | @since 0.1
hIsOpen ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsOpen = send . HIsOpen

-- | @since 0.1
hIsClosed ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsClosed = send . HIsClosed

-- | @since 0.1
hIsReadable ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsReadable = send . HIsReadable

-- | @since 0.1
hIsWritable ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsWritable = send . HIsWritable

-- | @since 0.1
hIsSeekable ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsSeekable = send . HIsSeekable

-- | @since 0.1
hIsTerminalDevice ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsTerminalDevice = send . HIsTerminalDevice

-- | @since 0.1
hGetEcho ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hGetEcho = send . HGetEcho

-- | @since 0.1
hWaitForInput ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Bool
hWaitForInput h = send . HWaitForInput h

-- | @since 0.1
hReady ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hReady = send . HReady

-- | @since 0.1
hGetChar ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Char
hGetChar = send . HGetChar

-- | @since 0.1
hGetLine ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es ByteString
hGetLine = send . HGetLine

-- | @since 0.1
hGetContents ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es ByteString
hGetContents = send . HGetContents

-- | @since 0.1
hGet ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es ByteString
hGet h = send . HGet h

-- | @since 0.1
hGetSome ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es ByteString
hGetSome h = send . HGetSome h

-- | @since 0.1
hGetNonBlocking ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es ByteString
hGetNonBlocking h = send . HGetNonBlocking h

-- | @since 0.1
hGetLineUtf8 ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es (Either UnicodeException Text)
hGetLineUtf8 = fmap decodeUtf8 . hGetLine

-- | @since 0.1
hGetLineUtf8Lenient ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Text
hGetLineUtf8Lenient = fmap decodeUtf8Lenient . hGetLine

-- | @since 0.1
hGetLineUtf8ThrowM ::
  ( CallStackEffect :> es,
    HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Text
hGetLineUtf8ThrowM = hGetLine >=> decodeUtf8ThrowM

-- | @since 0.1
hGetContentsUtf8 ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es (Either UnicodeException Text)
hGetContentsUtf8 = fmap decodeUtf8 . hGetContents

-- | @since 0.1
hGetContentsUtf8Lenient ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Text
hGetContentsUtf8Lenient = fmap decodeUtf8Lenient . hGetContents

-- | @since 0.1
hGetContentsUtf8ThrowM ::
  ( CallStackEffect :> es,
    HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Text
hGetContentsUtf8ThrowM = hGetContents >=> decodeUtf8ThrowM

-- | @since 0.1
hGetUtf8 ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetUtf8 h = fmap decodeUtf8 . hGet h

-- | @since 0.1
hGetUtf8Lenient ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetUtf8Lenient h = fmap decodeUtf8Lenient . hGet h

-- | @since 0.1
hGetUtf8ThrowM ::
  ( CallStackEffect :> es,
    HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetUtf8ThrowM h = hGet h >=> decodeUtf8ThrowM

-- | @since 0.1
hGetSomeUtf8 ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetSomeUtf8 h = fmap decodeUtf8 . hGetSome h

-- | @since 0.1
hGetSomeUtf8Lenient ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetSomeUtf8Lenient h = fmap decodeUtf8Lenient . hGetSome h

-- | @since 0.1
hGetSomeUtf8ThrowM ::
  ( CallStackEffect :> es,
    HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetSomeUtf8ThrowM h = hGetSome h >=> decodeUtf8ThrowM

-- | @since 0.1
hGetNonBlockingUtf8 ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetNonBlockingUtf8 h = fmap decodeUtf8 . hGetNonBlocking h

-- | @since 0.1
hGetNonBlockingUtf8Lenient ::
  ( HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetNonBlockingUtf8Lenient h = fmap decodeUtf8Lenient . hGetNonBlocking h

-- | @since 0.1
hGetNonBlockingUtf8ThrowM ::
  ( CallStackEffect :> es,
    HandleReaderEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetNonBlockingUtf8ThrowM h = hGetNonBlocking h >=> decodeUtf8ThrowM
