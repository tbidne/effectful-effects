{-# LANGUAGE UndecidableInstances #-}

-- | Provides namespaced logging functionality on top of 'LoggerDynamic'.
--
-- @since 0.1
module Effectful.LoggerNS.Dynamic
  ( -- * Effect
    LoggerNSDynamic (..),
    Namespace (..),
    addNamespace,
    getNamespace,
    localNamespace,

    -- * Formatting
    LogFormatter (..),
    defaultLogFormatter,
    LocStrategy (..),
    formatLog,

    -- * LogStr
    logStrToBs,
    logStrToText,

    -- * Optics
    _LocPartial,
    _LocStable,
    _LocNone,

    -- * Re-exports
    LogStr,
    Loc,
  )
where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Foldable (Foldable (foldMap'))
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncError
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (send)
import Effectful.Logger.Dynamic
  ( LogLevel
      ( LevelDebug,
        LevelError,
        LevelFatal,
        LevelInfo,
        LevelOther,
        LevelTrace,
        LevelWarn
      ),
    LogStr,
    ToLogStr (toLogStr),
  )
import Effectful.Time.Dynamic (TimeDynamic)
import Effectful.Time.Dynamic qualified as TimeDynamic
import GHC.Exts (IsList (Item, fromList, toList))
import GHC.Generics (Generic)
import Language.Haskell.TH (Loc (loc_filename, loc_start))
import Optics.Core
  ( A_Lens,
    An_Iso,
    LabelOptic (labelOptic),
    Prism',
    iso,
    lensVL,
    over',
    prism,
    view,
    (%),
    (^.),
    _1,
    _2,
  )
import System.Log.FastLogger qualified as FL

-- | Logging namespace.
--
-- @since 0.1
newtype Namespace = MkNamespace
  { -- | @since 0.1
    unNamespace :: Seq Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Monoid,
      -- | @since 0.1
      Semigroup
    )
    via (Seq Text)
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance
  (k ~ An_Iso, a ~ Seq Text, b ~ Seq Text) =>
  LabelOptic "unNamespace" k Namespace Namespace a b
  where
  labelOptic = iso (\(MkNamespace ns) -> ns) MkNamespace
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance IsString Namespace where
  fromString = MkNamespace . Seq.singleton . T.pack

-- | @since 0.1
instance IsList Namespace where
  type Item Namespace = Text
  fromList = MkNamespace . fromList
  toList = toList . unNamespace

displayNamespace :: Namespace -> Text
displayNamespace =
  foldMap' id
    . Seq.intersperse "."
    . view #unNamespace

-- | Dynamic effect for a namespaced logger.
--
-- @since 0.1
data LoggerNSDynamic :: Effect where
  GetNamespace :: LoggerNSDynamic es Namespace
  LocalNamespace ::
    (Namespace -> Namespace) ->
    m a ->
    LoggerNSDynamic m a

-- | @since 0.1
type instance DispatchOf LoggerNSDynamic = Dynamic

-- | Retrieves the namespace.
--
-- @since 0.1
getNamespace :: (LoggerNSDynamic :> es) => Eff es Namespace
getNamespace = send GetNamespace

-- | Locally modifies the namespace.
--
-- @since 0.1
localNamespace ::
  ( LoggerNSDynamic :> es
  ) =>
  (Namespace -> Namespace) ->
  Eff es a ->
  Eff es a
localNamespace f = send . LocalNamespace f

-- | Adds to the namespace.
--
-- @since 0.1
addNamespace ::
  ( LoggerNSDynamic :> es
  ) =>
  Text ->
  Eff es a ->
  Eff es a
addNamespace txt = localNamespace (over' #unNamespace (|> txt))

-- | Determines how we log location data.
--
-- @since 0.1
data LocStrategy
  = -- | Logs the location with filename, line, col.
    --
    -- @since 0.1
    LocPartial !Loc
  | -- | Logs the location with filename.
    --
    -- @since 0.1
    LocStable !Loc
  | -- | No location logging.
    --
    -- @since 0.1
    LocNone
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
_LocPartial :: Prism' LocStrategy Loc
_LocPartial =
  prism
    LocPartial
    ( \case
        LocPartial loc -> Right loc
        x -> Left x
    )
{-# INLINE _LocPartial #-}

-- | @since 0.1
_LocStable :: Prism' LocStrategy Loc
_LocStable =
  prism
    LocStable
    ( \case
        LocStable loc -> Right loc
        x -> Left x
    )
{-# INLINE _LocStable #-}

-- | @since 0.1
_LocNone :: Prism' LocStrategy ()
_LocNone =
  prism
    (const LocNone)
    ( \case
        LocNone -> Right ()
        x -> Left x
    )
{-# INLINE _LocNone #-}

-- | Formatter for logs.
--
-- @since 0.1
data LogFormatter = MkLogFormatter
  { -- | If true, append a newline.
    --
    -- @since 0.1
    newline :: !Bool,
    -- | How to log the code location.
    --
    -- @since 0.1
    locStrategy :: !LocStrategy,
    -- | Whether to include the timezone in the timestamp.
    --
    -- @since 0.1
    timezone :: !Bool
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "newline" k LogFormatter LogFormatter a b
  where
  labelOptic = lensVL $ \f (MkLogFormatter _newline _locStrategy _timezone) ->
    fmap (\newline' -> MkLogFormatter newline' _locStrategy _timezone) (f _newline)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ LocStrategy, b ~ LocStrategy) =>
  LabelOptic "locStrategy" k LogFormatter LogFormatter a b
  where
  labelOptic = lensVL $ \f (MkLogFormatter _newline _locStrategy _timezone) ->
    fmap
      ( \locStrategy' ->
          MkLogFormatter _newline locStrategy' _timezone
      )
      (f _locStrategy)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "timezone" k LogFormatter LogFormatter a b
  where
  labelOptic = lensVL $ \f (MkLogFormatter _newline _locStrategy _timezone) ->
    fmap (MkLogFormatter _newline _locStrategy) (f _timezone)
  {-# INLINE labelOptic #-}

-- | 'LogFormatter' with:
--
-- @
-- 'newline' = 'True'
-- 'locStrategy' = 'LocPartial' loc
-- 'timezone' = 'False'
-- @
--
-- @since 0.1
defaultLogFormatter :: Loc -> LogFormatter
defaultLogFormatter loc =
  MkLogFormatter
    { newline = True,
      locStrategy = LocPartial loc,
      timezone = False
    }

-- | Produces a formatted 'LogStr'.
--
-- @since 0.1
formatLog ::
  ( LoggerNSDynamic :> es,
    TimeDynamic :> es,
    ToLogStr msg
  ) =>
  LogFormatter ->
  LogLevel ->
  msg ->
  Eff es LogStr
formatLog formatter lvl msg = do
  timestampTxt <- timeFn
  namespace <- getNamespace
  let locTxt = case formatter ^. #locStrategy of
        LocPartial loc -> (brackets . toLogStr . partialLoc) loc
        LocStable loc -> (brackets . toLogStr . stableLoc) loc
        LocNone -> ""
      namespaceTxt = toLogStr $ displayNamespace namespace
      lvlTxt = toLogStr $ showLevel lvl
      msgTxt = toLogStr msg
      newline'
        | formatter ^. #newline = "\n"
        | otherwise = ""
      formatted =
        mconcat
          [ brackets timestampTxt,
            brackets namespaceTxt,
            brackets lvlTxt,
            locTxt,
            " ",
            msgTxt,
            newline'
          ]
  pure formatted
  where
    timeFn
      | formatter ^. #timezone =
          toLogStr <$> TimeDynamic.getSystemZonedTimeString
      | otherwise =
          toLogStr <$> TimeDynamic.getSystemTimeString

partialLoc :: Loc -> Builder
partialLoc loc =
  mconcat
    [ fromString $ view #loc_filename loc,
      ":" <> mkLine loc,
      ":" <> mkChar loc
    ]
  where
    mkLine = fromString . show . view (#loc_start % _1)
    mkChar = fromString . show . view (#loc_start % _2)

stableLoc :: Loc -> Builder
stableLoc loc = fromString $ view #loc_filename loc

showLevel :: LogLevel -> Text
showLevel LevelTrace = "Trace"
showLevel LevelDebug = "Debug"
showLevel LevelInfo = "Info"
showLevel LevelWarn = "Warn"
showLevel LevelError = "Error"
showLevel LevelFatal = "Fatal"
showLevel (LevelOther txt) = txt

-- LogStr uses ByteString's Builder internally, so we might as well use it
-- for constants.
brackets :: LogStr -> LogStr
brackets m = cLogStr "[" <> m <> cLogStr "]"

cLogStr :: Builder -> LogStr
cLogStr = toLogStr @Builder

-- | @since 0.1
logStrToBs :: LogStr -> ByteString
logStrToBs = FL.fromLogStr

-- | @since 0.1
logStrToText :: LogStr -> Text
logStrToText = TEnc.decodeUtf8With TEncError.lenientDecode . FL.fromLogStr
