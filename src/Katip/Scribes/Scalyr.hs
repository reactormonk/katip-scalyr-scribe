{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Katip.Scribes.Scalyr where

-------------------------------------------------------------------------------
import           Control.Applicative        as A
import           Control.Concurrent
import           Control.Exception          (bracket_, finally)
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Text
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe                 (maybeToList)
import           Data.Monoid
import           Data.Scientific            as S
import           Data.Text                  (Text, pack)
import           Data.Text.Internal.Builder
import qualified Data.Text.Lazy             as LT
import           Data.Text.Lazy.IO          as T
import           System.IO
-------------------------------------------------------------------------------
import           Katip.Core
import           Katip.Format.Time          (formatAsLogTime)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Logs to a file handle such as stdout, stderr, or a file. Contexts
-- and other information will be flattened out into bracketed
-- fields. The flattening converts field names to uppercase, e.g.
--
-- > {"foo": {"bar": 42}}
--
-- is parsed as
--
-- > fooBar = 42
--
-- Naturally, collisions between flattened fields and actual values may
-- happen. There is currently no mitigation, and I'm not sure what Scalyr
-- does in that case. Keep your json snake_case only and you should be
-- golden.
--
-- Returns the newly-created `Scribe`. The finalizer flushes the
-- handle. Handle mode is set to 'LineBuffering' automatically.
mkScalyrScribe :: Handle -> Severity -> Verbosity -> IO Scribe
mkScalyrScribe h sev verb = do
    hSetBuffering h LineBuffering
    lock <- newMVar ()
    let logger i@Item{..} =
          when (permitItem sev i) $ bracket_ (takeMVar lock) (putMVar lock ()) $
            T.hPutStrLn h $ encodeToLazyText $ Object $ formatItem verb i
    pure $ Scribe logger (hFlush h)

-------------------------------------------------------------------------------
formatItem :: LogItem a => Verbosity -> Item a -> HM.HashMap Text Value
formatItem verb item@Item{..} =
    HM.fromList $ [
      ("timestamp", String $ formatAsLogTime _itemTime)
    , ("namespace", String $ mconcat $ intercalateNs _itemNamespace)
    , ("applicationName", String $ mconcat $ unNamespace _itemApp)
    , ("environment", String $ getEnvironment _itemEnv)
    , ("severity", String $ renderSeverity _itemSeverity)
    , ("hostname", String $ pack _itemHost)
    , ("processId", String $ pack $ show _itemProcess)
    , ("threadId", String $ getThreadIdText _itemThread)
    , ("payload", itemJson verb item)
    , ("message", String $ LT.toStrict $ toLazyText $ unLogStr _itemMessage)
    ] <>
    maybeToList (fmap (("sourceLocation",) . String . pack . locationToString) _itemLoc)


-------------------------------------------------------------------------------
-- | Creates a scribe for scalyr. Pass the application name, and the environment,
-- e.g. staging or production.
scalyrLogEnv :: Text -> Environment -> Severity -> Verbosity -> IO LogEnv
scalyrLogEnv appName env sev verb = do
  le <- initLogEnv (Namespace [appName]) env
  lh <- mkScalyrScribe stdout sev verb
  registerScribe "scalyr" lh defaultScribeSettings le
