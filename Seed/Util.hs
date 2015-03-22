{-# LANGUAGE RankNTypes #-}
module Seed.Util (
  extractJSONBody
, sigINTHandler
, text2ByteString
, byteString2Text
, structuredLogStr
) where

import           Control.Concurrent (ThreadId)
import           Data.Aeson as JSON
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           System.Exit (ExitCode(ExitSuccess))
import           System.Log.FastLogger
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T

extractJSONBody :: (FromJSON a) => BC.ByteString -> IO (Maybe a)
extractJSONBody bytes = do
  case JSON.decode (BLC.fromStrict bytes) of
    Just a@(Just _) -> return a
    otherwise -> return Nothing

sigINTHandler :: ThreadId -- ^ main thread
              -> [LoggerSet]
              -> IO ()
sigINTHandler tid sets = do
  mapM_ rmLoggerSet sets
  E.throwTo tid ExitSuccess

text2ByteString :: Text -> BC.ByteString
text2ByteString = BC.pack . T.unpack

byteString2Text :: BC.ByteString -> Text
byteString2Text = T.pack . BC.unpack

-- | Create a structured key-value pair log that works with logging services
-- like Loggly and Splunk
structuredLogStr :: forall a. ToLogStr a => Text -> [(Text, a)] -> LogStr
structuredLogStr msg kvps = foldl appendKvps (toLogStr msg) kvps
  where
    appendKvps acc (k, v) = acc <> toLogStr " "
                         <> toLogStr k <> toLogStr "=" <> toLogStr v
