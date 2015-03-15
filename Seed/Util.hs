module Seed.Util where

import           Control.Concurrent (ThreadId)
import           Data.Aeson as JSON
import           Data.Text (Text)
import           System.Exit (ExitCode(ExitSuccess))
import           System.Log.FastLogger (rmLoggerSet, LoggerSet)
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
