{-# LANGUAGE ScopedTypeVariables #-}
module Seed.Util where

import           Control.Concurrent (ThreadId)
import           Control.Monad (liftM)
import           Data.Aeson as JSON
import           Data.Conduit (Sink)
import           Data.Text (Text)
import           System.Exit (ExitCode(..))
import           System.Log.FastLogger (rmLoggerSet, LoggerSet)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T

extractJSONBody :: (FromJSON a) => Sink BC.ByteString IO (Maybe a)
extractJSONBody = do
  mma <- liftM JSON.decode CB.sinkLbs
  case mma of
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
