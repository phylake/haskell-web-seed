{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolymorphicComponents #-}
module Seed.Data where

import           Control.Applicative
import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Text (Text)
import           Database.PostgreSQL.Simple as PG
import           Database.Redis as R
import           GHC.Generics
import           System.Log.FastLogger (ToLogStr)
import qualified Aws
import qualified Aws.S3 as S3

data SomeJson = SomeJson {
                           someKey :: Text
                         , someOtherKey :: Text
                         } deriving (Generic)
instance FromJSON SomeJson where

data Config = Config {
                       cfgBindPort :: Int
#ifdef USE_REDIS
                     , cfgRedisPort :: Int
#endif
#ifdef USE_POSTGRESQL
                     , cfgPgUser :: String
                     , cfgPgDatabase :: String
#endif
                     , cfgLogDir :: String
                     }

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
    (v .:? "bindPort" .!= 3000) <*>
#ifdef USE_REDIS
    (v .:? "rdsPort" .!= 6379) <*>
#endif
#ifdef USE_POSTGRESQL
    (v .:? "pgConnectUser" .!= "test_user") <*>
    (v .:? "pgConnectDatabase" .!= "test_db") <*>
#endif
    (v .:? "logDir" .!= "/tmp/haskell-web-seed-logs")
  parseJSON _ = mzero

data SeedEnv = SeedEnv {
                         logInfo :: (forall a. ToLogStr a => a -> IO ())
                       , logWarn :: (forall a. ToLogStr a => a -> IO ())
                       , logError :: (forall a. ToLogStr a => a -> IO ())
#ifdef USE_REDIS
                       , rConn :: R.Connection
#endif
#ifdef USE_POSTGRESQL
                       , sqlConn :: PG.Connection
#endif
                       , awsCfg :: Aws.Configuration
                       , s3Cfg :: S3.S3Configuration Aws.NormalQuery
                       }
