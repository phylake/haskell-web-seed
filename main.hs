{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Aws.S3
import           Codec.Compression.GZip (compress)
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Aeson (eitherDecode)
import           Data.Conduit
import           Data.Maybe (listToMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Database.Redis as R
import           Network.HTTP.Conduit (withManager, responseBody, RequestBody(..))
import           Network.HTTP.Types.Method (parseMethod, StdMethod(..))
import           Network.HTTP.Types.Status (status200)
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Parse
import           Seed.Data
import           Seed.HTTP
import           Seed.Util
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.Log.FastLogger
import           System.Posix.Signals (installHandler, Handler(..), sigINT)
import qualified Aws
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T

main :: IO ()
main = do
  cfgPath <- liftM (maybe "config.json" id . listToMaybe) getArgs
  (eitherConfig :: Either String Config) <- liftM eitherDecode
                                          $ BLC.readFile cfgPath
  case eitherConfig of
    Left err -> do
      putStrLn "couldn't decode config file"
      putStrLn err
      exitFailure
    Right Config{..} -> do

      -- BEGIN env
#ifdef USE_REDIS
      rConn <- R.connect R.defaultConnectInfo {
        R.connectPort = PortNumber (fromIntegral cfgRedisPort)
      }
#endif

      awsCfg <- Aws.baseConfiguration
      let s3Cfg = Aws.defServiceConfig :: S3Configuration Aws.NormalQuery

      -- logging
#ifdef DEBUG
      logInfoSet <- newStdoutLoggerSet defaultBufSize
      logWarnSet <- newStdoutLoggerSet defaultBufSize
      logErrorSet <- newStdoutLoggerSet defaultBufSize
      forkIO $ forever $ do
        threadDelay 1000000 -- 1s
        flushLogStr logInfoSet
        flushLogStr logWarnSet
        flushLogStr logErrorSet
#else
      createDirectoryIfMissing True cfgLogDir
      logInfoSet <- newFileLoggerSet defaultBufSize $ cfgLogDir </> "info.log"
      logWarnSet <- newFileLoggerSet defaultBufSize $ cfgLogDir </> "warn.log"
      logErrorSet <- newFileLoggerSet defaultBufSize $ cfgLogDir </> "error.log"
#endif
      let logInfo str = pushLogStr logInfoSet $ toLogStr str <> "\n"
      let logWarn str = pushLogStr logWarnSet $ toLogStr str <> "\n"
      let logError str = pushLogStr logErrorSet $ toLogStr str <> "\n"
      let logSets = [logInfoSet, logWarnSet, logErrorSet]

      let env = SeedEnv{..}
      -- END env

      -- BEGIN signals
      tid <- myThreadId
      installHandler sigINT (Catch $ sigINTHandler tid logSets) Nothing
      -- END signals

      logInfo $ "starting haskell-web-seed on " ++ show cfgBindPort
      let staticMiddleware = staticApp $ defaultFileServerSettings "public/"
      run cfgBindPort $ seedApplication env staticMiddleware

seedApplication :: SeedEnv -> Application -> Application
seedApplication SeedEnv{..} staticMiddleware req respond = do
  logInfo $ T.concat ["/", T.intercalate "/" $ pathInfo req]
  case parseMethod (requestMethod req) of
    Right GET -> case pathInfo req of
      ["health"] -> respond $ s200 ()
#ifdef USE_REDIS
      -- curl http://localhost:3000/redis/get/foo
      ["redis", "get", rdsKey] -> do
        (eKey :: Either Reply (Maybe BC.ByteString)) <- runRedis rConn
                                                      $ R.get
                                                      $ text2ByteString rdsKey
        case eKey of
          Left rep -> logError (show rep) >> respond s500
          Right maybeRedisValue -> case maybeRedisValue of
            Nothing -> respond $ s200 ()
            Just redisValue -> respond $ s200 $ byteString2Text redisValue
#endif
      -- try to find HTML to return
      path -> staticMiddleware req respond
    Right POST -> case pathInfo req of
      -- goto http://localhost:3000/image_form.html
      ["upload", "image"] -> do
        -- http://hackage.haskell.org/package/wai-extra-2.1.1.2/docs/Network-Wai-Parse.html#v:parseRequestBody
        -- http://hackage.haskell.org/package/wai-extra-2.1.1.2/docs/Network-Wai-Parse.html#v:fileContentType
        -- http://hackage.haskell.org/package/wai-extra-2.1.1.2/docs/Network-Wai-Parse.html#v:lbsBackEnd
        images <- liftM (filter ((=="image/jpeg") . fileContentType . snd) . snd)
                $ parseRequestBody lbsBackEnd req
        mapM (logInfo . B.append "uploaded file " . fileName . snd) images
        respond $ s200 ()
      
      -- curl -d @public/some.json http://localhost:3000/upload/json
      ["upload", "json"] -> do
        (maybeSomeJson :: Maybe SomeJson) <- requestBody req >>= extractJSONBody
        case maybeSomeJson of
          Nothing -> logWarn ("/upload/json bad input" :: Text) >> respond s400
          Just (SomeJson someKey someOtherKey) -> do
            logInfo ("uploaded some json" :: Text)
            logInfo $ T.append "someKey: " someKey
            logInfo $ T.append "someOtherKey: " someOtherKey
            respond $ s200 ()
      
      -- curl -d @public/some.json http://localhost:3000/upload/aws
      ["upload", "aws"] -> do
        -- read and gzip
        someJSON <- liftM (compress . BLC.fromStrict) $ requestBody req
        
        -- upload to S3
        withManager $ \mgr -> Aws.pureAws awsCfg s3Cfg mgr
          -- change "haskell-web-seed" to a bucket you've created
          -- and ensure the bucket has the correct permissions
          (putObject "haskell-web-seed" "" $ RequestBodyLBS someJSON) {
            poContentType = Just "text/json"
          , poContentEncoding = Just "gzip"
          , poAcl = Just AclPublicRead
          }
        
        respond $ s200 ()
#ifdef USE_REDIS
      -- curl -d foo=bar http://localhost:3000/redis/set
      ["redis", "set"] -> do
        params <- liftM fst $ parseRequestBody lbsBackEnd req
        case params of
          [(key, value)] -> do
            runRedis rConn $ R.set key value
            respond $ s201 ()
          otherwise -> respond s404
#endif
      otherwise -> do
        logInfo ("Right POST otherwise" :: Text)
        respond s404
