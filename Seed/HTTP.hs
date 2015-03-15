{-# LANGUAGE OverloadedStrings #-}
module Seed.HTTP where

import           Blaze.ByteString.Builder (copyByteString)
import           Data.Aeson (encode, ToJSON)
import           Network.HTTP.Types.Status
import           Network.Wai (Response, responseBuilder)
import qualified Data.ByteString.Lazy as BL

-- | http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.1
s200 :: (ToJSON a) => a -> Response
s200 b = responseBuilder status200 []
       $ copyByteString $ BL.toStrict $ encode b

-- | http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.2
s201 :: (ToJSON a) => a -> Response
s201 b = responseBuilder status201 []
       $ copyByteString $ BL.toStrict $ encode b

-- | 400 Bad Request
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.1
s400 :: Response
s400 = responseBuilder status400 [("Content-Type", "text/plain")]
     $ copyByteString "400"

-- | 404 Not Found
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.5
s404 :: Response
s404 = responseBuilder status404 [("Content-Type", "text/plain")]
     $ copyByteString "404"

-- | 500 Internal Server Error
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.5.1
s500 :: Response
s500 = responseBuilder status500 []
     $ copyByteString ""
