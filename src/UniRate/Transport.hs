{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The injectable HTTP layer.
--
-- 'Transport' abstracts the network so tests can run without touching it.
-- 'newDefaultTransport' is the production implementation, backed by
-- @http-client-tls@.
module UniRate.Transport
  ( HttpResponse (..)
  , Transport (..)
  , newDefaultTransport
  ) where

import Control.Exception (catch)
import Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
  ( HttpException
  , httpLbs
  , parseRequest
  , requestHeaders
  , responseBody
  , responseStatus
  , responseTimeout
  , responseTimeoutMicro
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (statusCode)

import UniRate.Errors (networkError)

-- | The minimal result a 'Transport' returns: HTTP status and raw body.
data HttpResponse = HttpResponse
  { respStatus :: Int
  , respBody   :: BL.ByteString
  }

-- | An injectable HTTP layer.
--
-- @runTransport url headers timeoutSeconds@ performs a GET and returns the
-- status and body — including for 4xx/5xx responses. Only a genuine transport
-- failure should throw (a 'UniRate.Errors.NetworkError').
newtype Transport = Transport
  { runTransport :: Text -> [Header] -> Int -> IO HttpResponse }

-- | The production 'Transport', backed by a shared TLS-enabled connection
-- manager. Create it once and reuse it for the life of the client.
newDefaultTransport :: IO Transport
newDefaultTransport = do
  mgr <- newTlsManager
  pure $ Transport $ \url headers timeoutSecs -> do
    req0 <- parseRequest (T.unpack url)
    let req = req0
          { requestHeaders  = headers
          , responseTimeout = responseTimeoutMicro (timeoutSecs * 1000000)
          }
    (do resp <- httpLbs req mgr
        pure (HttpResponse (statusCode (responseStatus resp)) (responseBody resp)))
      `catch` \(e :: HttpException) ->
        throwIO (networkError (T.pack (show e)))
