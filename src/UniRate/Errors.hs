{-# LANGUAGE OverloadedStrings #-}

-- | Error types for the UniRate client.
--
-- Every failure the client surfaces is a 'UniRateError'. Branch on 'errKind'
-- to distinguish the cases; the mapping from HTTP status to kind mirrors the
-- shared error-mapping table used across all UniRate clients.
module UniRate.Errors
  ( UniRateError (..)
  , UniRateErrorKind (..)
  , errorForStatus
  , networkError
  , decodeError
  ) where

import Control.Exception (Exception)
import Data.Text (Text)
import qualified Data.Text as T

-- | The kind of failure.
data UniRateErrorKind
  = AuthenticationError   -- ^ 401 — missing or invalid API key.
  | InvalidCurrencyError  -- ^ 404 — currency/country not found or no data.
  | InvalidDateError      -- ^ 400 — invalid request parameters.
  | RateLimitError        -- ^ 429 — rate limit exceeded.
  | APIError              -- ^ 403 / 503 / any other non-2xx status.
  | NetworkError          -- ^ transport failure (connection, timeout, TLS).
  | DecodeError           -- ^ a 2xx body that could not be parsed.
  deriving (Show, Eq)

-- | The single error type raised by the client.
--
-- 'errStatus' carries the HTTP status (0 when there was no HTTP response, e.g.
-- a transport failure or a malformed body) and 'errBody' the raw response body
-- when the server returned one for a non-2xx status.
data UniRateError = UniRateError
  { errKind    :: UniRateErrorKind
  , errMessage :: Text
  , errStatus  :: Int
  , errBody    :: Maybe Text
  } deriving (Eq)

instance Show UniRateError where
  show e =
    "UniRateError(" <> show (errKind e) <> ", status=" <> show (errStatus e)
      <> "): " <> T.unpack (errMessage e)

instance Exception UniRateError

-- | Map a non-2xx HTTP status to the matching error.
errorForStatus :: Int -> Text -> UniRateError
errorForStatus status body = case status of
  400 -> UniRateError InvalidDateError "Invalid request parameters" status (Just body)
  401 -> UniRateError AuthenticationError "Missing or invalid API key" status (Just body)
  403 -> UniRateError APIError "Endpoint requires a Pro subscription" status (Just body)
  404 -> UniRateError InvalidCurrencyError "Currency not found or no data available" status (Just body)
  429 -> UniRateError RateLimitError "Rate limit exceeded" status (Just body)
  503 -> UniRateError APIError "Service unavailable" status (Just body)
  _   -> UniRateError APIError
           ("API error (status " <> T.pack (show status) <> "): " <> body)
           status (Just body)

-- | Build a transport-failure error.
networkError :: Text -> UniRateError
networkError msg = UniRateError NetworkError ("Network error: " <> msg) 0 Nothing

-- | Build a response-decode error.
decodeError :: Text -> UniRateError
decodeError msg = UniRateError DecodeError ("Failed to decode response: " <> msg) 0 Nothing
