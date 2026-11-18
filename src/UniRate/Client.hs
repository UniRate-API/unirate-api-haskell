{-# LANGUAGE OverloadedStrings #-}

-- | The UniRate API client.
--
-- Create a client with 'newClient' and call the methods below. Every method
-- runs in 'IO' and throws a 'UniRateError' on failure (non-2xx status,
-- transport failure, or an undecodable body).
--
-- > client <- newClient "YOUR_API_KEY"
-- > rate   <- getRate client "USD" "EUR"
module UniRate.Client
  ( -- * Client
    UniRateClient
  , newClient
  , newClientWith
  , clientWithTransport
  , defaultBaseUrl
  , defaultTimeout
    -- * Current rates
  , getRate
  , getRates
  , convert
  , getSupportedCurrencies
    -- * Historical (Pro-gated)
  , getHistoricalRate
  , getHistoricalRates
  , convertHistorical
  , getTimeSeries
  , getHistoricalLimits
    -- * VAT
  , getVatRates
  , getVatRate
    -- * Re-exports
  , module UniRate.Errors
  , module UniRate.Types
  ) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.URI (urlEncode)

import UniRate.Errors
import UniRate.Transport
import UniRate.Types

-- | A configured client. Build it with 'newClient'.
data UniRateClient = UniRateClient
  { ucApiKey    :: Text
  , ucBaseUrl   :: Text
  , ucTimeout   :: Int        -- ^ seconds
  , ucTransport :: Transport
  , ucUserAgent :: Text
  }

-- | @https://api.unirateapi.com@
defaultBaseUrl :: Text
defaultBaseUrl = "https://api.unirateapi.com"

-- | 30 seconds.
defaultTimeout :: Int
defaultTimeout = 30

clientVersion :: Text
clientVersion = "0.1.0"

-- | Create a client with the production TLS transport and default settings.
newClient :: Text -> IO UniRateClient
newClient apiKey = newClientWith apiKey defaultBaseUrl defaultTimeout

-- | Create a client with an explicit base URL and timeout (seconds).
newClientWith :: Text -> Text -> Int -> IO UniRateClient
newClientWith apiKey baseUrl timeout = do
  transport <- newDefaultTransport
  pure (clientWithTransport apiKey baseUrl timeout transport)

-- | Create a client with an injected 'Transport'. Used by the test suite to
-- run without touching the network; also useful for custom HTTP behaviour.
clientWithTransport :: Text -> Text -> Int -> Transport -> UniRateClient
clientWithTransport apiKey baseUrl timeout transport = UniRateClient
  { ucApiKey    = apiKey
  , ucBaseUrl   = baseUrl
  , ucTimeout   = timeout
  , ucTransport = transport
  , ucUserAgent = "unirate-haskell/" <> clientVersion
  }

-- Internal ------------------------------------------------------------------

up :: Text -> Text
up = T.toUpper

tshow :: Double -> Text
tshow = T.pack . show

encodeParam :: Text -> Text
encodeParam = TE.decodeUtf8 . urlEncode True . TE.encodeUtf8

buildUrl :: Text -> Text -> [(Text, Text)] -> Text
buildUrl base path params =
  base <> path <> "?" <> T.intercalate "&"
    [ k <> "=" <> encodeParam v | (k, v) <- params ]

headersFor :: UniRateClient -> [Header]
headersFor c =
  [ ("Accept", "application/json")
  , ("User-Agent", TE.encodeUtf8 (ucUserAgent c))
  ]

doRequest :: UniRateClient -> Text -> [(Text, Text)] -> IO BL.ByteString
doRequest c path params = do
  let url = buildUrl (ucBaseUrl c) path (("api_key", ucApiKey c) : params)
  resp <- runTransport (ucTransport c) url (headersFor c) (ucTimeout c)
  let st   = respStatus resp
      body = respBody resp
  if st >= 200 && st < 300
    then pure body
    else throwIO (errorForStatus st (TE.decodeUtf8 (BL.toStrict body)))

decodeOrThrow :: FromJSON a => BL.ByteString -> IO a
decodeOrThrow body = case eitherDecode body of
  Right a -> pure a
  Left e  -> throwIO (decodeError (T.pack e))

-- Current rates -------------------------------------------------------------

-- | Current rate for a single pair: @getRate client from to@.
getRate :: UniRateClient -> Text -> Text -> IO Double
getRate c fromC toC = do
  body <- doRequest c "/api/rates" [("from", up fromC), ("to", up toC)]
  RateResp d <- decodeOrThrow body
  pure d

-- | Current rates for every currency against a base: @getRates client from@.
getRates :: UniRateClient -> Text -> IO (Map Text Double)
getRates c fromC = do
  body <- doRequest c "/api/rates" [("from", up fromC)]
  RatesResp m <- decodeOrThrow body
  pure m

-- | Convert an amount: @convert client to amount from@.
convert :: UniRateClient -> Text -> Double -> Text -> IO Double
convert c toC amount fromC = do
  body <- doRequest c "/api/convert"
    [("from", up fromC), ("to", up toC), ("amount", tshow amount)]
  ResultResp d <- decodeOrThrow body
  pure d

-- | The list of supported currency codes.
getSupportedCurrencies :: UniRateClient -> IO [Text]
getSupportedCurrencies c = do
  body <- doRequest c "/api/currencies" []
  CurrenciesResp xs <- decodeOrThrow body
  pure xs

-- Historical (Pro-gated) ----------------------------------------------------

-- | Historical rate (or converted amount) for a single pair on a date:
-- @getHistoricalRate client date amount from to@. Pro-gated (403 on free tier).
getHistoricalRate :: UniRateClient -> Text -> Double -> Text -> Text -> IO Double
getHistoricalRate c date amount fromC toC = do
  body <- doRequest c "/api/historical/rates"
    [("date", date), ("from", up fromC), ("to", up toC), ("amount", tshow amount)]
  SingleValueResp d <- decodeOrThrow body
  pure d

-- | Historical rates for every currency against a base on a date:
-- @getHistoricalRates client date amount base@. Pro-gated.
getHistoricalRates :: UniRateClient -> Text -> Double -> Text -> IO (Map Text Double)
getHistoricalRates c date amount base = do
  body <- doRequest c "/api/historical/rates"
    [("date", date), ("from", up base), ("amount", tshow amount)]
  RatesOrResultsResp m <- decodeOrThrow body
  pure m

-- | Thin alias over 'getHistoricalRate': @convertHistorical client amount from to date@.
convertHistorical :: UniRateClient -> Double -> Text -> Text -> Text -> IO Double
convertHistorical c amount fromC toC date = getHistoricalRate c date amount fromC toC

-- | Time series of rates between two dates:
-- @getTimeSeries client start end amount base currencies@ (empty list = all).
-- Pro-gated.
getTimeSeries
  :: UniRateClient -> Text -> Text -> Double -> Text -> [Text]
  -> IO (Map Text (Map Text Double))
getTimeSeries c start end amount base currencies = do
  let baseParams =
        [ ("start_date", start), ("end_date", end)
        , ("base", up base), ("amount", tshow amount) ]
      curParams =
        [ ("currencies", T.intercalate "," (map up currencies)) | not (null currencies) ]
  body <- doRequest c "/api/historical/timeseries" (baseParams ++ curParams)
  TimeSeriesResp m <- decodeOrThrow body
  pure m

-- | Historical data coverage limits. Pro-gated.
getHistoricalLimits :: UniRateClient -> IO HistoricalLimits
getHistoricalLimits c = do
  body <- doRequest c "/api/historical/limits" []
  decodeOrThrow body

-- VAT -----------------------------------------------------------------------

-- | VAT rates for every supported country.
getVatRates :: UniRateClient -> IO VatRatesResponse
getVatRates c = do
  body <- doRequest c "/api/vat/rates" []
  decodeOrThrow body

-- | VAT rate for a single country: @getVatRate client country@.
getVatRate :: UniRateClient -> Text -> IO VatRateInfo
getVatRate c country = do
  body <- doRequest c "/api/vat/rates" [("country", up country)]
  VatDataResp v <- decodeOrThrow body
  pure v
