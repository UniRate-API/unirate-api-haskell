{-# LANGUAGE OverloadedStrings #-}

-- | Response models and the JSON decoders that the client uses internally.
--
-- Rate values arrive from the API as JSON strings in some endpoints and as
-- JSON numbers in others; 'FlexDouble' accepts either.
module UniRate.Types
  ( FlexDouble (..)
  , RateResp (..)
  , RatesResp (..)
  , ResultResp (..)
  , CurrenciesResp (..)
  , SingleValueResp (..)
  , RatesOrResultsResp (..)
  , TimeSeriesResp (..)
  , VatRateInfo (..)
  , VatRatesResponse (..)
  , VatDataResp (..)
  , LimitInfo (..)
  , HistoricalLimits (..)
  ) where

import Data.Aeson (FromJSON (..), Value (Number, String), withObject, (.:), (.:?))
import Data.Map.Strict (Map)
import Data.Text (Text, unpack)
import Text.Read (readMaybe)

-- | A 'Double' that decodes from either a JSON number or a JSON string.
newtype FlexDouble = FlexDouble { unFlex :: Double }
  deriving (Show, Eq)

instance FromJSON FlexDouble where
  parseJSON v@(Number _) = FlexDouble <$> parseJSON v
  parseJSON (String s)   = case readMaybe (unpack s) of
    Just d  -> pure (FlexDouble d)
    Nothing -> fail ("expected a numeric string, got " <> unpack s)
  parseJSON _ = fail "expected a number or numeric string for a rate"

-- | @{ "rate": "0.92" }@
newtype RateResp = RateResp Double deriving (Show, Eq)

instance FromJSON RateResp where
  parseJSON = withObject "rate response" $ \o -> do
    FlexDouble d <- o .: "rate"
    pure (RateResp d)

-- | @{ "rates": { "EUR": "0.92", ... } }@
newtype RatesResp = RatesResp (Map Text Double) deriving (Show, Eq)

instance FromJSON RatesResp where
  parseJSON = withObject "rates response" $ \o -> do
    m <- o .: "rates"
    pure (RatesResp (fmap unFlex (m :: Map Text FlexDouble)))

-- | @{ "result": "92.50" }@
newtype ResultResp = ResultResp Double deriving (Show, Eq)

instance FromJSON ResultResp where
  parseJSON = withObject "result response" $ \o -> do
    FlexDouble d <- o .: "result"
    pure (ResultResp d)

-- | @{ "currencies": ["USD", "EUR", ...] }@
newtype CurrenciesResp = CurrenciesResp [Text] deriving (Show, Eq)

instance FromJSON CurrenciesResp where
  parseJSON = withObject "currencies response" $ \o ->
    CurrenciesResp <$> o .: "currencies"

-- | A historical single value: @{ "result": ... }@ (amount /= 1) or
-- @{ "rate": ... }@ (amount == 1).
newtype SingleValueResp = SingleValueResp Double deriving (Show, Eq)

instance FromJSON SingleValueResp where
  parseJSON = withObject "historical single value" $ \o -> do
    mr <- o .:? "result"
    case mr of
      Just (FlexDouble d) -> pure (SingleValueResp d)
      Nothing -> do
        FlexDouble d <- o .: "rate"
        pure (SingleValueResp d)

-- | A historical rate map: @{ "results": {...} }@ (amount /= 1) or
-- @{ "rates": {...} }@ (amount == 1).
newtype RatesOrResultsResp = RatesOrResultsResp (Map Text Double) deriving (Show, Eq)

instance FromJSON RatesOrResultsResp where
  parseJSON = withObject "historical rate map" $ \o -> do
    mr <- o .:? "results"
    m <- case mr of
      Just x  -> pure x
      Nothing -> o .: "rates"
    pure (RatesOrResultsResp (fmap unFlex (m :: Map Text FlexDouble)))

-- | @{ "data": { "2024-01-01": { "EUR": 0.92, ... }, ... } }@
newtype TimeSeriesResp = TimeSeriesResp (Map Text (Map Text Double)) deriving (Show, Eq)

instance FromJSON TimeSeriesResp where
  parseJSON = withObject "timeseries response" $ \o -> do
    m <- o .: "data"
    pure (TimeSeriesResp (fmap (fmap unFlex) (m :: Map Text (Map Text FlexDouble))))

-- | VAT details for one country.
data VatRateInfo = VatRateInfo
  { vatCountryCode :: Text
  , vatCountryName :: Text
  , vatRate        :: Double
  } deriving (Show, Eq)

instance FromJSON VatRateInfo where
  parseJSON = withObject "vat rate info" $ \o -> VatRateInfo
    <$> o .: "country_code"
    <*> o .: "country_name"
    <*> (unFlex <$> o .: "vat_rate")

-- | @GET /api/vat/rates@ without a @country@.
data VatRatesResponse = VatRatesResponse
  { vatTotalCountries :: Maybe Int
  , vatDate           :: Maybe Text
  , vatRates          :: Map Text VatRateInfo
  } deriving (Show, Eq)

instance FromJSON VatRatesResponse where
  parseJSON = withObject "vat rates response" $ \o -> VatRatesResponse
    <$> o .:? "total_countries"
    <*> o .:? "date"
    <*> o .: "vat_rates"

-- | @GET /api/vat/rates?country=XX@ — wraps the @vat_data@ object.
newtype VatDataResp = VatDataResp VatRateInfo deriving (Show, Eq)

instance FromJSON VatDataResp where
  parseJSON = withObject "vat data response" $ \o ->
    VatDataResp <$> o .: "vat_data"

-- | Per-currency coverage in the historical limits response.
data LimitInfo = LimitInfo
  { limitEarliestDate :: Maybe Text
  , limitLatestDate   :: Maybe Text
  , limitTotalDays    :: Maybe Int
  , limitDescription  :: Maybe Text
  } deriving (Show, Eq)

instance FromJSON LimitInfo where
  parseJSON = withObject "limit info" $ \o -> LimitInfo
    <$> o .:? "earliest_date"
    <*> o .:? "latest_date"
    <*> o .:? "total_days"
    <*> o .:? "description"

-- | @GET /api/historical/limits@ (Pro-gated).
data HistoricalLimits = HistoricalLimits
  { hlTotalCurrencies :: Maybe Int
  , hlDataSource      :: Maybe Text
  , hlCurrencies      :: Map Text LimitInfo
  } deriving (Show, Eq)

instance FromJSON HistoricalLimits where
  parseJSON = withObject "historical limits" $ \o -> HistoricalLimits
    <$> o .:? "total_currencies"
    <*> o .:? "data_source"
    <*> o .: "currencies"
