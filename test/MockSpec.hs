{-# LANGUAGE OverloadedStrings #-}

-- | Mock test suite. Runs fully offline via an injected 'Transport' — never
-- touches the network. A tiny stdlib-only harness keeps the test dependency
-- tree empty (no hspec, no QuickCheck).
--
-- Set @FORCE_FAIL=1@ to inject one guaranteed-false assertion: a
-- negative-control proving the harness exits non-zero when a check fails.
module Main (main) where

import Control.Exception (try)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text, isInfixOf)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

import UniRate

-- A transport that always returns the given status and body.
constTransport :: Int -> BL.ByteString -> Transport
constTransport st body = Transport $ \_ _ _ -> pure (HttpResponse st body)

-- A transport that records the request URL (into the IORef) and returns 200 + body.
recordingTransport :: (Text -> IO ()) -> BL.ByteString -> Transport
recordingTransport record body = Transport $ \url _ _ -> do
  record url
  pure (HttpResponse 200 body)

clientOK :: BL.ByteString -> UniRateClient
clientOK body =
  clientWithTransport "testkey" defaultBaseUrl defaultTimeout (constTransport 200 body)

clientStatus :: Int -> BL.ByteString -> UniRateClient
clientStatus st body =
  clientWithTransport "testkey" defaultBaseUrl defaultTimeout (constTransport st body)

-- Minimal assertion harness --------------------------------------------------

type Check = IO Bool

pass :: String -> IO Bool
pass name = printf "  ok   %s\n" name >> pure True

failWith :: String -> String -> IO Bool
failWith name msg = printf "  FAIL %s — %s\n" name msg >> pure False

checkEq :: (Eq a, Show a) => String -> a -> a -> Check
checkEq name expected actual
  | expected == actual = pass name
  | otherwise = failWith name ("expected " <> show expected <> ", got " <> show actual)

checkBool :: String -> Bool -> Check
checkBool name True  = pass name
checkBool name False = failWith name "expected True"

-- Run an action expected to throw a UniRateError of a given kind.
expectKind :: String -> UniRateErrorKind -> IO a -> Check
expectKind name kind act = do
  r <- try act
  case r of
    Left e | errKind e == kind -> pass name
           | otherwise -> failWith name ("wrong kind: " <> show (errKind e))
    Right _ -> failWith name "expected an error, got a value"

approx :: Double -> Double -> Bool
approx a b = abs (a - b) < 1e-9

main :: IO ()
main = do
  forceFail <- maybe False (== "1") <$> lookupEnv "FORCE_FAIL"

  -- request-building assertions need a recorded URL
  urlRef <- newIORef (""::Text)
  let recClient body =
        clientWithTransport "testkey" defaultBaseUrl defaultTimeout
          (recordingTransport (writeIORef urlRef) body)

  checks <- sequence $
    [ do c <- pure (clientOK "{\"rate\":\"0.92\"}")
         r <- getRate c "usd" "eur"
         checkBool "getRate parses and returns 0.92" (approx r 0.92)

    , do c <- pure (clientOK "{\"rates\":{\"EUR\":\"0.92\",\"GBP\":\"0.79\"}}")
         m <- getRates c "USD"
         checkBool "getRates parses a two-entry map"
           (Map.size m == 2 && maybe False (approx 0.79) (Map.lookup "GBP" m))

    , do c <- pure (clientOK "{\"result\":\"92.50\"}")
         r <- convert c "EUR" 100 "USD"
         checkBool "convert parses result 92.50" (approx r 92.5)

    , do c <- pure (clientOK "{\"currencies\":[\"USD\",\"EUR\",\"GBP\"]}")
         xs <- getSupportedCurrencies c
         checkEq "getSupportedCurrencies parses list" ["USD","EUR","GBP"] xs

    , do c <- pure (clientOK "{\"rate\":\"0.92\"}")
         r <- getHistoricalRate c "2024-01-01" 1 "USD" "EUR"
         checkBool "getHistoricalRate uses rate branch (amount 1)" (approx r 0.92)

    , do c <- pure (clientOK "{\"result\":\"92.00\"}")
         r <- getHistoricalRate c "2024-01-01" 100 "USD" "EUR"
         checkBool "getHistoricalRate uses result branch (amount != 1)" (approx r 92.0)

    , do c <- pure (clientOK "{\"rates\":{\"EUR\":\"0.92\"}}")
         m <- getHistoricalRates c "2024-01-01" 1 "USD"
         checkBool "getHistoricalRates uses rates branch"
           (maybe False (approx 0.92) (Map.lookup "EUR" m))

    , do c <- pure (clientOK "{\"results\":{\"EUR\":\"92.00\"}}")
         m <- getHistoricalRates c "2024-01-01" 100 "USD"
         checkBool "getHistoricalRates uses results branch"
           (maybe False (approx 92.0) (Map.lookup "EUR" m))

    , do c <- pure (clientOK "{\"data\":{\"2024-01-01\":{\"EUR\":0.92,\"GBP\":0.79}}}")
         m <- getTimeSeries c "2024-01-01" "2024-01-07" 1 "USD" ["EUR","GBP"]
         checkBool "getTimeSeries parses nested numeric map"
           (maybe False (\d -> maybe False (approx 0.92) (Map.lookup "EUR" d))
                        (Map.lookup "2024-01-01" m))

    , do c <- pure (clientOK "{\"total_currencies\":2,\"currencies\":{\"USD\":{\"earliest_date\":\"1999-01-01\",\"total_days\":9000}}}")
         hl <- getHistoricalLimits c
         checkBool "getHistoricalLimits parses coverage"
           (hlTotalCurrencies hl == Just 2
             && maybe False ((== Just 9000) . limitTotalDays) (Map.lookup "USD" (hlCurrencies hl)))

    , do c <- pure (clientOK "{\"total_countries\":1,\"date\":\"2026-04-22\",\"vat_rates\":{\"DE\":{\"country_code\":\"DE\",\"country_name\":\"Germany\",\"vat_rate\":19.0}}}")
         v <- getVatRates c
         checkBool "getVatRates parses the vat_rates map"
           (maybe False (\i -> vatRate i == 19.0 && vatCountryName i == "Germany")
                        (Map.lookup "DE" (vatRates v)))

    , do c <- pure (clientOK "{\"country\":\"DE\",\"vat_data\":{\"country_code\":\"DE\",\"country_name\":\"Germany\",\"vat_rate\":19.0}}")
         i <- getVatRate c "de"
         checkBool "getVatRate parses vat_data" (vatRate i == 19.0 && vatCountryCode i == "DE")

    -- error mapping
    , expectKind "401 -> AuthenticationError" AuthenticationError
        (getRate (clientStatus 401 "nope") "USD" "EUR")
    , expectKind "404 -> InvalidCurrencyError" InvalidCurrencyError
        (getRate (clientStatus 404 "nope") "USD" "ZZZ")
    , expectKind "403 -> APIError" APIError
        (getHistoricalRate (clientStatus 403 "pro") "2024-01-01" 1 "USD" "EUR")
    , expectKind "429 -> RateLimitError" RateLimitError
        (getRate (clientStatus 429 "slow down") "USD" "EUR")
    , expectKind "400 -> InvalidDateError" InvalidDateError
        (getHistoricalRate (clientStatus 400 "bad date") "nope" 1 "USD" "EUR")
    , expectKind "503 -> APIError" APIError
        (getRate (clientStatus 503 "down") "USD" "EUR")

    -- request building: api_key present, codes uppercased, encoded
    , do let c = recClient "{\"rate\":\"0.92\"}"
         _ <- getRate c "usd" "eur"
         url <- readIORef urlRef
         checkBool "request URL carries api_key + uppercased codes"
           ("api_key=testkey" `isInfixOf` url
             && "from=USD" `isInfixOf` url && "to=EUR" `isInfixOf` url)

    , do let c = recClient "{\"currencies\":[]}"
         _ <- getSupportedCurrencies c
         url <- readIORef urlRef
         checkBool "currencies endpoint path is correct"
           ("/api/currencies?" `isInfixOf` url)
    ]
    ++ [ checkBool "FORCE_FAIL negative control" False | forceFail ]

  let total  = length checks
      passed = length (filter id checks)
  printf "\n%d/%d checks passed\n" passed total
  if passed == total then exitSuccess else exitFailure
