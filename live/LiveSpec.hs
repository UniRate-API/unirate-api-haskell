{-# LANGUAGE OverloadedStrings #-}

-- | Live test suite — exercises only the free-tier endpoints against the real
-- API. Gated by the @UNIRATE_API_KEY@ environment variable: with no key the
-- suite prints a skip notice and exits 0, so the default @cabal test@ (mocks)
-- stays offline. Historical/timeseries/limits are Pro-gated and deliberately
-- NOT tested here (they 403 on the free tier).
module Main (main) where

import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

import UniRate

check :: String -> Bool -> IO Bool
check name ok = do
  printf "  %s %s\n" (if ok then "ok  " else "FAIL" :: String) name
  pure ok

main :: IO ()
main = do
  mKey <- lookupEnv "UNIRATE_API_KEY"
  case mKey of
    Nothing -> do
      putStrLn "UNIRATE_API_KEY not set — skipping live tests."
      exitSuccess
    Just key -> do
      c <- newClient (T.pack key)
      results <- sequence
        [ do r <- getRate c "USD" "EUR"
             check "getRate USD->EUR is positive" (r > 0)
        , do r <- convert c "EUR" 100 "USD"
             check "convert 100 USD->EUR is positive" (r > 0)
        , do xs <- getSupportedCurrencies c
             check "getSupportedCurrencies is non-empty" (not (null xs))
        , do m <- getRates c "USD"
             check "getRates USD returns entries" (not (Map.null m))
        , do v <- getVatRates c
             check "getVatRates returns countries" (not (Map.null (vatRates v)))
        , do i <- getVatRate c "DE"
             check "getVatRate DE returns Germany" (vatCountryCode i == "DE")
        ]
      let ok = and results
      unless ok (putStrLn "live tests failed")
      if ok then exitSuccess else exitFailure
