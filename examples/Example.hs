{-# LANGUAGE OverloadedStrings #-}

-- | A small runnable example. Reads the API key from @UNIRATE_API_KEY@ and
-- calls a few free-tier endpoints.
--
-- > UNIRATE_API_KEY=your_key cabal run unirate-example
module Main (main) where

import Control.Monad (forM_)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Text.Printf (printf)

import UniRate

main :: IO ()
main = do
  mKey <- lookupEnv "UNIRATE_API_KEY"
  case mKey of
    Nothing -> do
      putStrLn "Set UNIRATE_API_KEY to run this example."
      exitFailure
    Just key -> do
      client <- newClient (T.pack key)

      rate <- getRate client "USD" "EUR"
      printf "1 USD = %.4f EUR\n" rate

      total <- convert client "EUR" 100 "USD"
      printf "100 USD = %.2f EUR\n" total

      currencies <- getSupportedCurrencies client
      printf "Supported currencies: %d\n" (length currencies)
      forM_ (take 5 currencies) $ \code ->
        putStrLn ("  - " <> T.unpack code)
