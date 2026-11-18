-- | Official Haskell client for the <https://unirateapi.com UniRate API> —
-- free currency exchange rates, historical data, and VAT rates.
--
-- > import UniRate
-- >
-- > main :: IO ()
-- > main = do
-- >   client <- newClient "YOUR_API_KEY"
-- >   rate   <- getRate client "USD" "EUR"
-- >   print rate
--
-- This module re-exports everything in "UniRate.Client", "UniRate.Errors",
-- "UniRate.Types" and "UniRate.Transport".
module UniRate
  ( module UniRate.Client
  , module UniRate.Transport
  ) where

import UniRate.Client
import UniRate.Transport
