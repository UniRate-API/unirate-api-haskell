# unirate-api

Official Haskell client for the [UniRate API](https://unirateapi.com) — free
currency exchange rates, historical data, and VAT rates.

- Full API parity: current rates, conversion, supported currencies, historical
  rates and time series, and VAT rates.
- Small dependency footprint — `aeson` for JSON and `http-client-tls` for HTTPS.
- Injectable `Transport` so you can unit-test without touching the network.
- Typed errors (`UniRateError`) with a clear HTTP-status mapping.

## Install

Add `unirate-api` to your project's `build-depends`, or with `cabal`:

```
cabal install unirate-api
```

Get a free API key at [unirateapi.com](https://unirateapi.com).

## Quick start

```haskell
{-# LANGUAGE OverloadedStrings #-}
import UniRate

main :: IO ()
main = do
  client <- newClient "YOUR_API_KEY"

  -- Current rate for a pair
  rate <- getRate client "USD" "EUR"
  print rate                       -- 0.92

  -- Convert an amount
  total <- convert client "EUR" 100 "USD"
  print total                      -- 92.5

  -- All rates against a base
  rates <- getRates client "USD"
  print rates                      -- fromList [("EUR",0.92), ...]

  -- Supported currencies
  currencies <- getSupportedCurrencies client
  print (length currencies)
```

## API

| Method | Description |
|---|---|
| `getRate client from to` | Current rate for a single pair. |
| `getRates client from` | Current rates for every currency against a base. |
| `convert client to amount from` | Convert an amount between currencies. |
| `getSupportedCurrencies client` | List of supported currency codes. |
| `getHistoricalRate client date amount from to` | Historical rate/amount for a pair (Pro). |
| `getHistoricalRates client date amount base` | Historical rates against a base (Pro). |
| `convertHistorical client amount from to date` | Historical conversion (Pro). |
| `getTimeSeries client start end amount base currencies` | Rates over a date range (Pro). |
| `getHistoricalLimits client` | Historical data coverage (Pro). |
| `getVatRates client` | VAT rates for all countries. |
| `getVatRate client country` | VAT rate for one country. |

Currency and country codes are upper-cased before being sent.

## Error handling

Every method throws a `UniRateError` on failure. Branch on `errKind`:

```haskell
import Control.Exception (try)
import UniRate

safeRate :: UniRateClient -> IO ()
safeRate client = do
  result <- try (getRate client "USD" "ZZZ")
  case result of
    Right r -> print r
    Left e  -> case errKind (e :: UniRateError) of
      AuthenticationError  -> putStrLn "Check your API key"
      InvalidCurrencyError -> putStrLn "Unknown currency"
      RateLimitError       -> putStrLn "Slow down"
      _                    -> putStrLn ("API error: " <> show e)
```

| HTTP status | `errKind` |
|---|---|
| 400 | `InvalidDateError` |
| 401 | `AuthenticationError` |
| 403 | `APIError` (Pro-gated endpoint on the free tier) |
| 404 | `InvalidCurrencyError` |
| 429 | `RateLimitError` |
| 503 / other | `APIError` |
| transport failure | `NetworkError` |

## Rate limits

The free tier is rate-limited. On a `429`, back off and retry. Historical and
time-series endpoints require a Pro subscription and return `403` (mapped to
`APIError`) on the free tier.

## Testing without the network

`newClient` uses a TLS-backed transport. For tests, build a client with your
own `Transport` and return canned responses:

```haskell
import qualified Data.ByteString.Lazy.Char8 as BL
import UniRate

mock :: Int -> BL.ByteString -> Transport
mock status body = Transport $ \_ _ _ -> pure (HttpResponse status body)

testClient :: UniRateClient
testClient = clientWithTransport "testkey" defaultBaseUrl defaultTimeout
               (mock 200 "{\"rate\":\"0.92\"}")
```

## Related clients

UniRate ships official clients for many languages — Python, Node.js, Go, Rust,
Java, Ruby, PHP, Swift, and more. See [unirateapi.com](https://unirateapi.com).

## License

MIT — see [LICENSE](LICENSE).
