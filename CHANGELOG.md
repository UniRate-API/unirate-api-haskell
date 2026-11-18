# Changelog

## 0.1.0.0

- Initial release.
- Current rates (`getRate`, `getRates`), conversion (`convert`), and the
  supported-currency list (`getSupportedCurrencies`).
- Historical rates (`getHistoricalRate`, `getHistoricalRates`,
  `convertHistorical`), time series (`getTimeSeries`), and coverage limits
  (`getHistoricalLimits`) — Pro-gated endpoints.
- VAT rates (`getVatRates`, `getVatRate`).
- Injectable `Transport` for testing without the network; typed `UniRateError`
  with status-to-error mapping.
