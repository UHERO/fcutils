# fcutils 0.1.0

## Breaking changes

* The bank is set by options, not by package data. `make_xts()`, `addf()`
  and the other functions that need a bank read
  `getOption("fcutils.bnk_start")` and `getOption("fcutils.bnk_end")`, and
  stop with a message if they are not set. Set them once, for example in
  setup.R:

  ```r
  options(
    fcutils.bnk_start = as.Date("1970-01-01"),
    fcutils.bnk_end = as.Date("2070-12-31")
  )
  ```

  The datasets `bnk_start` and `bnk_end` are gone.
* `to_ymd()` reads a fixed set of forms and stops with an error naming any
  value it cannot read, where it used to return `NA` or guess. Accepted:
  years (`2019`), quarters (`2019.4`, `"2019Q4"`, `201904`), months
  (`"2019M10"`, `"2019-10"`) and days (`"2019-10-15"`, `20191015`, Dates).
  Months cannot be numbers, because `2019.10` equals `2019.1`.
* `p()` ends at the last day of the period given as its end: `p(2019.4, 2026)`
  is `"2019-10-01/2026-12-31"`, not `.../2026-01-01`.
* `index()` is renamed `ts_rebase()`, because it masked `zoo::index()` and
  `xts::index()`. `index()` still works, with a deprecation warning, and
  will be removed in the next version.

## New functions

* `extend()` continues a series past its last observation: through anchor
  values (`type = "level"`), through anchors on its annualized growth rate
  (`type = "growth"`), or with the growth of another series
  (`type = "chain"`). Linear or Stineman interpolation, optional loess
  smoothing.
* `get_series_src()` downloads series from FRED, Statistics Canada, the Bank
  of Canada, e-Stat and EIA.
* `set_api_key()` stores a key for the session or in the keyring;
  `set_udaman_token()` now calls it. Keys never appear in error messages or
  warnings.

## New arguments

* `multi_chain()`: `method = "diff"` adds y's changes to x's level;
  `direction = "forward"` or `"backward"` chains only one way;
  `match = "stem"` pairs `UR_AT_US_A` with `UR_AT_US_SOLA`.
* `disagg(extend_with)`: a series or `"last_growth"` extends the series
  before interpolating, so the result does not bend toward zero growth at
  the ends. The result is cut back to the series' own periods.
* `interpol(pattern = NULL)` interpolates linearly.
* `addf()` takes a table of ramps (columns start, end, from, to), or points
  as formulas: `addf(2026.3 ~ 0, 2027.4 ~ -0.3, 2030.4 ~ 0)`.
* `extend()` takes anchors as formulas, `2026.4 ~ 4.4`, so dates need no
  quotes.
* `plot_fc(rate = FALSE)` plots changes instead of growth rates.

## Fixes

* `AtoQ()` and `QtoA()` rename only the frequency ending of a name
  (`_SOLA` to `_SOLQ`), not the same letters elsewhere in it.
* `multi_chain()` matches the `.x` and `.y` suffixes only at the end of a
  name.
* `save_plot_list()` finds the pandoc that comes with Quarto.

## Other

* Tests (testthat 3) cover the exported functions.
