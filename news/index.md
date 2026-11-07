# Changelog

## fcutils 0.1.0

### Breaking changes

- The bank is set by options, not by package data.
  [`make_xts()`](https://uhero.github.io/fcutils/reference/make_xts.md),
  [`addf()`](https://uhero.github.io/fcutils/reference/addf.md) and the
  other functions that need a bank read `getOption("fcutils.bnk_start")`
  and `getOption("fcutils.bnk_end")`, and stop with a message if they
  are not set. Set them once, for example in setup.R:

  ``` r

  options(
    fcutils.bnk_start = as.Date("1970-01-01"),
    fcutils.bnk_end = as.Date("2070-12-31")
  )
  ```

  The datasets `bnk_start` and `bnk_end` are gone.

- [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md)
  reads a fixed set of forms and stops with an error naming any value it
  cannot read, where it used to return `NA` or guess. Accepted: years
  (`2019`), quarters (`2019.4`, `"2019Q4"`, `201904`), months
  (`"2019M10"`, `"2019-10"`) and days (`"2019-10-15"`, `20191015`,
  Dates). Months cannot be numbers, because `2019.10` equals `2019.1`.

- [`p()`](https://uhero.github.io/fcutils/reference/p.md) ends at the
  last day of the period given as its end: `p(2019.4, 2026)` is
  `"2019-10-01/2026-12-31"`, not `.../2026-01-01`.

- `index()` is renamed
  [`ts_rebase()`](https://uhero.github.io/fcutils/reference/ts_rebase.md),
  because it masked
  [`zoo::index()`](https://rdrr.io/pkg/zoo/man/index.html) and
  `xts::index()`. There is no deprecated alias: it would keep masking
  them, and its help page would overwrite the site’s reference index.
  Replace `index(x, ...)` with `ts_rebase(x, ...)`.

### New functions

- [`extend()`](https://uhero.github.io/fcutils/reference/extend.md)
  continues a series past its last observation: through anchor values
  (`type = "level"`), through anchors on its annualized growth rate
  (`type = "growth"`), or with the growth of another series
  (`type = "chain"`). Linear or Stineman interpolation, optional loess
  smoothing.
- [`get_series_src()`](https://uhero.github.io/fcutils/reference/get_series_src.md)
  downloads series from FRED, Statistics Canada, the Bank of Canada,
  e-Stat and EIA.
- [`set_api_key()`](https://uhero.github.io/fcutils/reference/set_api_key.md)
  stores a key for the session or in the keyring;
  [`set_udaman_token()`](https://uhero.github.io/fcutils/reference/set_udaman_token.md)
  now calls it. Keys never appear in error messages or warnings.

### New arguments

- [`multi_chain()`](https://uhero.github.io/fcutils/reference/multi_chain.md):
  `method = "diff"` adds y’s changes to x’s level;
  `direction = "forward"` or `"backward"` chains only one way;
  `match = "stem"` pairs `UR_AT_US_A` with `UR_AT_US_SOLA`.
- `disagg(extend_with)`: a series or `"last_growth"` extends the series
  before interpolating, so the result does not bend toward zero growth
  at the ends. The result is cut back to the series’ own periods.
- `interpol(pattern = NULL)` interpolates linearly.
- [`addf()`](https://uhero.github.io/fcutils/reference/addf.md) takes a
  table of ramps (columns start, end, from, to), or points as formulas:
  `addf(2026.3 ~ 0, 2027.4 ~ -0.3, 2030.4 ~ 0)`.
- [`extend()`](https://uhero.github.io/fcutils/reference/extend.md)
  takes anchors as formulas, `2026.4 ~ 4.4`, so dates need no quotes.
- `plot_fc(rate = FALSE)` plots changes instead of growth rates.

### Fixes

- [`AtoQ()`](https://uhero.github.io/fcutils/reference/AtoQ.md) and
  [`QtoA()`](https://uhero.github.io/fcutils/reference/QtoA.md) rename
  only the frequency ending of a name (`_SOLA` to `_SOLQ`), not the same
  letters elsewhere in it.
- [`multi_chain()`](https://uhero.github.io/fcutils/reference/multi_chain.md)
  matches the `.x` and `.y` suffixes only at the end of a name.
- [`multi_chain()`](https://uhero.github.io/fcutils/reference/multi_chain.md)
  accepts series stored as integers.
- [`save_plot_list()`](https://uhero.github.io/fcutils/reference/save_plot_list.md)
  finds the pandoc that comes with Quarto.

### Other

- Tests (testthat 3) cover the exported functions.
