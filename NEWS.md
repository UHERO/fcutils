# fcutils (development version)

* Renamed for consistency: `multi_chain()` is now `chain()`, `aggr()` and
  `faggr()` are now `agg()` and `fagg()`, and `ytd_cum()`, `fytd_cum()`,
  `mtd_cum()` and `ptd_cum()` are now `ytd_agg()`, `fytd_agg()`,
  `mtd_agg()` and `ptd_agg()`. The new functions take the same arguments
  and give the same results. The old names still work but warn; they will
  be removed in a later version (see `?"fcutils-deprecated"`). No installed
  package exports any of the new names.

# fcutils 0.1.2.1

* `rebase()` is renamed `index_to_base()`. It sets a series to a value in a
  base period, which is indexing. Rebasing is a different operation: it
  re-expresses a real series, such as real GDP, in the prices of another
  year. Replace `rebase(x, ...)` with `index_to_base(x, ...)`.

# fcutils 0.1.2

Results are unchanged.

* `multi_chain(verbose = FALSE)` does not print the series it extends.
* Faster growth rates and aggregation, with identical results. For monthly,
  quarterly and annual data with no missing dates, `pcmp()`, the to-date
  growth functions and `ptd_gr()` take a lag by position within each series
  instead of through `tsbox::ts_lag()`, and `aggr()` and `faggr()` take grouped
  means or sums instead of `tsbox::ts_frequency()` (when missing values sit
  only at the start or end of each series). Anything else goes through tsbox
  as before. On 200 quarterly series over a century: `aggr()` 0.4 s
  (4.5 s in 0.1.0), `pcmp()` 0.14 s (3.4 s), `ytd_gr()` 0.3 s (3.3 s),
  `ptd_gr()` 0.4 s (3.6 s). A test compares each fast path with tsbox on
  random series.
* Faster on large banks, with identical results: `conv_long()`, which most
  functions call first, skips `tsbox::ts_regular()`'s checks when no date is
  missing (0.9 s to 0.07 s on 200 quarterly series over a century), and
  `multi_chain()` places each chained series back by date instead of a join
  per series. On that bank `multi_chain()` takes 0.7 s instead of 3 s,
  `aggr()` 2.7 s instead of 4.5 s and `pcmp()` 1.5 s instead of 3.5 s.
* Less fixed cost per call, with identical results. `conv_long()` no longer
  asks `tsbox::ts_summary()` for the name of a single series that already
  has one, and `find_start()` and `find_end()` take the first and last date
  with a value directly. On one quarterly series `conv_long()` takes 4 ms
  instead of 9 ms and `find_end()` 5 ms instead of 13 ms; every function
  that starts with `conv_long()` gains about 4 ms.
* `get_series_exp()` combines the downloaded series in one step instead of
  one pair at a time (200 series: 0.02 s instead of 2 s).
* `ytd_cum()`, `ytd_gr()`, `fytd_cum()`, `fytd_gr()`, `mtd_cum()`,
  `mtd_gr()`, `ptd_cum()` and `ptd_gr()` share one implementation. Old and
  new give identical results on monthly, quarterly and daily data in every
  format.
* `plot_comp_2()` and `plot_comp_3()` share one implementation; the index
  panel is the only difference.

# fcutils 0.1.1

Results are unchanged, except where `faggr()` used to fail.

* `ts_rebase()` is renamed `rebase()`, because the `ts_` prefix marks tsbox
  functions. No other installed package exports a `rebase()`.

* `faggr()` aggregates all series at once instead of each series and year
  separately: 0.4 s instead of 33 s for 20 quarterly series over a century.
  It no longer stops on a fiscal year with no values (a series that starts
  or ends with missing values).
* `aggr()` and `pcmp()` aggregate or compute all series at once, and
  `conv_long()` reads series names from the id column instead of
  `ts_summary()`. On a bank of 200 quarterly series over a century, `aggr()`
  takes 3.5 s instead of 4.5 s and `pcmp()` 2.0 s instead of 3.5 s; most of
  what remains is `tsbox::ts_frequency()` and `tsbox::ts_regular()`.
* `get_series()` and `get_series_exp()` share their request, naming and
  format code. An unknown `rename` or `format` now stops with a message.
* The code is split into files by topic, and an internal helper replaces the
  block that returned results in the input's class in 19 functions.

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
  `xts::index()`. There is no deprecated alias: it would keep masking them,
  and its help page would overwrite the site's reference index. Replace
  `index(x, ...)` with `ts_rebase(x, ...)`.

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
* `multi_chain()` accepts series stored as integers.
* `save_plot_list()` finds the pandoc that comes with Quarto.

## Other

* Tests (testthat 3) cover the exported functions.
