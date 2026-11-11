# Download series from outside sources

Download series from outside sources

## Usage

``` r
get_series_src(
  id,
  source = c("fred", "statcan", "boc", "estat", "eia"),
  freq = NULL,
  start = NULL,
  filters = list(),
  format = c("wide", "long", "xts")
)
```

## Arguments

- id:

  series ids: FRED codes ("UNRATE"), StatCan vectors ("v2062810"), Bank
  of Canada Valet series ("FXMUSDCAD"), e-Stat table ids ("0004052037")
  or EIA series ids ("WTIPUUS"). Several ids are read one by one.

- source:

  "fred", "statcan", "boc", "estat" or "eia"

- freq:

  NULL (default) keeps the source's frequency. "month" or "quarter"
  averages the data to that frequency, which must be lower than the
  source's (daily, weekly or monthly to months or quarters). See Details
  for which periods are kept.

- start:

  first date to request (any form
  [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md)
  reads; default 1940). The data run from `start` to the latest date the
  source has published; there is no end date.

- filters:

  named list of further query parameters. e-Stat: the table's category
  codes, e.g. `list(cdTab = "1", cdCat01 = "0001", cdArea = "00000")`.
  EIA: `route` (e.g. "steo" or "aeo/2025"), `frequency` ("monthly",
  "annual") and facets such as `scenario = "ref2025"`; EIA returns at
  most 5,000 observations per series. FRED: added to the request as
  query parameters, e.g. `list(units = "pc1")`. StatCan and Bank of
  Canada: ignored.

- format:

  "wide" (default), "long" or "xts", as in
  [`get_series()`](https://uhero.github.io/fcutils/reference/get_series.md)

## Value

the series, in the chosen format, named by id

## Details

With `freq`, each period is the mean of the observations in it, and
these periods are left out:

- the current calendar period, which has not ended yet;

- a period that `start` falls inside of, e.g. the first quarter when
  `start` is February 15;

- for monthly (or quarterly) data, the first or last period of a series
  when it lacks its first or last month: a quarter with October and
  November published but not December. FRED leaves such a quarter out
  too.

Periods inside a series are kept when an observation is missing; the
mean is over the observations present. A quarter without October
averages November and December, as FRED does.

Keys come from the environment or the keyring (see
[`set_api_key()`](https://uhero.github.io/fcutils/reference/set_api_key.md));
StatCan and the Bank of Canada need none. Downloads are tried three
times. StatCan answers with an empty list during its nightly
maintenance; that gives an error saying so. For e-Stat, the filters must
leave one value per date; monthly and annual time codes are read.

## Examples

``` r
if (FALSE) { # interactive()
get_series_src(c("UNRATE", "CIVPART"), "fred", start = 2020)
get_series_src("DCOILWTICO", "fred", freq = "month", start = 2024)
get_series_src("UNRATE", "fred", freq = "quarter", start = 2024)
get_series_src("v2062810", "statcan")
get_series_src("FXMUSDCAD", "boc", start = 2017)
get_series_src(
  "0004052037", "estat",
  filters = list(cdTab = "1", cdCat01 = "0001", cdArea = "00000")
)
get_series_src(
  "WTIPUUS", "eia",
  filters = list(route = "steo", frequency = "monthly")
)
}
```
