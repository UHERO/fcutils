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

  NULL (default) keeps the source's frequency; "month" averages daily or
  weekly data to months, keeping complete months only (the current month
  is dropped)

- start:

  first date to request (any form
  [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md)
  reads; default 1940)

- filters:

  named list of further query parameters. e-Stat: the table's category
  codes, e.g. `list(cdTab = "1", cdCat01 = "0001", cdArea = "00000")`.
  EIA: `route` (e.g. "steo" or "aeo/2025"), `frequency` ("monthly",
  "annual") and facets such as `scenario = "ref2025"`.

- format:

  "wide" (default), "long" or "xts", as in
  [`get_series()`](https://uhero.github.io/fcutils/reference/get_series.md)

## Value

the series, in the chosen format, named by id

## Details

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
