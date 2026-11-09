# Getting started

fcutils is a set of functions for the time series work behind UHERO’s
forecasts. A forecast at UHERO starts from the history of many series,
such as payroll jobs or visitor arrivals. Each series is then extended
to a date some years ahead, using assumptions about growth or levels.
The results are summarised as annual averages, growth rates, tables and
plots. fcutils covers each of those steps: getting data, converting
between formats and frequencies, extending series into the forecast
period, and showing the results.

This article covers what every script needs first: the bank options, the
data formats the functions accept, series names and how to write dates.
[Building a
forecast](https://uhero.github.io/fcutils/articles/forecasting.md) and
[Growth rates, tables and
plots](https://uhero.github.io/fcutils/articles/growth-and-reporting.md)
follow on from it.

Install the package from GitHub:

``` r

remotes::install_github("UHERO/fcutils")
```

``` r

library(fcutils)
```

## The bank

A forecast holds many series, and they need to share the same dates so
that they can be added, divided and compared. fcutils calls that common
date range the *bank*: it runs from the start of the history to the end
of the forecast period.
[`make_xts()`](https://uhero.github.io/fcutils/reference/make_xts.md),
[`addf()`](https://uhero.github.io/fcutils/reference/addf.md),
[`extend()`](https://uhero.github.io/fcutils/reference/extend.md) and
[`span()`](https://uhero.github.io/fcutils/reference/span.md) without
dates return series that cover the whole bank, and they read its first
and last date from two options. Set them once at the top of a script,
before calling any of these functions:

``` r

options(
  fcutils.bnk_start = as.Date("1990-01-01"),
  fcutils.bnk_end = as.Date("2035-12-31")
)
```

Without the options, those functions stop with an error saying so.
[`make_xts()`](https://uhero.github.io/fcutils/reference/make_xts.md)
builds an empty or constant series on the bank, or a series from a
vector of values:

``` r

make_xts(per = "quarter", val = 0) |> head(3)
#>            value
#> 1990-01-01     0
#> 1990-04-01     0
#> 1990-07-01     0
make_xts(2024.1, per = "quarter", val = c(1.5, 1.7, 1.6))
#>            value
#> 2024-01-01   1.5
#> 2024-04-01   1.7
#> 2024-07-01   1.6
```

## Data formats

R holds time series in several ways, and fcutils takes most of them. A
*wide* data frame has a `time` column and one column per series. A
*long* data frame stacks the series in three columns, `id` (the series
name), `time` and `value`. An xts object is a matrix of values with
dates attached to the rows, from the
[xts](https://joshuaulrich.github.io/xts/) package; it is convenient for
arithmetic and for subsetting by date. The functions accept all of
these, along with anything else that the
[tsbox](https://docs.ropensci.org/tsbox/) package reads, such as base
R’s ts. They return the result in the format of the input.

The package ships three wide tibbles of Hawaii data from UDAMAN, UHERO’s
data system: `quarterly_data_example` (payroll jobs by industry,
1998–2025, in thousands), `monthly_data_example` (visitor arrivals and
related series, in thousands) and `daily_data_example` (visitor arrivals
by day, 2020–2022).

``` r

q <- quarterly_data_example[, c("time", "E_NF_HI", "ECT_HI")]
tail(q, 3)
#> # A tibble: 3 × 3
#>   time       E_NF_HI ECT_HI
#>   <date>       <dbl>  <dbl>
#> 1 2024-07-01    640.   39.6
#> 2 2024-10-01    643.   39.1
#> 3 2025-01-01    648.   39.4
```

[`agg()`](https://uhero.github.io/fcutils/reference/agg.md) averages
quarters to years. Given a wide tibble it returns a wide tibble, and
given xts it returns xts:

``` r

agg(q) |> tail(3)
#> # A tibble: 3 × 3
#>   time       E_NF_HI ECT_HI
#>   <date>       <dbl>  <dbl>
#> 1 2022-01-01    618.   37.2
#> 2 2023-01-01    632.   38.1
#> 3 2024-01-01    639.   39.2
agg(conv_xts(q)) |> tail(3)
#>             E_NF_HI   ECT_HI
#> 2022-01-01 618.0704 37.17124
#> 2023-01-01 632.2629 38.10377
#> 2024-01-01 639.0901 39.17640
```

[`conv_long()`](https://uhero.github.io/fcutils/reference/conv_long.md),
[`conv_wide()`](https://uhero.github.io/fcutils/reference/conv_wide.md),
[`conv_xts()`](https://uhero.github.io/fcutils/reference/conv_xts.md)
and
[`conv_tslist()`](https://uhero.github.io/fcutils/reference/conv_tslist.md)
convert between formats. Unlike
[`tsbox::ts_long()`](https://docs.ropensci.org/tsbox/reference/ts_long.html),
[`conv_long()`](https://uhero.github.io/fcutils/reference/conv_long.md)
takes wide data frames and gives a single series an `id` column:

``` r

conv_long(q) |> head(3)
#> # A tibble: 3 × 3
#>   id      time       value
#>   <chr>   <date>     <dbl>
#> 1 E_NF_HI 1998-01-01  531.
#> 2 E_NF_HI 1998-04-01  531.
#> 3 E_NF_HI 1998-07-01  533.
conv_long(conv_xts(q)$E_NF_HI) |> head(3)
#> # A tibble: 3 × 3
#>   id      time       value
#>   <chr>   <date>     <dbl>
#> 1 E_NF_HI 1998-01-01  531.
#> 2 E_NF_HI 1998-04-01  531.
#> 3 E_NF_HI 1998-07-01  533.
```

## Series names

The examples use UHERO’s series names. A name is a mnemonic for the
concept, an `@`, the geography and a frequency: `E_NF@HI.Q` is nonfarm
payroll jobs (`E_NF`) in Hawaii (`HI`), quarterly (`Q`). When fcutils
downloads a series it drops the frequency and replaces `@` with `_`, so
the column is called `E_NF_HI`. The series used below are:

| Name       | Series                                           |
|------------|--------------------------------------------------|
| `E_NF_HI`  | nonfarm payroll jobs, all industries             |
| `ECT_HI`   | construction jobs                                |
| `EAF_HI`   | accommodation and food services jobs             |
| `EGV_HI`   | government jobs                                  |
| `VISNS_HI` | visitor arrivals, not seasonally adjusted (`NS`) |

## Dates

Wherever a function takes a date,
[`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md) reads
it. A year, a quarter or a day can be a number or a string; as a number,
a quarter is either a decimal (`2024.3`) or six digits (`202403`). A
month must be a string, because R stores `2024.10` and `2024.1` as the
same number.

``` r

to_ymd(c("2024", "2024Q3", "2024M10", "2024-10-15"))
#> [1] "2024-01-01" "2024-07-01" "2024-10-01" "2024-10-15"
to_ymd(c(2024, 2024.3, 202403, 20241015))
#> [1] "2024-01-01" "2024-07-01" "2024-07-01" "2024-10-15"
```

[`p()`](https://uhero.github.io/fcutils/reference/p.md) turns two dates
into the range string that xts uses for subsetting. A year or a quarter
given as the end is included in full:

``` r

x <- conv_xts(q)$E_NF_HI
x[p(2024.2, 2024)]
#>             E_NF_HI
#> 2024-04-01 636.4715
#> 2024-07-01 639.7316
#> 2024-10-01 643.2565
```

[`span()`](https://uhero.github.io/fcutils/reference/span.md) cuts any
format to a range.
[`find_start()`](https://uhero.github.io/fcutils/reference/find_start.md)
and
[`find_end()`](https://uhero.github.io/fcutils/reference/find_end.md)
give the first and last date with a value:

``` r

span(q, 2024) |> head(3)
#> # A tibble: 3 × 3
#>   time       E_NF_HI ECT_HI
#>   <date>       <dbl>  <dbl>
#> 1 2024-01-01    637.   38.9
#> 2 2024-04-01    636.   39.2
#> 3 2024-07-01    640.   39.6
find_end(q)
#> [1] "2025-01-01" "2025-01-01"
```

With `extend = TRUE`,
[`span()`](https://uhero.github.io/fcutils/reference/span.md) also pads
a series with NA out to the end of the range:

``` r

span(x, 2024.4, 2025.4, extend = TRUE)
#>             E_NF_HI
#> 2024-10-01 643.2565
#> 2025-01-01 647.9022
#> 2025-04-01       NA
#> 2025-07-01       NA
#> 2025-10-01       NA
```

Without any dates,
[`span()`](https://uhero.github.io/fcutils/reference/span.md) fits a
series to the bank: it cuts the series at the bank’s start and end and
pads it with NA to both. Use it to put a series made some other way,
such as an annual average from
[`agg()`](https://uhero.github.io/fcutils/reference/agg.md), on the same
dates as the rest of the bank:

``` r

span(x) |> tsbox::ts_summary()
#>        id obs    diff freq      start        end
#> 1 span(x) 109 3 month    4 1998-01-01 2025-01-01
```

## Getting data

These functions need network access and keys, so this page does not run
them.

[`get_series()`](https://uhero.github.io/fcutils/reference/get_series.md)
downloads series from UDAMAN, UHERO’s data system, by name.
[`get_series_exp()`](https://uhero.github.io/fcutils/reference/get_series_exp.md)
downloads every series in a UDAMAN export. Both need the UDAMAN token,
stored once with
[`set_udaman_token()`](https://uhero.github.io/fcutils/reference/set_udaman_token.md)
(or as `udaman_token` in `.Renviron`), and the UHERO VPN unless
`public = TRUE`.

``` r

get_series(c("VISNS@HI.M", "E_NF@HI.Q"))
get_series_exp(74, format = "xts")
```

[`get_series_src()`](https://uhero.github.io/fcutils/reference/get_series_src.md)
downloads from FRED, Statistics Canada, the Bank of Canada, Japan’s
e-Stat and the EIA. FRED, e-Stat and the EIA need a key, stored with
[`set_api_key()`](https://uhero.github.io/fcutils/reference/set_api_key.md)
or in `.Renviron` (`FRED_API_KEY`, `ESTAT_API_KEY`, `EIA_API_KEY`):

``` r

get_series_src("UNRATE", "fred")
get_series_src(c("PAYEMS", "CPIAUCSL"), "fred", format = "xts")
```

Next, [Building a
forecast](https://uhero.github.io/fcutils/articles/forecasting.md)
extends series into the forecast period.
