# Create xts and fill with values

Create xts and fill with values

## Usage

``` r
make_xts(start = bank_start(), end = NULL, per = "year", val = NA_real_)
```

## Arguments

- start:

  date of series start (character: "yyyy-mm-dd", "yyyyqq", "yyyy")

- end:

  date of series end (character: "yyyy-mm-dd", "yyyyqq", "yyyy")

- per:

  periodicity of series (character: "year" - default) if start is
  written as a quarter or a month (see
  [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md)),
  per is set to "quarter" or "month"

- val:

  values to fill in (numeric scalar, vector, or tibble)

## Value

an xts series

## Details

when end is missing, but val is a vector of more than one element, the
end date is automatically determined by the length of the val vector. if
end is missing and val is a scalar, the end date is set to the bank end,
`getOption("fcutils.bnk_end")`. The default start is the bank start,
`getOption("fcutils.bnk_start")`. Set both once per session, e.g. in
setup.R:
`options(fcutils.bnk_start = as.Date("1970-01-01"), fcutils.bnk_end = as.Date("2070-12-31"))`.
if end is missing the remaining arguments have to be named. if val is a
tibble, the end date is automatically determined by the number of rows
in the tibble.

## Examples

``` r
options(fcutils.bnk_start = as.Date("1970-01-01"), fcutils.bnk_end = as.Date("2070-12-31"))
make_xts()
#>            value
#> 1970-01-01    NA
#> 1971-01-01    NA
#> 1972-01-01    NA
#> 1973-01-01    NA
#> 1974-01-01    NA
#> 1975-01-01    NA
#> 1976-01-01    NA
#> 1977-01-01    NA
#> 1978-01-01    NA
#> 1979-01-01    NA
#>        ...      
#> 2061-01-01    NA
#> 2062-01-01    NA
#> 2063-01-01    NA
#> 2064-01-01    NA
#> 2065-01-01    NA
#> 2066-01-01    NA
#> 2067-01-01    NA
#> 2068-01-01    NA
#> 2069-01-01    NA
#> 2070-01-01    NA
make_xts(val = 0, per = "m")
#>            value
#> 1970-01-01     0
#> 1970-02-01     0
#> 1970-03-01     0
#> 1970-04-01     0
#> 1970-05-01     0
#> 1970-06-01     0
#> 1970-07-01     0
#> 1970-08-01     0
#> 1970-09-01     0
#> 1970-10-01     0
#>        ...      
#> 2070-03-01     0
#> 2070-04-01     0
#> 2070-05-01     0
#> 2070-06-01     0
#> 2070-07-01     0
#> 2070-08-01     0
#> 2070-09-01     0
#> 2070-10-01     0
#> 2070-11-01     0
#> 2070-12-01     0
make_xts(start = 20100101, per = "quarter", val = 0)
#>            value
#> 2010-01-01     0
#> 2010-04-01     0
#> 2010-07-01     0
#> 2010-10-01     0
#> 2011-01-01     0
#> 2011-04-01     0
#> 2011-07-01     0
#> 2011-10-01     0
#> 2012-01-01     0
#> 2012-04-01     0
#>        ...      
#> 2068-07-01     0
#> 2068-10-01     0
#> 2069-01-01     0
#> 2069-04-01     0
#> 2069-07-01     0
#> 2069-10-01     0
#> 2070-01-01     0
#> 2070-04-01     0
#> 2070-07-01     0
#> 2070-10-01     0
make_xts(start = 2010.1, per = "q", val = 1:10)
#>            value
#> 2010-01-01     1
#> 2010-04-01     2
#> 2010-07-01     3
#> 2010-10-01     4
#> 2011-01-01     5
#> 2011-04-01     6
#> 2011-07-01     7
#> 2011-10-01     8
#> 2012-01-01     9
#> 2012-04-01    10
make_xts(2010.1, val = 1:10) # automatically set per = "quarter"
#>            value
#> 2010-01-01     1
#> 2010-04-01     2
#> 2010-07-01     3
#> 2010-10-01     4
#> 2011-01-01     5
#> 2011-04-01     6
#> 2011-07-01     7
#> 2011-10-01     8
#> 2012-01-01     9
#> 2012-04-01    10
make_xts(start = "2010-01-01", per = "m", val = 0)
#>            value
#> 2010-01-01     0
#> 2010-02-01     0
#> 2010-03-01     0
#> 2010-04-01     0
#> 2010-05-01     0
#> 2010-06-01     0
#> 2010-07-01     0
#> 2010-08-01     0
#> 2010-09-01     0
#> 2010-10-01     0
#>        ...      
#> 2070-03-01     0
#> 2070-04-01     0
#> 2070-05-01     0
#> 2070-06-01     0
#> 2070-07-01     0
#> 2070-08-01     0
#> 2070-09-01     0
#> 2070-10-01     0
#> 2070-11-01     0
#> 2070-12-01     0
make_xts(start = 201001, per = "q",
         val = tibble::tibble(E_NF_HON = c(1:10), ECT_HI = c(11:20)))
#>            E_NF_HON ECT_HI
#> 2010-01-01        1     11
#> 2010-04-01        2     12
#> 2010-07-01        3     13
#> 2010-10-01        4     14
#> 2011-01-01        5     15
#> 2011-04-01        6     16
#> 2011-07-01        7     17
#> 2011-10-01        8     18
#> 2012-01-01        9     19
#> 2012-04-01       10     20
```
