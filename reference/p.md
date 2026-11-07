# Concatenate dates to obtain period

Concatenate dates to obtain period

## Usage

``` r
p(dat1 = "", dat2 = "")
```

## Arguments

- dat1:

  date of period start (any form read by
  [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md))

- dat2:

  date of period end (any form read by
  [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md))

## Value

string containing date range, for subsetting xts objects

## Details

The range runs from the first day of dat1 to the last day of dat2: a
year, quarter or month given as dat2 is included in full, so
`p(2019.4, 2026)` ends on 2026-12-31. A day given as dat2 is kept as is.

## Examples

``` r
p("2010-01-01", "2020-01-01")
#> [1] "2010-01-01/2020-01-01"
p(20100101, 20200101)
#> [1] "2010-01-01/2020-01-01"
p(2010.1, 2020.4)
#> [1] "2010-01-01/2020-12-31"
p(,2020.4)
#> [1] "/2020-12-31"
p("2010Q1", "2020Q4")
#> [1] "2010-01-01/2020-12-31"
p(2010, 2020)
#> [1] "2010-01-01/2020-12-31"
p("2010M1", "2010M6")
#> [1] "2010-01-01/2010-06-30"
```
