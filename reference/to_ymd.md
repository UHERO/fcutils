# Read dates written as years, quarters, months or days

Read dates written as years, quarters, months or days

## Usage

``` r
to_ymd(x)
```

## Arguments

- x:

  dates as numbers, strings or Dates (see details)

## Value

Dates, each the first day of the period given

## Details

Accepted forms, as numbers or strings:

- year: `2019`

- quarter: `2019.4`, `"2019Q4"`, `"2019q4"`, `201904` (yyyyqq, quarters
  01 to 04)

- month (strings only): `"2019M10"`, `"2019m3"`, `"2019-10"`

- day: `"2019-10-15"`, `20191015`, a Date

All of `2019.4`, `"2019Q4"` and `201904` give 2019-10-01. Months cannot
be numbers, because `2019.10` and `2019.1` are the same number. Anything
else stops with an error naming the value. `NA` and `""` give `NA`.

## Examples

``` r
to_ymd(c(2020, 2020.2, "2020Q2", 202002, "2020M5", "2020-05", "2020-05-15"))
#> [1] "2020-01-01" "2020-04-01" "2020-04-01" "2020-04-01" "2020-05-01"
#> [6] "2020-05-01" "2020-05-15"
```
