# Parse strings into dates in yyyy-mm-dd format

Parse strings into dates in yyyy-mm-dd format

## Usage

``` r
to_ymd(x)
```

## Arguments

- x:

  string (string: yyyymmdd, yyyyqq, yyyy.q, yyyy)

## Value

formatted dates (yyyy-mm-dd)

## Examples

``` r
to_ymd(c("2010.0211", 202002, 2020.2, "2020"))
#> [1] "2010-02-11" "2020-04-01" "2020-04-01" "2020-01-01"
```
