# Convert dates from yyyy-mm-dd to yyyyQqq format

Convert dates from yyyy-mm-dd to yyyyQqq format

## Usage

``` r
ymd_to_yQq(x)
```

## Arguments

- x:

  dates (string: yyyy-mm-dd)

## Value

formatted dates (string: yyyyQqq)

## Examples

``` r
ymd_to_yQq(c("2010-01-01", "2020-10-01"))
#> [1] "2010Q1" "2020Q4"
ymd_to_yQq(c("2010-01-01", "2020-10-01")) |> lubridate::yq()
#> [1] "2010-01-01" "2020-10-01"
```
