# Calculate number of months between two dates yyyyMm or yyyy-mm-dd

Calculate number of months between two dates yyyyMm or yyyy-mm-dd

## Usage

``` r
nmons(dat1 = "", dat2 = "")
```

## Arguments

- dat1:

  date of period start (string: yyyyMm or yyyy-mm-dd; see
  [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md))

- dat2:

  date of period end (string: yyyyMm or yyyy-mm-dd; see
  [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md))

## Value

numeric length of date range in months

## Details

The endpoints are included in the result so subtract one for time
difference. Also, the result is rounded down so partial months are not
counted. See examples.

## Examples

``` r
nmons("2010M1", "2010M2")
#> [1] 2
nmons("2010M1", "2010M4")
#> [1] 4
nmons("2010-01-15", "2010-04-15")
#> [1] 4
nmons("2010-01-15", "2010-04-18")
#> [1] 4
nmons("2010-01-15", "2010-04-12")
#> [1] 3
```
