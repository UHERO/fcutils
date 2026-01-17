# Concatenate dates formatted as yyyyMm or yyyy.m to obtain period

Concatenate dates formatted as yyyyMm or yyyy.m to obtain period

## Usage

``` r
pm(dat1 = "", dat2 = "")
```

## Arguments

- dat1:

  date of period start (string: yyyyMm or yyyy.m)

- dat2:

  date of period end (string: yyyyMm or yyyy.m)

## Value

string containing date range

## Examples

``` r
pm("2010M1", "2020M4")
#> [1] "2010-01-01/2020-04-01"
pm(2010.1, 2020.4)
#> [1] "2010-01-01/2020-04-01"
pm(2010.1, )
#> [1] "2010-01-01/"
pm(, 2010.1)
#> [1] "/2010-01-01"
```
