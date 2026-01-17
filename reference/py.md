# Concatenate dates formatted as yyyy to obtain period

Concatenate dates formatted as yyyy to obtain period

## Usage

``` r
py(dat1 = "", dat2 = "")
```

## Arguments

- dat1:

  year of period start (string or numeric: yyyy)

- dat2:

  year of period end (string or numeric: yyyy)

## Value

string containing date range

## Examples

``` r
py("2010", "2020")
#> [1] "2010-01-01/2020-01-01"
py(2010, 2020)
#> [1] "2010-01-01/2020-01-01"
py(2010, )
#> [1] "2010-01-01/"
py(, 2010)
#> [1] "/2010-01-01"
```
