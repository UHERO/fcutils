# Concatenate dates formatted as yyyyMm or yyyy.m to obtain period

Concatenate dates formatted as yyyyMm or yyyy.m to obtain period

## Usage

``` r
pm(dat1 = "", dat2 = "")
```

## Arguments

- dat1:

  month of period start, as a string such as "2010M10". Numbers such as
  2010.10 are misread (as January).

- dat2:

  month of period end, as for dat1

## Value

string containing date range. It ends on the first day of dat2; for
subsetting monthly data that is the whole month.
[`p()`](https://uhero.github.io/fcutils/reference/p.md) reads any date
form and includes the end period in full at any frequency.

## Examples

``` r
pm("2010M1", "2020M4")
#> [1] "2010-01-01/2020-04-01"
pm("2010M10", "2020M12")
#> [1] "2010-10-01/2020-12-01"
pm(2010.1, )
#> [1] "2010-01-01/"
pm(, 2010.1)
#> [1] "/2010-01-01"
```
