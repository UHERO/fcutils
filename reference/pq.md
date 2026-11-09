# Concatenate dates formatted as yyyyQq or yyyy.q to obtain period

Concatenate dates formatted as yyyyQq or yyyy.q to obtain period

## Usage

``` r
pq(dat1 = "", dat2 = "")
```

## Arguments

- dat1:

  date of period start (yyyyQq or yyyy.q, as string or number)

- dat2:

  date of period end (yyyyQq or yyyy.q, as string or number)

## Value

string containing date range. It ends on the first day of dat2, so on
monthly data `pq(2010.1, 2010.4)` stops at October.
[`p()`](https://uhero.github.io/fcutils/reference/p.md) includes the end
period in full.

## Examples

``` r
pq("2010Q1", "2020Q4")
#> [1] "2010-01-01/2020-10-01"
pq(2010.1, 2020.4)
#> [1] "2010-01-01/2020-10-01"
pq(2010.1, )
#> [1] "2010-01-01/"
pq(, 2010.1)
#> [1] "/2010-01-01"
```
