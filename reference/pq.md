# Concatenate dates formatted as yyyyQq or yyyy.q to obtain period

Concatenate dates formatted as yyyyQq or yyyy.q to obtain period

## Usage

``` r
pq(dat1 = "", dat2 = "")
```

## Arguments

- dat1:

  date of period start (string: yyyyQq or yyyy.q)

- dat2:

  date of period end (string: yyyyQq or yyyy.q)

## Value

string containing date range

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
