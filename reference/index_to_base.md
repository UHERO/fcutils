# Index series to a base period (wrapper around tsbox::ts_index())

Sets each series to `base_value` in the base period and scales the rest
of the series by the same factor. Formerly `index()`, renamed because it
masked [`zoo::index()`](https://rdrr.io/pkg/zoo/man/index.html) and
`xts::index()`; then `ts_rebase()` in fcutils 0.1.0 (the `ts_` prefix is
tsbox's) and `rebase()` in 0.1.1 and 0.1.2 (rebasing re-expresses a real
series in the prices of another year, which this function does not do).

## Usage

``` r
index_to_base(x, base_per, base_value = 100)
```

## Arguments

- x:

  ts-boxable object to be indexed

- base_per:

  base period, in any form
  [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md)
  reads. With one date, the first observation on or after it equals
  `base_value`, so `2019` sets 2019Q1 (or January 2019) to `base_value`.
  With two dates, the mean of the observations from the first date to
  the second equals `base_value`. A date stands for the first day of its
  period, so `c(2019.1, 2019.4)` covers 2019 for quarterly data, and
  monthly data need `c("2019-01-01", "2019-12-31")`.

- base_value:

  numeric value of the index at base_per (e.g. 1 or 100)

## Value

indexed object of the same type as the input

## Examples

``` r
jobs <- conv_xts(quarterly_data_example)[, c("E_NF_HI", "ECT_HI")]
# 2010Q1 = 100
round(index_to_base(jobs, 2010.1)[p(2010.1, 2010.4)], 2)
#>            E_NF_HI ECT_HI
#> 2010-01-01  100.00 100.00
#> 2010-04-01  100.19  98.08
#> 2010-07-01  100.29  96.48
#> 2010-10-01  100.89  97.83
# the average of 2010 = 100
idx <- index_to_base(jobs, c(2010.1, 2010.4))
round(idx[p(2010.1, 2010.4)], 2)
#>            E_NF_HI ECT_HI
#> 2010-01-01   99.66 101.94
#> 2010-04-01   99.85  99.98
#> 2010-07-01   99.95  98.35
#> 2010-10-01  100.54  99.72
colMeans(idx[p(2010.1, 2010.4)])
#> E_NF_HI  ECT_HI 
#>     100     100 
# the same with dates, and 1 as the base value
round(index_to_base(jobs, c("2010-01-01", "2010-12-31"), 1)[p(2010.1, 2010.4)], 4)
#>            E_NF_HI ECT_HI
#> 2010-01-01  0.9966 1.0194
#> 2010-04-01  0.9985 0.9998
#> 2010-07-01  0.9995 0.9835
#> 2010-10-01  1.0054 0.9972
```
