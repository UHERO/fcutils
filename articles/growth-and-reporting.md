# Growth rates, tables and plots

A forecast is a set of levels, such as thousands of jobs in each
quarter. Readers mostly want to know how fast things change, so reports
show growth rates, comparisons with a year earlier and indexes alongside
the levels. This article covers the functions that compute those (growth
rates, period-to-date totals, indexes) and those that show them (tables
and interactive plots). The examples use the package’s Hawaii data:
payroll jobs (quarterly) and visitor arrivals (monthly and daily).

``` r

library(fcutils)
options(
  fcutils.bnk_start = as.Date("1998-01-01"),
  fcutils.bnk_end = as.Date("2035-12-31")
)
q <- conv_xts(quarterly_data_example)
jobs <- q$E_NF_HI
```

## Growth rates

`pcmp(x, lag, comp_freq)` (percent change, compounded) is the percent
change over `lag` periods. With `comp_freq = 4`, the change is
compounded four times, which turns a quarterly change into an annual
pace. One function covers the usual measures for a quarterly series:

``` r

gr <- tsbox::ts_c(
  q_q = pcmp(jobs, lag = 1),                     # quarter on quarter
  q_q_ann = pcmp(jobs, lag = 1, comp_freq = 4),  # annualized
  y_y = pcmp(jobs, lag = 4),                     # year on year
  ten_yr_ann = pcmp(jobs, lag = 40, comp_freq = 4)  # average over 10 years
)
round(tail(gr, 2), 2)
#>             q_q q_q_ann  y_y ten_yr_ann
#> 2024-10-01 0.55    2.22 0.45       0.18
#> 2025-01-01 0.72    2.92 0.43       0.21
```

[`pc_to_pca()`](https://uhero.github.io/fcutils/reference/pc_to_pca.md)
and
[`pca_to_pc()`](https://uhero.github.io/fcutils/reference/pca_to_pc.md)
convert between period and annualized rates, and
[`cagr()`](https://uhero.github.io/fcutils/reference/cagr.md) gives the
compound annual growth rate between the first and last observation, in
percent, with the years elapsed:

``` r

cagr(jobs[p(2015, 2024)])
#> # A tibble: 1 × 2
#>   years_elapsed E_NF_HI
#>           <dbl>   <dbl>
#> 1          9.75   0.142
```

`ma(x, order)` is a trailing moving average that includes the current
period: `ma(x, 4)` averages each quarter with the three before it.

Series of different size are hard to compare as levels. Total jobs are
about six times accommodation and food services jobs.
`index_to_base(x, base_per)` divides each series by its value in a base
period and multiplies by 100. Both series then equal 100 in the base
period, and the index shows the percent above or below the base. A year
or two dates as the base set the average over that period to 100. Here
the two series against 2019, through the 2020 pandemic:

``` r

idx <- index_to_base(q[, c("E_NF_HI", "EAF_HI")], 2019)
round(idx[p(2019.4, 2020.3)], 1)
#>            E_NF_HI EAF_HI
#> 2019-10-01   100.3  100.1
#> 2020-01-01   100.4  100.3
#> 2020-04-01    78.2   45.8
#> 2020-07-01    79.5   45.1
```

## Period to date

Monthly and daily data that are not seasonally adjusted, such as visitor
arrivals, rise and fall with the seasons. Comparing March with February
mixes seasonal swings with real change. A common fix is to compare the
months of this year so far with the same months a year earlier. The
*to-date* functions do this. They accumulate within a year, fiscal year
(July to June), quarter or month, and compare with the same span a year
earlier.

| Function | Period | Returns |
|----|----|----|
| `ytd_agg(x, avg)` | calendar year | average (or sum, `avg = FALSE`) so far |
| `ytd_gr(x)` | calendar year | % change of the year so far on a year earlier |
| [`fytd_agg()`](https://uhero.github.io/fcutils/reference/fytd_agg.md), [`fytd_gr()`](https://uhero.github.io/fcutils/reference/fytd_gr.md) | fiscal year | the same for the fiscal year |
| [`mtd_agg()`](https://uhero.github.io/fcutils/reference/mtd_agg.md), [`mtd_gr()`](https://uhero.github.io/fcutils/reference/mtd_gr.md) | month | the same within each month, for daily data |
| `ptd_agg(x, per)`, `ptd_gr(x, per)` | `per`: “year”, “quarter”, “month” | the general form |

Visitor arrivals in early 2025:

``` r

vis <- conv_xts(monthly_data_example)$VISNS_HI
round(tail(tsbox::ts_c(
  arrivals = vis,
  ytd_avg = ytd_agg(vis),
  ytd_sum = ytd_agg(vis, avg = FALSE),
  ytd_gr = ytd_gr(vis),
  fytd_gr = fytd_gr(vis)
), 4), 1)
#>            arrivals ytd_avg ytd_sum ytd_gr fytd_gr
#> 2024-12-01    910.1   809.3  9711.4    0.6     4.6
#> 2025-01-01    792.2   792.2   792.2    3.8     4.5
#> 2025-02-01    759.7   775.9  1551.9    1.0     3.8
#> 2025-03-01    903.9   818.6  2455.7    1.7     3.7
```

In March, `ytd_avg` is the average of January to March and `ytd_sum`
their total. The year to March 2025 is 1.7% above the same months of
2024. The fiscal year to March, which started in July 2024, is 3.7%
above July 2023 to March 2024.

The month-to-date functions do the same with daily data within each
month. Daily visitor arrivals in March 2022, as travel recovered from
the pandemic:

``` r

vis_d <- conv_xts(daily_data_example)$VISPNS_HI
round(tail(tsbox::ts_c(
  arrivals = vis_d,
  mtd_avg = mtd_agg(vis_d),
  mtd_gr = mtd_gr(vis_d)
), 3), 1)
#>            arrivals mtd_avg mtd_gr
#> 2022-03-23    22693 22627.0   86.5
#> 2022-03-24    25682 22754.3   85.5
#> 2022-03-25    25556 22866.4   82.9
```

The first 25 days of March 2022 averaged 22,866 arrivals a day, 83% more
than the first 25 days of March 2021.

## Tables

[`gen_table()`](https://uhero.github.io/fcutils/reference/gen_table.md)
lays out series with their growth rates. By default the dates run across
and each series and growth rate has a row; `time_across = FALSE` turns
that around, which suits a short page. `percent` picks the growth rate
shown with each series: `"pc"` (period on period), `"pcy"` (year on
year), `"pca"` (annualized) or `"none"`.

``` r

gen_table(q[, c("E_NF_HI", "EAF_HI")],
  tbl_start = "2024-01-01", tbl_end = "2025-01-01", percent = "pcy",
  time_across = FALSE
)
#> # A tibble: 5 × 5
#>   time       EAF_HI `EAF_HI (YoY%)` E_NF_HI `E_NF_HI (YoY%)`
#>   <date>      <dbl>           <dbl>   <dbl>            <dbl>
#> 1 2024-01-01   106.            0.66    637.             0.89
#> 2 2024-04-01   106.           -0.27    636.             0.43
#> 3 2024-07-01   106.            1.13    640.             1.19
#> 4 2024-10-01   107.            2.66    643.             1.81
#> 5 2025-01-01   108.            1.7     648.             1.73
```

With `save_loc`, it writes the table to a CSV file or, for `.html`, to a
searchable page.

## Interactive plots

The plot functions draw [dygraphs](https://rstudio.github.io/dygraphs/):
hover to read values, drag to zoom, and use the range selector at the
bottom. `rng_start` and `rng_end` set the initial window. All plots on a
page zoom together, so a page of plots keeps one date window; the two
plots below share 2016 to 2028.

[`plot_fc()`](https://uhero.github.io/fcutils/reference/plot_fc.md)
shows up to three series, typically the forecast, an earlier forecast
and history, as lines, with the growth of the first as bars on the right
axis and a table below. This is the jobs forecast from [Building a
forecast](https://uhero.github.io/fcutils/articles/forecasting.md)
against history:

``` r

jobs_fc <- extend(jobs, 2025.4 ~ 1.2, 2027.4 ~ 0.8, type = "growth")
plot_fc(
  tsbox::ts_c(forecast = jobs_fc, history = jobs),
  rng_start = "2016-01-01", rng_end = "2028-01-01",
  table_start = "2024-01-01", table_end = "2028-01-01",
  width = 700
)
```

`rate = TRUE` shows the change in points instead of the percent change,
for rates such as the unemployment rate, and `yoy_gr = FALSE` shows
annualized growth instead of year on year.

[`plot_1()`](https://uhero.github.io/fcutils/reference/plot_1.md) plots
one or more series with the growth of the first, and
[`plot_2ax()`](https://uhero.github.io/fcutils/reference/plot_2ax.md)
puts the second series on its own axis.
[`plot_comp_2()`](https://uhero.github.io/fcutils/reference/plot_comp_2.md)
shows several series in two panels, levels and growth.
[`plot_comp_3()`](https://uhero.github.io/fcutils/reference/plot_comp_3.md)
adds a panel with each series indexed to 1 at `base_date`, which
compares series of different size. Accommodation and food services jobs
fell much further in 2020 than total jobs:

``` r

plot_comp_3(q[, c("E_NF_HI", "EAF_HI")],
  base_date = "2019-10-01",
  rng_start = "2016-01-01", rng_end = "2028-01-01", width = 700
)
```

To collect many plots on one page that can be shared as a single file,
put them in a list and pass it to
[`save_plot_list()`](https://uhero.github.io/fcutils/reference/save_plot_list.md).
