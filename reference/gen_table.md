# Generate a table with time series

Generate a table with time series

## Usage

``` r
gen_table(
  x,
  tbl_start = as.character(Sys.Date() - lubridate::years(10)),
  tbl_end = as.character(Sys.Date() + lubridate::years(2)),
  percent = "pc",
  time_across = TRUE,
  tbl_height = 800,
  save_loc = NULL
)
```

## Arguments

- x:

  a ts-boxable object

- tbl_start:

  start period for table

- tbl_end:

  end period for table

- percent:

  what type of percent should be added ("none", "pc" (default), "pcy",
  "pca")

- time_across:

  should time be in column headers and variable names in first column
  (default TRUE)

- tbl_height:

  the height of the table in px (default 800)

- save_loc:

  file path for saving table incl. extension ("html" or "csv") (default
  NULL)

## Value

table formatted for output

## Examples

``` r
quarterly_data_example %>%
  tsbox::ts_long() %>%
  tsbox::ts_tslist() %>%
  gen_table()
#> # A tibble: 60 × 37
#>    id           `2016-04-01` `2016-07-01` `2016-10-01` `2017-01-01` `2017-04-01`
#>    <chr>               <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
#>  1 EAD_HI              39.2         39.3         39.0         39           39.0 
#>  2 EAD_HI (%)          -0.59         0.26        -0.76         0.09         0.09
#>  3 EAFAC_HI            41.1         41.1         41.5         41.8         42.5 
#>  4 EAFAC_HI (%)         0.63        -0.07         0.89         0.91         1.57
#>  5 EAFFD_HI            68.0         68.9         69.4         70.7         71.2 
#>  6 EAFFD_HI (%)         1.09         1.38         0.76         1.85         0.74
#>  7 EAF_HI             109.         110          111.         113.         114.  
#>  8 EAF_HI (%)           0.92         0.83         0.81         1.49         1.05
#>  9 ECT_HI              39.4         39.3         38.4         37.9         37.6 
#> 10 ECT_HI (%)          -0.02        -0.15        -2.31        -1.33        -0.84
#> # ℹ 50 more rows
#> # ℹ 31 more variables: `2017-07-01` <dbl>, `2017-10-01` <dbl>,
#> #   `2018-01-01` <dbl>, `2018-04-01` <dbl>, `2018-07-01` <dbl>,
#> #   `2018-10-01` <dbl>, `2019-01-01` <dbl>, `2019-04-01` <dbl>,
#> #   `2019-07-01` <dbl>, `2019-10-01` <dbl>, `2020-01-01` <dbl>,
#> #   `2020-04-01` <dbl>, `2020-07-01` <dbl>, `2020-10-01` <dbl>,
#> #   `2021-01-01` <dbl>, `2021-04-01` <dbl>, `2021-07-01` <dbl>, …
gen_table(quarterly_data_example)
#> # A tibble: 60 × 37
#>    id           `2016-04-01` `2016-07-01` `2016-10-01` `2017-01-01` `2017-04-01`
#>    <chr>               <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
#>  1 EAD_HI              39.2         39.3         39.0         39           39.0 
#>  2 EAD_HI (%)          -0.59         0.26        -0.76         0.09         0.09
#>  3 EAFAC_HI            41.1         41.1         41.5         41.8         42.5 
#>  4 EAFAC_HI (%)         0.63        -0.07         0.89         0.91         1.57
#>  5 EAFFD_HI            68.0         68.9         69.4         70.7         71.2 
#>  6 EAFFD_HI (%)         1.09         1.38         0.76         1.85         0.74
#>  7 EAF_HI             109.         110          111.         113.         114.  
#>  8 EAF_HI (%)           0.92         0.83         0.81         1.49         1.05
#>  9 ECT_HI              39.4         39.3         38.4         37.9         37.6 
#> 10 ECT_HI (%)          -0.02        -0.15        -2.31        -1.33        -0.84
#> # ℹ 50 more rows
#> # ℹ 31 more variables: `2017-07-01` <dbl>, `2017-10-01` <dbl>,
#> #   `2018-01-01` <dbl>, `2018-04-01` <dbl>, `2018-07-01` <dbl>,
#> #   `2018-10-01` <dbl>, `2019-01-01` <dbl>, `2019-04-01` <dbl>,
#> #   `2019-07-01` <dbl>, `2019-10-01` <dbl>, `2020-01-01` <dbl>,
#> #   `2020-04-01` <dbl>, `2020-07-01` <dbl>, `2020-10-01` <dbl>,
#> #   `2021-01-01` <dbl>, `2021-04-01` <dbl>, `2021-07-01` <dbl>, …
gen_table(quarterly_data_example, percent = "none")
#> # A tibble: 30 × 37
#>    id           `2016-04-01` `2016-07-01` `2016-10-01` `2017-01-01` `2017-04-01`
#>    <chr>               <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
#>  1 EAD_HI               39.2         39.3         39.0         39           39.0
#>  2 EAFAC_HI             41.1         41.1         41.5         41.8         42.5
#>  3 EAFFD_HI             68.0         68.9         69.4         70.7         71.2
#>  4 EAF_HI              109.         110          111.         113.         114. 
#>  5 ECT_HI               39.4         39.3         38.4         37.9         37.6
#>  6 EED_HI               14.6         14.9         14.4         14.6         14.6
#>  7 EFI_HI               16.2         16.3         16.4         16.4         16.3
#>  8 EGVFD_HI             33.1         33.3         33.4         33.6         33.3
#>  9 EGVLC_HI             19           19.0         18.9         19.0         19.0
#> 10 EGVSTEDNS_HI         51.6         43.9         52.4         51.8         52.1
#> # ℹ 20 more rows
#> # ℹ 31 more variables: `2017-07-01` <dbl>, `2017-10-01` <dbl>,
#> #   `2018-01-01` <dbl>, `2018-04-01` <dbl>, `2018-07-01` <dbl>,
#> #   `2018-10-01` <dbl>, `2019-01-01` <dbl>, `2019-04-01` <dbl>,
#> #   `2019-07-01` <dbl>, `2019-10-01` <dbl>, `2020-01-01` <dbl>,
#> #   `2020-04-01` <dbl>, `2020-07-01` <dbl>, `2020-10-01` <dbl>,
#> #   `2021-01-01` <dbl>, `2021-04-01` <dbl>, `2021-07-01` <dbl>, …
gen_table(quarterly_data_example, percent = "pcy", time_across = FALSE)
#> # A tibble: 36 × 61
#>    time       EAD_HI `EAD_HI (YoY%)` EAFAC_HI `EAFAC_HI (YoY%)` EAFFD_HI
#>    <date>      <dbl>           <dbl>    <dbl>             <dbl>    <dbl>
#>  1 2016-04-01   39.2           -3.29     41.1              3.01     68.0
#>  2 2016-07-01   39.3           -2.73     41.1              2.83     68.9
#>  3 2016-10-01   39.0           -3.39     41.5              2.71     69.4
#>  4 2017-01-01   39             -1.02     41.8              2.37     70.7
#>  5 2017-04-01   39.0           -0.34     42.5              3.33     71.2
#>  6 2017-07-01   39.2           -0.25     42.5              3.36     71.7
#>  7 2017-10-01   38.9           -0.26     43.0              3.6      71.6
#>  8 2018-01-01   38.7           -0.85     42.9              2.49     71.4
#>  9 2018-04-01   38.7           -0.85     43.0              1.07     70.3
#> 10 2018-07-01   39             -0.43     42.9              0.96     70.4
#> # ℹ 26 more rows
#> # ℹ 55 more variables: `EAFFD_HI (YoY%)` <dbl>, EAF_HI <dbl>,
#> #   `EAF_HI (YoY%)` <dbl>, ECT_HI <dbl>, `ECT_HI (YoY%)` <dbl>, EED_HI <dbl>,
#> #   `EED_HI (YoY%)` <dbl>, EFI_HI <dbl>, `EFI_HI (YoY%)` <dbl>, EGVFD_HI <dbl>,
#> #   `EGVFD_HI (YoY%)` <dbl>, EGVLC_HI <dbl>, `EGVLC_HI (YoY%)` <dbl>,
#> #   EGVSTEDNS_HI <dbl>, `EGVSTEDNS_HI (YoY%)` <dbl>, EGVST_HI <dbl>,
#> #   `EGVST_HI (YoY%)` <dbl>, EGV_HI <dbl>, `EGV_HI (YoY%)` <dbl>, …
if (FALSE) { # interactive()
gen_table(quarterly_data_example,
  percent = "pcy",
  time_across = FALSE, save_loc = "~/Downloads/temp.csv"
)
gen_table(quarterly_data_example,
  percent = "pcy", time_across = TRUE,
  save_loc = "~/Downloads/temp.html"
)
}
```
