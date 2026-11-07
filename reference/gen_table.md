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
#> # A tibble: 60 × 34
#>    id           `2017-01-01` `2017-04-01` `2017-07-01` `2017-10-01` `2018-01-01`
#>    <chr>               <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
#>  1 EAD_HI              39           39.0         39.2         38.9         38.7 
#>  2 EAD_HI (%)           0.09         0.09         0.34        -0.77        -0.51
#>  3 EAFAC_HI            41.8         42.5         42.5         43.0         42.9 
#>  4 EAFAC_HI (%)         0.91         1.57        -0.04         1.13        -0.18
#>  5 EAFFD_HI            70.7         71.2         71.7         71.6         71.4 
#>  6 EAFFD_HI (%)         1.85         0.74         0.65        -0.11        -0.26
#>  7 EAF_HI             113.         114.         114.         115.         114.  
#>  8 EAF_HI (%)           1.49         1.05         0.39         0.35        -0.23
#>  9 ECT_HI              37.9         37.6         37.1         36.9         37.1 
#> 10 ECT_HI (%)          -1.33        -0.84        -1.21        -0.72         0.56
#> # ℹ 50 more rows
#> # ℹ 28 more variables: `2018-04-01` <dbl>, `2018-07-01` <dbl>,
#> #   `2018-10-01` <dbl>, `2019-01-01` <dbl>, `2019-04-01` <dbl>,
#> #   `2019-07-01` <dbl>, `2019-10-01` <dbl>, `2020-01-01` <dbl>,
#> #   `2020-04-01` <dbl>, `2020-07-01` <dbl>, `2020-10-01` <dbl>,
#> #   `2021-01-01` <dbl>, `2021-04-01` <dbl>, `2021-07-01` <dbl>,
#> #   `2021-10-01` <dbl>, `2022-01-01` <dbl>, `2022-04-01` <dbl>, …
gen_table(quarterly_data_example)
#> # A tibble: 60 × 34
#>    id           `2017-01-01` `2017-04-01` `2017-07-01` `2017-10-01` `2018-01-01`
#>    <chr>               <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
#>  1 EAD_HI              39           39.0         39.2         38.9         38.7 
#>  2 EAD_HI (%)           0.09         0.09         0.34        -0.77        -0.51
#>  3 EAFAC_HI            41.8         42.5         42.5         43.0         42.9 
#>  4 EAFAC_HI (%)         0.91         1.57        -0.04         1.13        -0.18
#>  5 EAFFD_HI            70.7         71.2         71.7         71.6         71.4 
#>  6 EAFFD_HI (%)         1.85         0.74         0.65        -0.11        -0.26
#>  7 EAF_HI             113.         114.         114.         115.         114.  
#>  8 EAF_HI (%)           1.49         1.05         0.39         0.35        -0.23
#>  9 ECT_HI              37.9         37.6         37.1         36.9         37.1 
#> 10 ECT_HI (%)          -1.33        -0.84        -1.21        -0.72         0.56
#> # ℹ 50 more rows
#> # ℹ 28 more variables: `2018-04-01` <dbl>, `2018-07-01` <dbl>,
#> #   `2018-10-01` <dbl>, `2019-01-01` <dbl>, `2019-04-01` <dbl>,
#> #   `2019-07-01` <dbl>, `2019-10-01` <dbl>, `2020-01-01` <dbl>,
#> #   `2020-04-01` <dbl>, `2020-07-01` <dbl>, `2020-10-01` <dbl>,
#> #   `2021-01-01` <dbl>, `2021-04-01` <dbl>, `2021-07-01` <dbl>,
#> #   `2021-10-01` <dbl>, `2022-01-01` <dbl>, `2022-04-01` <dbl>, …
gen_table(quarterly_data_example, percent = "none")
#> # A tibble: 30 × 34
#>    id           `2017-01-01` `2017-04-01` `2017-07-01` `2017-10-01` `2018-01-01`
#>    <chr>               <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
#>  1 EAD_HI               39           39.0         39.2         38.9         38.7
#>  2 EAFAC_HI             41.8         42.5         42.5         43.0         42.9
#>  3 EAFFD_HI             70.7         71.2         71.7         71.6         71.4
#>  4 EAF_HI              113.         114.         114.         115.         114. 
#>  5 ECT_HI               37.9         37.6         37.1         36.9         37.1
#>  6 EED_HI               14.6         14.6         14.4         14.7         14.8
#>  7 EFI_HI               16.4         16.3         16.3         16.2         16.2
#>  8 EGVFD_HI             33.6         33.3         33.3         33.3         33.4
#>  9 EGVLC_HI             19.0         19.0         19           19.0         18.9
#> 10 EGVSTEDNS_HI         51.8         52.1         43.8         52.7         51.7
#> # ℹ 20 more rows
#> # ℹ 28 more variables: `2018-04-01` <dbl>, `2018-07-01` <dbl>,
#> #   `2018-10-01` <dbl>, `2019-01-01` <dbl>, `2019-04-01` <dbl>,
#> #   `2019-07-01` <dbl>, `2019-10-01` <dbl>, `2020-01-01` <dbl>,
#> #   `2020-04-01` <dbl>, `2020-07-01` <dbl>, `2020-10-01` <dbl>,
#> #   `2021-01-01` <dbl>, `2021-04-01` <dbl>, `2021-07-01` <dbl>,
#> #   `2021-10-01` <dbl>, `2022-01-01` <dbl>, `2022-04-01` <dbl>, …
gen_table(quarterly_data_example, percent = "pcy", time_across = FALSE)
#> # A tibble: 33 × 61
#>    time       EAD_HI `EAD_HI (YoY%)` EAFAC_HI `EAFAC_HI (YoY%)` EAFFD_HI
#>    <date>      <dbl>           <dbl>    <dbl>             <dbl>    <dbl>
#>  1 2017-01-01   39             -1.02     41.8              2.37     70.7
#>  2 2017-04-01   39.0           -0.34     42.5              3.33     71.2
#>  3 2017-07-01   39.2           -0.25     42.5              3.36     71.7
#>  4 2017-10-01   38.9           -0.26     43.0              3.6      71.6
#>  5 2018-01-01   38.7           -0.85     42.9              2.49     71.4
#>  6 2018-04-01   38.7           -0.85     43.0              1.07     70.3
#>  7 2018-07-01   39             -0.43     42.9              0.96     70.4
#>  8 2018-10-01   39.3            1.2      43.2              0.52     70.2
#>  9 2019-01-01   39.0            0.78     42.8             -0.28     70.3
#> 10 2019-04-01   38.6           -0.26     42.4             -1.17     70.4
#> # ℹ 23 more rows
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
