# Calculate compound annual growth

Calculate compound annual growth

## Usage

``` r
cagr(x)
```

## Arguments

- x:

  ts-boxable object for which growth is calculated between first and
  last period

## Value

a tibble with one row: `years_elapsed`, the years between the first and
last dates in x, and for each series its compound annual growth over
that span (in percent). A series that starts later or ends earlier than
the others gets NA.

## Examples

``` r
quarterly_data_example |>
  cagr()
#> # A tibble: 1 × 31
#>   years_elapsed E_NF_HI ECT_HI EMN_HI EWT_HI   ERT_HI E_TRADE_HI E_TU_HI
#>           <dbl>   <dbl>  <dbl>  <dbl>  <dbl>    <dbl>      <dbl>   <dbl>
#> 1          27.0   0.743   1.97 -0.696  0.314 -0.00569     0.0622   0.830
#> # ℹ 23 more variables: ETWTANS_HI <dbl>, ETWNS_HI <dbl>, EUT_HI <dbl>,
#> #   EIF_HI <dbl>, EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>,
#> #   E_PBS_HI <dbl>, E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>,
#> #   EHC_HI <dbl>, EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>,
#> #   EGV_HI <dbl>, EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>,
#> #   EGVLC_HI <dbl>
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_xts() |>
  cagr()
#> # A tibble: 1 × 31
#>   years_elapsed E_NF_HI ECT_HI EMN_HI EWT_HI   ERT_HI E_TRADE_HI E_TU_HI
#>           <dbl>   <dbl>  <dbl>  <dbl>  <dbl>    <dbl>      <dbl>   <dbl>
#> 1          27.0   0.743   1.97 -0.696  0.314 -0.00569     0.0622   0.830
#> # ℹ 23 more variables: ETWTANS_HI <dbl>, ETWNS_HI <dbl>, EUT_HI <dbl>,
#> #   EIF_HI <dbl>, EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>,
#> #   E_PBS_HI <dbl>, E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>,
#> #   EHC_HI <dbl>, EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>,
#> #   EGV_HI <dbl>, EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>,
#> #   EGVLC_HI <dbl>
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_xts() |>
  tsbox::ts_span("2000-01-01", "2020-01-01") |>
  tsbox::ts_pick("E_NF_HI") |>
  cagr()
#> # A tibble: 1 × 2
#>   years_elapsed E_NF_HI
#>           <dbl>   <dbl>
#> 1            20   0.972
```
