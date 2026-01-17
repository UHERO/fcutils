# Specify span of time series (wrapper around tsbox::ts_span())

Specify span of time series (wrapper around tsbox::ts_span())

## Usage

``` r
span(x, start = NULL, end = NULL, template = NULL, extend = FALSE)
```

## Arguments

- x:

  ts-boxable object to filter by span

- start:

  start date (see examples)

- end:

  end date (see examples)

- template:

  ts-boxable time series (see tsbox::ts_span)

- extend:

  logical. If true, the start and end values are allowed to extend the
  series (by adding NA values).

## Value

filtered object of the same type as the input

## Examples

``` r
quarterly_data_example |>
  span(2010.1)
#> # A tibble: 61 × 31
#>    time       E_NF_HI ECT_HI EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>    <date>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#>  1 2010-01-01    586.   30.4   13.4   17.9   66.2       84.1    26.4       7   
#>  2 2010-04-01    587.   29.8   13.1   18.0   66.4       84.3    26.6       7.07
#>  3 2010-07-01    588.   29.3   13.1   17.9   66.4       84.3    26.8       7.1 
#>  4 2010-10-01    591.   29.7   13.3   17.9   66.4       84.3    27.1       7.2 
#>  5 2011-01-01    592.   29.4   13.4   17.7   67.0       84.7    27.1       7.2 
#>  6 2011-04-01    592.   29.7   13.5   17.7   67.1       84.8    27.1       7.03
#>  7 2011-07-01    596.   30.0   13.7   17.8   67.4       85.2    27.3       7.07
#>  8 2011-10-01    599.   29.9   13.7   17.9   68.0       85.9    27.7       7.2 
#>  9 2012-01-01    598.   30.0   13.6   17.9   68.6       86.6    27.9       7.33
#> 10 2012-04-01    606.   30.0   13.4   17.9   69.1       87.0    28.3       7.67
#> # ℹ 51 more rows
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
quarterly_data_example |>
  span(2010.1, 2010.4)
#> # A tibble: 4 × 31
#>   time       E_NF_HI ECT_HI EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>   <date>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#> 1 2010-01-01    586.   30.4   13.4   17.9   66.2       84.1    26.4       7   
#> 2 2010-04-01    587.   29.8   13.1   18.0   66.4       84.3    26.6       7.07
#> 3 2010-07-01    588.   29.3   13.1   17.9   66.4       84.3    26.8       7.1 
#> 4 2010-10-01    591.   29.7   13.3   17.9   66.4       84.3    27.1       7.2 
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
quarterly_data_example |>
  span("2010-01-01", "2010-12-31")
#> # A tibble: 4 × 31
#>   time       E_NF_HI ECT_HI EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>   <date>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#> 1 2010-01-01    586.   30.4   13.4   17.9   66.2       84.1    26.4       7   
#> 2 2010-04-01    587.   29.8   13.1   18.0   66.4       84.3    26.6       7.07
#> 3 2010-07-01    588.   29.3   13.1   17.9   66.4       84.3    26.8       7.1 
#> 4 2010-10-01    591.   29.7   13.3   17.9   66.4       84.3    27.1       7.2 
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
```
