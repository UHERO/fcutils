# Disaggregate univariate or multivariate time series from low to high frequency

Disaggregate univariate or multivariate time series from low to high
frequency

## Usage

``` r
disagg(x, conv_type = "mean", target_freq = "quarter", pattern = NULL)
```

## Arguments

- x:

  a tx-boxable object at a low frequency (e.g. annual or quarterly)

- conv_type:

  match the interpolated value via "first", "last", "sum", "mean". If
  conv_type == "uhero" then the name of the time series x is compared to
  the internal variable `sum_pattern`. For matching series names the
  interpolation is based on "sum"; for all others it is based on "mean."

- target_freq:

  target frequency "quarter" or "month"

- pattern:

  a single high-frequency pattern that the interpolation should follow

## Value

interpolated object of the same type as the input

## Details

the time-span of the high-frequency pattern has to match or be larger
than the time-span of the low frequency series. NA values are not
allowed.

## Examples

``` r
quarterly_data_example |>
  disagg(conv_type = "mean", target_freq = "month")
#> # A tibble: 327 × 31
#>    time       E_NF_HI ECT_HI EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>    <date>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#>  1 1998-01-01    530.   23.3   15.9   15.8   65.3       81.1    27.9       9.96
#>  2 1998-02-01    530.   23.3   15.9   15.9   65.2       81.0    27.9       9.96
#>  3 1998-03-01    531.   23.2   15.9   15.9   65.0       80.9    27.9       9.98
#>  4 1998-04-01    531.   23.1   15.8   15.9   64.8       80.7    27.9      10.00
#>  5 1998-05-01    531.   23.0   15.7   15.9   64.6       80.6    27.9      10.0 
#>  6 1998-06-01    532.   22.9   15.7   15.9   64.5       80.4    27.8      10.1 
#>  7 1998-07-01    532.   23.0   15.8   15.9   64.3       80.2    27.7      10.1 
#>  8 1998-08-01    533.   23.0   15.8   15.9   64.2       80.1    27.6      10.1 
#>  9 1998-09-01    533.   23.0   15.8   15.9   64.2       80.1    27.5      10.1 
#> 10 1998-10-01    532.   23.1   15.8   16.0   64.2       80.3    27.4      10.1 
#> # ℹ 317 more rows
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
quarterly_data_example |>
  disagg(conv_type = "mean", target_freq = "month") |>
  tsbox::ts_long() |>
  tsbox::ts_frequency(to = "quarter", aggregate = "mean") |>
  tsbox::ts_wide() # this matches original data
#> # A tibble: 109 × 31
#>    time       E_NF_HI ECT_HI EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>    <date>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#>  1 1998-01-01    531.   23.3   15.9   15.9   65.2       81.0    27.9       9.97
#>  2 1998-04-01    531.   23.0   15.8   15.9   64.6       80.6    27.9      10.0 
#>  3 1998-07-01    533.   23.0   15.8   15.9   64.2       80.1    27.6      10.1 
#>  4 1998-10-01    531.   23.1   15.8   16.0   64.2       80.2    27.3      10.00
#>  5 1999-01-01    530.   22.8   15.8   15.8   64.1       79.8    27.0       9.73
#>  6 1999-04-01    533.   22.9   15.9   15.8   64.8       80.6    27.4       9.97
#>  7 1999-07-01    537.   22.9   16.1   15.8   65.1       81.0    27.6      10.2 
#>  8 1999-10-01    541.   23.3   16.3   16.0   65.9       82.0    27.9      10.3 
#>  9 2000-01-01    544.   24.3   16.3   16.2   66.0       82.3    28.0      10.4 
#> 10 2000-04-01    551.   25.0   16.4   16.2   66.0       82.3    28.1      10.6 
#> # ℹ 99 more rows
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
# works with a single series too
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pick("E_NF_HI") |>
  disagg(conv_type = "mean", target_freq = "month") |>
  tsbox::ts_plot()

# using a high-frequency pattern
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_span("2005-01-01", "2020-01-01") |>
  disagg(
    conv_type = "mean", target_freq = "month", pattern = monthly_data_example |>
      tsbox::ts_long() |>
      tsbox::ts_pick("VISNS_HI")
  )
#> # A tibble: 5,490 × 3
#>    id      time       value
#>    <chr>   <date>     <dbl>
#>  1 E_NF_HI 2005-01-01  594.
#>  2 E_NF_HI 2005-02-01  594.
#>  3 E_NF_HI 2005-03-01  597.
#>  4 E_NF_HI 2005-04-01  599.
#>  5 E_NF_HI 2005-05-01  601.
#>  6 E_NF_HI 2005-06-01  604.
#>  7 E_NF_HI 2005-07-01  606.
#>  8 E_NF_HI 2005-08-01  607.
#>  9 E_NF_HI 2005-09-01  607.
#> 10 E_NF_HI 2005-10-01  607.
#> # ℹ 5,480 more rows
# multiple low-frequency series, same number of high-frequency patterns
purrr::map2(
  quarterly_data_example |>
    tsbox::ts_long() |>
    tsbox::ts_pick("E_NF_HI", "ECT_HI") |>
    tsbox::ts_span("2005-01-01", "2020-01-01") |>
    tsbox::ts_tslist(),
  monthly_data_example |>
    tsbox::ts_long() |>
    tsbox::ts_pick("VISNS_HI", "VAPNS_HI") |>
    tsbox::ts_long() |>
    tsbox::ts_tslist(),
  ~ disagg(.x, conv_type = "mean", target_freq = "month", pattern = .y)
)
#> Additional [id] column(s): 'id'
#> $E_NF_HI
#>           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
#> 2005 593.5753 594.4022 596.6407 598.5051 600.9794 603.5696 605.9370 606.9738
#> 2006 610.2481 611.7445 613.7957 615.2965 616.7820 618.8737 620.6766 621.4406
#> 2007 623.4409 624.2504 625.3155 624.9905 625.1412 625.8599 626.4472 626.8867
#> 2008 627.7795 627.8961 627.9520 626.5636 625.4027 623.8746 621.9084 619.5016
#> 2009 603.9452 600.9723 598.5895 596.4164 594.2373 592.4586 590.9630 589.1497
#> 2010 585.5025 585.9885 586.8910 587.0610 587.2102 587.4611 587.5378 587.7391
#> 2011 592.3479 592.2961 592.3595 591.7721 591.8837 593.0158 594.7481 596.0777
#> 2012 597.3045 597.4582 599.9745 603.4662 606.2969 608.7082 610.1792 611.5022
#> 2013 613.4237 613.5908 615.4623 617.4404 619.1242 620.6341 621.2539 621.9768
#> 2014 625.0227 624.7347 625.6039 626.1358 626.9365 628.2387 629.3912 629.9887
#> 2015 633.5067 634.1812 635.5755 635.6895 636.8314 638.4349 640.1771 641.2485
#> 2016 643.1670 642.6904 643.4401 643.7322 645.1837 647.6114 650.3680 651.4812
#> 2017 652.4014 653.1906 654.6685 655.1579 655.3883 655.5678 655.1127 654.5016
#> 2018 656.9303 656.8859 657.6496 657.2103 657.2868 657.8324 658.0446 657.9347
#> 2019 658.7017 657.5767 657.8038 657.0752 657.0702 657.9244 658.7797 658.9804
#> 2020 661.2452 661.3586 659.3207                                             
#>           Sep      Oct      Nov      Dec
#> 2005 606.8640 607.0749 607.3696 608.9341
#> 2006 621.1745 621.1578 621.4643 622.7804
#> 2007 626.2374 626.5574 626.8558 627.7154
#> 2008 616.0902 613.5874 610.4775 607.6185
#> 2009 586.9696 585.6750 584.8459 585.2405
#> 2010 588.2096 590.2043 591.3154 592.4418
#> 2011 596.9899 598.6042 598.9402 599.0816
#> 2012 612.1357 613.8294 614.5130 614.7855
#> 2013 622.3811 624.2333 625.0341 625.7447
#> 2014 630.1162 631.0175 631.6227 633.1265
#> 2015 641.9430 643.3818 643.7960 644.4056
#> 2016 651.1506 650.5703 650.2965 651.5625
#> 2017 654.2668 655.5160 656.2851 657.3788
#> 2018 658.0589 659.5995 660.1195 660.4008
#> 2019 658.5008 659.4221 660.0339 661.3305
#> 2020                                    
#> 
#> $ECT_HI
#>           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
#> 2005 32.07939 32.25980 32.74226 33.27636 33.80971 34.25911 34.55435 34.77252
#> 2006 35.76071 35.90913 36.14147 36.27730 36.50683 36.86308 37.25220 37.60856
#> 2007 38.97614 39.03035 39.28600 39.48329 39.69815 39.92639 40.09850 40.20406
#> 2008 40.47124 40.30157 40.09897 39.60811 39.27129 38.97909 38.72151 38.35185
#> 2009 35.34426 34.59120 33.95789 33.31188 32.73949 32.23654 31.81130 31.34772
#> 2010 30.40860 30.38553 30.32349 30.00562 29.77579 29.58631 29.35820 29.25623
#> 2011 29.42473 29.28411 29.38747 29.56309 29.74884 29.91639 29.99963 29.98473
#> 2012 29.99516 29.99903 30.06822 29.98023 30.02411 30.12423 30.24166 30.39323
#> 2013 31.52913 31.66182 31.93221 32.07844 32.19504 32.22785 32.09164 32.01890
#> 2014 32.49913 32.50743 32.63269 32.62143 32.76577 33.00192 33.27336 33.47812
#> 2015 33.96134 33.99657 34.37921 34.80845 35.30204 35.73414 36.05937 36.46803
#> 2016 39.27063 39.40901 39.52274 39.35618 39.36581 39.45101 39.54353 39.39522
#> 2017 38.03677 37.85791 37.84489 37.69031 37.59041 37.50505 37.35237 37.14242
#> 2018 36.94906 37.02259 37.26271 37.35598 37.52775 37.68539 37.79041 37.78568
#> 2019 37.84267 37.68104 37.69345 37.49513 37.48148 37.55392 37.65149 37.63539
#> 2020 37.53637 37.41453 37.12507                                             
#>           Sep      Oct      Nov      Dec
#> 2005 34.89774 35.15780 35.34613 35.61685
#> 2006 37.94654 38.47736 38.79770 39.02154
#> 2007 40.22516 40.38576 40.45136 40.53393
#> 2008 37.82619 37.38859 36.77868 36.14870
#> 2009 30.85881 30.52233 30.29499 30.32092
#> 2010 29.29910 29.66108 29.74746 29.72993
#> 2011 29.90082 29.87777 29.84879 29.94689
#> 2012 30.54841 30.93251 31.19283 31.44606
#> 2013 31.97829 32.19461 32.30957 32.48306
#> 2014 33.65217 33.97910 34.08670 34.16751
#> 2015 37.00408 37.84781 38.45241 39.01576
#> 2016 39.06186 38.69731 38.35490 38.21878
#> 2017 36.92442 36.87081 36.81345 36.93537
#> 2018 37.78565 37.92761 37.94935 38.00233
#> 2019 37.55195 37.66041 37.63611 37.71717
#> 2020                                    
#> 
```
