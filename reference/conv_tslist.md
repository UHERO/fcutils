# Convert "ts-boxable" objects to tslist (extension of `tsbox::ts_tslist()`)

Convert "ts-boxable" objects to tslist (extension of
[`tsbox::ts_tslist()`](https://docs.ropensci.org/tsbox/reference/ts_ts.html))

## Usage

``` r
conv_tslist(x)
```

## Arguments

- x:

  a "ts-boxable" object to be converted

## Value

returns an object as a tslist.

## Details

This function performs a similar operation to
[`tsbox::ts_tslist()`](https://docs.ropensci.org/tsbox/reference/ts_ts.html).
It converts ts-boxable objects to the tslist format. An important
difference compared with
[`tsbox::ts_tslist()`](https://docs.ropensci.org/tsbox/reference/ts_ts.html)
is that the x argument of `conv_tslist()` can be a wide tbl.

## Examples

``` r
quarterly_data_example |>
  conv_xts() |>
  conv_tslist() |>
  names() |>
  head()
#> [1] "E_NF_HI"    "ECT_HI"     "EMN_HI"     "EWT_HI"     "ERT_HI"    
#> [6] "E_TRADE_HI"
quarterly_data_example |>
  conv_wide() |>
  conv_tslist() |>
  utils::str(list.len = 2)
#> List of 30
#>  $ E_NF_HI     : Time-Series [1:109] from 1998 to 2025: 531 531 533 531 530 ...
#>  $ ECT_HI      : Time-Series [1:109] from 1998 to 2025: 23.3 23 23 23.1 22.8 ...
#>   [list output truncated]
#>  - attr(*, "class")= chr [1:2] "tslist" "list"
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pick("E_NF_HI") |>
  conv_wide() |>
  conv_tslist()
#> $E_NF_HI
#>          Qtr1     Qtr2     Qtr3     Qtr4
#> 1998 530.5066 531.2062 532.6954 531.4825
#> 1999 529.5065 533.2071 537.3354 541.2599
#> 2000 544.4418 551.0984 554.2636 557.8412
#> 2001 557.4090 557.8977 559.0919 547.6859
#> 2002 550.9452 556.0814 559.8555 562.7480
#> 2003 566.0548 566.1611 569.1561 571.9215
#> 2004 575.9935 580.9815 586.2997 592.2795
#> 2005 594.8728 601.0180 606.5916 607.7929
#> 2006 611.9294 616.9841 621.0972 621.8008
#> 2007 624.3356 625.3305 626.5238 627.0428
#> 2008 627.8759 625.2803 619.1668 610.5611
#> 2009 601.1690 594.3708 589.0274 585.2538
#> 2010 586.1273 587.2441 587.8288 591.3205
#> 2011 592.3345 592.2238 595.9386 598.8753
#> 2012 598.2457 606.1571 611.2724 614.3760
#> 2013 614.1589 619.0662 621.8706 625.0041
#> 2014 625.1205 627.1037 629.8320 631.9222
#> 2015 634.4211 636.9853 641.1229 643.8611
#> 2016 643.0992 645.5091 650.9999 650.8098
#> 2017 653.4202 655.3713 654.6270 656.3933
#> 2018 657.1552 657.4432 658.0127 660.0399
#> 2019 658.0274 657.3566 658.7536 660.2622
#> 2020 660.6415 514.4334 523.1838 541.7863
#> 2021 558.6996 584.5277 600.1625 604.0009
#> 2022 607.7994 613.9718 622.9109 627.5995
#> 2023 631.2647 633.7569 632.2179 631.8120
#> 2024 636.9010 636.4715 639.7316 643.2565
#> 2025 647.9022                           
#> 
#> attr(,"class")
#> [1] "tslist" "list"  
```
