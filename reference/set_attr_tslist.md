# Set class attribute to tslist

Set class attribute to tslist

## Usage

``` r
set_attr_tslist(x)
```

## Arguments

- x:

  list, typically a result of purrr::map() applied to a tslist

## Value

list with class attributes set to list and tslist

## Details

A purrr::map() function applied to a tslist (obtained by
tsbox::ts_tslist()) drops the tslist class attribute. This function
resets that attribute.

## Examples

``` r
monthly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_tslist() |>
  purrr::map(~ .x / 1000) |>
  set_attr_tslist() |>
  tsbox::ts_tbl() |>
  tsbox::ts_wide()
#> # A tibble: 295 × 4
#>    time       VISNS_HI VAPNS_HI VADCNS_HI
#>    <date>        <dbl>    <dbl>     <dbl>
#>  1 2000-09-01    0.539    0.546     0.146
#>  2 2000-10-01    0.567    0.604     0.153
#>  3 2000-11-01    0.535    0.577     0.154
#>  4 2000-12-01    0.584    0.617     0.181
#>  5 2001-01-01    0.558    0.578     0.182
#>  6 2001-02-01    0.554    0.559     0.176
#>  7 2001-03-01    0.618    0.636     0.173
#>  8 2001-04-01    0.558    0.594     0.155
#>  9 2001-05-01    0.528    0.586     0.142
#> 10 2001-06-01    0.597    0.672     0.183
#> # ℹ 285 more rows
```
