# Set tsrange for behavioral equations to available data range

Set tsrange for behavioral equations to available data range

## Usage

``` r
set_tsrange(model_w_dat, max_lag = 4, eqns = NULL)
```

## Arguments

- model_w_dat:

  bimets model (with data) to be estimated

- max_lag:

  the largest lag (default = 4) in the model (to offset starting point
  for estimation)

- eqns:

  names of behavioral equations to set tsrange for (default = NULL: all
  equations)

## Value

bimets model with tsrange set for estimation

## Details

Takes the rows where every variable in the equation has a value, starts
at the (max_lag + 1)th such row and ends at the last one. Gaps inside
the sample are spanned, not split. The range is written as year and
quarter, so this applies to quarterly models only.

## Examples

``` r
if (FALSE) { # interactive()
set_tsrange(scen_model_dat, 4)
}
```
