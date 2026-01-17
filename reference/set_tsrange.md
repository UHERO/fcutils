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

Find periods where all variables in the equation are available. Shift
beginning of the sample by max_lag periods. Set the tsrange for each
equation (used in estimation).

## Examples

``` r
if (FALSE) { # interactive()
set_tsrange(scen_model_dat, 4)
}
```
