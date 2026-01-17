# Update a bimets model with new/modified equations

Update a bimets model with new/modified equations

## Usage

``` r
update_eqs(model_1, model_2, eqList)
```

## Arguments

- model_1:

  original estimated bimets model

- model_2:

  estimated bimets model containing updates (only updated equations need
  to be estimated)

- eqList:

  names of updated behavioral equations (vector of strings), others
  taken from model_1 (equations missing from model_2 are removed)

## Value

estimated bimets model containing updates

## Details

Start by making a copy of the original model's equations (txt file).
Re-specify some equations, add new equations, and remove not needed
equations. Load the new model as model_2 and estimate the modified/new
equations (ok to estimate all). Replace the equations in model_2 that
should remain the same as in model_1 by the estimated equations from
model_1. Equations that are to remain unchanged have to be present in
both model_1 and model_2, and not present in eqList.

## Examples

``` r
if (FALSE) { # interactive()
update_eqs(scen_model_1_est, scen_model_2_est, c("E_NF_AT_HI_Q", "Y_R_AT_HI_Q"))
}
```
