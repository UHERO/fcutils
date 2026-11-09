# Parse gets output and extract underlying data (GETS model development)

Parse gets output and extract underlying data (GETS model development)

## Usage

``` r
extract_data(model_in, y_name)
```

## Arguments

- model_in:

  a model estimated by arx, isat, or getsm

- y_name:

  the actual name of the y variable

## Value

an xts with the dependent variable named y_name, AR lags named y_name.k
(the lag notation model_equation() reads), indicator saturation dummies
named IIS_yyyy_mm_dd and SIS_yyyy_mm_dd, and the other regressors as in
the model

## Examples

``` r
if (FALSE) { # interactive()
q <- conv_xts(quarterly_data_example)
mod <- gets::arx(tsbox::ts_pc(q$E_NF_HI)["2000/2019"], ar = 1:2)
extract_data(mod, "E_NF_HI") |> head(3)
}
```
