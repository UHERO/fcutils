# Construct a series name from variable components and retrieve the series

Construct a series name from variable components and retrieve the series

## Usage

``` r
get_var(ser_in, env = parent.frame())
```

## Arguments

- ser_in:

  a name with glue expressions in braces, such as `"E{ser_i}_{cnty_i}"`.
  The expressions are evaluated in the global environment.

- env:

  environment in which the resulting name is looked up

## Value

the object with that name

## Examples

``` r
ser_i <- "_NF"
cnty_i <- "HI"
q <- conv_xts(quarterly_data_example)
with(as.list(q), get_var("E{ser_i}_{cnty_i}")) |> tail(2)
#> Error: Failed to evaluate glue component {ser_i}
#> Caused by error:
#> ! object 'ser_i' not found
```
