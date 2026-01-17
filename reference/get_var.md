# Construct a series name from variable components and retrieve the series

Construct a series name from variable components and retrieve the series

## Usage

``` r
get_var(ser_in, env = parent.frame())
```

## Arguments

- ser_in:

  a variable name (character string with substituted expressions)

- env:

  environment where the expression should be evaluated

## Value

variable

## Examples

``` r
if (FALSE) { # interactive()
ser_i <- "_NF"
cnty_i <- "HI"
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_xts() %$% get_var("E{ser_i}_{cnty_i}")
}
```
