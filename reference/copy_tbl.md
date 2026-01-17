# Copy a data frame to clipboard (only works on MacOS)

Copy a data frame to clipboard (only works on MacOS)

## Usage

``` r
copy_tbl(x, dec = 2)
```

## Arguments

- x:

  tibble (or data frame) to be copied

- dec:

  number of decimals to round numeric columns to (default: 2)

## Value

copy_tbl() returns the input x invisibly

## Examples

``` r
if (FALSE) { # interactive()
monthly_data_example |> copy_tbl()
monthly_data_example |> copy_tbl(1)
}
```
