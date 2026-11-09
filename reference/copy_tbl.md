# Copy time series to the clipboard as a tab-separated table (macOS only)

Copy time series to the clipboard as a tab-separated table (macOS only)

## Usage

``` r
copy_tbl(x, dec = 2)
```

## Arguments

- x:

  a ts-boxable object; it is copied in wide form, one column per series

- dec:

  number of decimals to round to (default: 2)

## Value

the copied table invisibly, a tibble of text with numbers rounded to
`dec` decimals

## Examples

``` r
if (FALSE) { # interactive()
monthly_data_example |> copy_tbl()
monthly_data_example |> copy_tbl(1)
}
```
