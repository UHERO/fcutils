# Find the date of the first observation (NAs are dropped)

Find the date of the first observation (NAs are dropped)

## Usage

``` r
find_start(x)
```

## Arguments

- x:

  ts-boxable object

## Value

dates associated with first observation

## Examples

``` r
quarterly_data_example |>
  dplyr::mutate(E_NF_HI = dplyr::if_else(time < "2000-01-01", NA_real_, E_NF_HI)) |>
  find_start()
#>  [1] "2000-01-01" "1998-01-01" "1998-01-01" "1998-01-01" "1998-01-01"
#>  [6] "1998-01-01" "1998-01-01" "1998-01-01" "1998-01-01" "1998-01-01"
#> [11] "1998-01-01" "1998-01-01" "1998-01-01" "1998-01-01" "1998-01-01"
#> [16] "1998-01-01" "1998-01-01" "1998-01-01" "1998-01-01" "1998-01-01"
#> [21] "1998-01-01" "1998-01-01" "1998-01-01" "1998-01-01" "1998-01-01"
#> [26] "1998-01-01" "1998-01-01" "1998-01-01" "1998-01-01" "1998-01-01"
```
