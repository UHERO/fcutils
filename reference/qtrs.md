# Convert period in quarters to period in months

Convert period in quarters to period in months

## Usage

``` r
qtrs(nr_quarters)
```

## Arguments

- nr_quarters:

  number of quarters in period (integer)

## Value

number of months in period

## Examples

``` r
qtrs(3)
#> [1] "9m 0d 0H 0M 0S"
lubridate::ymd("2020-01-01") + qtrs(3)
#> [1] "2020-10-01"
```
