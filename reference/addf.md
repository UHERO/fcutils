# Create xts addfactor

Create xts addfactor

## Usage

``` r
addf(
  start = bnk_start,
  end = bnk_end,
  from = 0,
  to = 0,
  ser_name = "value",
  per = "year"
)
```

## Arguments

- start:

  start date of linear interpolation (character: "yyyy-mm-dd", "yyyyqq",
  "yyyy")

- end:

  end date of linear interpolation (character: "yyyy-mm-dd", "yyyyqq",
  "yyyy")

- from:

  first value for linear interpolation (numeric)

- to:

  last value for linear interpolation (numeric)

- ser_name:

  name of the xts series (string)

- per:

  periodicity of series (character: "year" - default) if date format of
  start is quarterly, automatically set to "quarter"

## Value

a single xts series spanning bnk_start-bnk_end

## Details

this is a wrapper around make_xts with some additional functionality.
the start and end dates specify the span of the non-zero add-factor
value. the remaining period between start and end is filled with zeros.

## Examples

``` r
addf()
#>            value
#> 1970-01-01     0
#> 1971-01-01     0
#> 1972-01-01     0
#> 1973-01-01     0
#> 1974-01-01     0
#> 1975-01-01     0
#> 1976-01-01     0
#> 1977-01-01     0
#> 1978-01-01     0
#> 1979-01-01     0
#>        ...      
#> 2091-01-01     0
#> 2092-01-01     0
#> 2093-01-01     0
#> 2094-01-01     0
#> 2095-01-01     0
#> 2096-01-01     0
#> 2097-01-01     0
#> 2098-01-01     0
#> 2099-01-01     0
#> 2100-01-01     0
addf(201002, 201504, 1, 2)
#>            value
#> 1970-01-01     0
#> 1970-04-01     0
#> 1970-07-01     0
#> 1970-10-01     0
#> 1971-01-01     0
#> 1971-04-01     0
#> 1971-07-01     0
#> 1971-10-01     0
#> 1972-01-01     0
#> 1972-04-01     0
#>        ...      
#> 2098-07-01     0
#> 2098-10-01     0
#> 2099-01-01     0
#> 2099-04-01     0
#> 2099-07-01     0
#> 2099-10-01     0
#> 2100-01-01     0
#> 2100-04-01     0
#> 2100-07-01     0
#> 2100-10-01     0
addf(20100101, 20601201, 1, 2, per = "month")
#>            value
#> 1970-01-01     0
#> 1970-02-01     0
#> 1970-03-01     0
#> 1970-04-01     0
#> 1970-05-01     0
#> 1970-06-01     0
#> 1970-07-01     0
#> 1970-08-01     0
#> 1970-09-01     0
#> 1970-10-01     0
#>        ...      
#> 2100-03-01     0
#> 2100-04-01     0
#> 2100-05-01     0
#> 2100-06-01     0
#> 2100-07-01     0
#> 2100-08-01     0
#> 2100-09-01     0
#> 2100-10-01     0
#> 2100-11-01     0
#> 2100-12-01     0
addf(20100101, from = 1, to = 2, per = "quarter")
#>               value
#> 1970-01-01 0.000000
#> 1970-04-01 0.000000
#> 1970-07-01 0.000000
#> 1970-10-01 0.000000
#> 1971-01-01 0.000000
#> 1971-04-01 0.000000
#> 1971-07-01 0.000000
#> 1971-10-01 0.000000
#> 1972-01-01 0.000000
#> 1972-04-01 0.000000
#>        ...         
#> 2098-07-01 1.975207
#> 2098-10-01 1.977961
#> 2099-01-01 1.980716
#> 2099-04-01 1.983471
#> 2099-07-01 1.986226
#> 2099-10-01 1.988981
#> 2100-01-01 1.991736
#> 2100-04-01 1.994490
#> 2100-07-01 1.997245
#> 2100-10-01 2.000000
addf(2010.2, 2015.4, 1, 2, "ECT_HI")
#>            ECT_HI
#> 1970-01-01      0
#> 1970-04-01      0
#> 1970-07-01      0
#> 1970-10-01      0
#> 1971-01-01      0
#> 1971-04-01      0
#> 1971-07-01      0
#> 1971-10-01      0
#> 1972-01-01      0
#> 1972-04-01      0
#>        ...       
#> 2098-07-01      0
#> 2098-10-01      0
#> 2099-01-01      0
#> 2099-04-01      0
#> 2099-07-01      0
#> 2099-10-01      0
#> 2100-01-01      0
#> 2100-04-01      0
#> 2100-07-01      0
#> 2100-10-01      0
```
