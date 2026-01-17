# Create xts and fill with values

Create xts and fill with values

## Usage

``` r
make_xts(start = bnk_start, end = NULL, per = "year", val = NA_real_)
```

## Arguments

- start:

  date of series start (character: "yyyy-mm-dd", "yyyyqq", "yyyy")

- end:

  date of series end (character: "yyyy-mm-dd", "yyyyqq", "yyyy")

- per:

  periodicity of series (character: "year" - default) if date format of
  start is quarterly, automatically set to "quarter"

- val:

  values to fill in (numeric scalar, vector, or tibble)

## Value

an xts series

## Details

when end is missing, but val is a vector of more than one element, the
end date is automatically determined by the length of the val vector. if
end is missing and val is a scalar, the end date is set to bnk_end. if
end is missing the remaining arguments have to be named. if val is a
tibble, the end date is automatically determined by the number of rows
in the tibble.

## Examples

``` r
make_xts()
#>            value
#> 1970-01-01    NA
#> 1971-01-01    NA
#> 1972-01-01    NA
#> 1973-01-01    NA
#> 1974-01-01    NA
#> 1975-01-01    NA
#> 1976-01-01    NA
#> 1977-01-01    NA
#> 1978-01-01    NA
#> 1979-01-01    NA
#>        ...      
#> 2091-01-01    NA
#> 2092-01-01    NA
#> 2093-01-01    NA
#> 2094-01-01    NA
#> 2095-01-01    NA
#> 2096-01-01    NA
#> 2097-01-01    NA
#> 2098-01-01    NA
#> 2099-01-01    NA
#> 2100-01-01    NA
make_xts(val = 0, per = "m")
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
make_xts(start = 20100101, per = "quarter", val = 0)
#>            value
#> 2010-01-01     0
#> 2010-04-01     0
#> 2010-07-01     0
#> 2010-10-01     0
#> 2011-01-01     0
#> 2011-04-01     0
#> 2011-07-01     0
#> 2011-10-01     0
#> 2012-01-01     0
#> 2012-04-01     0
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
make_xts(start = 2010.1, per = "q", val = 1:10)
#>            value
#> 2010-01-01     1
#> 2010-04-01     2
#> 2010-07-01     3
#> 2010-10-01     4
#> 2011-01-01     5
#> 2011-04-01     6
#> 2011-07-01     7
#> 2011-10-01     8
#> 2012-01-01     9
#> 2012-04-01    10
make_xts(2010.1, val = 1:10) # automatically set per = "quarter"
#>            value
#> 2010-01-01     1
#> 2010-04-01     2
#> 2010-07-01     3
#> 2010-10-01     4
#> 2011-01-01     5
#> 2011-04-01     6
#> 2011-07-01     7
#> 2011-10-01     8
#> 2012-01-01     9
#> 2012-04-01    10
make_xts(start = "2010-01-01", per = "m", val = 0)
#>            value
#> 2010-01-01     0
#> 2010-02-01     0
#> 2010-03-01     0
#> 2010-04-01     0
#> 2010-05-01     0
#> 2010-06-01     0
#> 2010-07-01     0
#> 2010-08-01     0
#> 2010-09-01     0
#> 2010-10-01     0
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
make_xts(start = 201001, per = "q",
         val = tibble::tibble(E_NF_HON = c(1:10), ECT_HI = c(11:20)))
#>            E_NF_HON ECT_HI
#> 2010-01-01        1     11
#> 2010-04-01        2     12
#> 2010-07-01        3     13
#> 2010-10-01        4     14
#> 2011-01-01        5     15
#> 2011-04-01        6     16
#> 2011-07-01        7     17
#> 2011-10-01        8     18
#> 2012-01-01        9     19
#> 2012-04-01       10     20
```
