# Interpolate univariate or multivariate time series with missing values

Interpolate univariate or multivariate time series with missing values

## Usage

``` r
interpol(
  incomplete,
  pattern = NULL,
  extend_range = FALSE,
  match = c("position", "name", "stem"),
  interp = c("linear", "stine")
)
```

## Arguments

- incomplete:

  a ts-boxable object with NAs

- pattern:

  a ts-boxable object at the same frequency, or NULL (default) to fill
  the gaps inside each series by `interp`. `match` sets how pattern
  series pair with the series in `incomplete`; with "position", a single
  pattern series serves every series, with a warning. `interpol()`
  prints the mapping it uses.

- extend_range:

  logical. If TRUE, the result covers every date of either input, and
  each series is continued along its pattern over the added dates. If
  FALSE (default), the result keeps the dates of `incomplete`.

- match:

  how series in `incomplete` find their pattern: "position" (default)
  pairs them in order; "name" needs the same name; "stem" compares names
  without a final frequency or solution ending, as in
  [`chain()`](https://uhero.github.io/fcutils/reference/chain.md), so
  `VISNS_HI_Q` pairs with `VISNS_HI_M`. With "name" or "stem", every
  series needs a partner, or interpol() stops. A single series in each
  always pairs.

- interp:

  without a pattern: "linear" (default) fills a gap along a straight
  line, "stine" along a Stineman spline through the observed values,
  which bends smoothly without overshooting them (as in
  [`extend()`](https://uhero.github.io/fcutils/reference/extend.md)).
  With a pattern only "linear" is allowed; "stine" is an error.

## Value

interpolated object of the same type as the input

## Details

With a pattern, a gap inside a series follows the pattern's growth from
the last known value, plus a correction that grows linearly across the
gap so the path ends on the next known value. A gap at the start or end
of a series follows the pattern's growth from the nearest known value.
Where the pattern is NA, or does not cover a gap's end points, the gap
stays NA.

Without a pattern, missing values between a series' first and last
observation are filled by `interp` (by position, so each period counts
the same); missing dates in a regular series are added first, and
leading and trailing gaps stay empty. extend_range has no effect then.

## Examples

``` r
# straight-line fill of an internal gap
make_xts("2025M7", val = c(1, 2, NA, 4, 5)) |>
  interpol()
#>            value
#> 2025-07-01     1
#> 2025-08-01     2
#> 2025-09-01     3
#> 2025-10-01     4
#> 2025-11-01     5
# a smooth fill along the curve of the neighbouring values
make_xts("2025M7", val = c(1, 1.2, 2, NA, NA, 8, 8.5)) |>
  interpol(interp = "stine")
#>               value
#> 2025-07-01 1.000000
#> 2025-08-01 1.200000
#> 2025-09-01 2.000000
#> 2025-10-01 3.746101
#> 2025-11-01 6.275314
#> 2025-12-01 8.000000
#> 2026-01-01 8.500000
monthly_data_example |>
  dplyr::filter(time > "2002-01-01") |>
  tsbox::ts_long() |>
  dplyr::mutate(
    value = dplyr::if_else(time > "2021-01-01" & time < "2022-01-01",
    NA_real_, value)
  )|>
  interpol(monthly_data_example, extend_range = TRUE) |>
  span("2020-11-01", "2022-02-01")
#> The mapping between interpolated series and pattern series is the following:
#>  VISNS_HI_I  VAPNS_HI_I VADCNS_HI_I 
#>  VISNS_HI_P  VAPNS_HI_P VADCNS_HI_P 
#> # A tibble: 48 × 3
#>    id       time       value
#>    <chr>    <date>     <dbl>
#>  1 VISNS_HI 2020-11-01  184.
#>  2 VISNS_HI 2020-12-01  237.
#>  3 VISNS_HI 2021-01-01  172.
#>  4 VISNS_HI 2021-02-01  235.
#>  5 VISNS_HI 2021-03-01  440.
#>  6 VISNS_HI 2021-04-01  485.
#>  7 VISNS_HI 2021-05-01  630.
#>  8 VISNS_HI 2021-06-01  792.
#>  9 VISNS_HI 2021-07-01  880.
#> 10 VISNS_HI 2021-08-01  723.
#> # ℹ 38 more rows
monthly_data_example |>
  tsbox::ts_long() |>
  dplyr::mutate(
    value = dplyr::if_else(
      time > "2021-01-01" & time < "2022-01-01",
      NA_real_,
      value
    )
  ) |>
  interpol(
    # missing values in the pattern
    monthly_data_example |>
      tsbox::ts_long() |>
      dplyr::mutate(
        value = dplyr::if_else(
          time > "2021-03-01" & time < "2021-09-01",
          NA_real_,
          value
        )
      )
  ) %>%
  # no interpolation where pattern has NAs (April to August 2021)
  tsbox::ts_wide() |>
  span("2020-11-01", "2022-02-01")
#> The mapping between interpolated series and pattern series is the following:
#>  VISNS_HI_I  VAPNS_HI_I VADCNS_HI_I 
#>  VISNS_HI_P  VAPNS_HI_P VADCNS_HI_P 
#> # A tibble: 16 × 4
#>    time       VISNS_HI VAPNS_HI VADCNS_HI
#>    <date>        <dbl>    <dbl>     <dbl>
#>  1 2020-11-01     184.     226.      76.2
#>  2 2020-12-01     237.     289.      94.7
#>  3 2021-01-01     172.     227.      80.8
#>  4 2021-02-01     235.     272.      90.7
#>  5 2021-03-01     440.     492.     137. 
#>  6 2021-04-01      NA       NA       NA  
#>  7 2021-05-01      NA       NA       NA  
#>  8 2021-06-01      NA       NA       NA  
#>  9 2021-07-01      NA       NA       NA  
#> 10 2021-08-01      NA       NA       NA  
#> 11 2021-09-01     505.     597.     154. 
#> 12 2021-10-01     551.     660.     165. 
#> 13 2021-11-01     614.     714.     191. 
#> 14 2021-12-01     753.     838.     247. 
#> 15 2022-01-01     574.     662.     201. 
#> 16 2022-02-01     620.     676.     206. 
```
