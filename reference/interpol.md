# Interpolate univariate or multivariate time series with missing values

Interpolate univariate or multivariate time series with missing values

## Usage

``` r
interpol(
  incomplete,
  pattern = NULL,
  extend_range = FALSE,
  interp = c("linear", "stine")
)
```

## Arguments

- incomplete:

  a ts-boxable object with NAs

- pattern:

  a ts-boxable object no missing values, or NULL (default) to fill the
  gaps inside each series by `interp`

- extend_range:

  logical. If TRUE, uses a full_join to potentially extend the date
  range. If FALSE (default), uses a left_join to only fill NAs within
  the original date range.

- interp:

  without a pattern: "linear" (default) fills a gap along a straight
  line, "stine" along a Stineman spline through the observed values,
  which bends smoothly without overshooting them (as in
  [`extend()`](https://uhero.github.io/fcutils/reference/extend.md)).
  Not used with a pattern.

## Value

interpolated object of the same type as the input

## Details

the function aligns two sets of time series by date and then
interpolates the first set using the second set as a pattern. Without a
pattern, missing values between a series' first and last observation are
filled by `interp` (by position, so each period counts the same);
missing dates in a regular series are added first, and leading and
trailing gaps stay empty. extend_range has no effect then. The time-span
of the pattern has to match or be larger than the time-span of the
incomplete data.

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
  interpol(monthly_data_example, extend_range = TRUE)
#> The mapping betwen interpolated series and pattern series is the following:
#>  VISNS_HI_I  VAPNS_HI_I VADCNS_HI_I 
#>  VISNS_HI_P  VAPNS_HI_P VADCNS_HI_P 
#> # A tibble: 885 × 3
#>    id       time       value
#>    <chr>    <date>     <dbl>
#>  1 VISNS_HI 2000-09-01  539.
#>  2 VISNS_HI 2000-10-01  567.
#>  3 VISNS_HI 2000-11-01  535.
#>  4 VISNS_HI 2000-12-01  584.
#>  5 VISNS_HI 2001-01-01  558.
#>  6 VISNS_HI 2001-02-01  554.
#>  7 VISNS_HI 2001-03-01  618.
#>  8 VISNS_HI 2001-04-01  558.
#>  9 VISNS_HI 2001-05-01  528.
#> 10 VISNS_HI 2001-06-01  597.
#> # ℹ 875 more rows
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
  # no interpolation where pattern has NAs
  tsbox::ts_wide()
#> The mapping betwen interpolated series and pattern series is the following:
#>  VISNS_HI_I  VAPNS_HI_I VADCNS_HI_I 
#>  VISNS_HI_P  VAPNS_HI_P VADCNS_HI_P 
#> # A tibble: 295 × 4
#>    time       VISNS_HI VAPNS_HI VADCNS_HI
#>    <date>        <dbl>    <dbl>     <dbl>
#>  1 2000-09-01     539.     546.      146.
#>  2 2000-10-01     567.     604.      153.
#>  3 2000-11-01     535.     577.      154.
#>  4 2000-12-01     584.     617.      181.
#>  5 2001-01-01     558.     578.      182.
#>  6 2001-02-01     554.     559.      176.
#>  7 2001-03-01     618.     636.      173.
#>  8 2001-04-01     558.     594.      155.
#>  9 2001-05-01     528.     586.      142.
#> 10 2001-06-01     597.     672.      183.
#> # ℹ 285 more rows
```
