# Aggregate from quarterly to annual frequency (superseded by tsbox::ts_frequency())

Aggregate from quarterly to annual frequency (superseded by
tsbox::ts_frequency())

## Usage

``` r
QtoA(ser_in, aggr = "mean")
```

## Arguments

- ser_in:

  the xts series to be converted (freq = q)

- aggr:

  aggregate via mean (default) or sum

## Value

converted xts series (freq = a)

## Examples

``` r
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_xts() |>
  tsbox::ts_pick("E_NF_HI") |>
  QtoA() |> # this matches with below
  AtoQ() |>
  QtoA() |> # this matches with above
  tsbox::ts_plot()
#> Joining with `by = join_by(time)`
```
