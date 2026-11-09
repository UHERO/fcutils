# Pattern in series names that should be aggregated as sum

A regular expression matched against upper-cased series names when
`conv_type = "uhero"` in
[`agg()`](https://uhero.github.io/fcutils/reference/agg.md),
[`fagg()`](https://uhero.github.io/fcutils/reference/fagg.md) and
[`disagg()`](https://uhero.github.io/fcutils/reference/disagg.md).
Matching series are aggregated by sum, all others by mean.

## Usage

``` r
sum_pattern
```

## Format

string of patterns in series names that should be aggregated via sum

## Source

author

## References

<https://uhero.hawaii.edu>

## Author

Peter Fuleky <fuleky@hawaii.edu>
