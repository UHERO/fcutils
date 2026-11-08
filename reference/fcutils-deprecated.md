# Deprecated functions

These functions were renamed in fcutils 0.1.3. The old names still work,
but they warn and will be removed in a later version. Replace them with
the new names, which take the same arguments:

## Usage

``` r
multi_chain(...)

aggr(...)

ytd_cum(...)

fytd_cum(...)

mtd_cum(...)

ptd_cum(...)

faggr(...)
```

## Arguments

- ...:

  arguments passed to the new function

## Value

the result of the new function

## Details

|  |  |
|----|----|
| Old | New |
| `multi_chain()` | [`chain()`](https://uhero.github.io/fcutils/reference/chain.md) |
| `aggr()` | [`agg()`](https://uhero.github.io/fcutils/reference/agg.md) |
| `faggr()` | [`fagg()`](https://uhero.github.io/fcutils/reference/fagg.md) |
| `ytd_cum()` | [`ytd_agg()`](https://uhero.github.io/fcutils/reference/ytd_agg.md) |
| `fytd_cum()` | [`fytd_agg()`](https://uhero.github.io/fcutils/reference/fytd_agg.md) |
| `mtd_cum()` | [`mtd_agg()`](https://uhero.github.io/fcutils/reference/mtd_agg.md) |
| `ptd_cum()` | [`ptd_agg()`](https://uhero.github.io/fcutils/reference/ptd_agg.md) |
