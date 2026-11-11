# Download a table, save it, and read it back, or read the saved copy

Scripts download their data on a full run and reread it on a rerun, so
the rerun works offline and gives the same numbers. `fetch_cached()`
does both. `fetch` downloads the table; `path` is where the copy is
kept.

## Usage

``` r
fetch_cached(path, fetch, mode = c("refresh", "missing", "offline"), ...)
```

## Arguments

- path:

  CSV file that holds the saved copy. Its folder is created if needed.

- fetch:

  function with no arguments that downloads the table and returns a data
  frame, e.g. `function() get_series_exp(324)`.

- mode:

  - "refresh" (default): download, save to `path`, and read the saved
    file back.

    - "missing": download only when `path` does not exist yet; otherwise
      read it. For large files that change rarely.

    - "offline": only read `path`; stop if it does not exist.

- ...:

  passed to
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html),
  e.g. `col_types`.

## Value

the table, as read from `path`

## Details

The table is always read back from the file, also right after a
download, so a full run and a later offline rerun see identical values.
readr writes numbers so that they read back exactly.

## Examples

``` r
if (FALSE) { # interactive()
from_disk <- FALSE
hist <- fetch_cached(
  "data/raw/324.csv",
  function() get_series_exp(324, rename = "no"),
  mode = if (from_disk) "offline" else "refresh"
)
}
```
