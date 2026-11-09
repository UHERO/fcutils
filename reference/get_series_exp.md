# Download series listed in an export table from udaman

Download series listed in an export table from udaman

## Usage

``` r
get_series_exp(
  exp_id,
  format = "wide",
  raw = TRUE,
  rename = "compact",
  descr = FALSE,
  public = FALSE,
  save_loc = NULL
)
```

## Arguments

- exp_id:

  export id (character or numeric)

- format:

  "wide" (default) or "long" or "xts"

- raw:

  TRUE (default) or FALSE (TRUE downloads raw data, FALSE downloads
  scaled and rounded data)

- rename:

  "compact" (default), "full", "no". "compact": @ replaced by \_ and no
  frequency; "full": @ replaced by *AT* and . by \_; "no": no renaming,
  keep UDAMAN names

- descr:

  if TRUE add to the udaman series name the series description in
  parentheses (default: FALSE)

- public:

  if TRUE use the public API interface - does not require VPN (default:
  FALSE)

- save_loc:

  path of a CSV file to write (default NULL: no file). The file always
  holds the wide table, whatever `format` is.

## Value

the series in the format chosen by `format`

## Details

This function requires permission to access UDAMAN. Store the udaman
token in the .Renviron file using the following format: udaman_token =
"-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890=" Or run set_udaman_token(key) to
set it for the current session only. Or store the udaman token among
your credentials (e.g. keychain) using keyring:
keyring::key_set_with_value(service = "udaman_token", password =
"-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890=")

## Examples

``` r
if (FALSE) { # interactive()
get_series_exp(exp_id = 74)
get_series_exp(74, format = "xts")
}
```
