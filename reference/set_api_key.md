# Set an API key for a data source

Set an API key for a data source

## Usage

``` r
set_api_key(service, key, store = c("session", "keyring"))
```

## Arguments

- service:

  "fred", "estat", "eia" or "udaman"

- key:

  the key (string)

- store:

  "session" (default) sets it for this R session only; "keyring" stores
  it among the system's credentials (e.g. the macOS keychain), where
  later sessions find it

## Value

nothing (the key is stored)

## Details

Functions that need a key look first in the environment variable
(`FRED_API_KEY`, `ESTAT_API_KEY`, `EIA_API_KEY`, `udaman_token`), which
can also be set in `.Renviron`, and then in the keyring under the same
name.

## Examples

``` r
if (FALSE) { # interactive()
set_api_key("fred", "abcdefghijklmnopqrstuvwxyz123456")
set_api_key("eia", "abc123", store = "keyring")
}
```
