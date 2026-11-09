# Set udaman token for API access

Set udaman token for API access

## Usage

``` r
set_udaman_token(key)
```

## Arguments

- key:

  a string containing 44 characters

## Value

NULL, invisibly

## Details

Sets the udaman token for this R session (environment variable
`udaman_token`) and warns if the key is not 44 characters long. To keep
the token for later sessions, add `udaman_token = "<key>"` to .Renviron,
or run `set_api_key("udaman", key, store = "keyring")`.

## Examples

``` r
if (FALSE) { # interactive()
set_udaman_token("-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdef=")
}
```
