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

true if setting the token in .Renviron succeeded

## Details

Save the token in .Renviron as udaman_token = key.

## Examples

``` r
if (FALSE) { # interactive()
set_udaman_token("-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890=")
}
```
