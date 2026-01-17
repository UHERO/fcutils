# Save a ts-boxable object in tsd format

Save a ts-boxable object in tsd format

## Usage

``` r
write_tsd(x, file)
```

## Arguments

- x:

  a ts-boxable object (only M, Q, A frequency)

- file:

  character string denoting the location and name of the output file

## Value

nothing (silently save the contents of the tsd file to a user defined
location)

## Examples

``` r
if (FALSE) { # interactive()
quarterly_data_example |> write_tsd("out.tsd")
}
```
