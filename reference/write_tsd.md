# Save a ts-boxable object in tsd format

Writes the TSD text format read by AREMOS and UHERO's visualization.
Each series takes an 80-column header line (its name, optionally
followed by a description), a line with the date written, the first and
last period and the frequency, and then its values, five to a line.

## Usage

``` r
write_tsd(x, file, descriptions = NULL)
```

## Arguments

- x:

  a ts-boxable object (monthly, quarterly or annual)

- file:

  character string denoting the location and name of the output file

- descriptions:

  optional named character vector, `c(id = "label")`, with a description
  for some or all series. With descriptions, the header line holds the
  name in columns 1-16 and the description in columns 17-80; longer
  names or descriptions are cut, with a warning. Without them, the name
  fills the header line.

## Value

nothing (silently save the contents of the tsd file to a user defined
location)

## Examples

``` r
if (FALSE) { # interactive()
quarterly_data_example |> write_tsd("out.tsd")
quarterly_data_example |>
  write_tsd("out.tsd", descriptions = c(E_NF_HI = "Nonfarm payroll jobs"))
}
```
