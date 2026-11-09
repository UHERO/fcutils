# Format series names to udaman format (mnemonic@loc.freq)

Format series names to udaman format (mnemonic@loc.freq)

## Usage

``` r
rename_udaman(ser_in, freq = NULL)
```

## Arguments

- ser_in:

  series names in any of the forms MNEMONIC_LOC, MNEMONIC\_\_LOC_F,
  MNEMONIC_AT_LOC_F or MNEMONIC@LOC.F. One string may hold several names
  separated by commas, semicolons or spaces.

- freq:

  frequency of the series ("D", "W", "M", "Q", "S", "A"), required if
  not contained in the series names; it replaces any frequency in the
  names

## Value

series names following udaman convention "mnemonic@loc.freq"

## Details

Recognised locations are HI, HON, HAW, MAU, KAU, MAUI, MOL, LAN, NBI, US
and JP; the location is the one just before the frequency or the end of
the name. HI5, HI3, HIALL and CNTY expand to the matching sets of
counties, so the output can be longer than the input. A name with no
recognised location, or with no frequency when `freq` is NULL, stops
with an error.

## Examples

``` r
rename_udaman(c("E_NF_HI", "ECT_HI", "E_TU_HAW"), freq = "M")
#> [1] "E_NF@HI.M"  "ECT@HI.M"   "E_TU@HAW.M"
rename_udaman(c("E_NF__HI_M", "ECT__HI_M", "VAP__HAW_W"))
#> [1] "E_NF@HI.M" "ECT@HI.M"  "VAP@HAW.W"
rename_udaman(c("E_NF_AT_HI_M", "ECT_AT_HI_M", "VAP_AT_HAW_W"))
#> [1] "E_NF@HI.M" "ECT@HI.M"  "VAP@HAW.W"
rename_udaman(c("E_NF@HI.M", "ECT@HI.M", "VAP@HAW.W"))
#> [1] "E_NF@HI.M" "ECT@HI.M"  "VAP@HAW.W"
rename_udaman(c("SH_US@HI.M", "SH_JP__HON_M"))
#> [1] "SH_US@HI.M"  "SH_JP@HON.M"
rename_udaman(c("E_NF_HI5", "ECT__HIALL", "E_TU@CNTY"), freq = "M")
#>  [1] "E_NF@HI.M"  "E_NF@HON.M" "E_NF@HAW.M" "E_NF@MAU.M" "E_NF@KAU.M"
#>  [6] "ECT@HI.M"   "ECT@HON.M"  "ECT@HAW.M"  "ECT@MAU.M"  "ECT@KAU.M" 
#> [11] "ECT@MAUI.M" "ECT@MOL.M"  "ECT@LAN.M"  "E_TU@HON.M" "E_TU@HAW.M"
#> [16] "E_TU@MAU.M" "E_TU@KAU.M"
rename_udaman(c("E_NF_HI5 , ECT__HIALL  E_TU@CNTY", "VAP_HAW ; ECT_HON"), freq = "M")
#>  [1] "E_NF@HI.M"  "E_NF@HON.M" "E_NF@HAW.M" "E_NF@MAU.M" "E_NF@KAU.M"
#>  [6] "ECT@HI.M"   "ECT@HON.M"  "ECT@HAW.M"  "ECT@MAU.M"  "ECT@KAU.M" 
#> [11] "ECT@MAUI.M" "ECT@MOL.M"  "ECT@LAN.M"  "E_TU@HON.M" "E_TU@HAW.M"
#> [16] "E_TU@MAU.M" "E_TU@KAU.M" "VAP@HAW.M"  "ECT@HON.M" 
quarterly_data_example |>
  dplyr::rename_with(~ rename_udaman(., freq = "Q"), .cols = -1) |>
  names() |>
  head()
#> [1] "time"      "E_NF@HI.Q" "ECT@HI.Q"  "EMN@HI.Q"  "EWT@HI.Q"  "ERT@HI.Q" 
quarterly_data_example |>
  tsbox::ts_long() |>
  dplyr::mutate(id = rename_udaman(id, freq = "Q")) |>
  tsbox::ts_xts() |>
  colnames() |>
  head()
#> [1] "E_NF@HI.Q"    "ECT@HI.Q"     "EMN@HI.Q"     "EWT@HI.Q"     "ERT@HI.Q"    
#> [6] "E_TRADE@HI.Q"
```
