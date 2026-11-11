# Convert series names between udaman and R forms

Convert series names between udaman and R forms

## Usage

``` r
rename_udaman(ser_in, freq = NULL, to = c("udaman", "full", "compact"))
```

## Arguments

- ser_in:

  series names in any of the forms MNEMONIC_LOC, MNEMONIC\_\_LOC_F,
  MNEMONIC_AT_LOC_F or MNEMONIC@LOC.F. One string may hold several names
  separated by commas, semicolons or spaces.

- freq:

  what follows the location, replacing any frequency in the names: a
  frequency ("D", "W", "M", "Q", "S", "A"), another suffix such as
  "SOLA" for an AREMOS solution bank, or "" for none (`UR@US`). NULL
  (default) keeps the frequency in the names. Not used when `to` is
  "compact", which has no frequency.

- to:

  the form of the output: "udaman" (default, `E_NF@HI.M`), "full"
  (`E_NF_AT_HI_M`, the R form that keeps the frequency) or "compact"
  (`E_NF_HI`, no frequency). "full" and "compact" are the forms of
  `get_series(rename = )`.

## Value

series names in the form `to`

## Details

In MNEMONIC_AT_LOC and MNEMONIC@LOC the location is the part after
`_AT_` or `@`, so any location works (CAN, CA, ...). In MNEMONIC_LOC and
MNEMONIC\_\_LOC the location must be one of HI, HON, HAW, MAU, KAU,
MAUI, MOL, LAN, NBI, US and JP, just before the frequency or the end of
the name. HI5, HI3, HIALL and CNTY expand to the matching sets of
counties, so the output can be longer than the input. A name whose
location cannot be read stops with an error naming it, and so does a
name with no frequency when `to` is "udaman" and `freq` is NULL. The R
forms replace the % of udaman names with P (OCUP% becomes OCUPP).

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
rename_udaman(c("EAF_AT_HAW", "GDP_R_AT_CAN"), freq = "SOLA")
#> [1] "EAF@HAW.SOLA"   "GDP_R@CAN.SOLA"
rename_udaman(c("UR_AT_US", "GDP_R_AT_US"), freq = "")
#> [1] "UR@US"    "GDP_R@US"
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
# from udaman names to the R forms
rename_udaman(c("E_NF@HI.M", "OCUP%@HAW.M"), to = "full")
#> [1] "E_NF_AT_HI_M"   "OCUPP_AT_HAW_M"
rename_udaman(c("E_NF@HI.M", "OCUP%@HAW.M"), to = "compact")
#> [1] "E_NF_HI"   "OCUPP_HAW"
rename_udaman("E_NF_HI", freq = "Q", to = "full")
#> [1] "E_NF_AT_HI_Q"
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
