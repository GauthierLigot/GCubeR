# Volume Estimation Using the Algan Method

Computes aerial total volume (`algan_vta`) and merchantable volume
(`algan_vc22`) according to the Algan method. The function validates
input data, ensures required columns are present and applies formulas
only to compatible species.

## Usage

``` r
algan_vta_vc22(data, output = NULL)
```

## Arguments

- data:

  A data frame containing tree measurements. Must include:

  - `species_code`: species name in uppercase Latin format (e.g.
    `"ABIES_ALBA"`).

  - `dbh`: diameter at breast height (cm).

  - `htot`: total tree height (m).

- output:

  Optional file path where the resulting data frame should be exported
  as a CSV. If NULL (default), no file is written. Export is handled by
  the utility function
  [`export_output()`](https://gauthierligot.github.io/GCubeR/reference/export_output.md)
  and failures trigger warnings without interrupting execution.

## Value

A data frame with the original input columns plus two new outputs:

- `algan_vta`: aerial total volume (m3). Computed only for
  `"ABIES_ALBA"`, `NA` otherwise.

- `algan_vc22`: merchantable volume (m3). Computed only for compatible
  species (`ABIES_ALBA`, `PICEA_ABIES`, `ALNUS_GLUTINOSA`,
  `PRUNUS_AVIUM`, `BETULA_SP`), `NA` otherwise.

## Details

- Input `dbh` must be in centimeters (cm). The function converts it
  internally to meters.

- Input `htot` must be in meters (m).

- Formula for aerial total volume (only `"ABIES_ALBA"`): \$\$algan_vta =
  0.4 \* (dbh/100)^2 \* htot\$\$

- Formula for merchantable volume (compatible species): \$\$algan_vc22 =
  0.33 \* (dbh/100)^2 \* htot\$\$

  - Domain of application:

  - For `"ABIES_ALBA"` and `"PICEA_ABIES"`, the Algan method is valid
    only if `dbh > 15 cm`.

  - For other compatible species (`ALNUS_GLUTINOSA`, `PRUNUS_AVIUM`,
    `BETULA_SP`), no minimum dbh threshold is applied.

- Resulting volumes are expressed in cubic meters (m3).

- If required columns are missing or non-numeric, the function stops
  with an error.

- Both output columns are always created to ensure consistency for
  downstream functions.

## Examples

``` r
df <- data.frame(
  species_code = c("ABIES_ALBA", "PICEA_ABIES", "BETULA_SP", "QUERCUS_ROBUR"),
  dbh = c(30, 25, 20, 40),   # cm
  htot = c(20, 18, 15, 22)   # m
)
algan_vta_vc22(df)
#>    species_code dbh htot algan_vc22 algan_vta
#> 1    ABIES_ALBA  30   20    0.59400      0.72
#> 2   PICEA_ABIES  25   18    0.37125        NA
#> 3     BETULA_SP  20   15    0.19800        NA
#> 4 QUERCUS_ROBUR  40   22         NA        NA
```
