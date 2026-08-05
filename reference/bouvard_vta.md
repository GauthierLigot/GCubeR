# Volume Estimation Using the Bouvard Method

Computes aerial total volume (`bouvard_vta`) according to the Bouvard
method. The function validates input data, ensures required columns are
present, and applies the formula only to species `"QUERCUS_SP"`.

## Usage

``` r
bouvard_vta(data, output = NULL)
```

## Arguments

- data:

  A data frame containing tree measurements. Must include:

  - `species_code`: species name in uppercase Latin format (e.g.
    `"QUERCUS_SP"`).

  - `dbh`: diameter at breast height (cm).

  - `htot`: total tree height (m).

- output:

  Optional file path where the resulting data frame should be exported
  as a CSV. If NULL (default), no file is written. Export is handled by
  the utility function
  [`export_output()`](https://gauthierligot.github.io/GCubeR/reference/export_output.md)
  and failures trigger warnings without interrupting execution.

## Value

A data frame with the original input columns plus one new output:

- `bouvard_vta`: aerial total volume (m3). Computed only for
  `"QUERCUS_SP"`, otherwise not created.

## Details

- Input `dbh` must be in centimeters (cm). The function converts it
  internally to meters.

- Input `htot` must be in meters (m).

- Formula for aerial total volume (only `"QUERCUS_SP"`): \$\$bouvard_vta
  = 0.5 \* (dbh/100)^2 \* htot\$\$

- Resulting volumes are expressed in cubic meters (m3).

- If required columns are missing or non-numeric, the function stops
  with an error.

- The output column is created only if at least one `"QUERCUS_SP"` row
  is present, otherwise a warning message is displayed and no column is
  added.

## Examples

``` r
df <- data.frame(
  species_code = c("QUERCUS_SP", "PICEA_ABIES", "FAGUS_SYLVATICA"),
  dbh = c(30, 25, 40),   # cm
  htot = c(20, 18, 22)   # m
)
bouvard_vta(df)
#>      species_code dbh htot bouvard_vta
#> 1      QUERCUS_SP  30   20         0.9
#> 2     PICEA_ABIES  25   18          NA
#> 3 FAGUS_SYLVATICA  40   22          NA
```
