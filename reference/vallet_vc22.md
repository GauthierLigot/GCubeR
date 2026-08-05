# Calculate Commercial Volume (vc22) up to 7cm Diameter at Breast Height (dbh)

Computes the commercial wood volume (vc22, over bark, up to a 7 cm
top-diameter) using the Vallet polynomial model, based on dbh (cm) and
htot (m).

## Usage

``` r
vallet_vc22(data, na_action = c("error", "omit"), output = NULL)
```

## Arguments

- data:

  A data frame containing tree measurements. Must include the columns:
  `species_code`, `dbh` (diameter at 1.30m, in cm), and `htot` (total
  height, in m).

- na_action:

  How to handle missing input values. `"error"` (default) stops if core
  required values are explicitly `NA`. `"omit"` removes rows with
  missing core data.

- output:

  Optional file path where the resulting data frame should be exported
  as a CSV. If NULL (default), no file is written. Export is handled by
  the utility function
  [`export_output()`](https://gauthierligot.github.io/GCubeR/reference/export_output.md)
  and failures trigger warnings without interrupting execution.

## Value

The resulting data frame with the new column `vallet_vc22` (Commercial
Volume in **m3**).

## Details

The model is valid only for trees with a diameter at 1.30m (`dbh`)
greater than or equal to 7 cm.

The polynomial formula used is: \$\$VC22\_{dm^3} = a \cdot
\frac{h\_{tot}}{dbh} + (b + c \cdot dbh) \cdot \frac{\pi \cdot dbh^2
\cdot h\_{tot}}{40}\$\$

Coefficients a, b, c are species-specific and loaded from the
`vallet_vc22.csv` file.

## Examples

``` r
data_test_vc22 <- data.frame(
  species_code = c("PICEA_ABIES", "FAGUS_SYLVATICA", "UNKNOWN_SPECIES", "QUERCUS_ROBUR"),
  dbh = c(19.1, 25.5, 15.9, 6.4), # dbh 6.4 cm is below 7 cm constraint (NA result)
  htot = c(25, 18, 20, 22)
)

# Expect negative/unrealistic results due to coefficient incompatibility
results_console <- vallet_vc22(data_test_vc22)
#> Warning: Diameter (dbh) constraint violated: 1 tree(s) have dbh < 7 cm. vc22 will be set to NA for these rows: 4
#> Warning: Unknown species (missing vc22 coefficients): UNKNOWN_SPECIES. vc22 will be set to NA for these rows.
#>      species_code  dbh htot vallet_vc22
#> 1     PICEA_ABIES 19.1   25   0.4032410
#> 2 FAGUS_SYLVATICA 25.5   18   0.4631734
#> 3 UNKNOWN_SPECIES 15.9   20          NA
#> 4   QUERCUS_ROBUR  6.4   22          NA
print(results_console)
#>      species_code  dbh htot vallet_vc22
#> 1     PICEA_ABIES 19.1   25   0.4032410
#> 2 FAGUS_SYLVATICA 25.5   18   0.4631734
#> 3 UNKNOWN_SPECIES 15.9   20          NA
#> 4   QUERCUS_ROBUR  6.4   22          NA
```
