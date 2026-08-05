# Two-entry Dagnelie volume (tarif 2)

Computes the standing volume \\v\_{c,22}\\ (in cubic metres) using
Dagnelie's two-entry tarif 2 equations. The volume is calculated from
the stem circumference at 1.30 m (`c130`, in cm), the total height of
the tree (`htot`, in m), and the tree species, using species-specific
polynomial coefficients stored in `dan2`.

## Usage

``` r
dagnelie_vc22_2(data, output = NULL)
```

## Arguments

- data:

  A `data.frame` containing at least the columns `c130` (stem
  circumference at 1.30 m, in cm), `htot` (height of the tree, in m),
  and `species_code` (character code of the tree species).

- output:

  Optional file path where the resulting data frame should be exported
  as a CSV. If NULL (default), no file is written. Export is handled by
  the utility function
  [`export_output()`](https://gauthierligot.github.io/GCubeR/reference/export_output.md)
  and failures trigger warnings without interrupting execution.

## Value

A `data.frame` identical to `data` but augmented with:

- the joined columns from `dan2` (`coeff_a`, `coeff_b`, `coeff_c`,
  `coeff_d`, `coeff_e`, `coeff_f`, `min_c130`, `max_c130`, `min_htot`,
  `max_htot`),

- `tarif2`: the Dagnelie two-entry volume \\v\_{c,22}\\ in m\\^3\\ per
  tree.

## Details

The function:

- checks that the input data frame contains the required columns `c130`,
  `htot` and `species_code`,

- validates that all species codes are present in the `dan2` table,

- merges the input data with `dan2` to retrieve: `coeff_a`, `coeff_b`,
  `coeff_c`, `coeff_d`, `coeff_e`, `coeff_f`, as well as the
  species-specific valid ranges: `min_c130`, `max_c130`, `min_htot`,
  `max_htot`,

- issues a warning for trees whose `c130` is outside the valid range
  `[min_c130, max_c130]`,

- issues a warning for trees whose `htot` is outside the valid range
  `[min_htot, max_htot]`,

- computes the tarif 2 volume using the species-specific polynomial:
  \$\$ v\_{c,22} = coeff_a + coeff_b \cdot c130 + coeff_c \cdot c130^2 +
  coeff_d \cdot c130^3 + coeff_e \cdot htot + coeff_f \cdot c130^2 \cdot
  htot \$\$

Species codes must match those available in the `dan2` reference table.
If one or more species are not found, the function issues a warning.

For trees where `c130` or `htot` is outside the species-specific
validity ranges `[min_c130, max_c130]` and `[min_htot, max_htot]`,
warnings are issued, but the volume is still computed.

## Supported species

The following species codes are currently supported by
`dagnelie_vc22_2`:

- `"QUERCUS_SP"`

- `"QUERCUS_ROBUR"`

- `"QUERCUS_PETRAEA"`

- `"QUERCUS_PUBESCENS"`

- `"QUERCUS_RUBRA"`

- `"FAGUS_SYLVATICA"`

- `"ACER_PSEUDOPLATANUS"`

- `"FRAXINUS_EXCELSIOR"`

- `"ULMUS_SP"`

- `"PRUNUS_AVIUM"`

- `"BETULA_SP"`

- `"ALNUS_GLUTINOSA"`

- `"PICEA_ABIES"`

- `"PSEUDOTSUGA_MENZIESII"`

- `"LARIX_SP"`

- `"PINUS_SYLVESTRIS"`

- `"CRATAEGUS_SP"`

- `"PRUNUS_SP"`

- `"CARPINUS_SP"`

- `"CASTANEA_SATIVA"`

- `"CORYLUS_AVELLANA"`

- `"MALUS_SP"`

- `"PYRUS_SP"`

- `"SORBUS_ARIA"`

- `"SAMBUCUS_SP"`

- `"RHAMNUS_FRANGULA"`

- `"PRUNUS_CERASUS"`

- `"ALNUS_INCANA"`

- `"POPULUSxCANADENSIS"`

- `"POPULUS_TREMULA"`

- `"PINUS_NIGRA"`

- `"PINUS_LARICIO"`

- `"TAXUS_BACCATA"`

- `"ACER_PLATANOIDES"`

- `"ACER_CAMPESTRE"`

- `"SORBUS_AUCUPARIA"`

- `"JUNGLANS_SP"`

- `"TILLIA_SP"`

- `"PICEA_SITCHENSIS"`

- `"ABIES_ALBA"`

- `"TSUGA_CANADENSIS"`

- `"ABIES_GRANDIS"`

- `"CUPRESSUS_SP"`

- `"THUJA_PLICATA"`

- `"AESCULUS_HIPPOCASTANUM"`

- `"ROBINIA_PSEUDOACACIA"`

- `"SALIX_SP"`

## See also

[`dan2`](https://gauthierligot.github.io/GCubeR/reference/dan2.md) for
the species-specific coefficients and ranges.

## Examples

``` r
df <- data.frame(
  c130         = c(145, 156, 234, 233),
  htot         = c(25, 23, 45, 34),
  species_code = c("PINUS_SYLVESTRIS", "QUERCUS_RUBRA",
                   "QUERCUS_SP", "FAGUS_SYLVATICA")
)
dagnelie_vc22_2(data = df)
#> Warning: htot out of range for 1 tree(s): row 3 (species QUERCUS_SP, min=8, max=34, found=45)
#>   c130 htot     species_code dagnelie_vc22_2
#> 1  145   25 PINUS_SYLVESTRIS        1.759086
#> 2  156   23    QUERCUS_RUBRA        1.864919
#> 3  234   45       QUERCUS_SP        9.121719
#> 4  233   34  FAGUS_SYLVATICA        6.246509
```
