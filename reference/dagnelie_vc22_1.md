# Single-entry Dagnelie volume (tarif 1)

Computes the standing volume \\v\_{c,22}\\ (in cubic metres per tree)
using Dagnelie's single-entry tarif-1 equations. The volume is derived
from the stem circumference at 1.30 m (`c130`, in cm) and the tree
species, using species-specific polynomial coefficients stored in the
reference table `dan1`.

## Usage

``` r
dagnelie_vc22_1(data, output = NULL)
```

## Arguments

- data:

  A `data.frame` containing at least:

  - `c130`: stem circumference at 1.30 m (cm),

  - `species_code`: tree species code.

- output:

  Optional file path where the resulting data frame should be exported
  as a CSV. If NULL (default), no file is written. Export is handled by
  the utility function
  [`export_output()`](https://gauthierligot.github.io/GCubeR/reference/export_output.md)
  and failures trigger warnings without interrupting execution.

## Value

A `data.frame` identical to the input `data` but augmented with:

- species-specific coefficients and validity ranges,

- `dagnelie_vc22_1`: the computed Dagnelie tarif-1 volume (m\\^3\\ per
  tree).

## Details

The tarif-1 volume is calculated as: \$\$ v\_{c,22} = coeff\\a +
coeff\\b \cdot c130 + coeff\\c \cdot c130^2 + coeff\\d \cdot c130^3 \$\$
where \\a\\, \\b\\, \\c\\, and \\d\\ are species-specific coefficients.

The function performs the following steps:

- checks that the input data frame contains the required variables
  `c130` and `species_code`,

- validates that `c130` is numeric,

- verifies that all species are available in the `dan1` reference table
  and issues a warning otherwise,

- merges the input with `dan1` to retrieve coefficients and
  species-specific validity ranges (`min_c130`, `max_c130`),

- warns when `c130` values fall outside the recommended range,

- computes tarif-1 volume and returns the augmented data frame.

If one or more species codes are not found in `dan1`, the function
issues a warning and returns `NA`-values for missing coefficients and
volumes. Trees with `c130` values outside the recommended
species-specific range produce a warning but still receive a computed
volume.

## Supported species

The following species codes are supported by `dagnelie_vc22_1`:

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

[`dan1`](https://gauthierligot.github.io/GCubeR/reference/dan1.md) for
species-specific coefficients.

## Examples

``` r
df <- data.frame(
  c130         = c(145, 156, 234, 233),
  species_code = c("PINUS_SYLVESTRIS", "QUERCUS_RUBRA",
                   "QUERCUS_SP", "FAGUS_SYLVATICA")
)
dagnelie_vc22_1(df)
#>   c130     species_code dagnelie_vc22_1
#> 1  145 PINUS_SYLVESTRIS        1.702759
#> 2  156    QUERCUS_RUBRA        1.936043
#> 3  234       QUERCUS_SP        4.915508
#> 4  233  FAGUS_SYLVATICA        5.625484
```
