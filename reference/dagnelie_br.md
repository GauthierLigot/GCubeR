# Single-entry Dagnelie branch volume (tarif "br")

Computes the branch volume \\v\_{br}\\ (in cubic metres per tree) using
Dagnelie's single-entry "br" equations. The branch volume is derived
from the stem circumference at 1.30 m (`c130`, in cm) and the tree
species, using species-specific polynomial coefficients stored in the
reference table `danbr`.

## Usage

``` r
dagnelie_br(data, output = NULL)
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

- `dagnelie_br`: the computed Dagnelie tarif-"br" branch volume (m\\^3\\
  per tree).

## Details

The "br" tarif branch volume is calculated as: \$\$ v\_{br} = a +
b\\c130 + c\\c130^2 + d\\c130^3 \$\$ where \\a\\, \\b\\, \\c\\, and
\\d\\ are species-specific coefficients.

The function performs the following steps:

- checks that the input data frame contains the required variables
  `c130` and `species_code`,

- validates that `c130` is numeric,

- verifies that all species are available in the `danbr` reference table
  and issues a warning otherwise,

- merges the input with `danbr` to retrieve coefficients and
  species-specific validity ranges (`min_c130`, `max_c130`),

- warns when `c130` values fall outside the recommended range,

- computes tarif-"br" branch volume and returns the augmented data
  frame.

If one or more species codes are not found in `danbr`, the function
issues a warning and returns `NA`-values for missing coefficients and
volumes. Trees with `c130` values outside the recommended
species-specific range produce a warning but still receive a computed
branch volume. In some specific cases (mainly for small trees), the
equation returns negative volume estimates. In these cases, the
estimates are replaced by 0 and a warning is issued.

## Supported species

The following species codes are supported by `dagnelie_br`:

- `"QUERCUS_SP"`,

- `"QUERCUS_ROBUR"`,

- `"QUERCUS_PETRAEA"`,

- `"QUERCUS_PUBESCENS"`,

- `"QUERCUS_RUBRA"`

- `"FAGUS_SYLVATICA"`,

- `"ACER_PSEUDOPLATANUS"`,

- `"FRAXINUS_EXCELSIOR"`,

- `"ULMUS_SP"`,

- `"PRUNUS_AVIUM"`

- `"BETULA_SP"`,

- `"ALNUS_GLUTINOSA"`,

- `"LARIX_SP"`,

- `"PINUS_SYLVESTRIS"`,

- `"CRATAEGUS_SP"`

- `"PRUNUS_SP"`,

- `"CARPINUS_SP"`,

- `"CASTANEA_SATIVA"`,

- `"CORYLUS_AVELLANA"`,

- `"MALUS_SP"`

- `"PYRUS_SP"`,

- `"SORBUS_ARIA"`,

- `"SAMBUCUS_SP"`,

- `"RHAMNUS_FRANGULA"`,

- `"PRUNUS_CERASUS"`

- `"ALNUS_INCANA"`,

- `"POPULUSxCANADENSIS"`,

- `"POPULUS_TREMULA"`,

- `"PINUS_NIGRA"`,

- `"PINUS_LARICIO"`

- `"TAXUS_BACCATA"`,

- `"ACER_PLATANOIDES"`,

- `"ACER_CAMPESTRE"`,

- `"SORBUS_AUCUPARIA"`,

- `"JUNGLANS_SP"`

- `"TILLIA_SP"`,

- `"AESCULUS_HIPPOCASTANUM"`,

- `"ROBINIA_PSEUDOACACIA"`,

- `"SALIX_SP"`

## See also

[`danbr`](https://gauthierligot.github.io/GCubeR/reference/danbr.md) for
species-specific coefficients.

## Examples

``` r
df <- data.frame(
  c130         = c(145, 156, 234, 233),
  species_code = c("PINUS_SYLVESTRIS", "QUERCUS_RUBRA",
                   "QUERCUS_SP", "FAGUS_SYLVATICA")
)
dagnelie_br(df)
#>   c130     species_code dagnelie_br
#> 1  145 PINUS_SYLVESTRIS  0.05035402
#> 2  156    QUERCUS_RUBRA  0.54602889
#> 3  234       QUERCUS_SP  1.73326649
#> 4  233  FAGUS_SYLVATICA  2.25615067
```
