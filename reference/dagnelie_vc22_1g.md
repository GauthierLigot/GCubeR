# Graduated single-entry Dagnelie volume (tarif 1g)

Computes the standing volume \\v\_{c,22}\\ (in cubic metres per tree)
using Dagnelie's *tarif 1g* equations. The volume is calculated from the
stem circumference at 1.30 m (`c130`, in cm), the dominant height
(`hdom`, in m), and the tree species, using species-specific polynomial
coefficients stored in `dan1g`.

## Usage

``` r
dagnelie_vc22_1g(data, output = NULL)
```

## Arguments

- data:

  A `data.frame` containing the columns:

  - `c130` (stem circumference at 1.30 m, in cm)

  - `hdom` (dominant height, in m)

  - `species_code` (character code of the tree species)

- output:

  Optional file path where the resulting data frame should be exported
  as a CSV. If `NULL` (default), no file is written. Export is handled
  by the utility function
  [`export_output()`](https://gauthierligot.github.io/GCubeR/reference/export_output.md).

## Value

A `data.frame` identical to `data`, augmented with:

- the joined columns from `dan1g` (`coeff_a`, `coeff_b`, `coeff_c`,
  `coeff_d`, `coeff_e`, `coeff_f`, `min_c130`, `max_c130`, `min_hdom`,
  `max_hdom`)

- `dagnelie_vc22_1g`: the computed volume (m\\^3\\ per tree)

## Details

The function:

- checks that the input data frame contains the required columns `c130`,
  `hdom` and `species_code`,

- validates that all species codes are present in the `dan1g` table,

- merges the input data with `dan1g` to retrieve: `coeff_a`, `coeff_b`,
  `coeff_c`, `coeff_d`, `coeff_e`, `coeff_f`, as well as the
  species-specific valid ranges `min_c130`, `max_c130`, `min_hdom`,
  `max_hdom`,

- issues a warning for trees whose `c130` is outside the valid range
  `[min_c130, max_c130]`,

- issues a warning for trees whose `hdom` is outside the valid range
  `[min_hdom, max_hdom]`,

- computes the tarif 1g volume using the species-specific polynomial:
  \$\$ v\_{c,22} = coeff_a + coeff_b \cdot c130 + coeff_c \cdot c130^2 +
  coeff_d \cdot c130^3 + coeff_e \cdot hdom + coeff_f \cdot c130^2 \cdot
  hdom \$\$

Species codes must match those available in the `dan1g` table. If one or
more species are not found, the function issues a warning.

If a tree's `c130` or `hdom` falls outside the species-specific validity
ranges `[min_c130, max_c130]` or `[min_hdom, max_hdom]`, a warning is
issued, but the volume is still computed.

## See also

[`dan1g`](https://gauthierligot.github.io/GCubeR/reference/dan1g.md) for
the species-specific coefficients and ranges.

## Examples

``` r
df <- data.frame(
  c130         = c(145, 156, 234, 233),
  hdom         = c(25, 23, 45, 34),
  species_code = c("PINUS_SYLVESTRIS", "QUERCUS_RUBRA",
                   "QUERCUS_SP", "FAGUS_SYLVATICA")
)
dagnelie_vc22_1g(data = df)
#> Warning: hdom out of range for 1 tree(s): row 3 (species QUERCUS_SP, min=10, max=32, found=45)
#>   c130 hdom     species_code dagnelie_vc22_1g
#> 1  145   25 PINUS_SYLVESTRIS         1.703138
#> 2  156   23    QUERCUS_RUBRA         1.791659
#> 3  234   45       QUERCUS_SP         8.308537
#> 4  233   34  FAGUS_SYLVATICA         5.950880
```
