# Calculate Total and Commercial Stem Volume (Rondeux, larch)

Computes the total stem volume (`rondeux_vtot`) and the commercial stem
volume at 22 cm (`rondeux_vc22`) for larch trees according to Rondeux
equations, based on the circumference at 1.30 m (`c130`, in cm) and the
total height (`htot`, in meters).

## Usage

``` r
rondeux_vc22_vtot(data, na_action = c("error", "omit"), output = NULL)
```

## Arguments

- data:

  A data frame containing tree measurements. Must include:
  `species_code` (uppercase character), `c130` (circumference at 1.30 m,
  in cm), `htot` (total height, in m).

- na_action:

  How to handle missing essential values (`c130`, `htot`). `"error"`
  (default) stops if missing values are detected. `"omit"` removes rows
  with missing essential fields.

- output:

  Optional file path where the resulting data frame should be exported
  as a CSV. If NULL (default), no file is written. Export is handled by
  the utility function
  [`export_output()`](https://gauthierligot.github.io/GCubeR/reference/export_output.md)
  and failures trigger warnings without interrupting execution.

## Value

A data frame identical to `data`, with two added columns:

- `rondeux_vtot`: total stem volume (m3)

- `rondeux_vc22`: commercial stem volume at 22 cm (m3)

## Details

The implemented equations are:

\$\$v\_{\mathrm{tot}} = a\_{\mathrm{vtot}} \cdot c\_{130}^2 \cdot
h\_{\mathrm{tot}}\$\$ where \\a\_{\mathrm{vtot}} = 0.406780 \times
10^{-5}\\.

\$\$v\_{c22} = a\_{\mathrm{vc22}} + b\_{\mathrm{vc22}} \cdot c\_{130}^2
\cdot h\_{\mathrm{tot}}\$\$ where \\a\_{\mathrm{vc22}} = -0.008877\\ and
\\b\_{\mathrm{vc22}} = 0.412411 \times 10^{-5}\\.

Expected units:

- `c130`: cm

- `htot`: m

- output volumes: m3

These equations are valid **only for larch**. Rows with other species
are returned with `NA` volumes and a warning is issued.

## Examples

``` r
df <- data.frame(
  species_code = c("LARIX_DECIDUA", "LARIX_DECIDUA", "LARIX_DECIDUA"),
  c130 = c(60, 65, 55),
  htot = c(15, 18, 20)
)

rondeux_vc22_vtot(df)
#>    species_code c130 htot rondeux_vtot rondeux_vc22
#> 1 LARIX_DECIDUA   60   15    0.2196612    0.2138249
#> 2 LARIX_DECIDUA   65   18    0.3093562    0.3047616
#> 3 LARIX_DECIDUA   55   20    0.2461019    0.2406317
```
