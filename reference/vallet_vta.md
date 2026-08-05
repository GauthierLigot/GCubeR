# Calculate Total Aboveground Volume (VTA) Vallet Method

Computes the total aboveground volume (VTA) for trees based on the
circumference at 1.30m (c130) and total height (htot) using the Vallet
form factor method.

## Usage

``` r
vallet_vta(data, na_action = c("error", "omit"), output = NULL)
```

## Arguments

- data:

  A data frame containing tree measurements. Must include the columns:
  `species_code` (in uppercase Latin format), `c130` (circumference at
  1.30m, in cm), and `htot` (total height, in m).

- na_action:

  How to handle missing input values. `"error"` (default) stops if core
  required values are explicitly `NA`. `"omit"` removes rows with
  missing core data. Note: Model constraint violation (`c130` \< 45 cm)
  and unknown species are always handled by setting VTA and Form Factor
  to NA, preserving input values.

- output:

  Optional file path (string). If provided, the resulting data frame
  will be written to this file using semicolon (;) as a delimiter. NA
  values are written as empty strings (""). Defaults to `NULL`.

## Value

The resulting data frame (same as the printed data) with the new columns
and `vallet_vta` (Total Aboveground Volume in **m3**)..

## Details

The model is only valid for trees with a circumference at 1.30m (`c130`)
of at least 45 cm. For non-compliant trees or unknown species, results
are set to `NA`.

The Form Factor (`form`) is calculated as: \$\$form = (a + b \cdot
c\_{130} + c \cdot \frac{\sqrt{c\_{130}}}{h\_{tot}}) \cdot \left(1 +
\frac{d}{c\_{130}^2}\right)\$\$

The Total Aboveground Volume (`VTA`) is then: \$\$VTA = form \cdot
\frac{1}{\pi \cdot 40000} \cdot c\_{130}^2 \cdot h\_{tot}\$\$

Coefficients a, b, c, d are loaded from the `vallet_vta.csv` file.

## Supported species

The Vallet VTA method is currently implemented for the following species
(via their `species_code`):

- `"PICEA_ABIES"`

- `"QUERCUS_ROBUR"`

- `"FAGUS_SYLVATICA"`

- `"PINUS_SYLVESTRIS"`

- `"PINUS_PINASTER"`

- `"ABIES_ALBA"`

- `"PSEUDOTSUGA_MENZIESII"`

## Examples

``` r
data_test <- data.frame(
  species_code = c("PICEA_ABIES", "FAGUS_SYLVATICA", "UNKNOWN_SPECIES", "QUERCUS_ROBUR"),
  c130 = c(60, 80, 50, 40), 
  htot = c(25, 18, 20, 22)
)

# Case 1: Print results to console (default)
results_console <- vallet_vta(data_test)
#> Warning: Circumference (c130) constraint violated: 1 tree(s) have c130 < 45 cm. Form factor and VTA will be set to NA for these rows: 4
#> Warning: Unknown species (missing vta coefficients): UNKNOWN_SPECIES. Form factor and vta will be set to NA for these rows.

# Case 2: Write results to a file
# vallet_vta(data_test, output = "vta_results.csv")
```
