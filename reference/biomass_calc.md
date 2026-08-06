# Total Biomass, Carbon and CO2 Estimation for Tree Species

Computes total biomass (aboveground + root), carbon content and CO2
equivalent for tree species using CNPF (with multiple trunk volume
sources) and Vallet methods.

## Usage

``` r
biomass_calc(data, na_action = c("error", "omit"), output = NULL)
```

## Arguments

- data:

  A data frame containing volume and species information for each tree.
  Must include:

  - `species_code`: species name in uppercase Latin format (e.g.
    `"PICEA_ABIES"`), matched against a density table.

  - At least one volume column:

    - For CNPF method (trunk volume):

      - Dagnelie equations: `dagnelie_vc22_2`, `dagnelie_vc22_1g`,
        `dagnelie_vc22_1` (priority order: `dagnelie_vc22_2` \>
        `dagnelie_vc22_1g` \> `dagnelie_vc22_1`)

      - Vallet equation: `vallet_vc22`

      - Rondeux equation: `rondeux_vc22`

      - Algan equation: `algan_vc22`

    - For Vallet method (total aboveground volume): `vallet_vta`

  If multiple trunk volumes are provided, CNPF is computed separately
  for each source. If only one is available, the corresponding method is
  applied. All volume columns must be numeric and expressed in cubic
  meters (m3).

- na_action:

  How to handle missing values. `"error"` (default) stops if any
  required value is missing. `"omit"` removes rows with missing values.

- output:

  Optional file path where the resulting data frame should be exported
  as a CSV. If NULL (default), no file is written. Export is handled by
  the utility function
  [`export_output()`](https://gauthierligot.github.io/GCubeR/reference/export_output.md)
  and failures trigger warnings without interrupting execution.

## Value

A data frame with one row per tree, including:

- `species_code`: species name in uppercase Latin format.

- `dagnelie_vc22_1`, `dagnelie_vc22_1g`, `dagnelie_vc22_2`,
  `vallet_vc22`, `rondeux_vc22`,`algan_vc22`: optional trunk volume
  inputs (in m3).

- `vallet_vta`: optional total aboveground volume (in m3) for Vallet
  method.

- `vc22_dagnelie`: selected trunk volume used for CNPF (Dagnelie), based
  on priority.

- `vc22_source`: name of the Dagnelie column used to populate `vc22`.

### CNPF method outputs:

- From Dagnelie (priority selection):

  - `cnpf_dagnelie_bag`, `cnpf_dagnelie_bbg`, `cnpf_dagnelie_btot`,
    `cnpf_dagnelie_c`, `cnpf_dagnelie_co2`

- From Vallet trunk volume (`vallet_vc22`):

  - `cnpf_vallet_bag`, `cnpf_vallet_bbg`, `cnpf_vallet_btot`,
    `cnpf_vallet_c`, `cnpf_vallet_co2`

- From Rondeux trunk volume (`rondeux_vc22`):

  - `cnpf_rondeux_bag`, `cnpf_rondeux_bbg`, `cnpf_rondeux_btot`,
    `cnpf_rondeux_c`, `cnpf_rondeux_co2`

- From Algan trunk volume (`algan_vc22`):

  - `cnpf_algan_bag`, `cnpf_algan_bbg`, `cnpf_algan_btot`,
    `cnpf_algan_c`, `cnpf_algan_co2`

### Vallet method outputs (if `vallet_vta` is available and species is compatible):

- `vallet_bag`, `vallet_bbg`, `vallet_btot`, `vallet_c`, `vallet_co2`

## Details

- The density table provides:

  - `density`: wood density in tonnes of dry matter per cubic meter
    (t/m3).

  - `con_broad`: species group, either `"conifer"` or `"broadleaf"`.

- The expansion factor `feb` is derived from `con_broad`:

  - `feb = 1.3` for conifers

  - `feb = 1.56` for broadleaves

- Dagnelie trunk volume (`vc22_dagnelie`) is automatically selected from
  the best available column, in the following priority:
  `dagnelie_vc22_2` \> `dagnelie_vc22_1g` \> `dagnelie_vc22_1`.

- CNPF outputs are computed separately for each trunk volume source
  (Dagnelie, Vallet, Rondeux, Algan).

- Vallet method is applied only to a predefined list of compatible
  species using `vallet_vta`.

- If required columns are missing, the corresponding method is skipped
  with a warning.

- Warnings are also displayed if trunk volume columns exist but contain
  missing values (`NA`).

- All biomass values are expressed in tonnes of dry matter (t), carbon
  in tonnes of carbon (t C), and CO2 in tonnes of CO2 equivalent (t
  CO2).

## Examples

``` r
# Example dataset
data <- data.frame(
  species_code = c("PICEA_ABIES", "QUERCUS_ROBUR", "FAGUS_SYLVATICA"),
  dagnelie_vc22_2 = c(1.1, NA, NA),
  dagnelie_vc22_1g = c(NA, NA, NA),
  dagnelie_vc22_1 = c(NA, 0.9, NA),
  vallet_vc22 = c(NA, 1.2, NA),
  rondeux_vc22 = c(NA, NA, 1.0),
  algan_vc22 = c(NA,0.8,NA),
  vallet_vta = c(1.5, NA, 1.3)
)

# Run biomass calculation and export to CSV
output_path <- tempfile(fileext = ".csv")
results <- biomass_calc(data, output = output_path)
#> The following rows have no trunk volume values in any of the Dagnelie columns (dagnelie_vc22_2, dagnelie_vc22_1g, dagnelie_vc22_1). CNPF (Dagnelie) will be skipped for these rows: 3
#> The following rows have no trunk volume values in column 'vallet_vc22'. CNPF (Vallet) will be skipped for these rows: 1, 3
#> The following rows have no trunk volume values in column 'rondeux_vc22'. CNPF (Rondeux) will be skipped for these rows: 1, 2
#> The following rows have no trunk volume values in column 'algan_vc22'. CNPF (Algan) will be skipped for these rows: 1, 3
#> File successfully written to: /tmp/RtmpVjzPwY/file1a3f4650ab1c.csv
if (file.exists(output_path)) {
  message("CSV file successfully created.")
} else {
  warning("CSV file was not created.")
}
#> CSV file successfully created.
```
