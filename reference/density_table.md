# Wood density table for biomass calculation

Provides species-specific wood density values (t/m3) and species group
classification (conifer vs broadleaf) used in CNPF and Vallet biomass
estimation methods.

## Usage

``` r
data(density_table)
```

## Format

A data frame with columns:

- species_code:

  Tree species code (character, uppercase Latin format)

- density:

  Wood density in tonnes of dry matter per cubic meter (numeric)

- con_broad:

  Species group: "conifer" or "broadleaf"

## Source

Internal CSV file `data-raw/density_table.csv`
