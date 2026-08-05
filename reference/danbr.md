# Dagnelie branch coefficients (tarif "br")

Species-specific polynomial coefficients for the Dagnelie branch volume
model (tarif "br"). Loaded from `data-raw/danbr.csv`.

## Usage

``` r
data(danbr)
```

## Format

A data frame with columns:

- species_code:

  Tree species code (character)

- coeff_a:

  Coefficient a (numeric)

- coeff_b:

  Coefficient b (numeric)

- coeff_c:

  Coefficient c (numeric)

- coeff_d:

  Coefficient d (numeric)

- min_c130:

  Minimum circumference at 1.30 m (cm)

- max_c130:

  Maximum circumference at 1.30 m (cm)

## Source

Internal CSV file `data-raw/danbr.csv`
