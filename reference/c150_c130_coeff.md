# Coefficients for circumference conversion (1.50 m ↔ 1.30 m)

Species-specific linear coefficients used to convert stem circumference
between 1.50 m (`c150`) and 1.30 m (`c130`). These coefficients are used
internally by `c150_c130`.

## Usage

``` r
data(c150_c130_coeff)
```

## Format

A data frame with columns:

- species_code:

  Tree species code (character)

- coeff_a:

  Slope coefficient a (numeric)

- coeff_b:

  Intercept coefficient b (numeric)

- min_c150:

  Minimum valid circumference at 1.50 m (cm)

- max_c150:

  Maximum valid circumference at 1.50 m (cm)

## Source

Internal CSV file `data-raw/c150_c130_coeff.csv`
