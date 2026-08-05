# Equations metadata for GCubeR

A reference table compiling metadata about allometric equations used in
GCubeR (Vallet, Dagnelie, Algan, Rondeux, CNPF, etc.). This dataset is
provided for information purposes only and is not directly used by
package functions.

## Usage

``` r
data(equations_GCubeR)
```

## Format

A data frame with columns:

- eq_id:

  Equation identifier (character)

- method:

  Method family (Vallet, Dagnelie, Algan, Rondeux, CNPF…)

- predicted_variable:

  Predicted variable (volume, biomass, carbon…)

- output_unit:

  Unit of the output (m3, kg, tdm…)

- species_id:

  Numeric species identifier (integer)

- species_name_fr:

  Species name in French (character)

- species_code:

  Species code (uppercase Latin name)

- validity_region:

  Region of validity (text)

- validity_range:

  Range of validity (text)

- input_variable:

  Input variables required (e.g. c130, htot, dbh)

- input_unit:

  Units of input variables (e.g. cm, m)

- formula_type:

  Equation type (e.g. polynomial, exponential)

- explicit_formula:

  Explicit formula as text

- coeff_a:

  Equation coefficient a (numeric)

- coeff_b:

  Equation coefficient b (numeric)

- coeff_c:

  Equation coefficient c (numeric)

- coeff_d:

  Equation coefficient d (numeric)

- coeff_e:

  Equation coefficient e (numeric)

- coeff_f:

  Equation coefficient f (numeric)

- remarks:

  Additional notes

- reference_source:

  Bibliographic source

## Source

Internal CSV file `data-raw/equations_GCubeR.csv`
