# Export data frame to CSV with warnings instead of errors

This function exports a data.frame to a CSV file. If `output` is NULL,
nothing is done. If the path is invalid or the export fails, a warning
is issued but the function does not stop, and still returns (invisibly)
a logical value indicating success.

## Usage

``` r
export_output(data, output)
```

## Arguments

- data:

  A data.frame to export.

- output:

  Character string: path to the CSV file. If NULL, nothing is done.

## Value

Invisibly returns `TRUE` if the export succeeded, `FALSE` otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  id = 1:3,
  volume = c(10.5, 12.3, 9.8)
)

# Export to CSV :
export_output(df, "results/volumes.csv")
} # }
```
