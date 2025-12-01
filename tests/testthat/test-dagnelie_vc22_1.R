# tests/testthat/test-dagnelie_vc22_1.R

test_that("dagnelie_vc22_1 computes volume correctly for valid input", {
  df <- data.frame(
    c130 = 145,
    species_code = "PINUS_SYLVESTRIS"
  )
  result <- dagnelie_vc22_1(df)
  
  # Vérifie que la colonne de sortie existe
  expect_true("dagnelie_vc22_1" %in% names(result))
  
  # Vérifie que le résultat est numérique
  expect_type(result$dagnelie_vc22_1, "double")
  
  # Vérifie qu'il n'y a pas de NA
  expect_false(anyNA(result$dagnelie_vc22_1))
})

test_that("dagnelie_vc22_1 warns for unknown species", {
  df <- data.frame(
    c130 = 150,
    species_code = "UNKNOWN_SPECIES"
  )
  expect_warning(
    result <- dagnelie_vc22_1(df),
    regexp = "Unknown species"
  )
  # Le volume doit être NA car pas de coefficients
  expect_true(all(is.na(result$dagnelie_vc22_1)))
})

test_that("dagnelie_vc22_1 errors if c130 is not numeric", {
  df <- data.frame(
    c130 = "not_numeric",
    species_code = "PINUS_SYLVESTRIS"
  )
  expect_error(
    dagnelie_vc22_1(df),
    regexp = "c130 must be numeric"
  )
})

test_that("dagnelie_vc22_1 errors if required columns are missing", {
  df <- data.frame(
    circumference = 145,
    species = "PINUS_SYLVESTRIS"
  )
  expect_error(
    dagnelie_vc22_1(df),
    regexp = "Missing column"
  )
})

test_that("dagnelie_vc22_1 warns when c130 is out of range", {
  df <- data.frame(
    c130 = 9999, # valeur volontairement hors plage
    species_code = "PINUS_SYLVESTRIS"
  )
  expect_warning(
    result <- dagnelie_vc22_1(df),
    regexp = "c130 out of range"
  )
  # Le volume est quand même calculé
  expect_true(!is.na(result$dagnelie_vc22_1))
})

