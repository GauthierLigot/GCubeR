# tests/testthat/test-c150_c130.R

# Pour les tests, on crée une table de coefficients factice
fake_coeff <- data.frame(
  species_code = c("PINUS_SYLVESTRIS", "QUERCUS_RUBRA"),
  coeff_a = c(1.0064, 1.0154),
  coeff_b = c(0.8076, -0.3087),
  min_c150 = c(35, 35),
  max_c150 = c(235, 265)
)

withr::local_package("GCubeR", {
  assign("c150_c130_coeff", fake_coeff, envir = asNamespace("GCubeR"))
})

test_that("c150_c130 errors on invalid input", {
  expect_error(c150_c130(list()), "is.data.frame")
  
  df <- data.frame(c150 = 120)
  expect_error(c150_c130(df), "Missing column 'species_code'")
  
  df2 <- data.frame(species_code = "PINUS_SYLVESTRIS")
  expect_error(c150_c130(df2), "Provide at least one of 'c150' or 'c130'")
})

test_that("c150_c130 computes c130 from c150 within range", {
  df <- data.frame(species_code = "PINUS_SYLVESTRIS", c150 = 150)
  result <- c150_c130(df)
  expect_true("c130" %in% names(result))
  expect_equal(result$c130[1], 1.0064 * 150 + 0.8076, tolerance = 1e-8)
})

test_that("c150_c130 warns when c150 out of range", {
  df <- data.frame(species_code = "PINUS_SYLVESTRIS", c150 = 9999)
  expect_warning(result <- c150_c130(df), "c150 out of range")
  # c130 ne doit pas être calculé car hors plage
  expect_true(is.na(result$c130[1]))
})

test_that("c150_c130 computes c150 from c130 when missing", {
  df <- data.frame(species_code = "QUERCUS_RUBRA", c150 = NA, c130 = 156)
  result <- c150_c130(df)
  expect_true("c150" %in% names(result))
  expect_equal(result$c150[1], (156 - -0.3087) / 1.0154, tolerance = 1e-8)
})

test_that("c150_c130 handles NA values gracefully", {
  df <- data.frame(species_code = "PINUS_SYLVESTRIS", c150 = NA, c130 = NA)
  result <- c150_c130(df)
  expect_true(is.na(result$c150[1]))
  expect_true(is.na(result$c130[1]))
})

test_that("c150_c130 warns for unknown species", {
  df <- data.frame(species_code = "UNKNOWN_SPECIES", c150 = 120)
  # Comme l'espèce n'est pas dans fake_coeff, les coeffs sont NA
  result <- c150_c130(df)
  expect_true(is.na(result$c130[1]))
})

test_that("c150_c130 returns a data.frame with expected columns", {
  df <- data.frame(species_code = "PINUS_SYLVESTRIS", c150 = 150)
  result <- c150_c130(df)
  expect_s3_class(result, "data.frame")
  expect_true("c150" %in% names(result))
  expect_true("c130" %in% names(result))
})

test_that("c150_c130 adds missing c150 column when only c130 is provided", {
  df <- data.frame(
    species_code = "PINUS_SYLVESTRIS",
    c130 = 156
  )
  result <- c150_c130(df)
  
  # Vérifie que la colonne c150 a bien été ajoutée
  expect_true("c150" %in% names(result))
  expect_s3_class(result, "data.frame")
  expect_true(!is.na(result$c150[1]))
})