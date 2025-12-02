# tests/testthat/test-dagnelie_br.R

test_that("dagnelie_br errors on invalid input", {
  expect_error(dagnelie_br(list()), "is.data.frame")
  
  df <- data.frame(species_code = "PINUS_NIGRA")
  expect_error(dagnelie_br(df), "Missing column")
  
  df2 <- data.frame(c130 = 120)
  expect_error(dagnelie_br(df2), "Missing column")
  
  df3 <- data.frame(c130 = "not_numeric", species_code = "PINUS_NIGRA")
  expect_error(dagnelie_br(df3), "c130 must be numeric")
})

test_that("dagnelie_br computes branch volume correctly within range", {
  df <- data.frame(species_code = "PINUS_NIGRA", c130 = 150)
  result <- dagnelie_br(df)
  
  expect_true("dagnelie_br" %in% names(result))
  
  coeff <- GCubeR::danbr
  a <- as.numeric(coeff$coeff_a[coeff$species_code == "PINUS_NIGRA"])
  b <- as.numeric(coeff$coeff_b[coeff$species_code == "PINUS_NIGRA"])
  c <- as.numeric(coeff$coeff_c[coeff$species_code == "PINUS_NIGRA"])
  d <- as.numeric(coeff$coeff_d[coeff$species_code == "PINUS_NIGRA"])
  
  expected <- a + b*150 + c*150^2 + d*150^3
  expect_equal(result$dagnelie_br[1], expected, tolerance = 1e-8)
})

test_that("dagnelie_br warns when c130 out of range", {
  df <- data.frame(species_code = "PINUS_NIGRA", c130 = 9999)
  expect_warning(result <- dagnelie_br(df), "c130 out of range")
  
  coeff <- GCubeR::danbr
  a <- as.numeric(coeff$coeff_a[coeff$species_code == "PINUS_NIGRA"])
  b <- as.numeric(coeff$coeff_b[coeff$species_code == "PINUS_NIGRA"])
  c <- as.numeric(coeff$coeff_c[coeff$species_code == "PINUS_NIGRA"])
  d <- as.numeric(coeff$coeff_d[coeff$species_code == "PINUS_NIGRA"])
  
  expected <- a + b*9999 + c*9999^2 + d*9999^3
  expect_equal(result$dagnelie_br[1], expected, tolerance = 1e-8)
})

test_that("dagnelie_br warns for unknown species", {
  df <- data.frame(species_code = "UNKNOWN_SPECIES", c130 = 120)
  expect_warning(
    result <- dagnelie_br(df),
    regexp = "Unknown species"
  )
  expect_true(is.na(result$dagnelie_br[1]))
})

test_that("dagnelie_br handles NA values gracefully", {
  df <- data.frame(species_code = "PINUS_NIGRA", c130 = NA_real_)
  result <- dagnelie_br(df)
  expect_true(is.na(result$dagnelie_br[1]))
})

test_that("dagnelie_br works with multiple rows and mixed species", {
  df <- data.frame(
    species_code = c("PINUS_NIGRA", "QUERCUS_RUBRA", "UNKNOWN_SPECIES"),
    c130 = c(150, 200, 120)
  )
  
  expect_warning(
    result <- dagnelie_br(df),
    regexp = "Unknown species"
  )
  
  # Ligne 1 : PINUS_NIGRA
  coeff <- GCubeR::danbr
  a <- as.numeric(coeff$coeff_a[coeff$species_code == "PINUS_NIGRA"])
  b <- as.numeric(coeff$coeff_b[coeff$species_code == "PINUS_NIGRA"])
  c <- as.numeric(coeff$coeff_c[coeff$species_code == "PINUS_NIGRA"])
  d <- as.numeric(coeff$coeff_d[coeff$species_code == "PINUS_NIGRA"])
  
  expected <- a + b*150 + c*150^2 + d*150^3
  expect_equal(result$dagnelie_br[1], expected, tolerance = 1e-8)
  
  # Ligne 2 : QUERCUS_RUBRA
  a <- as.numeric(coeff$coeff_a[coeff$species_code == "QUERCUS_RUBRA"])
  b <- as.numeric(coeff$coeff_b[coeff$species_code == "QUERCUS_RUBRA"])
  c <- as.numeric(coeff$coeff_c[coeff$species_code == "QUERCUS_RUBRA"])
  d <- as.numeric(coeff$coeff_d[coeff$species_code == "QUERCUS_RUBRA"])
  
  expected <- a + b*200 + c*200^2 + d*200^3
  expect_equal(result$dagnelie_br[2], expected, tolerance = 1e-8)
  
  # Ligne 3 : espèce inconnue → NA
  expect_true(is.na(result$dagnelie_br[3]))
})

test_that("dagnelie_br returns a data.frame with expected columns", {
  df <- data.frame(species_code = "PINUS_NIGRA", c130 = 150)
  result <- dagnelie_br(df)
  expect_s3_class(result, "data.frame")
  expect_true("c130" %in% names(result))
  expect_true("species_code" %in% names(result))
  expect_true("dagnelie_br" %in% names(result))
})