# tests/testthat/test-dagnelie_vc22_1.R

test_that("dagnelie_vc22_1 errors on invalid input", {
  expect_error(dagnelie_vc22_1(list()), "is.data.frame")
  
  df <- data.frame(species_code = "PINUS_NIGRA")
  expect_error(dagnelie_vc22_1(df), "Missing column")
  
  df2 <- data.frame(c130 = 120)
  expect_error(dagnelie_vc22_1(df2), "Missing column")
  
  df3 <- data.frame(c130 = "not_numeric", species_code = "PINUS_NIGRA")
  expect_error(dagnelie_vc22_1(df3), "c130 must be numeric")
})

test_that("dagnelie_vc22_1 computes volume correctly within range", {
  df <- data.frame(species_code = "PINUS_NIGRA", c130 = 150)
  result <- dagnelie_vc22_1(df)
  
  expect_true("dagnelie_vc22_1" %in% names(result))
  
  coeff <- GCubeR::dan1
  a <- as.numeric(coeff$coeff_a[coeff$species_code == "PINUS_NIGRA"])
  b <- as.numeric(coeff$coeff_b[coeff$species_code == "PINUS_NIGRA"])
  c <- as.numeric(coeff$coeff_c[coeff$species_code == "PINUS_NIGRA"])
  d <- as.numeric(coeff$coeff_d[coeff$species_code == "PINUS_NIGRA"])
  
  expected <- a + b*150 + c*150^2 + d*150^3
  expect_equal(result$dagnelie_vc22_1[1], expected, tolerance = 1e-8)
})

test_that("dagnelie_vc22_1 warns when c130 out of range", {
  df <- data.frame(species_code = "PINUS_NIGRA", c130 = 9999)
  expect_warning(result <- dagnelie_vc22_1(df), "c130 out of range")
  
  coeff <- GCubeR::dan1
  a <- as.numeric(coeff$coeff_a[coeff$species_code == "PINUS_NIGRA"])
  b <- as.numeric(coeff$coeff_b[coeff$species_code == "PINUS_NIGRA"])
  c <- as.numeric(coeff$coeff_c[coeff$species_code == "PINUS_NIGRA"])
  d <- as.numeric(coeff$coeff_d[coeff$species_code == "PINUS_NIGRA"])
  
  expected <- a + b*9999 + c*9999^2 + d*9999^3
  expect_equal(result$dagnelie_vc22_1[1], expected, tolerance = 1e-8)
})

test_that("dagnelie_vc22_1 warns for unknown species", {
  df <- data.frame(species_code = "UNKNOWN_SPECIES", c130 = 120)
  expect_warning(
    result <- dagnelie_vc22_1(df),
    regexp = "Unknown species"
  )
  expect_true(is.na(result$dagnelie_vc22_1[1]))
})

test_that("dagnelie_vc22_1 handles NA values gracefully", {
  df <- data.frame(species_code = "PINUS_NIGRA", c130 = NA_real_)
  result <- dagnelie_vc22_1(df)
  expect_true(is.na(result$dagnelie_vc22_1[1]))
})

test_that("dagnelie_vc22_1 works with multiple rows and mixed species", {
  df <- data.frame(
    species_code = c("PINUS_NIGRA", "QUERCUS_RUBRA", "UNKNOWN_SPECIES"),
    c130 = c(150, 200, 120)
  )
  
  expect_warning(
    result <- dagnelie_vc22_1(df),
    regexp = "Unknown species"
  )
  
  # Row 1 : PINUS_NIGRA
  coeff <- GCubeR::dan1
  a <- as.numeric(coeff$coeff_a[coeff$species_code == "PINUS_NIGRA"])
  b <- as.numeric(coeff$coeff_b[coeff$species_code == "PINUS_NIGRA"])
  c <- as.numeric(coeff$coeff_c[coeff$species_code == "PINUS_NIGRA"])
  d <- as.numeric(coeff$coeff_d[coeff$species_code == "PINUS_NIGRA"])
  
  expected <- a + b*150 + c*150^2 + d*150^3
  expect_equal(result$dagnelie_vc22_1[1], expected, tolerance = 1e-8)
  
  # Row 2 : QUERCUS_RUBRA
  a <- as.numeric(coeff$coeff_a[coeff$species_code == "QUERCUS_RUBRA"])
  b <- as.numeric(coeff$coeff_b[coeff$species_code == "QUERCUS_RUBRA"])
  c <- as.numeric(coeff$coeff_c[coeff$species_code == "QUERCUS_RUBRA"])
  d <- as.numeric(coeff$coeff_d[coeff$species_code == "QUERCUS_RUBRA"])
  
  expected <- a + b*200 + c*200^2 + d*200^3
  expect_equal(result$dagnelie_vc22_1[2], expected, tolerance = 1e-8)
  
  # Row 3 :UNKNOWN_SPECIES → NA
  expect_true(is.na(result$dagnelie_vc22_1[3]))
})

test_that("dagnelie_vc22_1 returns a data.frame with expected columns", {
  df <- data.frame(species_code = "PINUS_NIGRA", c130 = 150)
  result <- dagnelie_vc22_1(df)
  expect_s3_class(result, "data.frame")
  expect_true("c130" %in% names(result))
  expect_true("species_code" %in% names(result))
  expect_true("dagnelie_vc22_1" %in% names(result))
})

test_that("dagnelie_vc22_1 can export output", {
  df <- data.frame(species_code = "PINUS_NIGRA", c130 = 150)
  tmp <- tempfile(fileext = ".csv")
  result <- dagnelie_vc22_1(df, output = tmp)
  expect_s3_class(result, "data.frame")
  expect_true(file.exists(tmp))
})