# tests/testthat/test-dagnelie_vc22_1g.R

test_that("dagnelie_vc22_1g errors on invalid input and missing columns", {
  expect_error(dagnelie_vc22_1g(list()), "is.data.frame")
  
  df1 <- data.frame(c130 = 150, species_code = "PINUS_SYLVESTRIS")
  expect_error(dagnelie_vc22_1g(df1), "Missing column")
  
  df2 <- data.frame(hdom = 25, species_code = "PINUS_SYLVESTRIS")
  expect_error(dagnelie_vc22_1g(df2), "Missing column")
  
  df3 <- data.frame(c130 = 150, hdom = 25)
  expect_error(dagnelie_vc22_1g(df3), "Missing column")
})

test_that("dagnelie_vc22_1g errors if both c130 and hdom are non-numeric", {
  df <- data.frame(
    c130 = "not_numeric",
    hdom = "not_numeric",
    species_code = "PINUS_SYLVESTRIS"
  )
  expect_error(dagnelie_vc22_1g(df), "c130 must be numeric")
})

test_that("dagnelie_vc22_1g computes volume correctly within range", {
  df <- data.frame(c130 = 150, hdom = 25, species_code = "PINUS_SYLVESTRIS")
  result <- dagnelie_vc22_1g(df)
  
  expect_true("dagnelie_vc22_1g" %in% names(result))
  expect_type(result$dagnelie_vc22_1g, "double")
  expect_false(anyNA(result$dagnelie_vc22_1g))
  
  coeff <- GCubeR::dan1g
  a <- as.numeric(coeff$coeff_a[coeff$species_code == "PINUS_SYLVESTRIS"])
  b <- as.numeric(coeff$coeff_b[coeff$species_code == "PINUS_SYLVESTRIS"])
  c <- as.numeric(coeff$coeff_c[coeff$species_code == "PINUS_SYLVESTRIS"])
  d <- as.numeric(coeff$coeff_d[coeff$species_code == "PINUS_SYLVESTRIS"])
  e <- as.numeric(coeff$coeff_e[coeff$species_code == "PINUS_SYLVESTRIS"])
  f <- as.numeric(coeff$coeff_f[coeff$species_code == "PINUS_SYLVESTRIS"])
  
  expected <- a + b*150 + c*150^2 + d*150^3 + e*25 + f*150^2*25
  expect_equal(result$dagnelie_vc22_1g[1], expected, tolerance = 1e-8)
})

test_that("dagnelie_vc22_1g warns for unknown species", {
  df <- data.frame(c130 = 150, hdom = 25, species_code = "UNKNOWN_SPECIES")
  expect_warning(result <- dagnelie_vc22_1g(df), "Unknown species")
  expect_true(all(is.na(result$dagnelie_vc22_1g)))
})

test_that("dagnelie_vc22_1g handles NA values gracefully", {
  df1 <- data.frame(c130 = NA_real_, hdom = 25, species_code = "PINUS_SYLVESTRIS")
  expect_true(is.na(dagnelie_vc22_1g(df1)$dagnelie_vc22_1g))
  
  df2 <- data.frame(c130 = 150, hdom = NA_real_, species_code = "PINUS_SYLVESTRIS")
  expect_true(is.na(dagnelie_vc22_1g(df2)$dagnelie_vc22_1g))
})

test_that("dagnelie_vc22_1g warns when hdom is out of range", {
  df <- data.frame(c130 = 150, hdom = 9999, species_code = "PINUS_SYLVESTRIS")
  expect_warning(result <- dagnelie_vc22_1g(df), "hdom out of range")
  expect_true(!is.na(result$dagnelie_vc22_1g))
})

test_that("dagnelie_vc22_1g warns when c130 is out of range", {
  df <- data.frame(c130 = 9999, hdom = 25, species_code = "PINUS_SYLVESTRIS")
  expect_warning(result <- dagnelie_vc22_1g(df), "c130 out of range")
  expect_true(!is.na(result$dagnelie_vc22_1g))
})

test_that("dagnelie_vc22_1g works with multiple rows and mixed species", {
  df <- data.frame(
    c130 = c(150, 200, 180),
    hdom = c(25, 30, 20),
    species_code = c("PINUS_SYLVESTRIS", "QUERCUS_RUBRA", "UNKNOWN_SPECIES")
  )
  expect_warning(result <- dagnelie_vc22_1g(df), "Unknown species")
  
  coeff <- GCubeR::dan1g
  # Row 1
  a <- as.numeric(coeff$coeff_a[coeff$species_code == "PINUS_SYLVESTRIS"])
  b <- as.numeric(coeff$coeff_b[coeff$species_code == "PINUS_SYLVESTRIS"])
  c <- as.numeric(coeff$coeff_c[coeff$species_code == "PINUS_SYLVESTRIS"])
  d <- as.numeric(coeff$coeff_d[coeff$species_code == "PINUS_SYLVESTRIS"])
  e <- as.numeric(coeff$coeff_e[coeff$species_code == "PINUS_SYLVESTRIS"])
  f <- as.numeric(coeff$coeff_f[coeff$species_code == "PINUS_SYLVESTRIS"])
  expected1 <- a + b*150 + c*150^2 + d*150^3 + e*25 + f*150^2*25
  expect_equal(result$dagnelie_vc22_1g[1], expected1, tolerance = 1e-8)
  
  # Row 2
  a <- as.numeric(coeff$coeff_a[coeff$species_code == "QUERCUS_RUBRA"])
  b <- as.numeric(coeff$coeff_b[coeff$species_code == "QUERCUS_RUBRA"])
  c <- as.numeric(coeff$coeff_c[coeff$species_code == "QUERCUS_RUBRA"])
  d <- as.numeric(coeff$coeff_d[coeff$species_code == "QUERCUS_RUBRA"])
  e <- as.numeric(coeff$coeff_e[coeff$species_code == "QUERCUS_RUBRA"])
  f <- as.numeric(coeff$coeff_f[coeff$species_code == "QUERCUS_RUBRA"])
  expected2 <- a + b*200 + c*200^2 + d*200^3 + e*30 + f*200^2*30
  expect_equal(result$dagnelie_vc22_1g[2], expected2, tolerance = 1e-8)
  
  # Row 3 → NA
  expect_true(is.na(result$dagnelie_vc22_1g[3]))
})

test_that("dagnelie_vc22_1g returns a data.frame with expected columns", {
  df <- data.frame(c130 = 150, hdom = 25, species_code = "PINUS_SYLVESTRIS")
  result <- dagnelie_vc22_1g(df)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("c130","hdom","species_code","dagnelie_vc22_1g") %in% names(result)))
})

test_that("dagnelie_vc22_1g can export output", {
  df <- data.frame(c130 = 150, hdom = 25, species_code = "PINUS_SYLVESTRIS")
  tmp <- tempfile(fileext = ".csv")
  result <- dagnelie_vc22_1g(df, output = tmp)
  expect_s3_class(result, "data.frame")
  expect_true(file.exists(tmp))
})