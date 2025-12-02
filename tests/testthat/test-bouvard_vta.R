# tests/testthat/test-bouvard_vta.R

test_that("bouvard_vta errors on missing required columns", {
  df <- data.frame(dbh = 20, htot = 15)
  expect_error(bouvard_vta(df), "Missing required columns")
  
  df2 <- data.frame(species_code = "QUERCUS_SP", htot = 15)
  expect_error(bouvard_vta(df2), "Missing required columns")
  
  df3 <- data.frame(species_code = "QUERCUS_SP", dbh = 20)
  expect_error(bouvard_vta(df3), "Missing required columns")
})

test_that("bouvard_vta errors on non-numeric dbh or htot", {
  df <- data.frame(species_code = "QUERCUS_SP", dbh = "not_numeric", htot = 15)
  expect_error(bouvard_vta(df), "'dbh' must be numeric")
  
  df2 <- data.frame(species_code = "QUERCUS_SP", dbh = 20, htot = "not_numeric")
  expect_error(bouvard_vta(df2), "'htot' must be numeric")
})

test_that("bouvard_vta cleans species names (case and whitespace)", {
  df <- data.frame(species_code = " quercus_sp ", dbh = 30, htot = 20)
  result <- bouvard_vta(df)
  expect_equal(result$species_code[1], "QUERCUS_SP")
})

test_that("bouvard_vta returns message and no column if no compatible species", {
  df <- data.frame(species_code = c("PICEA_ABIES", "FAGUS_SYLVATICA"),
                   dbh = c(25, 40), htot = c(18, 22))
  expect_message(result <- bouvard_vta(df),
                 "No compatible species found")
  expect_false("bouvard_vta" %in% names(result))
})

test_that("bouvard_vta computes volume correctly for QUERCUS_SP", {
  df <- data.frame(species_code = "QUERCUS_SP", dbh = 30, htot = 20)
  result <- bouvard_vta(df)
  
  expect_true("bouvard_vta" %in% names(result))
  expect_equal(result$bouvard_vta[1], 0.5 * (0.30^2) * 20, tolerance = 1e-8)
})

test_that("bouvard_vta handles multiple rows with mixed species", {
  df <- data.frame(
    species_code = c("QUERCUS_SP", "PICEA_ABIES", "QUERCUS_SP"),
    dbh = c(30, 25, 40),
    htot = c(20, 18, 22)
  )
  result <- bouvard_vta(df)
  
  expect_true("bouvard_vta" %in% names(result))
  
  # Row 1 : QUERCUS_SP → compute
  expect_equal(result$bouvard_vta[1], 0.5 * (0.30^2) * 20, tolerance = 1e-8)
  
  # Row 2 : PICEA_ABIES → NA
  expect_true(is.na(result$bouvard_vta[2]))
  
  # Row 3 : QUERCUS_SP → compute
  expect_equal(result$bouvard_vta[3], 0.5 * (0.40^2) * 22, tolerance = 1e-8)
})

test_that("bouvard_vta always returns a data.frame", {
  df <- data.frame(species_code = "QUERCUS_SP", dbh = 30, htot = 20)
  result <- bouvard_vta(df)
  expect_s3_class(result, "data.frame")
})