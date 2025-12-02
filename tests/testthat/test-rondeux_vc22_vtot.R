# tests/testthat/test-rondeux_vc22_vtot.R

test_that("rondeux_vc22_vtot errors on missing required columns", {
  df <- data.frame(c130 = 50, htot = 20)
  expect_error(rondeux_vc22_vtot(df), "Input data is missing required columns")
})

test_that("rondeux_vc22_vtot handles NA values with na_action = 'error'", {
  df <- data.frame(
    species_code = "LARIX_DECIDUA",
    c130 = NA_real_,
    htot = 20
  )
  expect_message(rondeux_vc22_vtot(df, na_action = "error"),
                 "Missing values detected")
})

test_that("rondeux_vc22_vtot handles NA values with na_action = 'omit'", {
  df <- data.frame(
    species_code = "LARIX_DECIDUA",
    c130 = NA_real_,
    htot = 20
  )
  result <- rondeux_vc22_vtot(df, na_action = "omit")
  expect_equal(nrow(result), 0)
})

test_that("rondeux_vc22_vtot computes correct volumes for larch", {
  df <- data.frame(
    species_code = "LARIX_DECIDUA",
    c130 = 60,
    htot = 15
  )
  result <- rondeux_vc22_vtot(df)
  
  # Expected values
  a_vtot <- 0.406780e-5
  a_vc22 <- -0.008877
  b_vc22 <- 0.412411e-5
  expected_vtot <- a_vtot * (60^2) * 15
  expected_vc22 <- a_vc22 + b_vc22 * (60^2) * 15
  
  expect_equal(result$rondeux_vtot, expected_vtot, tolerance = 1e-8)
  expect_equal(result$rondeux_vc22, expected_vc22, tolerance = 1e-8)
})

test_that("rondeux_vc22_vtot sets NA for non-larch species", {
  df <- data.frame(
    species_code = "PINUS_SYLVESTRIS",
    c130 = 50,
    htot = 20
  )
  expect_message(result <- rondeux_vc22_vtot(df),
                 "No compatible species")
  expect_false("rondeux_vtot" %in% names(result))
  expect_false("rondeux_vc22" %in% names(result))
})

test_that("rondeux_vc22_vtot invalidates volumes when c130 out of range", {
  df <- data.frame(
    species_code = "LARIX_DECIDUA",
    c130 = 100, # out of range
    htot = 20
  )
  expect_message(result <- rondeux_vc22_vtot(df),
                 "Circumference constraint violated")
  
  if ("rondeux_vtot" %in% names(result)) {
    expect_true(is.na(result$rondeux_vtot))
    expect_true(is.na(result$rondeux_vc22))
  } else {
    expect_false("rondeux_vtot" %in% names(result))
    expect_false("rondeux_vc22" %in% names(result))
  }
})

test_that("rondeux_vc22_vtot drops columns if all results are NA", {
  df <- data.frame(
    species_code = "LARIX_DECIDUA",
    c130 = 100, # out of range
    htot = 20
  )
  expect_message(result <- rondeux_vc22_vtot(df),
                 "No valid Rondeux volumes")
  expect_false("rondeux_vtot" %in% names(result))
  expect_false("rondeux_vc22" %in% names(result))
})

test_that("rondeux_vc22_vtot works with multiple rows and mixed species", {
  df <- data.frame(
    species_code = c("LARIX_DECIDUA", "PINUS_SYLVESTRIS", "LARIX_SP"),
    c130 = c(60, 50, 65),
    htot = c(15, 20, 18)
  )
  result <- rondeux_vc22_vtot(df)
  
  expect_true(!is.na(result$rondeux_vtot[1]))
  expect_true(is.na(result$rondeux_vtot[2])) # non-larch
  expect_true(!is.na(result$rondeux_vtot[3]))
})

test_that("rondeux_vc22_vtot can export output", {
  df <- data.frame(
    species_code = "LARIX_DECIDUA",
    c130 = 60,
    htot = 15
  )
  tmp <- tempfile(fileext = ".csv")
  result <- rondeux_vc22_vtot(df, output = tmp)
  expect_s3_class(result, "data.frame")
  expect_true(file.exists(tmp))
})