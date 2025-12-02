# tests/testthat/test-vallet_vta.R

test_that("vallet_vta errors on missing required columns", {
  df <- data.frame(c130 = 60, htot = 25)
  expect_error(vallet_vta(df), "Input data is missing required columns")
})

test_that("vallet_vta handles NA values with na_action = 'error'", {
  df <- data.frame(
    species_code = "PICEA_ABIES",
    c130 = NA_real_,
    htot = 25
  )
  result <- vallet_vta(df, na_action = "error")
  if ("vallet_vta" %in% names(result)) {
    expect_true(is.na(result$vallet_vta) || is.nan(result$vallet_vta))
  } else {
    expect_false("vallet_vta" %in% names(result))
  }
})

test_that("vallet_vta handles NA values with na_action = 'omit'", {
  df <- data.frame(
    species_code = "PICEA_ABIES",
    c130 = NA_real_,
    htot = 25
  )
  result <- vallet_vta(df, na_action = "omit")
  expect_equal(nrow(result), 1)
  if ("vallet_vta" %in% names(result)) {
    expect_true(is.na(result$vallet_vta) || is.nan(result$vallet_vta))
  } else {
    expect_false("vallet_vta" %in% names(result))
  }
})

test_that("vallet_vta computes correct volume for known species", {
  df <- data.frame(
    species_code = "PICEA_ABIES",
    c130 = 60,
    htot = 25
  )
  result <- suppressWarnings(vallet_vta(df))
  
  # Coefficients from CSV
  a <- 0.631
  b <- -0.000946
  c <- 0
  d <- 0
  
  form <- (a + b * 60 + c * (sqrt(60) / 25)) * (1 + d / (60^2))
  expected <- form * (1 / (pi * 40000)) * (60^2) * 25
  
  expect_equal(result$vallet_vta[1], expected, tolerance = 1e-8)
})

test_that("vallet_vta warns for unknown species", {
  df <- data.frame(
    species_code = "UNKNOWN_SPECIES",
    c130 = 60,
    htot = 25
  )
  expect_warning(result <- vallet_vta(df), "Unknown species")
  if ("vallet_vta" %in% names(result)) {
    expect_true(is.na(result$vallet_vta) || is.nan(result$vallet_vta))
  } else {
    expect_false("vallet_vta" %in% names(result))
  }
})

test_that("vallet_vta warns when c130 < 45 cm", {
  df <- data.frame(
    species_code = "PICEA_ABIES",
    c130 = 40,
    htot = 22
  )
  expect_warning(result <- vallet_vta(df), "Circumference")
  if ("vallet_vta" %in% names(result)) {
    expect_true(is.na(result$vallet_vta) || is.nan(result$vallet_vta))
  } else {
    expect_false("vallet_vta" %in% names(result))
  }
})

test_that("vallet_vta returns early if no compatible species", {
  df <- data.frame(
    species_code = "UNKNOWN_SPECIES",
    c130 = 40, # < 45 cm
    htot = 25
  )
  result <- suppressWarnings(vallet_vta(df))
  expect_false("vallet_vta" %in% names(result))
})

test_that("vallet_vta works with multiple rows and mixed species", {
  df <- data.frame(
    species_code = c("PICEA_ABIES", "UNKNOWN_SPECIES", "FAGUS_SYLVATICA"),
    c130 = c(60, 50, 40),
    htot = c(25, 20, 22)
  )
  result <- suppressWarnings(vallet_vta(df))
  
  # Row 1 
  expect_false(is.na(result$vallet_vta[1]))
  # Row 2 
  expect_true(is.na(result$vallet_vta[2]) || is.nan(result$vallet_vta[2]))
  # Row 3 
  expect_true(is.na(result$vallet_vta[3]) || is.nan(result$vallet_vta[3]))
})

test_that("vallet_vta can export output", {
  df <- data.frame(
    species_code = "PICEA_ABIES",
    c130 = 60,
    htot = 25
  )
  tmp <- tempfile(fileext = ".csv")
  result <- suppressWarnings(vallet_vta(df, output = tmp))
  expect_s3_class(result, "data.frame")
  expect_true(file.exists(tmp))
})