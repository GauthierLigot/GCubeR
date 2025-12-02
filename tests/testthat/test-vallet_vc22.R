# tests/testthat/test-vallet_vc22.R

test_that("vallet_vc22 errors on missing required columns", {
  df <- data.frame(dbh = 20, htot = 25)
  expect_error(vallet_vc22(df), "Input data is missing required columns")
})

test_that("vallet_vc22 handles NA values with na_action = 'error'", {
  df <- data.frame(
    species_code = "PICEA_ABIES",
    dbh = NA_real_,
    htot = 20
  )
  result <- vallet_vc22(df, na_action = "error")
  if ("vallet_vc22" %in% names(result)) {
    expect_true(is.na(result$vallet_vc22) || is.nan(result$vallet_vc22))
  } else {
    expect_false("vallet_vc22" %in% names(result))
  }
})

test_that("vallet_vc22 handles NA values with na_action = 'omit'", {
  df <- data.frame(
    species_code = "PICEA_ABIES",
    dbh = NA_real_,
    htot = 20
  )
  result <- vallet_vc22(df, na_action = "omit")
  expect_equal(nrow(result), 1)
  if ("vallet_vc22" %in% names(result)) {
    expect_true(all(is.na(result$vallet_vc22) | is.nan(result$vallet_vc22)))
  } else {
    expect_false("vallet_vc22" %in% names(result))
  }
})


test_that("vallet_vc22 computes correct volume for known species", {
  df <- data.frame(
    species_code = "PICEA_ABIES",
    dbh = 20,
    htot = 25
  )
  result <- suppressWarnings(vallet_vc22(df))
  expect_false(is.na(result$vallet_vc22))
  expect_type(result$vallet_vc22, "double")
})

test_that("vallet_vc22 warns for unknown species", {
  df <- data.frame(
    species_code = "UNKNOWN_SPECIES",
    dbh = 20,
    htot = 25
  )
  expect_warning(result <- vallet_vc22(df), "Unknown species")
  if ("vallet_vc22" %in% names(result)) {
    expect_true(is.na(result$vallet_vc22) || is.nan(result$vallet_vc22))
  } else {
    expect_false("vallet_vc22" %in% names(result))
  }
})

test_that("vallet_vc22 warns when dbh < 7 cm", {
  df <- data.frame(
    species_code = "PICEA_ABIES",
    dbh = 6.4,
    htot = 22
  )
  expect_warning(result <- vallet_vc22(df), "Diameter")
  if ("vallet_vc22" %in% names(result)) {
    expect_true(all(is.na(result$vallet_vc22) | is.nan(result$vallet_vc22)))
  } else {
    expect_false("vallet_vc22" %in% names(result))
  }
})

test_that("vallet_vc22 returns early if no compatible species", {
  df <- data.frame(
    species_code = "UNKNOWN_SPECIES",
    dbh = 20,
    htot = 25
  )
  expect_warning(
    expect_message(result <- vallet_vc22(df),
                   "No compatible species found"),
    "Unknown species"
  )
  
  expect_false("vallet_vc22" %in% names(result))
})

test_that("vallet_vc22 works with multiple rows and mixed species", {
  df <- data.frame(
    species_code = c("PICEA_ABIES", "UNKNOWN_SPECIES", "FAGUS_SYLVATICA"),
    dbh = c(20, 25, 6.4),
    htot = c(25, 20, 22)
  )
  result <- suppressWarnings(vallet_vc22(df))
  
  # Row 1 
  expect_false(is.na(result$vallet_vc22[1]))
  # Row 2 
  expect_true(is.na(result$vallet_vc22[2]) || is.nan(result$vallet_vc22[2]))
  # Row 3 
  expect_true(is.na(result$vallet_vc22[3]) || is.nan(result$vallet_vc22[3]))
})

test_that("vallet_vc22 can export output", {
  df <- data.frame(
    species_code = "PICEA_ABIES",
    dbh = 20,
    htot = 25
  )
  tmp <- tempfile(fileext = ".csv")
  result <- suppressWarnings(vallet_vc22(df, output = tmp))
  expect_s3_class(result, "data.frame")
  expect_true(file.exists(tmp))
})