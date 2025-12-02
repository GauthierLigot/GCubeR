# tests/testthat/test-add_c130_dbh.R

test_that("add_c130_dbh errors on invalid input", {
  expect_error(add_c130_dbh(list()), "is.data.frame")
  
  df <- data.frame(x = 1:3)
  expect_error(add_c130_dbh(df), "Data must contain either")
})

test_that("add_c130_dbh computes dbh from c130", {
  df <- data.frame(c130 = c(31.4, 62.8))
  expect_message(result <- add_c130_dbh(df), "dbh' column added")
  expect_equal(result$dbh, df$c130 / pi, tolerance = 1e-8)
})

test_that("add_c130_dbh computes c130 from dbh", {
  df <- data.frame(dbh = c(10, 20))
  expect_message(result <- add_c130_dbh(df), "c130' column added")
  expect_equal(result$c130, df$dbh * pi, tolerance = 1e-8)
})

test_that("add_c130_dbh fills missing values correctly", {
  df <- data.frame(
    c130 = c(NA, 31.4, NA),
    dbh  = c(10, NA, NA)
  )
  result <- add_c130_dbh(df)
  
  # Case 1
  expect_equal(result$c130[1], 10 * pi, tolerance = 1e-8)
  
  # Case 2 
  expect_equal(result$dbh[2], 31.4 / pi, tolerance = 1e-8)
  
  # Case 3 
  expect_true(is.na(result$c130[3]))
  expect_true(is.na(result$dbh[3]))
})

test_that("add_c130_dbh leaves values unchanged when both columns complete", {
  df <- data.frame(
    c130 = c(31.4, 62.8),
    dbh  = c(10, 20)
  )
  result <- add_c130_dbh(df)
  expect_equal(result$c130, df$c130)
  expect_equal(result$dbh, df$dbh)
})

test_that("add_c130_dbh handles multiple rows consistently", {
  df <- data.frame(
    c130 = c(31.4, NA, 62.8, NA),
    dbh  = c(NA, 10, NA, NA)
  )
  result <- add_c130_dbh(df)
  
  expect_equal(result$dbh[1], 31.4 / pi, tolerance = 1e-8)

  expect_equal(result$c130[2], 10 * pi, tolerance = 1e-8)
  
  expect_equal(result$dbh[3], 62.8 / pi, tolerance = 1e-8)
  
  expect_true(is.na(result$c130[4]))
  expect_true(is.na(result$dbh[4]))
})

test_that("add_c130_dbh returns a data.frame", {
  df <- data.frame(c130 = 31.4)
  result <- add_c130_dbh(df)
  expect_s3_class(result, "data.frame")
})

test_that("add_c130_dbh messages are informative", {
  df <- data.frame(c130 = 31.4)
  expect_message(add_c130_dbh(df), "dbh' column added")
  
  df2 <- data.frame(dbh = 10)
  expect_message(add_c130_dbh(df2), "c130' column added")
  
  df3 <- data.frame(c130 = c(NA, 31.4), dbh = c(10, NA))
  expect_message(add_c130_dbh(df3), "values filled")
})