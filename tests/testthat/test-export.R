# tests/testthat/test-export_output.R

test_that("export_output returns FALSE invisibly when output is NULL", {
  df <- data.frame(id = 1:3, volume = c(10.5, 12.3, 9.8))
  res <- export_output(df, NULL)
  expect_false(res)
  expect_invisible(export_output(df, NULL))
})

test_that("export_output warns and returns FALSE when output is not a single string", {
  df <- data.frame(id = 1:3, volume = c(10.5, 12.3, 9.8))
  expect_warning(res <- export_output(df, 123),
                 regexp = "Parameter 'output' must be a single string")
  expect_false(res)
  
  expect_warning(res <- export_output(df, c("file1.csv", "file2.csv")),
                 regexp = "Parameter 'output' must be a single string")
  expect_false(res)
})

test_that("export_output writes CSV successfully and returns TRUE", {
  df <- data.frame(id = 1:3, volume = c(10.5, 12.3, 9.8))
  tmp <- tempfile(fileext = ".csv")
  res <- export_output(df, tmp)
  expect_true(res)
  expect_true(file.exists(tmp))
  content <- read.csv2(tmp)
  expect_equal(nrow(content), 3)
  expect_equal(names(content), c("id", "volume"))
})

test_that("export_output returns FALSE if writing fails", {
  df <- data.frame(id = 1:3, volume = c(10.5, 12.3, 9.8))
  bad_path <- file.path(tempdir(), "nonexistent_dir", "file.csv")
  res <- suppressWarnings(export_output(df, bad_path))
  expect_false(res)
})

test_that("export_output returns invisible logical values", {
  df <- data.frame(id = 1:3, volume = c(10.5, 12.3, 9.8))
  tmp <- tempfile(fileext = ".csv")
  expect_invisible(export_output(df, tmp))
  expect_invisible(export_output(df, NULL))
})