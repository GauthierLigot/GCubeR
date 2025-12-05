# tests/testthat/test-plot_by_class.R

test_that("plot_by_class errors if input is not a data.frame", {
  expect_error(plot_by_class(list()), "is.data.frame")
})

test_that("plot_by_class errors if volume_col missing", {
  df <- data.frame(c130 = 100, species_code = "QUERCUS_ROBUR")
  expect_error(plot_by_class(df, volume_col = "dagnelie_vc22_2"),
               "Volume column 'dagnelie_vc22_2' not found")
})

test_that("plot_by_class errors if c130 missing", {
  df <- data.frame(species_code = "QUERCUS_ROBUR", dagnelie_vc22_2 = 1.2)
  expect_error(plot_by_class(df), "Column 'c130' is required")
})

test_that("plot_by_class errors if species_code missing", {
  df <- data.frame(c130 = 100, dagnelie_vc22_2 = 1.2)
  expect_error(plot_by_class(df), "Column 'species_code' is required")
})

test_that("plot_by_class errors if breaks not equally spaced", {
  df <- data.frame(
    c130 = c(100, 120),
    species_code = c("QUERCUS_ROBUR", "PICEA_ABIES"),
    dagnelie_vc22_2 = c(1.2, 2.3)
  )
  expect_error(plot_by_class(df, breaks = c(30, 60, 100, 150, 200, 230)),
               "Argument 'breaks' must define equally spaced classes")
})

test_that("plot_by_class errors if no valid rows", {
  df <- data.frame(
    c130 = c(100, 120),
    species_code = c("QUERCUS_ROBUR", "PICEA_ABIES"),
    dagnelie_vc22_2 = c(NA, NA)
  )
  expect_error(plot_by_class(df), "No valid rows with non-NA volume")
})

test_that("plot_by_class produces correct table structure", {
  df <- data.frame(
    c130 = c(100, 120, 180),
    species_code = c("QUERCUS_ROBUR", "PICEA_ABIES", "QUERCUS_ROBUR"),
    dagnelie_vc22_2 = c(1.2, 2.3, 3.4)
  )
  res <- plot_by_class(df)
  tbl <- res$table
  expect_true("TOTAL" %in% tbl$species_code)
  expect_true(all(c("QUERCUS_ROBUR", "PICEA_ABIES") %in% tbl$species_code))
  expect_true(any(grepl("\\[", names(tbl))))
})

test_that("plot_by_class returns a ggplot object when make_plot = TRUE", {
  df <- data.frame(
    c130 = c(100, 120, 180),
    species_code = c("QUERCUS_ROBUR", "PICEA_ABIES", "QUERCUS_ROBUR"),
    dagnelie_vc22_2 = c(1.2, 2.3, 3.4)
  )
  res <- plot_by_class(df, make_plot = TRUE)
  expect_s3_class(res$plot, "ggplot")
})

test_that("plot_by_class returns NULL plot when make_plot = FALSE", {
  df <- data.frame(
    c130 = c(100, 120, 180),
    species_code = c("QUERCUS_ROBUR", "PICEA_ABIES", "QUERCUS_ROBUR"),
    dagnelie_vc22_2 = c(1.2, 2.3, 3.4)
  )
  res <- plot_by_class(df, make_plot = FALSE)
  expect_null(res$plot)
})

test_that("plot_by_class can export output to CSV", {
  df <- data.frame(
    c130 = c(100, 120, 180),
    species_code = c("QUERCUS_ROBUR", "PICEA_ABIES", "QUERCUS_ROBUR"),
    dagnelie_vc22_2 = c(1.2, 2.3, 3.4)
  )
  tmp <- tempfile(fileext = ".csv")
  res <- plot_by_class(df, output = tmp)
  expect_s3_class(res$table, "data.frame")
  expect_true(file.exists(tmp))
})

test_that("plot_by_class respects small_limit and medium_limit", {
  df <- data.frame(
    c130 = c(50, 100, 150),
    species_code = c("QUERCUS_ROBUR", "PICEA_ABIES", "QUERCUS_ROBUR"),
    dagnelie_vc22_2 = c(1.2, 2.3, 3.4)
  )
  res <- plot_by_class(df, small_limit = 55, medium_limit = 110)
  expect_s3_class(res$plot, "ggplot")
  layers <- sapply(res$plot$layers, function(l) class(l$geom)[1])
  expect_true("GeomVline" %in% layers)
})