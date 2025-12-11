# tests/testthat/test-biomass_calc.R

test_that("biomass_calc errors when species_code missing or no usable volume (na_action = 'error')", {
  df <- data.frame(
    species_code     = NA_character_, 
    dagnelie_vc22_1  = NA_real_,
    vallet_vc22      = NA_real_,
    rondeux_vc22     = NA_real_,
    algan_vc22       = NA_real_,
    vallet_vta       = NA_real_
  )
  expect_error(
    biomass_calc(df, na_action = "error"),
    "Missing values detected"
  )
})

test_that("biomass_calc with na_action = 'omit' drops rows without species or any volume source", {
  df <- data.frame(
    species_code = c(NA_character_, "PICEA_ABIES", "QUERCUS_ROBUR"),
    dagnelie_vc22_1 = c(NA_real_, NA_real_, 0.30),
    vallet_vc22     = c(NA_real_, 0.20, NA_real_),
    rondeux_vc22    = c(NA_real_, NA_real_, NA_real_),
    algan_vc22      = c(NA_real_, NA_real_, NA_real_),
    vallet_vta      = c(NA_real_, NA_real_, NA_real_)
  )
  res <- biomass_calc(df, na_action = "omit")
  # Keep rows 2 and 3 only (row 1 lacks species and volume)
  expect_equal(nrow(res), 2)
})

test_that("Dagnelie vc22 priority order and vc22_source assignment are correct", {
  df <- data.frame(
    species_code     = c("PICEA_ABIES", "PICEA_ABIES", "PICEA_ABIES", "PICEA_ABIES"),
    dagnelie_vc22_1  = c(0.10, NA, NA, 0.40),
    dagnelie_vc22_1g = c(0.20, 0.25, NA, NA),
    dagnelie_vc22_2  = c(0.30, NA, 0.35, NA)
  )
  res <- suppressMessages(biomass_calc(df))
  expect_equal(res$vc22_dagnelie, c(0.30, 0.25, 0.35, 0.40))
  expect_equal(res$vc22_source, c("dagnelie_vc22_2", "dagnelie_vc22_1g", "dagnelie_vc22_2", "dagnelie_vc22_1"))
})

test_that("Row-level message is emitted when all Dagnelie columns are NA", {
  df <- data.frame(
    species_code     = c("PICEA_ABIES", "PICEA_ABIES"),
    dagnelie_vc22_1  = c(NA, NA),
    dagnelie_vc22_1g = c(NA, NA),
    dagnelie_vc22_2  = c(NA, 0.25)
  )
  expect_message(
    biomass_calc(df),
    "The following rows have no trunk volume values in any of the Dagnelie columns"
  )
})

test_that("Column-not-found messages are emitted for missing sources", {
  df <- data.frame(
    species_code     = "PICEA_ABIES",
    dagnelie_vc22_1  = 0.12
  )
  expect_message(biomass_calc(df), "Column 'vallet_vta' not found")
  expect_message(biomass_calc(df), "Column 'vallet_vc22' not found")
  expect_message(biomass_calc(df), "Column 'rondeux_vc22' not found")
  expect_message(biomass_calc(df), "Column 'algan_vc22' not found")
})

test_that("Unknown species are reported against density table", {
  df <- data.frame(
    species_code     = "UNKNOWN_SPECIES",
    dagnelie_vc22_1  = 0.12
  )
  expect_message(biomass_calc(df), "Unknown species : UNKNOWN_SPECIES")
})

test_that("CNPF computation for Dagnelie source produces all output columns and finite values", {
  df <- data.frame(
    species_code     = "PICEA_ABIES",  # conifer, density 0.37, feb 1.3
    dagnelie_vc22_1  = 0.20
  )
  res <- suppressMessages(biomass_calc(df))
  need <- c("cnpf_dagnelie_bag", "cnpf_dagnelie_bbg",
            "cnpf_dagnelie_btot", "cnpf_dagnelie_c", "cnpf_dagnelie_co2")
  expect_true(all(need %in% names(res)))
  expect_true(is.finite(res$cnpf_dagnelie_bag[1]))
  # Check a precise expected bag using density table value for PICEA_ABIES (0.37) and feb=1.3
  expected_bag <- 0.20 * 1.3 * 0.37
  expect_equal(res$cnpf_dagnelie_bag[1], expected_bag, tolerance = 1e-12)
})

test_that("CNPF computation for Vallet vc22 produces columns", {
  df <- data.frame(
    species_code     = "FAGUS_SYLVATICA", # broadleaf, density 0.55, feb 1.56
    vallet_vc22      = 0.18
  )
  res <- suppressMessages(biomass_calc(df))
  need <- c("cnpf_vallet_bag", "cnpf_vallet_bbg",
            "cnpf_vallet_btot", "cnpf_vallet_c", "cnpf_vallet_co2")
  expect_true(all(need %in% names(res)))
  expected_bag <- 0.18 * 1.56 * 0.55
  expect_equal(res$cnpf_vallet_bag[1], expected_bag, tolerance = 1e-12)
})

test_that("CNPF computation for Rondeux vc22 produces columns", {
  df <- data.frame(
    species_code     = "PICEA_ABIES",
    rondeux_vc22     = 0.22
  )
  res <- suppressMessages(biomass_calc(df))
  need <- c("cnpf_rondeux_bag", "cnpf_rondeux_bbg",
            "cnpf_rondeux_btot", "cnpf_rondeux_c", "cnpf_rondeux_co2")
  expect_true(all(need %in% names(res)))
  expected_bag <- 0.22 * 1.3 * 0.37
  expect_equal(res$cnpf_rondeux_bag[1], expected_bag, tolerance = 1e-12)
})

test_that("CNPF computation for Algan vc22 produces columns", {
  df <- data.frame(
    species_code = "ABIES_ALBA",
    algan_vc22   = 0.16,
    vallet_vc22  = 0.10  # ajout pour satisfaire la condition
  )
  res <- suppressMessages(biomass_calc(df))
  need <- c("cnpf_algan_bag", "cnpf_algan_bbg",
            "cnpf_algan_btot", "cnpf_algan_c", "cnpf_algan_co2")
  expect_true(all(need %in% names(res)))
  expected_bag <- 0.16 * 1.3 * 0.38  # density ABIES_ALBA = 0.38
  expect_equal(res$cnpf_algan_bag[1], expected_bag, tolerance = 1e-12)
})

test_that("Vallet method outputs computed only for supported species and when vallet_vta present", {
  df <- data.frame(
    species_code = c("PICEA_ABIES", "QUERCUS_ROBUR", "UNKNOWN_SPECIES"),
    vallet_vta   = c(1.50, 1.20, 1.30)
  )
  res <- suppressMessages(biomass_calc(df))
  need <- c("vallet_bag", "vallet_bbg", "vallet_btot", "vallet_c", "vallet_co2")
  expect_true(all(need %in% names(res)))
  # Supported species compute numeric outputs (density 0.37 and 0.54)
  expect_false(is.na(res$vallet_bag[1]))
  expect_false(is.na(res$vallet_bag[2]))
  # Unknown species → NA
  expect_true(is.na(res$vallet_bag[3]))
})

test_that("Temporary columns feb, con_broad, density are removed in final output", {
  df <- data.frame(
    species_code     = "PICEA_ABIES",
    dagnelie_vc22_1  = 0.12,
    vallet_vc22      = 0.11,
    rondeux_vc22     = 0.10,
    algan_vc22       = 0.09,
    vallet_vta       = 0.30
  )
  res <- suppressMessages(biomass_calc(df))
  expect_false(any(c("feb", "con_broad", "density") %in% names(res)))
})

test_that("Messages emitted when specific volume columns contain NA values", {
  df <- data.frame(
    species_code     = c("PICEA_ABIES", "PICEA_ABIES"),
    vallet_vc22      = c(NA_real_, 0.10),
    rondeux_vc22     = c(0.12, NA_real_),
    algan_vc22       = c(NA_real_, NA_real_)
  )
  expect_message(biomass_calc(df), "The following rows have no trunk volume values in column 'vallet_vc22'")
  expect_message(biomass_calc(df), "The following rows have no trunk volume values in column 'rondeux_vc22'")
  expect_message(biomass_calc(df), "The following rows have no trunk volume values in column 'algan_vc22'")
})

test_that("Export_output is invoked and file is created when output path provided", {
  df <- data.frame(
    species_code     = "PICEA_ABIES",
    dagnelie_vc22_2  = 0.25,
    vallet_vc22      = 0.18,
    rondeux_vc22     = NA_real_,
    algan_vc22       = NA_real_,
    vallet_vta       = 1.50
  )
  tmp <- tempfile(fileext = ".csv")
  res <- suppressMessages(biomass_calc(df, output = tmp))
  expect_s3_class(res, "data.frame")
  expect_true(file.exists(tmp))
})