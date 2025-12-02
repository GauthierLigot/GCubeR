# tests/testthat/test-GCubeR.R

test_that("GCubeR errors if input is not a data.frame", {
  expect_error(GCubeR(list()), "is.data.frame")
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
})

test_that("GCubeR errors if species_code column is missing", {
  df <- data.frame(c130 = 120, htot = 25)
  expect_error(GCubeR(df), "Missing column 'species_code'")
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
})

test_that("GCubeR adds c130 and dbh when only c150 is provided", {
  df <- data.frame(
    species_code = "QUERCUS_ROBUR", 
    c150 = 145,
    htot = 25,
    vallet_vc22 = 1.2   
  )
  
  pdf(NULL)
  res <- suppressWarnings(GCubeR(df))
  dev.off()
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  
  expect_true("c130" %in% names(res))
  expect_true("dbh" %in% names(res))
  expect_true(any(c("vallet_vta", "vallet_vc22", "dagnelie_vc22_1") %in% names(res)))
})

test_that("GCubeR runs always-applied functions and adds expected columns", {
  df <- data.frame(
    species_code = "QUERCUS_ROBUR",
    c130 = 156,
    htot = 25
  )
  
  pdf(NULL)
  res <- suppressWarnings(GCubeR(df))
  dev.off()
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  
  expect_true("dagnelie_vc22_1" %in% names(res))
  expect_true("dagnelie_br" %in% names(res))
})

test_that("GCubeR runs dagnelie_vc22_1g when hdom is present", {
  df <- data.frame(
    species_code = "QUERCUS_ROBUR",
    c130 = 156,
    htot = 30,
    hdom = 32
  )
  
  pdf(NULL)
  res <- suppressWarnings(GCubeR(df))
  dev.off()
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  
  expect_true("dagnelie_vc22_1g" %in% names(res))
})

test_that("GCubeR runs htot-dependent functions when htot is present", {
  df <- data.frame(
    species_code = "QUERCUS_ROBUR",
    c130 = 140,
    htot = 28
  )
  
  pdf(NULL)
  res <- suppressWarnings(GCubeR(df))
  dev.off()
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  
  expect_true("dagnelie_vc22_2" %in% names(res))
  expect_true("vallet_vta" %in% names(res))
  expect_true("vallet_vc22" %in% names(res))
  
  if ("algan_vta_vc22" %in% names(res)) {
    expect_type(res$algan_vta_vc22, "double")
  }
  if ("rondeux_vc22_vtot" %in% names(res)) {
    expect_type(res$rondeux_vc22_vtot, "double")
  }
  if ("bouvard_vta" %in% names(res)) {
    expect_type(res$bouvard_vta, "double")
  }
})

test_that("GCubeR always applies biomass_calc at the end", {
  df <- data.frame(
    species_code = "QUERCUS_ROBUR",
    c130 = 120,
    htot = 25
  )
  
  pdf(NULL)
  res <- suppressWarnings(GCubeR(df))
  dev.off()
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  
  # Biomass_calc outputs
  expect_true(any(grepl("cniefeb", names(res))) || any(grepl("vallet_c", names(res))))
})

test_that("GCubeR can export output to CSV", {
  df <- data.frame(
    species_code = "QUERCUS_ROBUR",
    c130 = 120,
    htot = 25
  )
  
  tmp <- tempfile(fileext = ".csv")
  
  pdf(NULL)
  res <- suppressWarnings(GCubeR(df, output = tmp))
  dev.off()
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  
  expect_s3_class(res, "data.frame")
  expect_true(file.exists(tmp))
})

test_that("GCubeR produces enriched dataframe with expected structure", {
  df <- data.frame(
    tree_id = 1:2,
    species_code = c("QUERCUS_ROBUR", "PICEA_ABIES"),
    c150 = c(145, NA),
    c130 = c(NA, 156),
    dbh  = c(NA, 40),
    htot = c(25, 30),
    hdom = c(NA, 32)
  )
  
  pdf(NULL)
  res <- suppressWarnings(GCubeR(df))
  dev.off()
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  
  expect_true(all(c("c130", "dbh") %in% names(res)))
  expect_true(any(grepl("dagnelie", names(res))))
  expect_true(any(grepl("vallet", names(res))))
  if (any(grepl("rondeux", names(res)))) {
    expect_true(any(grepl("rondeux", names(res))))
  }
  if (any(grepl("algan", names(res)))) {
    expect_true(any(grepl("algan", names(res))))
  }
  if (any(grepl("bouvard", names(res)))) {
    expect_true(any(grepl("bouvard", names(res))))
  }
  expect_true(any(grepl("cniefeb", names(res))))
})

test_that("GCubeR allows user to choose volume_col explicitly", {
  df <- data.frame(
    species_code = "QUERCUS_ROBUR",
    c130 = 140,
    htot = 28,
    vallet_vc22 = 2.5
  )
  
  pdf(NULL)
  res <- suppressWarnings(GCubeR(df, volume_col = "vallet_vc22"))
  dev.off()
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  
  expect_true("vallet_vc22" %in% names(res))
})

test_that("GCubeR does not fail if no volume_col is available", {
  df <- data.frame(
    species_code = "QUERCUS_ROBUR",
    c130 = 140
  )
  
  pdf(NULL)
  res <- suppressWarnings(GCubeR(df))
  dev.off()
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  
  expect_s3_class(res, "data.frame")
})