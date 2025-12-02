# tests/testthat/test-algan_vta_vc22.R

test_that("algan_vta_vc22 errors on missing required columns", {
  df <- data.frame(dbh = 20, htot = 15)
  expect_error(algan_vta_vc22(df), "Missing required columns")
  
  df2 <- data.frame(species_code = "ABIES_ALBA", htot = 15)
  expect_error(algan_vta_vc22(df2), "Missing required columns")
  
  df3 <- data.frame(species_code = "ABIES_ALBA", dbh = 20)
  expect_error(algan_vta_vc22(df3), "Missing required columns")
})

test_that("algan_vta_vc22 errors on non-numeric dbh or htot", {
  df <- data.frame(species_code = "ABIES_ALBA", dbh = "not_numeric", htot = 15)
  expect_error(algan_vta_vc22(df), "'dbh' must be numeric")
  
  df2 <- data.frame(species_code = "ABIES_ALBA", dbh = 20, htot = "not_numeric")
  expect_error(algan_vta_vc22(df2), "'htot' must be numeric")
})

test_that("algan_vta_vc22 computes vc22 for compatible species", {
  df <- data.frame(
    species_code = c("ABIES_ALBA", "PICEA_ABIES", "ALNUS_GLUTINOSA", "PRUNUS_AVIUM", "BETULA_SP"),
    dbh = c(30, 25, 20, 18, 22), # cm
    htot = c(20, 18, 15, 12, 10) # m
  )
  result <- algan_vta_vc22(df)
  
  # Vérifie que la colonne existe
  expect_true("algan_vc22" %in% names(result))
  
  # Vérifie les calculs
  expect_equal(result$algan_vc22[1], 0.33 * (0.30^2) * 20, tolerance = 1e-8)
  expect_equal(result$algan_vc22[2], 0.33 * (0.25^2) * 18, tolerance = 1e-8)
  expect_equal(result$algan_vc22[3], 0.33 * (0.20^2) * 15, tolerance = 1e-8)
  expect_equal(result$algan_vc22[4], 0.33 * (0.18^2) * 12, tolerance = 1e-8)
  expect_equal(result$algan_vc22[5], 0.33 * (0.22^2) * 10, tolerance = 1e-8)
})

test_that("algan_vta_vc22 computes vta only for ABIES_ALBA with dbh > 15", {
  df <- data.frame(
    species_code = c("ABIES_ALBA", "ABIES_ALBA", "PICEA_ABIES"),
    dbh = c(30, 10, 25), # cm
    htot = c(20, 18, 15) # m
  )
  result <- algan_vta_vc22(df)
  
  # Vérifie que la colonne existe
  expect_true("algan_vta" %in% names(result))
  
  # Premier cas : ABIES_ALBA avec dbh > 15 → calculé
  expect_equal(result$algan_vta[1], 0.4 * (0.30^2) * 20, tolerance = 1e-8)
  
  # Deuxième cas : ABIES_ALBA avec dbh <= 15 → NA
  expect_true(is.na(result$algan_vta[2]))
  
  # Troisième cas : PICEA_ABIES → NA
  expect_true(is.na(result$algan_vta[3]))
})

test_that("algan_vta_vc22 returns NA for non-compatible species", {
  df <- data.frame(
    species_code = c("QUERCUS_ROBUR", "FAGUS_SYLVATICA"),
    dbh = c(30, 25),
    htot = c(20, 18)
  )
  result <- algan_vta_vc22(df)
  
  expect_true(all(is.na(result$algan_vc22)))
  expect_true(all(is.na(result$algan_vta)))
})

test_that("algan_vta_vc22 messages when domain condition violated", {
  df <- data.frame(
    species_code = c("ABIES_ALBA", "PICEA_ABIES"),
    dbh = c(10, 12), # <= 15 cm
    htot = c(20, 18)
  )
  expect_message(algan_vta_vc22(df), "Rows outside domain")
})

test_that("algan_vta_vc22 cleans species names (case and whitespace)", {
  df <- data.frame(
    species_code = c(" abies_alba ", "picea_abies"),
    dbh = c(30, 25),
    htot = c(20, 18)
  )
  result <- algan_vta_vc22(df)
  
  expect_equal(result$species_code[1], "ABIES_ALBA")
  expect_equal(result$species_code[2], "PICEA_ABIES")
})

test_that("algan_vta_vc22 creates columns only when applicable", {
  # Cas 1 : espèce compatible → colonne créée
  df <- data.frame(species_code = "BETULA_SP", dbh = 22, htot = 10)
  result <- algan_vta_vc22(df)
  expect_true("algan_vc22" %in% names(result))
  expect_false("algan_vta" %in% names(result)) # BETULA_SP ne calcule pas vta
  
  # Cas 2 : espèce non compatible → aucune colonne créée
  df2 <- data.frame(species_code = "QUERCUS_ROBUR", dbh = 30, htot = 20)
  result2 <- algan_vta_vc22(df2)
  expect_false("algan_vc22" %in% names(result2))
  expect_false("algan_vta" %in% names(result2))
  
  # Cas 3 : ABIES_ALBA avec dbh > 15 → les deux colonnes créées
  df3 <- data.frame(species_code = "ABIES_ALBA", dbh = 30, htot = 20)
  result3 <- algan_vta_vc22(df3)
  expect_true("algan_vc22" %in% names(result3))
  expect_true("algan_vta" %in% names(result3))
})