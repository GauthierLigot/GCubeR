 data <- data.frame(
   species_code = c("PICEA_ABIES", "FAGUS_SYLVATICA", "UNKNOWN_SPECIES", "QUERCUS_ROBUR"),
   c130 = c(60, 80, 50, 40), 
   htot = c(25, 18, 20, 22),
   hdom = c(25,25,20,20)
 )
 
 # Case 1: Print results to console (default)
 data <- dagnelie_tarif1(data)
 data <- dagnelie_tarif2(data)
 data <- dagnelie_tarif1b(data)
 data <- add_c130_dbh(data)
 data <- vallet_vta(data)
 data <- vallet_vc22(data)
 data <- rondeux_vc22_vtot(data)
 data <- biomass_calc(data) 
 