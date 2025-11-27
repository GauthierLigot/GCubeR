 data <- data.frame(
   species_code = c("PICEA_ABIES", "FAGUS_SYLVATICA", "QUERCUS_SP", "QUERCUS_ROBUR"),
   c130 = c(60, 80, 50, 40), 
   htot = c(25, 18, 20, 22),
   hdom = c(25,25,20,20)
 )
 
 # Case 1: Print results to console (default)
 data <- add_c130_dbh(data)
 data <- vallet_vta(data)
 data <- vallet_vc22(data)
 data <- rondeux_vc22_vtot(data)
 data <- algan_vta_vc22(data)
 data  <- bouvard_vta(data)
 data <- dagnelie_vc22_1(data)
 data <- dagnelie_vc22_1g(data)
 data<- dagnelie_vc22_2(data)
 data <- biomass_calc(data) 

