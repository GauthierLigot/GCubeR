library(tidyverse)
library(usethis)

# Load and clean the data from Excel marteloscope sheet
data_rondeux <- readxl::read_excel("data-raw/Excel_data_jacquesrondeux_Ligot_Claessens (maj)-GL.xlsx", sheet = 2) %>%
  
  dplyr::mutate(species_code = str_replace(str_to_upper(TrSpec)," ","_"),
                species_code = ifelse(species_code == "SALIX_CAPREA",yes = "SALIX_SP", no=species_code),
                species_code = ifelse(species_code == "BETULA_PENDULA",yes = "BETULA_SP", no=species_code),
                species_code = ifelse(species_code == "BETULA_PUBESCENS",yes = "BETULA_SP", no=species_code),
                species_code = ifelse(species_code == "CARPINUS_BETULUS",yes = "CARPINUS_SP", no=species_code),
                species_code = ifelse(species_code == "POPULUS_×CANESCENS",yes = "POPULUSxCANADENSIS", no=species_code))%>%
  dplyr::mutate(c150 = `d 1.3 [cm]`*pi) %>% 
  
  dplyr::filter(Status == 1) %>% 
  
  dplyr::select(id_tree = "Tree No", 
                species = "TrSpec", species_code,
                c150, htot = "h [m]")

# ADD DATASETS TO PACKAGE RESOURCES ----
usethis::use_data(data_rondeux, overwrite = TRUE)
