val_vta <- read.csv("data-raw/val_vta.csv", sep = ";")
usethis::use_data(val_vta, overwrite = TRUE)