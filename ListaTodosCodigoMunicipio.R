# Clear all
rm(list=ls())

# bibliotecas utilizadas
library(readxl)
library(dplyr)
library(tidyr)


full <- NULL
for (ano in 1995:2018) {
  
  # nome do arquivo
  path <- "C:/Users/bteba/Dropbox/bruno-tebaldi/RAIS Base de dados/XLS/"
  file.adm <- paste(path, "consulta_adm_", ano," (todos).xlsx", sep = "")
  file.des <- paste(path, "consulta_des_", ano," (todos).xlsx", sep = "")


  data.adm <- read_excel(file.adm, range = cell_limits(c(1, 1), c(NA, 14)))
  data.des <- read_excel(file.des, range = cell_limits(c(1, 1), c(NA, 14)))    

  a <- data.adm %>% select(1)
  d <- data.des %>% select(1)
  
  c <- unique(bind_rows(a, d))
  full <- unique(bind_rows(full, c))
}

save(full, file = "./Database/codigos_municipios.RData")
