# Clear all
rm(list = ls())

# Carrega dados dos municipios
load(file = "./dados.full.Rdata")

# libraries
library(readxl)
library(dplyr)

# mostra dados dos municipios
head(dados.full)

# carrega dados das AMCs
amc <- read_excel("Database/Amcs_70_91_00_v4.xlsx", range = cell_limits(c(1,1), c(NA,3)))


# Retirar codigos ignorados, 99 e 0
dados.full <- dados.full %>% filter(!(Municipio %in% c(99, 0)))


head(amc)

amc[is.na(amc$amc),]

colnames(amc)

aa <- dados.full %>% left_join(amc, by = c("Municipio" = "munic"))

bb = aa[is.na(aa$amc),]
