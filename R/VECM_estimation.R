rm(list = ls())

library(readr)
library(readxl)
library(dplyr)
library(urca)
library(vars)
library(tsDyn)

region.db <- read_csv("~/GitHub/Tese/Ox Metrics GVAR/Database/DatabaseDesAdm_RA_v1.csv")
head(region.db)

macro.db <- read_excel("~/GitHub/Tese/Ox Metrics GVAR/Database/MacroVariables_forR_20210604.xlsx")
head(macro.db)



i <- 1
cols.sufix <- c("_Admitidos", "_Desligados")

tbl <- region.db[, paste("R", i, cols.sufix, sep="")]

mdl <- tsDyn::VECM(tbl, 12, r=1, include = "const")

summary(mdl)










