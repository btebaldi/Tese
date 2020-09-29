# Data: 2020-09-29
#
# Autor: Bruno Tebaldi de Queiroz Barbosa
#
# Script que cria banco para o consumo do OX.
#
# Inputs: Micro_adm.csv e Micro_des.csv
#
# Outputs:
#
# Micro_Nivel.csv : Arquivo com o nivel de emprego de cada microregiao
# (calculado a partir do nivel de 1995 e a seguir soma-se os fluxos ano a ano)
#
# Micro_EmpLiq.csv : Arquivo com o nivel de emprego liquido de cada microregiao
# (calculado a partir dos dados de nivel de admissao e demissao)
#
# Micro_adm.csv e Micro_des.csv : Arquivo com os fluxos de emprego (admissao e
# demissao) de cada microregiao


# Clear all
rm(list=ls())

# Load libraries
library(readr)
library(stringr)

# Load database
load("./Database/Micro.Rdata")

# preview data
head(full.adm)
head(full.des)

# Regexp pattern para extracao das datas
pattern <- "(?<=(ADM_)|(DES_))\\d{4}M\\d{1,2}"

adm.mat <- full.adm %>% select(-1) %>% t()
colnames(adm.mat) <- paste("R", full.adm$ID_Micro, sep = "_")
rownames(adm.mat) <- stringr::str_extract(rownames(adm.mat), stringr::regex(pattern))


des.mat <- full.des %>% select(-1) %>% t()
colnames(des.mat) <- paste("R", full.adm$ID_Micro, sep = "_")
rownames(des.mat) <- stringr::str_extract(rownames(des.mat), stringr::regex(pattern))



