# Data: 2020-09-29
#
# Autor: Bruno Tebaldi de Queiroz Barbosa
#
# Script que cria banco para o consumo do OX.
#
# Inputs: Micro.Rdata (contem full.adm e full.des)
#
# Outputs:
#
# Dicionario Ox-GVAR.csv : Arquivo com a relação de de-para dar regioes do Ox,
# GVAR e IBGE
#
# Dicionario_microregioes.RData : Arquivo com a relação de de-para dar regioes
# do Ox, GVAR e IBGE
#
# DatabaseDesAdm_micro_v1.csv : Arquivo os dados de desligados e Admitidos para
# consumo direto no OxMetrics.
# 

# Clear all
rm(list=ls())

# Load libraries
library(readr)
library(dplyr)
library(stringr)

# Load database
load("./Database/Micro.Rdata")

# preview data
head(full.adm)
head(full.des)

# Crio o dicionario de dados entre os nomes das regioes
dicionario_OxGvar <- data.frame(ID_Micro=full.adm$ID_Micro,
                                Short_name=paste("R", full.adm$ID_Micro, sep = "_"),
                                Ox=paste("R", 1:nrow(full.adm), sep = ""))

# Regexp pattern para extracao das datas
pattern <- "(?<=(ADM_)|(DES_))\\d{4}M\\d{1,2}"

adm.mat <- full.adm %>% select(-1) %>% t()
colnames(adm.mat) <- paste(dicionario_OxGvar$Ox, "Admitidos", sep = "_")
rownames(adm.mat) <- stringr::str_extract(rownames(adm.mat), stringr::regex(pattern))


des.mat <- full.des %>% select(-1) %>% t()
colnames(des.mat) <- paste(dicionario_OxGvar$Ox, "Desligados", sep = "_")
rownames(des.mat) <- stringr::str_extract(rownames(des.mat), stringr::regex(pattern))

readr::write_excel_csv(dicionario_OxGvar, path = "./Excel Export/Dicionario Ox-GVAR.csv")
save(dicionario_OxGvar, file = "./Database/Dicionario_microregioes.RData")

full.mat <- cbind(des.mat, adm.mat)

write.csv(full.mat, file = "./Excel Export/DatabaseDesAdm_micro_v1.csv")

