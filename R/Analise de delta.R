# clear all
rm(list=ls())

# Biblioteca
library(readxl)
library(dplyr)

# lista dos anos
anos = 1985:2018

for (ano in anos) {
  # constroi o nome dos arquivos
  fileName.adm = paste("consulta_adm", "_", ano," (todos).xlsx", sep = "")
  fileName.des = paste("consulta_des", "_", ano," (todos).xlsx", sep = "")
  
  # constroi o caminho para os arquivos
  filePath.adm = file.path("C:/Users/bteba/Dropbox/bruno-tebaldi/RAIS Base de dados/XLS", fileName.adm)
  filePath.des = file.path("C:/Users/bteba/Dropbox/bruno-tebaldi/RAIS Base de dados/XLS", fileName.des)
  # cat(file.exists(filePath))
  
  # Carrega os dados de admitidos
  dados.adm <- read_excel(filePath.adm, range = cell_limits(c(1,1), c(NA, 2)))
  colnames(dados.adm) <- c("Municipio", "Nao_admitido_ano")
  
  # Carrega os dados de desligados
  dados.des <- read_excel(filePath.des, range = cell_limits(c(1,1), c(NA, 2)))
  colnames(dados.des) <- c("Municipio", "Nao_desligado_ano")
  
  # junta a informacao de adm e des
  dados <- dplyr::inner_join(dados.adm, dados.des, by=c("Municipio"="Municipio"))
  
  # anexa a informacao do ano aos dados
  dados <- dados %>% dplyr::mutate(ano= ano)
  
  # Guarda os dados para a proxima interacao
  if(exists("dados.full")){
    dados.full <- dplyr::bind_rows(dados.full, dados)
  } else {
    dados.full <- dados
  }
  
  # remove variaveis nao utlizadas mais.
  rm(list = c("dados", "dados.adm", "dados.des", "fileName.adm", "fileName.des", "filePath.adm", "filePath.des"))
}

# carrega base de dados
save(dados.full, file = "dados.full.Rdata")



