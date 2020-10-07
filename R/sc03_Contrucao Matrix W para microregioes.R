#  VALE ESTE AQUI
# Clear all
rm(list=ls())


# ---- bibliotecas utilizadas ----
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)



# ---- Load data sources ----
# Carrego a lista dos municipios
load(file = "./Database/codigos_municipios.RData")
colnames(full) = "Muni"

# carrega o dicionario com relacao do codigo da mesoregioao
load("./Database/Dicionario_microregioes.RData")
Dicionario <- dicionario_OxGvar

# carrego as informacoes de mesoregioes.
Muni_Micro.info <- readxl::read_excel("Database/Cadastro Municipios.xlsx", sheet = "TabelaCompleta")

# Crio a variavel de municipio sem o digito verificador
Muni_Micro.info$ID_Municipio2 <- trunc(Muni_Micro.info$ID_Municipio / 10)


# Carrega a tabela de conexoes entre as regioes
Connexoes.df <- read_excel("C:/Users/bteba/Dropbox/bruno-tebaldi/Base de dados/MatrixW.xlsx",
                           sheet = "Conex",
                           range = "A1:O24930")

# Acerta o nome das colunas 
colnames(Connexoes.df) <- c("Cod_Municipio",
                            "Nome", 
                            "Cod_Detino",
                            "Nome_Destino",
                            "Tipo_Link",
                            "Id_Tipo_Link",
                            "Unitario",
                            "Meso_Origem", 
                            "Meso_Destino",
                            "LinkUnico",
                            "LinkUnico_por_tipo", 
                            "Mesma_regiao",
                            "Qtd_Link_entre_meso", 
                            "Qtd_Link_Centro_fonte_centro",
                            "Qtd_Link_fonte Demais")

# Ajusto o banco de dados de conexoes
Connexoes.df <- Connexoes.df %>% 
  dplyr::inner_join(Muni_Micro.info, by = c("Cod_Municipio"="ID_Municipio2")) %>% 
  dplyr::mutate(Micro_Origem = ID_Micro) %>% 
  dplyr::select("Cod_Municipio", "Nome", "Micro_Origem", "Cod_Detino", "Nome_Destino") %>% 
  dplyr::inner_join(Muni_Micro.info, by = c("Cod_Detino"="ID_Municipio2")) %>% 
  dplyr::mutate(Micro_Destino = ID_Micro) %>% 
  dplyr::select("Cod_Municipio", "Nome", "Micro_Origem", "Cod_Detino", "Nome_Destino", "Micro_Destino")


# ---- Macroregiao ou Microregiao ----
# Selecao entre construcao de matriz
i=2
if(i==1){
  qtd_of_regions <- 137;
} else {
  qtd_of_regions <- 558;  
}

# ---- Algoritimo de construcao da matrix ----


# Constroe uma matrix de pessos zerada
W.mat <- matrix(0, ncol = qtd_of_regions, nrow = qtd_of_regions)
colnames(W.mat) <- Dicionario$Short_name
rownames(W.mat) <- Dicionario$Short_name


# Faz a construção efetiva da matrix de pesos
for (col in 1:ncol(W.mat)) {
  # busca qual o nome da coluna atual
  nomeColuna <- colnames(W.mat)[col]
  
  # busca o codigo da coluna (regiao de origem)
  Origem <- Dicionario$ID_Micro[which(Dicionario$Short_name == nomeColuna)]
  
  for (row in 1:nrow(W.mat)) {
    # cat(sprintf("%d, %d\n", row, col))
    
    # busca qual o nome da coluna atual
    nomeLinha <- rownames(W.mat)[row]
    
    # busca o codigo da coluna (regiao de origem)
    Destino <- Dicionario$ID_Micro[which(Dicionario$Short_name == nomeLinha)]
    
    if(Origem == Destino){
      W.mat[row, col] <- 0;
    } else {
      W.mat[row, col] <- Connexoes.df %>% 
        filter(Micro_Origem == Origem, Micro_Destino == Destino) %>% 
        summarise(Total=n()) %>% pull(Total)
    }
  }
}


# --- Normaliza as colunas da matrix ----
for (col in 1:ncol(W.mat)) {
  somaColuna <- sum(W.mat[,col]);
  W.mat[,col] <- W.mat[,col] / somaColuna
}

# --- Salva a matrix em arquivo .mat ----
fileConn <- file("./Excel Export/data.mat")
writeLines(sprintf("%d %d // A %d by %d matrix", qtd_of_regions, qtd_of_regions, qtd_of_regions, qtd_of_regions), fileConn)
close(fileConn)

write.table(x = W.mat, file = "./Excel Export/data.mat",
            append = TRUE,
            col.names = FALSE,
            row.names = FALSE)