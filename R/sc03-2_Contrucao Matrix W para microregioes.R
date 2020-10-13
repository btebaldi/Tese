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
# carrega o dicionario com relacao do codigo da mesoregioao
load("./Database/Dicionario_microregioes.RData")
Dicionario <- dicionario_OxGvar

# carrego as informacoes de mesoregioes.
Muni_Micro.info <- readxl::read_excel("Database/Cadastro Municipios.xlsx", sheet = "TabelaCompleta")
Municipios.info <- readxl::read_excel("Database/Cadastro Municipios.xlsx", sheet = "Municipios", range = "A1:G5679")


ArranjoPopulacional.Info <- read_excel("./Database/Regioes de influencia 2020/base_tabular/REGIC2018_Arranjos_Populacionais.xlsx")

ArranjoPopulacional.Info2 <- ArranjoPopulacional.Info %>% 
  dplyr::inner_join(Muni_Micro.info, by=c("Código do AP"="ID_Municipio")) %>% select(c("Codmun", "Código do AP", "ID_Micro"))

aa <- Muni_Micro.info %>% dplyr::left_join(ArranjoPopulacional.Info2, by=c("ID_Municipio"="Codmun"))

bb <- aa[!is.na(aa$`Código do AP`), ] %>% mutate(t=ID_Micro.x==ID_Micro.y)

bb[bb$t == FALSE,]

unique(aa$ID_Micro[!is.na(aa$Cod_AP)])

# Carrega a tabela de conexoes entre as regioes
Connexoes.df <- read_excel("./Database/Regioes de influencia 2020/base_tabular/REGIC2018_Ligacoes_entre_Cidades_clean.xlsx")


# Acerta o nome das colunas 
# colnames(Connexoes.df) <- c("Cod_Municipio",
#                             "Nome", 
#                             "Cod_Detino",
#                             "Nome_Destino",
#                             "Tipo_Link",
#                             "Id_Tipo_Link",
#                             "Unitario",
#                             "Meso_Origem", 
#                             "Meso_Destino",
#                             "LinkUnico",
#                             "LinkUnico_por_tipo", 
#                             "Mesma_regiao",
#                             "Qtd_Link_entre_meso", 
#                             "Qtd_Link_Centro_fonte_centro",
#                             "Qtd_Link_fonte Demais")

# Ajusto o banco de dados de conexoes
Connexoes.df <- 
  Connexoes.df %>% 
  dplyr::inner_join(Muni_Micro.info, by = c("cod_ori"="ID_Municipio")) %>% 
  dplyr::mutate(Micro_Origem = ID_Micro) %>% 
  dplyr::select("cod_ori", "nome_ori", "Micro_Origem", "cod_dest", "nome_dest") %>% 
  dplyr::inner_join(Muni_Micro.info, by = c("cod_dest"="ID_Municipio")) %>% 
  dplyr::mutate(Micro_Destino = ID_Micro) %>% 
  dplyr::select("cod_ori", "nome_ori", "Micro_Origem", "cod_dest", "nome_dest", "Micro_Destino")


s <- !(dicionario_OxGvar$ID_Micro %in% unique(Connexoes.df$Micro_Origem))

s2 <- Muni_Micro.info$ID_Micro %in% dicionario_OxGvar$ID_Micro[s]

Muni_Micro.info$ID_Municipio[s2 ]

s3 <- Municipios.info$ID_Municipio %in% Muni_Micro.info$ID_Municipio[s2 ]

aa <- Municipios.info[s3, ]

Connexoes.df$nome_dest

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