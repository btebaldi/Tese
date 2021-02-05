# Data: 2020-10-13
#
# Autor: Bruno Tebaldi de Queiroz Barbosa
#
# Script para construcao da matrix de pesos do GVAR utilizado pelo OxMetrics
#



# Setup -------------------------------------------------------------------

# Clear all
rm(list=ls())

# bibliotecas utilizadas
library(readxl)
library(dplyr)



# Dataload ----------------------------------------------------------------


# carrega o dicionario com relacao do codigo da regiao de agregacao
Dicionario <- readxl::read_excel("./Database/Relacao_Agregacao_Ox.xlsx")

# carrego as informacoes dos municipios e suas regioes de agregacao.
Muni_RA.info <- readxl::read_excel("./Database/Agregacao de Municipios.xlsx", 
                                   sheet = "TabelaCompleta",
                                   range = cell_limits(c(1, 1), c(NA, 11))
)

# Filtro para ter apernas regioes nao ignoradas
Muni_RA.info <- Muni_RA.info %>% dplyr::filter(Ignorado == 0)

# Carrega a tabela de conexoes entre as regioes
Connexoes.df <- read_excel("./Database/Ligacoes_entre_Cidades.xlsx",
                           sheet = "ligacoes")

# Ajusto o banco de dados de conexoes
Connexoes.df <- 
  Connexoes.df %>% 
  dplyr::inner_join(Muni_RA.info, by = c("cod_ori"="ID_Municipio")) %>% 
  dplyr::mutate(RA_Origem = RegiaoAgregacao) %>% 
  dplyr::select("cod_ori", "nome_ori", "RA_Origem", "cod_dest", "nome_dest") %>% 
  dplyr::inner_join(Muni_RA.info, by = c("cod_dest"="ID_Municipio")) %>% 
  dplyr::mutate(RA_Destino = RegiaoAgregacao) %>% 
  dplyr::select("cod_ori", "nome_ori", "RA_Origem", "cod_dest", "nome_dest", "RA_Destino")




# carrego as informacoes de PIB
PIB.info <- readxl::read_excel("Database/Agregacao de Municipios.xlsx", 
                               col_types = c("skip", "skip", "skip", "skip",
                                             "numeric", "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric"),
                               
                               sheet = "TabelaCompleta",
                               range = cell_limits(c(1, 1), c(NA, 11)))


PIB.RA.2016 <- PIB.info %>% 
  dplyr::filter(Ignorado == 0) %>% 
  dplyr::group_by(RegiaoAgregacao) %>% 
  dplyr::summarise(PIB = sum(PIB),
                   Pop = sum(Pop2016),
                   PIB_PerCapta=sum(PIB_perCap))


# Contrucao de Matriz de pesos --------------------------------------------

# Constroe uma matrix de pessos zerada
qtd_of_regions <- nrow(Dicionario)

W.mat <- matrix(0, ncol = qtd_of_regions, nrow = qtd_of_regions)
colnames(W.mat) <- Dicionario$RegiaoOx
rownames(W.mat) <- Dicionario$RegiaoOx


source("./ScriptRegioes.R")

# Faz a construção efetiva da matrix de pesos
for (col in 1:ncol(W.mat)) {
  # busca qual o nome da coluna atual
  nomeColuna <- colnames(W.mat)[col]
  
  # busca o codigo da coluna (regiao de origem)
  Origem <- Dicionario$RegiaoAgregacao[which(Dicionario$RegiaoOx == nomeColuna)]
  
  for (row in 1:nrow(W.mat)) {
    # cat(sprintf("%d, %d\n", row, col))
    
    # busca qual o nome da coluna atual
    nomeLinha <- rownames(W.mat)[row]
    
    # busca o codigo da coluna (regiao de origem)
    Destino <- Dicionario$RegiaoAgregacao[which(Dicionario$RegiaoOx == nomeLinha)]
    
    if(Origem == Destino){
      W.mat[row, col] <- 0;
    } else {
      
      if(col %in% data_Conn){
        # Matrix do tipo classica
        W.mat[row, col] <- Connexoes.df %>% 
          filter(RA_Origem == Origem, RA_Destino == Destino) %>% 
          summarise(Total=n()) %>% pull(Total)
      } else if (col %in% data_Pib) {
        # Coloca na matrix de connexao o total do pib baseano na conexao entre as cidades.
        existe.con <- Connexoes.df %>% 
          filter(RA_Origem == Origem, RA_Destino == Destino) %>% 
          summarise(Total=n()) %>% pull(Total)
        
        if(existe.con > 0){
          W.mat[row, col] <- PIB.RA.2016 %>% 
            filter(RegiaoAgregacao == Destino) %>% pull(PIB)
        } else {
          W.mat[row, col] <- 0
        }
      } else if (col %in% data_PibPerCapta) {
        # Coloca na matrix de connexao o total do pib per capta baseano na conexao entre as cidades.
        
        existe.con <- Connexoes.df %>% 
          filter(RA_Origem == Origem, RA_Destino == Destino) %>% 
          summarise(Total=n()) %>% pull(Total)
        
        if(existe.con > 0){
          W.mat[row, col] <- PIB.RA.2016 %>% 
            filter(RegiaoAgregacao == Destino) %>% pull(PIB_PerCapta)
        } else {
          W.mat[row, col] <- 0
        }
        
      } else if (col %in% data_Populacao) {
        # Coloca na matrix de connexao o total da populacao na conexao entre as cidades.
        existe.con <- Connexoes.df %>% 
          filter(RA_Origem == Origem, RA_Destino == Destino) %>% 
          summarise(Total=n()) %>% pull(Total)
        
        if(existe.con > 0){
          W.mat[row, col] <- PIB.RA.2016 %>% 
            filter(RegiaoAgregacao == Destino) %>% pull(Pop)
        } else {
          W.mat[row, col] <- 0
        }
        
      } else if(col %in% data_Equal_Weight) {
        # Coloca na matrix de connexao o total da populacao na conexao entre as cidades.
        existe.con <- Connexoes.df %>% 
          filter(RA_Origem == Origem, RA_Destino == Destino) %>% 
          summarise(Total=n()) %>% pull(Total)
        
        if(existe.con > 0){
          W.mat[row, col] <- 1
        } else {
          W.mat[row, col] <- 0
        }
      } else {
        stop("Tipo de matrix nao informado corretamente")
      }
      
    }
  }
}


# --- Normaliza as colunas da matrix ----
for (col in 1:ncol(W.mat)) {
  somaColuna <- sum(W.mat[,col]);
  W.mat[,col] <- W.mat[,col] / somaColuna
}

# --- Salva a matrix em arquivo .mat ----

file.name.sufix <- "FINAL"
file.name <- sprintf("./Excel Export/data_%s.mat", file.name.sufix)


fileConn <- file(file.name)
writeLines( text = sprintf("%1$d %1$d // A %1$d by %1$d matrix (%2$s)", qtd_of_regions, file.name.sufix),
            con = fileConn)
close(fileConn)

write.table(x = W.mat, file = file.name,
            append = TRUE,
            col.names = FALSE,
            row.names = FALSE)



