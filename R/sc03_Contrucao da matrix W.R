# ---- Header ----
#
# Script para construcao da matrix de pesos do GVAR utilizado pelo OxMetrics
#
# Autor: Bruno Tebaldi de Queiroz Barbosa
#
# Data de criacao: 2020-10-13


# Clear all
rm(list=ls())

# bibliotecas utilizadas
library(readxl)
library(dplyr)


# carrega o dicionario com relacao do codigo da regiao de agregacao
Dicionario <- readxl::read_excel("./Database/Relacao_Agregacao_Ox.xlsx")

# carrego as informacoes dos municipios e suas regioes de agregacao.
Muni_RA.info <- readxl::read_excel("./Database/Agregacao de Municipios.xlsx", 
                                      sheet = "TabelaCompleta")

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



# ---- Contrucao de Matriz de pesos ----

# Constroe uma matrix de pessos zerada
qtd_of_regions <- nrow(Dicionario)

W.mat <- matrix(0, ncol = qtd_of_regions, nrow = qtd_of_regions)
colnames(W.mat) <- Dicionario$RegiaoOx
rownames(W.mat) <- Dicionario$RegiaoOx


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
      W.mat[row, col] <- Connexoes.df %>% 
        filter(RA_Origem == Origem, RA_Destino == Destino) %>% 
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
writeLines( text = sprintf("%1$d %1$d // A %1$d by %1$d matrix (%2$s)", qtd_of_regions, "connections"),
            con = fileConn)
close(fileConn)

write.table(x = W.mat, file = "./Excel Export/data.mat",
            append = TRUE,
            col.names = FALSE,
            row.names = FALSE)



