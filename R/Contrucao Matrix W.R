# Clear all
rm(list=ls())

# bibliotecas utilizadas
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)


# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

#  ToDo: Fazer uma check de que a lista de municiopios eh sempre a mesma, acho
#  que posso fazer uma lista "Basica" a qual será utilizada como gabarito para
#  qualquer analise.

# Carrego a lista dos municipios
load(file = "./Database/codigos_municipios.RData")
colnames(full) = "Muni"

# carrego as informacoes de mesoregioes.
meso.info <- readxl::read_excel("Database/Cadastro Municipios.xlsx", sheet = "Mesoregioes")

# carrego as informacoes de PIB
PIB.info <- readxl::read_excel("Database/PIB_Municipios_2010a2016.xlsx", 
                               col_types = c("numeric", "numeric", "text", 
                                             "numeric", "text", "text", "numeric", 
                                             "text", "skip", "numeric", "text", 
                                             "skip", "skip", "skip", "skip", "skip", 
                                             "skip", "skip", "skip", "skip", "skip", 
                                             "skip", "skip", "skip", "skip", "skip", 
                                             "skip", "skip", "skip", "skip", "skip", 
                                             "skip", "skip", "skip", "skip", "skip", 
                                             "skip", "skip", "skip", "numeric",
                                             "numeric", "numeric", "skip", "skip",
                                             "skip"))
colnames(PIB.info) <- c("Ano",
                        "Cod_GrandeRegiao",
                        "Nome_GrandeRegiao",
                        "Cod_UniFed",
                        "Sigla_UniFed", 
                        "Nome_UniFed",
                        "Cod_Municipio",
                        "Nome_Municipio",
                        "Cod_Meso",
                        "Nome_Meso",
                        "PIB_corrente",
                        "Populacao",
                        "PIB_perCapita")


# Seleciona o PIN e POPULACAO para o ano de 2016
PIB.info %>% 
  dplyr::filter(Ano == 2016) %>% 
  dplyr::group_by(Cod_Meso) %>% 
  dplyr::select(Cod_Meso, PIB_corrente, Populacao) %>% 
  dplyr::summarise(PIB = sum(PIB_corrente), Pop = sum(Populacao)) %>% 
  apply(MARGIN = 2, FUN=sum) %>% format(big.mark=" ")

PIB.info.2016 <- PIB.info %>% 
  dplyr::filter(Ano == 2016) %>% 
  dplyr::group_by(Cod_Meso) %>% 
  dplyr::select(Cod_Meso, PIB_corrente, Populacao) %>% 
  dplyr::summarise(PIB = sum(PIB_corrente), Pop = sum(Populacao))


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

# carrega o dicionario com relacao do codigo da mesoregioao
Dicionario <- read_excel("./Database/GVAR Data/Dicionario.xlsx", 
                         # col_types = c("text", "text", "text", "text")
)

# Arruma o nome das colunas 
colnames(Dicionario) <- c("Name", "Short_name", "Code", "Ox")


# Constroe uma matrix de pessos zerada
W.mat <- matrix(0, ncol = 137, nrow = 137)
colnames(W.mat) <- Dicionario$Short_name
rownames(W.mat) <- Dicionario$Short_name


# Faz a construção efetiva da matrix de pesos
for (col in 1:ncol(W.mat)) {
  # busca qual o nome da coluna atual
  nomeColuna <- colnames(W.mat)[col]
  
  # busca o codigo da coluna (regiao de origem)
  Origem <- Dicionario$Code[which(Dicionario$Short_name == nomeColuna)]
  
  for (row in 1:nrow(W.mat)) {
    # cat(sprintf("%d, %d\n", row, col))
    
    # busca qual o nome da coluna atual
    nomeLinha <- rownames(W.mat)[row]
    
    # busca o codigo da coluna (regiao de origem)
    Destino <- Dicionario$Code[which(Dicionario$Short_name == nomeLinha)]
    
    if(Origem == Destino){
      W.mat[row, col] <- 0;
    } else {
      
      Qtd_connex <- Connexoes.df %>% 
        filter(Meso_Origem == Origem, Meso_Destino == Destino) %>% 
        summarise(Total=n()) %>% pull(Total)
      
      # Coloca na matrix de connexao o total de conexao entre as cidades.
      # W.mat[row, col] <- Qtd_connex
      
      
      # Coloca na matrix de connexao o total do pib baseano na conexao entre as cidades.
      if(Qtd_connex > 0){
        W.mat[row, col] <- PIB.info.2016 %>% 
          filter(Cod_Meso == Destino) %>% pull(Pop)  
      } else {
        W.mat[row, col] <- 0
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
fileConn <- file("./Excel Export/Teste.mat")
writeLines(c("137 137 // A 137 by 137 matrix"), fileConn)
close(fileConn)

write.table(x = W.mat, file = "./Excel Export/Teste.mat",
            append = TRUE,
            col.names = FALSE,
            row.names = FALSE)




