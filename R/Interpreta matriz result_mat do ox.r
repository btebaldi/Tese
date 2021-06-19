
# Setup -------------------------------------------------------------------
# Clear all
rm(list = ls())

# Load library
library(stringr)

mGyL.file_list <-   c(
  # "mGy_inv_X_mC"
  "mGy_inv_X_mGyL1",
  "mGy_inv_X_mGyL2",
  "mGy_inv_X_mGyL3",
  "mGy_inv_X_mGyL4",
  "mGy_inv_X_mGyL5",
  "mGy_inv_X_mGyL6",
  "mGy_inv_X_mGyL7",
  "mGy_inv_X_mGyL8",
  "mGy_inv_X_mGyL9",
  "mGy_inv_X_mGyL10",
  "mGy_inv_X_mGyL11",
  "mGy_inv_X_mGyL12",
  "mGy_inv_X_mGyL13",
  "mGy_inv_X_mL"
)

main_dir <- "C:/Users/Teo/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/COM IIS 2016 0-003/"

for(item in mGyL.file_list) {
  
  # Caminho do arquivo a ser lido
  fileName.mask <- paste(main_dir, "%s.mat", sep = "")
  
  fileName <- sprintf(fileName.mask, item)
  
  # Abre o aqruivo para leitura
  con <- file(fileName, open="r")
  
  # Faz a leitura das linhas
  line <- readLines(con) 

  # Fecha a conexao com o arquivo
  close(con)
  
  # Matriz de pesos
  M <- matrix(NA, nrow = 1107, ncol = 1107)
  
  col = 1
  row <- 1
  for (i in 2:length(line)){
    
    line.splited <- str_split(line[i], "\\s+", n = Inf, simplify = FALSE)
    line.splited <- as.numeric(unlist(line.splited))
    
    for(j in 2:length(line.splited)){
      M[row, col] = line.splited[j]
      
      col = col + 1
    }
    
    # se chegou ao final, refaz as colunas
    if(col==1108){
      row= row+1
      col=1
    }
    
  }

  saveRDS(M, file = sprintf(paste(main_dir, "%s.rds", sep = ""), item))
  
}




# Arquivo mGy_inv_X_mC ----------------------------------------------------


# Caminho do arquivo a ser lido
fileName.mask <- paste(main_dir, "%s.mat", sep = "")
  

fileName <- sprintf(fileName.mask, "mGy_inv_X_mC")

# Abre o aqruivo para leitura
con <- file(fileName, open="r")

# Faz a leitura das linhas
line <- readLines(con) 

# Fecha a conexao com o arquivo
close(con)

# Matriz de pesos
M <- matrix(NA, nrow = 1107, ncol = 300)

col = 1
row <- 1
for (i in 2:length(line)){
  
  line.splited <- str_split(line[i], "\\s+", n = Inf, simplify = FALSE)
  line.splited <- as.numeric(unlist(line.splited))
  
  for(j in 2:length(line.splited)){
    M[row, col] = line.splited[j]
    
    col = col + 1
  }
  
  # se chegou ao final, refaz as colunas
  if(col==301){
    row= row+1
    col=1
  }
  
}

saveRDS(M, file = sprintf(paste(main_dir, "%s.rds", sep = "") , "mGy_inv_X_mC"))


