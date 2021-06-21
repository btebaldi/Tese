
# Setup -------------------------------------------------------------------
# Clear all
rm(list = ls())

# Load library
library(stringr)



dir <- "COM IIS - Modelo 8"


filepath = file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir, "Read Me Config.txt")
file.out.path = file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir, "Criterios_de_Inforacao.csv")
file.exists(filepath)

main_dir <- dirname(filepath)


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


for(item in mGyL.file_list) {
  
  # Caminho do arquivo a ser lido
  fileName <- sprintf("%s.mat", item)
  # fileName.mask <- paste(main_dir, "%s.mat", sep = "")
  
  fileName <- file.path(main_dir, fileName)
  # fileName <- sprintf(fileName.mask, item)

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

  file.out <- file.path(main_dir, sprintf("%s.rds", item))
  saveRDS(M, file = file.out)
  
}




# Arquivo mGy_inv_X_mC ----------------------------------------------------


# Caminho do arquivo a ser lido
fileName <- sprintf("%s.mat", "mGy_inv_X_mC")

fileName <- file.path(main_dir, fileName)


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

file.out <- file.path(main_dir, sprintf("%s.rds", "mGy_inv_X_mC"))
saveRDS(M, file = file.out)

