
# Setup -------------------------------------------------------------------
# Clear all
rm(list = ls())

# Load library
library(stringr)


# item <- "D_R404_U"
# item <- "D_R404_Lambda"
item <- "D_R407_D"

# Caminho do arquivo a ser lido
fileName.mask <- "C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/RawMatrix/%s.mat"

item <- "A401_0"
fileName.mask <- "C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/A_Matrix/%s.mat"

item <- "W_w"
fileName.mask <- "C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/W_Matrix/%s.mat"

item <- "CoInt_MacroVar"
fileName.mask <- "C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Cointegration/%s.mat"


# item <- "A404_13"
# fileName.mask <- "C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/A_Matrix/%s.mat"

# item <- "D_Stacked"
# fileName.mask <- "C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/G_Matrix/%s.mat"
# 
# item <- "mGy"
# fileName.mask <- "C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/%s.mat"


fileName <- sprintf(fileName.mask, item)

# Abre o aqruivo para leitura
con <- file(fileName, open="r")

# Faz a leitura das linhas
line <- readLines(con) 

# Fecha a conexao com o arquivo
close(con)

dimensions <- str_split(line[1], " ", simplify=TRUE) 

# Matriz de pesos
M <- matrix(NA, nrow = as.numeric(dimensions[1]), ncol = as.numeric(dimensions[2]))

col = 1
row <- 1
for (i in 2:length(line)){
  
  line.splited <- str_split(line[i], "\\s+", n = Inf, simplify = TRUE)
  line.splited <- as.numeric(line.splited[-1])
  
  for(j in 1:length(line.splited)){
    M[row, col] = line.splited[j]
    
    col = col + 1
  }
  
  # se chegou ao final, refaz as colunas
  if(col==as.numeric(dimensions[2])+1){
    row= row+1
    col=1
  }
  
}
which(sort(rep(1:552,2)) == 404)

dim(M)
M

# colnames(M) <- paste(rep(c("P", "S", "I"),14), sort(rep(0:13, 3)))

# M[(807:808)+3, 1:3]

