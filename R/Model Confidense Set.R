# Model Confidense set Script


# Setup -------------------------------------------------------------------

rm(list =ls())

library(dplyr)
library(tidyr)


# User defined variables --------------------------------------------------

p_value_crit <- 0.1
LossFunctionflag <- "MAE"
# LossFunctionflag <- "MSE"

if(LossFunctionflag == "MAE"){
  LossFunction <- function(x){
    ret <- abs(x)
    return(ret)
  }
} else {
  LossFunction <- function(x){
    ret <- (x)^2
    return(ret)  
  }
}
# Analise de MCS de emprego liquido ---------------------------------------
GVAR.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "SEM IIS - Modelo 0", "Model_erros.rds")
VECM.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "VECM", "Model_erros.rds")
PCA.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "PCA", "Model_erros.rds")

AR1.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "AR1", "Model_erros.rds")
AR13.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "AR13", "Model_erros.rds")

tbl.gvar <- readRDS(GVAR.file)
tbl.vecm <- readRDS(VECM.file)
tbl.pca <- readRDS(PCA.file)
# tbl.ar1 <- readRDS(AR1.file)
# tbl.ar13 <- readRDS(AR13.file)

serie_name <- "EmpLiq"
# tbl.ar1$Tipo[tbl.ar1$Tipo=="EmpLiqui"] <- "EmpLiq"
# tbl.ar13$Tipo[tbl.ar13$Tipo=="EmpLiqui"] <- "EmpLiq"



tbl.gvar <- tbl.gvar %>% filter(Tipo == serie_name)
tbl.vecm <- tbl.vecm %>% filter(Tipo == serie_name)
tbl.pca <- tbl.pca %>% filter(Tipo == serie_name)
# tbl.ar1 <- tbl.ar1 %>% filter(Tipo == serie_name)
# tbl.ar13 <- tbl.ar13 %>% filter(Tipo == serie_name)

tbl.gvar <- tbl.gvar %>% pivot_longer(cols = starts_with("Error"))
tbl.vecm <- tbl.vecm %>% pivot_longer(cols = starts_with("Error"))
tbl.pca <-  tbl.pca %>% pivot_longer(cols = starts_with("Error"))
# tbl.ar1 <- tbl.ar1 %>% pivot_longer(cols = starts_with("Error"))
# tbl.ar13 <- tbl.ar13 %>% pivot_longer(cols = starts_with("Error"))



tbl <- bind_rows(tbl.gvar, tbl.vecm, tbl.pca)

IIS_model_list <- c(1,2,3,6,7,8,9,10,11,12,13,14,15,16,17,19)
for (i in IIS_model_list) {
  
  dir.COMIIS <- sprintf("COM IIS - Modelo %d", i)
  GVAR.IIS.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir.COMIIS, "Model_erros.rds")
  
  if(file.exists(GVAR.IIS.file)){
    cat(sprintf("\nAdicionando Modelo IIS %d", i))
    tbl.IIS <- readRDS(GVAR.IIS.file)
    tbl.IIS <- tbl.IIS %>% filter(Tipo == serie_name)
    tbl.IIS$source <- sprintf("IIS_%d", i)
    tbl.IIS <- tbl.IIS %>% pivot_longer(cols = starts_with("Error"))
    
    tbl <- bind_rows(tbl, tbl.IIS)
  } else {
    cat(sprintf("\nModelo IIS %d não encontrado\n", i))
  }
}

rm(list = setdiff(ls(), c("tbl", "p_value_crit", "LossFunctionflag", "LossFunction")))


tbl <- tbl %>% select(-variavel) %>% pivot_wider(names_from = source, values_from = value)


model_list = factor(setdiff(colnames(tbl), c("Regiao", "Tipo", "name")))

# Aplicando Loss Function
tbl.loss <- tbl %>% mutate_at(.vars = as.character(model_list), .funs = LossFunction)

# Tabela com os erros totais
MAE <- tbl.loss %>% pivot_longer(cols = as.character(model_list),
                                 names_to = "source",
                                 values_to = "value") %>% 
  group_by(source) %>% 
  summarise(MAE=mean(value), .groups = "drop")

MAE$keep <- TRUE

if(LossFunctionflag == "MSE"){
  MAE$MAE <- MAE$MAE^0.5
}

numberModels <- length(model_list)
tbl.darude <- tibble(Id=1:(numberModels^2),
                     model_i=sort(rep(as.character(model_list), numberModels)),
                     model_j=rep(as.character(model_list), numberModels),
                     # t=as.character(NA),
                     d_ij=as.numeric(NA),
                     var_d_ij=as.numeric(NA),
                     df=as.numeric(NA))

# Calculando dij
for (row in seq_len(nrow(tbl.darude))) {
  current_i <- tbl.darude$model_i[row]
  current_j <- tbl.darude$model_j[row]
  
  dij <- mean(tbl.loss[[current_i]] - tbl.loss[[current_j]])
  vardij <- var(tbl.loss[[current_i]] - tbl.loss[[current_j]])
  df <- length(tbl.loss[[current_i]]) -1
  
  tbl.darude$d_ij[row] <- dij
  tbl.darude$var_d_ij[row] <- vardij
  tbl.darude$df[row] <- df
}


# Calculando dij
tbl.MCS <- tibble(Id=1:numberModels,
                  model_i=as.character(model_list),
                  d_i=as.numeric(NA), 
                  var_di=as.numeric(NA),
                  df=as.numeric(NA))

MCS_ALGO <- TRUE
while(MCS_ALGO){
  
  cat(sprintf("\nCalculando MCS"))
  
  for (row in seq_len(nrow(tbl.MCS))) {
    current_i <- tbl.MCS$model_i[row]
    
    tbl2 <- tbl.darude %>% filter(model_i == current_i)
    tbl2 <- tbl2 %>% filter(model_j != current_i)
    tbl2 <- tbl2 %>% filter(model_j %in% tbl.MCS$model_i)
    
    tbl.MCS$d_i[row] <- mean(tbl2$d_ij)
    tbl.MCS$var_di[row] <- var(tbl2$d_ij)
    tbl.MCS$df[row] <- length(tbl2$d_ij)-1
  }
  
  if(nrow(tbl.MCS) > 2){
    tbl.MCS$t <- tbl.MCS$d_i/(tbl.MCS$var_di)^0.5
    tbl.MCS$pvalue <- 2*pt(abs(tbl.MCS$t), tbl.MCS$df, lower.tail = FALSE)
  } else {
    tbl.MCS$t <- tbl2$d_ij/(tbl2$var_d_ij)^0.5
    tbl.MCS$pvalue <- 2*pt(abs(tbl.MCS$t), tbl2$df, lower.tail = FALSE)
  }
  
  
  Update_MCS <- FALSE
  for (row in seq_len(nrow(tbl.MCS))) {
    keep <- tbl.MCS$pvalue[row] > p_value_crit
    if(!keep){
      # Selection rule
      Update_MCS <- TRUE
      break()
    }
  }
  
  if(Update_MCS){
    tbl3 <- MAE[MAE$keep, ]
    idx <- which.max(tbl3$MAE)
    
    Model_to_exclude <- tbl3[[idx, "source"]]
    
    cat(sprintf("\nMarcando %s como excluido no MAE", MAE$source[MAE$source == Model_to_exclude]))
    MAE[MAE$source == Model_to_exclude,"keep"] <- FALSE
    
    cat(sprintf("\nRetirando %s do MCS\n", MAE$source[MAE$source == Model_to_exclude]))
    tbl.MCS <- tbl.MCS %>% filter(model_i %in% MAE$source[MAE$keep])
    MAE[MAE$keep,]
  } else {
    cat(sprintf("\nMCS finalizado\n"))
    MCS_ALGO <- FALSE
  }
}

cat("MCS - EMPREGO LIQUIDO - size:", nrow(tbl.MCS))

print(tbl.MCS)


# Analise de MCS de Adminitidos ---------------------------------------
GVAR.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "SEM IIS - Modelo 0", "Model_erros.rds")
VECM.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "VECM", "Model_erros.rds")
PCA.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "PCA", "Model_erros.rds")

tbl.gvar <- readRDS(GVAR.file)
tbl.vecm <- readRDS(VECM.file)
tbl.pca <- readRDS(PCA.file)

tbl.gvar <- readRDS(GVAR.file)
tbl.vecm <- readRDS(VECM.file)
tbl.pca <- readRDS(PCA.file)

unique(tbl.vecm$Tipo)

serie_name <- "Admitidos"

tbl.gvar <- tbl.gvar %>% filter(Tipo == serie_name)
tbl.vecm <- tbl.vecm %>% filter(Tipo == serie_name)
tbl.pca <- tbl.pca %>% filter(Tipo == serie_name)

tbl.gvar <- tbl.gvar %>% pivot_longer(cols = starts_with("Error"))
tbl.vecm <- tbl.vecm %>% pivot_longer(cols = starts_with("Error"))
tbl.pca <-  tbl.pca %>% pivot_longer(cols = starts_with("Error"))


tbl <- bind_rows(tbl.gvar, tbl.vecm, tbl.pca)

IIS_model_list <- c(1,2,3,6,7,8,9,10,11,12,13,14,15,16,17,19)
for (i in IIS_model_list) {
  
  dir.COMIIS <- sprintf("COM IIS - Modelo %d", i)
  GVAR.IIS.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir.COMIIS, "Model_erros.rds")
  
  if(file.exists(GVAR.IIS.file)){
    cat(sprintf("\nAdicionando Modelo IIS %d", i))
    tbl.IIS <- readRDS(GVAR.IIS.file)
    tbl.IIS <- tbl.IIS %>% filter(Tipo == serie_name)
    tbl.IIS$source <- sprintf("IIS_%d", i)
    tbl.IIS <- tbl.IIS %>% pivot_longer(cols = starts_with("Error"))
    
    tbl <- bind_rows(tbl, tbl.IIS)
  } else {
    cat(sprintf("\nModelo IIS %d não encontrado\n", i))
  }
}

rm(list = setdiff(ls(), c("tbl", "p_value_crit", "LossFunctionflag", "LossFunction")))


tbl <- tbl %>% select(-variavel) %>% pivot_wider(names_from = source, values_from = value)


model_list = factor(setdiff(colnames(tbl), c("Regiao", "Tipo", "name")))

# Aplicando Loss Function
tbl.loss <- tbl %>% mutate_at(.vars = as.character(model_list), .funs = LossFunction)

# Tabela com os erros totais
MAE <- tbl.loss %>% pivot_longer(cols = as.character(model_list),
                                 names_to = "source",
                                 values_to = "value") %>% 
  group_by(source) %>% 
  summarise(MAE=mean(value), .groups = "drop")

MAE$keep <- TRUE


numberModels <- length(model_list)
tbl.darude <- tibble(Id=1:(numberModels^2),
                     model_i=sort(rep(as.character(model_list), numberModels)),
                     model_j=rep(as.character(model_list), numberModels),
                     # t=as.character(NA),
                     d_ij=as.numeric(NA),
                     var_d_ij=as.numeric(NA),
                     df=as.numeric(NA))

# Calculando dij
for (row in seq_len(nrow(tbl.darude))) {
  current_i <- tbl.darude$model_i[row]
  current_j <- tbl.darude$model_j[row]
  
  dij <- mean(tbl.loss[[current_i]] - tbl.loss[[current_j]])
  vardij <- var(tbl.loss[[current_i]] - tbl.loss[[current_j]])
  df <- length(tbl.loss[[current_i]]) -1
  
  tbl.darude$d_ij[row] <- dij
  tbl.darude$var_d_ij[row] <- vardij
  tbl.darude$df[row] <- df
}


# Calculando dij
tbl.MCS <- tibble(Id=1:numberModels,
                  model_i=as.character(model_list),
                  d_i=as.numeric(NA), 
                  var_di=as.numeric(NA),
                  df=as.numeric(NA))

MCS_ALGO <- TRUE
while(MCS_ALGO){
  
  cat(sprintf("\nCalculando MCS"))
  
  for (row in seq_len(nrow(tbl.MCS))) {
    current_i <- tbl.MCS$model_i[row]
    
    tbl2 <- tbl.darude %>% filter(model_i == current_i)
    tbl2 <- tbl2 %>% filter(model_j != current_i)
    tbl2 <- tbl2 %>% filter(model_j %in% tbl.MCS$model_i)
    
    tbl.MCS$d_i[row] <- mean(tbl2$d_ij)
    tbl.MCS$var_di[row] <- var(tbl2$d_ij)
    tbl.MCS$df[row] <- length(tbl2$d_ij)-1
  }
  
  if(nrow(tbl.MCS) > 2){
    tbl.MCS$t <- tbl.MCS$d_i/(tbl.MCS$var_di)^0.5
    tbl.MCS$pvalue <- 2*pt(abs(tbl.MCS$t), tbl.MCS$df, lower.tail = FALSE)
  } else {
    tbl.MCS$t <- tbl2$d_ij/(tbl2$var_d_ij)^0.5
    tbl.MCS$pvalue <- 2*pt(abs(tbl.MCS$t), tbl2$df, lower.tail = FALSE)
  }
  
  
  Update_MCS <- FALSE
  for (row in seq_len(nrow(tbl.MCS))) {
    keep <- tbl.MCS$pvalue[row] > p_value_crit
    if(!keep){
      # Selection rule
      Update_MCS <- TRUE
      break()
    }
  }
  
  if(Update_MCS){
    tbl3 <- MAE[MAE$keep, ]
    idx <- which.max(tbl3$MAE)
    
    Model_to_exclude <- tbl3[[idx, "source"]]
    
    cat(sprintf("\nMarcando %s como excluido no MAE", MAE$source[MAE$source == Model_to_exclude]))
    MAE[MAE$source == Model_to_exclude,"keep"] <- FALSE
    
    cat(sprintf("\nRetirando %s do MCS\n", MAE$source[MAE$source == Model_to_exclude]))
    tbl.MCS <- tbl.MCS %>% filter(model_i %in% MAE$source[MAE$keep])
    MAE[MAE$keep,]
  } else {
    cat(sprintf("\nMCS finalizado\n"))
    MCS_ALGO <- FALSE
  }
}

cat("MCS - ADMITIDOS - size:", nrow(tbl.MCS))

print(tbl.MCS)

# Analise de MCS de Demitidos ---------------------------------------
GVAR.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "SEM IIS - Modelo 0", "Model_erros.rds")
VECM.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "VECM", "Model_erros.rds")
PCA.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "PCA", "Model_erros.rds")

tbl.gvar <- readRDS(GVAR.file)
tbl.vecm <- readRDS(VECM.file)
tbl.pca <- readRDS(PCA.file)

tbl.gvar <- readRDS(GVAR.file)
tbl.vecm <- readRDS(VECM.file)
tbl.pca <- readRDS(PCA.file)

unique(tbl.vecm$Tipo)

serie_name <- "Desligados"

tbl.gvar <- tbl.gvar %>% filter(Tipo == serie_name)
tbl.vecm <- tbl.vecm %>% filter(Tipo == serie_name)
tbl.pca <- tbl.pca %>% filter(Tipo == serie_name)

tbl.gvar <- tbl.gvar %>% pivot_longer(cols = starts_with("Error"))
tbl.vecm <- tbl.vecm %>% pivot_longer(cols = starts_with("Error"))
tbl.pca <-  tbl.pca %>% pivot_longer(cols = starts_with("Error"))


tbl <- bind_rows(tbl.gvar, tbl.vecm, tbl.pca)

IIS_model_list <- c(1,2,3,6,7,8,9,10,11,12,13,14,15,16,17,19)
for (i in IIS_model_list) {
  
  dir.COMIIS <- sprintf("COM IIS - Modelo %d", i)
  GVAR.IIS.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir.COMIIS, "Model_erros.rds")
  
  if(file.exists(GVAR.IIS.file)){
    cat(sprintf("\nAdicionando Modelo IIS %d", i))
    tbl.IIS <- readRDS(GVAR.IIS.file)
    tbl.IIS <- tbl.IIS %>% filter(Tipo == serie_name)
    tbl.IIS$source <- sprintf("IIS_%d", i)
    tbl.IIS <- tbl.IIS %>% pivot_longer(cols = starts_with("Error"))
    
    tbl <- bind_rows(tbl, tbl.IIS)
  } else {
    cat(sprintf("\nModelo IIS %d não encontrado\n", i))
  }
}

rm(list = setdiff(ls(), c("tbl", "p_value_crit", "LossFunctionflag", "LossFunction")))


tbl <- tbl %>% select(-variavel) %>% pivot_wider(names_from = source, values_from = value)


model_list = factor(setdiff(colnames(tbl), c("Regiao", "Tipo", "name")))

# Aplicando Loss Function
tbl.loss <- tbl %>% mutate_at(.vars = as.character(model_list), .funs = LossFunction)

# Tabela com os erros totais
MAE <- tbl.loss %>% pivot_longer(cols = as.character(model_list),
                                 names_to = "source",
                                 values_to = "value") %>% 
  group_by(source) %>% 
  summarise(MAE=mean(value), .groups = "drop")

MAE$keep <- TRUE


numberModels <- length(model_list)
tbl.darude <- tibble(Id=1:(numberModels^2),
                     model_i=sort(rep(as.character(model_list), numberModels)),
                     model_j=rep(as.character(model_list), numberModels),
                     # t=as.character(NA),
                     d_ij=as.numeric(NA),
                     var_d_ij=as.numeric(NA),
                     df=as.numeric(NA))

# Calculando dij
for (row in seq_len(nrow(tbl.darude))) {
  current_i <- tbl.darude$model_i[row]
  current_j <- tbl.darude$model_j[row]
  
  dij <- mean(tbl.loss[[current_i]] - tbl.loss[[current_j]])
  vardij <- var(tbl.loss[[current_i]] - tbl.loss[[current_j]])
  df <- length(tbl.loss[[current_i]]) -1
  
  tbl.darude$d_ij[row] <- dij
  tbl.darude$var_d_ij[row] <- vardij
  tbl.darude$df[row] <- df
}


# Calculando dij
tbl.MCS <- tibble(Id=1:numberModels,
                  model_i=as.character(model_list),
                  d_i=as.numeric(NA), 
                  var_di=as.numeric(NA),
                  df=as.numeric(NA))

MCS_ALGO <- TRUE
while(MCS_ALGO){
  
  cat(sprintf("\nCalculando MCS"))
  
  for (row in seq_len(nrow(tbl.MCS))) {
    current_i <- tbl.MCS$model_i[row]
    
    tbl2 <- tbl.darude %>% filter(model_i == current_i)
    tbl2 <- tbl2 %>% filter(model_j != current_i)
    tbl2 <- tbl2 %>% filter(model_j %in% tbl.MCS$model_i)
    
    tbl.MCS$d_i[row] <- mean(tbl2$d_ij)
    tbl.MCS$var_di[row] <- var(tbl2$d_ij)
    tbl.MCS$df[row] <- length(tbl2$d_ij)-1
  }
  
  if(nrow(tbl.MCS) > 2){
    tbl.MCS$t <- tbl.MCS$d_i/(tbl.MCS$var_di)^0.5
    tbl.MCS$pvalue <- 2*pt(abs(tbl.MCS$t), tbl.MCS$df, lower.tail = FALSE)
  } else {
    tbl.MCS$t <- tbl2$d_ij/(tbl2$var_d_ij)^0.5
    tbl.MCS$pvalue <- 2*pt(abs(tbl.MCS$t), tbl2$df, lower.tail = FALSE)
  }
  
  
  Update_MCS <- FALSE
  for (row in seq_len(nrow(tbl.MCS))) {
    keep <- tbl.MCS$pvalue[row] > p_value_crit
    if(!keep){
      # Selection rule
      Update_MCS <- TRUE
      break()
    }
  }
  
  if(Update_MCS){
    tbl3 <- MAE[MAE$keep, ]
    idx <- which.max(tbl3$MAE)
    
    Model_to_exclude <- tbl3[[idx, "source"]]
    
    cat(sprintf("\nMarcando %s como excluido no MAE", MAE$source[MAE$source == Model_to_exclude]))
    MAE[MAE$source == Model_to_exclude,"keep"] <- FALSE
    
    cat(sprintf("\nRetirando %s do MCS\n", MAE$source[MAE$source == Model_to_exclude]))
    tbl.MCS <- tbl.MCS %>% filter(model_i %in% MAE$source[MAE$keep])
    MAE[MAE$keep,]
  } else {
    cat(sprintf("\nMCS finalizado\n"))
    MCS_ALGO <- FALSE
  }
}

cat("MCS - DEMITIDOS - size:", nrow(tbl.MCS))

print(tbl.MCS)

