rm(list = ls())

library(ggplot2)
<<<<<<< HEAD


# dataload ----------------------------------------------------------------

GVAR.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "SEM IIS - Modelo 0", "Model_erros.rds")
VECM.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "VECM", "Model_erros.rds")
PCA.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "PCA", "Model_erros.rds")

AR1.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "AR1", "Model_erros.rds")
AR13.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "AR13", "Model_erros.rds")

tbl.gvar <- readRDS(GVAR.file)
tbl.vecm <- readRDS(VECM.file)
tbl.pca <- readRDS(PCA.file)
tbl.ar1 <- readRDS(AR1.file)
tbl.ar13 <- readRDS(AR13.file)

serie_name <- "EmpLiq"
tbl.ar1$Tipo[tbl.ar1$Tipo=="EmpLiqui"] <- "EmpLiq"
tbl.ar13$Tipo[tbl.ar13$Tipo=="EmpLiqui"] <- "EmpLiq"



tbl.gvar <- tbl.gvar %>% filter(Tipo == serie_name)
tbl.vecm <- tbl.vecm %>% filter(Tipo == serie_name)
tbl.pca <- tbl.pca %>% filter(Tipo == serie_name)
tbl.ar1 <- tbl.ar1 %>% filter(Tipo == serie_name)
tbl.ar13 <- tbl.ar13 %>% filter(Tipo == serie_name)





tbl.gvar <- tbl.gvar %>% pivot_longer(cols = starts_with("Error"))
tbl.vecm <- tbl.vecm %>% pivot_longer(cols = starts_with("Error"))
tbl.pca <-  tbl.pca %>% pivot_longer(cols = starts_with("Error"))
tbl.ar1 <- tbl.ar1 %>% pivot_longer(cols = starts_with("Error"))
tbl.ar13 <- tbl.ar13 %>% pivot_longer(cols = starts_with("Error"))

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


tbl %>% 









=======
>>>>>>> 05ebf2e0457d47d1eff0aa8c5f9d857c117eeaa5
