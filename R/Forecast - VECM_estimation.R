
# Setup -------------------------------------------------------------------
rm(list = ls())

library(readr)
library(readxl)
library(dplyr)
library(urca)
library(vars)
library(tsDyn)


# Dataload ----------------------------------------------------------------

# busca dados de estimacao
region.db <- read_csv("~/GitHub/Tese/Ox Metrics GVAR/Database/DatabaseDesAdm_RA_v1.csv")
head(region.db)

# busca dados Macro (nao utilizados)
# macro.db <- read_excel("~/GitHub/Tese/Ox Metrics GVAR/Database/MacroVariables_forR_20210604.xlsx", 
#                        na = "#N/A")
# head(macro.db)


# data para forecast em diferenca
DX.df <- read_excel("Excel Export/DatabaseDesAdm_RA_vForecast_v3.xlsx", 
                    range = "A1:AK1108")

DX <- DX.df[,-1] %>% data.matrix()

# data para forecast em nivel
X.df <- read_excel("Excel Export/DatabaseDesAdm_RA_vForecast_v3.xlsx", 
                   range = "A1115:AK2222")

X <- X.df[,-1] %>% data.matrix()

# Vetor de datas
datelist <- seq(from = as.Date("2017-01-01"),
                to = as.Date("2019-12-01"),
                by="month")

# Matriz com resultados
results.tbl <-  tibble(variavel=X.df$Variavel)


j <- 1
cols.sufix <- c("_Admitidos", "_Desligados")

# para cada regiao, estimar um VECM e fazer o forecast
for (j in 1:552) {
  # Busca dados especificos da regiao dados para estimacao do VECM
  tbl <- region.db[, paste("R", j, cols.sufix, sep="")]
  
  # Estima modelo VECM
  mdl <- tsDyn::VECM(tbl, 13, r=1, include = "const")
  
  # summary(mdl)
  
  # Determina coeficientes de longo prazo
  mLagLR <- mdl$coefficients[,"ECT"] %*% t(mdl$model.specific$beta)
  
  # determina coeficientes de Lag
  mLag1 <- mdl$coefficients[, c(sprintf("R%d_Admitidos -1", j), sprintf("R%d_Desligados -1", j))]
  mLag2 <- mdl$coefficients[, c(sprintf("R%d_Admitidos -2", j), sprintf("R%d_Desligados -2", j))]
  mLag3 <- mdl$coefficients[, c(sprintf("R%d_Admitidos -3", j), sprintf("R%d_Desligados -3", j))]
  mLag4 <- mdl$coefficients[, c(sprintf("R%d_Admitidos -4", j), sprintf("R%d_Desligados -4", j))]
  mLag5 <- mdl$coefficients[, c(sprintf("R%d_Admitidos -5", j), sprintf("R%d_Desligados -5", j))]
  mLag6 <- mdl$coefficients[, c(sprintf("R%d_Admitidos -6", j), sprintf("R%d_Desligados -6", j))]
  mLag7 <- mdl$coefficients[, c(sprintf("R%d_Admitidos -7", j), sprintf("R%d_Desligados -7", j))]
  mLag8 <- mdl$coefficients[, c(sprintf("R%d_Admitidos -8", j), sprintf("R%d_Desligados -8", j))]
  mLag9 <- mdl$coefficients[, c(sprintf("R%d_Admitidos -9", j), sprintf("R%d_Desligados -9", j))]
  mLag10 <- mdl$coefficients[, c(sprintf("R%d_Admitidos -10", j), sprintf("R%d_Desligados -10", j))]
  mLag11 <- mdl$coefficients[, c(sprintf("R%d_Admitidos -11", j), sprintf("R%d_Desligados -11", j))]
  mLag12 <- mdl$coefficients[, c(sprintf("R%d_Admitidos -12", j), sprintf("R%d_Desligados -12", j))]
  mLag13 <- mdl$coefficients[, c(sprintf("R%d_Admitidos -13", j), sprintf("R%d_Desligados -13", j))]
  
  # Determina coeficientes de constante 
  mLagDm <- mdl$coefficients[,"Intercept"]
  
  # Determina qual a lina a regiao esta na matriz de forecast
  idx <-  which(DX.df$Variavel %in% paste("R", j, c("_Admitidos", "_Desligados"), sep = ""))
  
  # i=0
  # para cada lag calcula o forecast
  for(i in 0:11){
    
    n <- 25+i
    cat(sprintf("%3d - %s\n",j,datelist[n]))
    
    # resultado atual 
    results.tbl[idx, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))] <- DX[idx,sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))]
    
    
    # forecast curto prazo
    Forecast.SR <- mLag1  %*% DX[idx, sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))] + 
      mLag2  %*% DX[idx,sprintf("D%dM%02d", year(datelist[n-2]), month(datelist[n-2]))] + 
      mLag3  %*% DX[idx,sprintf("D%dM%02d", year(datelist[n-3]), month(datelist[n-3]))] + 
      mLag4  %*% DX[idx,sprintf("D%dM%02d", year(datelist[n-4]), month(datelist[n-4]))] + 
      mLag5  %*% DX[idx,sprintf("D%dM%02d", year(datelist[n-5]), month(datelist[n-5]))] + 
      mLag6  %*% DX[idx,sprintf("D%dM%02d", year(datelist[n-6]), month(datelist[n-6]))] + 
      mLag7  %*% DX[idx,sprintf("D%dM%02d", year(datelist[n-7]), month(datelist[n-7]))] + 
      mLag8  %*% DX[idx,sprintf("D%dM%02d", year(datelist[n-8]), month(datelist[n-8]))] + 
      mLag9  %*% DX[idx,sprintf("D%dM%02d", year(datelist[n-9]), month(datelist[n-9]))] + 
      mLag10 %*% DX[idx,sprintf("D%dM%02d", year(datelist[n-10]), month(datelist[n-10]))] + 
      mLag11 %*% DX[idx,sprintf("D%dM%02d", year(datelist[n-11]), month(datelist[n-11]))] + 
      mLag12 %*% DX[idx,sprintf("D%dM%02d", year(datelist[n-12]), month(datelist[n-12]))] + 
      mLag13 %*% DX[idx,sprintf("D%dM%02d", year(datelist[n-13]), month(datelist[n-13]))]
    
    # forecast longo prazo
    Forecast.LR <- mLagLR %*% X[idx,sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))]
    
    
    # forecast constante e dummies
    
    Forecast.Dm <- as.matrix(mLagDm)
    Forecast <- Forecast.SR + Forecast.LR + Forecast.Dm
    
    results.tbl[idx, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] <- Forecast
    
    results.tbl[idx,sprintf("Error_%dM%02d", year(datelist[n]), month(datelist[n]))] <- results.tbl[idx, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))] - results.tbl[idx, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))]
  }
  
}

# Faz strip do numero da regiao e tipo
results.tbl$Regiao = str_split(results.tbl$variavel, "\\_", simplify = TRUE)[,1]
results.tbl$Tipo = str_split(results.tbl$variavel, "\\_", simplify = TRUE)[,2]

# Salva os dados para o futuro
readr::write_excel_csv(results.tbl, file = "./Excel Export/forecast_result_VECM.csv")

# Relacao_Agregacao_Ox <- read_excel("Database/Relacao_Agregacao_Ox.xlsx")
# 
# 
# 
# 
# dm <- Relacao_Agregacao_Ox %>%
#   mutate(Reg2=sprintf("R%d",Id)) %>%
#   right_join(results.tbl, by=c("Reg2"="Regiao")) %>% 
#   select(Id, Pop, starts_with("Error")) %>%
#   filter(Id > 0) %>%
#   dplyr::transmute(Id, 
#                    Error_1_pc = Error_2019M01/Pop,
#                    Error_2_pc = Error_2019M02/Pop,
#                    Error_3_pc = Error_2019M03/Pop,
#                    
#                    Error_4_pc = Error_2019M04/Pop,
#                    Error_5_pc = Error_2019M05/Pop,
#                    Error_6_pc = Error_2019M06/Pop,
#                    
#                    Error_7_pc = Error_2019M07/Pop,
#                    Error_8_pc = Error_2019M08/Pop,
#                    Error_9_pc = Error_2019M09/Pop,
#                    
#                    Error_10_pc = Error_2019M10/Pop,
#                    Error_11_pc = Error_2019M11/Pop,
#                    Error_12_pc = Error_2019M12/Pop, 
#                    
#                    
#                    Error_1 = Error_2019M01,
#                    Error_2 = Error_2019M02,
#                    Error_3 = Error_2019M03,
#                    
#                    Error_4 = Error_2019M04,
#                    Error_5 = Error_2019M05,
#                    Error_6 = Error_2019M06,
#                    
#                    Error_7 = Error_2019M07,
#                    Error_8 = Error_2019M08,
#                    Error_9 = Error_2019M09,
#                    
#                    Error_10 = Error_2019M10,
#                    Error_11 = Error_2019M11,
#                    Error_12 = Error_2019M12)
# 
# summary(dm)
# 
# library(ggplot2)
# library(tidyr)
# dm %>% pivot_longer(cols = starts_with("Error")) %>% 
#   ggplot() +
#   geom_boxplot(aes(x=name, y = value)) 
# 
# 
# boxplot(t(dm[dm$Id == 404,-1]))
# 
# 
