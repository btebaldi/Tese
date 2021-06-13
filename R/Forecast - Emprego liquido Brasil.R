
# Setup -------------------------------------------------------------------
rm(list = ls())

library(readr)
library(readxl)
library(dplyr)
library(urca)
library(vars)
library(tsDyn)
library(lubridate)
library(stringr)

# Dataload ----------------------------------------------------------------

# busca dados de estimacao
region.db <- read_csv("../Ox Metrics GVAR/Database/DatabaseDesAdm_RA_v1.csv")
# region.db <- read_csv("../Ox Metrics GVAR/Database/DatabaseDesAdm_RA_v1.csv")
selVector <- lubridate::ymd(region.db$X1, truncated = 1) < as.Date("2017-01-01")

region.db <- region.db[selVector, ]

head(region.db)

# busca dados Macro (nao utilizados)
# macro.db <- read_excel("~/GitHub/Tese/Ox Metrics GVAR/Database/MacroVariables_forR_20210604.xlsx", 
#                        na = "#N/A")
# head(macro.db)


# data para forecast em diferenca
DX.df <- read_excel("Excel Export/DatabaseDesAdm_RA_vForecast_v3.xlsx", 
                    range = "A1:BR1108")

DX <- DX.df[,-1] %>% data.matrix()

# data para forecast em nivel
X.df <- read_excel("Excel Export/DatabaseDesAdm_RA_vForecast_v3.xlsx", 
                   range = "A1115:BR2222")

X <- X.df[,-1] %>% data.matrix()

# Vetor de datas
datelist <- seq(from = as.Date("2015-01-01"),
                to = as.Date("2019-12-01"),
                by="month")

# Matriz com resultados
results.tbl <-  tibble(variavel=paste("R", 1:552, "_EmpLiqui", sep=""))
results.tbl_ar1 <-  tibble(variavel=paste("R", 1:552, "_EmpLiqui", sep=""))


j <- 1
cols.sufix <- c("_Admitidos", "_Desligados")

# para cada regiao, estimar um VECM e fazer o forecast
for (j in 1:552) {
  # Busca dados especificos da regiao dados para estimacao do VECM
  tbl <- region.db[, paste("R", j, cols.sufix, sep="")]
  tbl[, paste("R", j, "_EmpLiquido", sep="")] <- tbl[, paste("R", j, "_Admitidos", sep="")] - tbl[, paste("R", j, "_Desligados", sep="")]
  
  y <- tbl[[paste("R", j, "_EmpLiquido", sep="")]]
  
  dy <- diff(y)
  
  mdl.ar1 <- stats::arima(dy, order = c(1,0,0), method = "CSS")
  summary(mdl.ar1)
  
  mdl.ar13 <- stats::arima(dy, order = c(13,0,0), method = "CSS")
  summary(mdl.ar13)
  
  mLag1_ar1 <- mdl.ar1$coef["ar1"]
  mLagDm_ar1 <- mdl.ar1$coef["intercept"]
  # mLagDm_ar1 <- 0
  
  # determina coeficientes de Lag
  mLag1 <- mdl.ar13$coef["ar1"]
  mLag2 <- mdl.ar13$coef["ar2"]
  mLag3 <- mdl.ar13$coef["ar3"]
  mLag4 <- mdl.ar13$coef["ar4"]
  mLag5 <- mdl.ar13$coef["ar5"]
  mLag6 <- mdl.ar13$coef["ar6"]
  mLag7 <- mdl.ar13$coef["ar7"]
  mLag8 <- mdl.ar13$coef["ar8"]
  mLag9 <- mdl.ar13$coef["ar9"]
  mLag10 <- mdl.ar13$coef["ar10"]
  mLag11 <- mdl.ar13$coef["ar11"]
  mLag12 <- mdl.ar13$coef["ar12"]
  mLag13 <- mdl.ar13$coef["ar13"]
  
  # Determina coeficientes de constante 
  mLagDm <- mdl.ar13$coef["intercept"]
  # mLagDm <- 0
  
  # Determina qual a lina a regiao esta na matriz de forecast
  idx <-  which(DX.df$Variavel %in% paste("R", j, c("_Admitidos", "_Desligados"), sep = ""))
  
  # i=0
  # para cada lag calcula o forecast
  for(i in 0:35){
    
    n <- 25+i
    cat(sprintf("%3d - %s\n",j,datelist[n]))
    
    # resultado atual 
    results.tbl[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))] <- DX[idx[1],sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))] - DX[idx[2],sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))]
    results.tbl_ar1[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))] <- DX[idx[1],sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))] - DX[idx[2],sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))]    
    
    # forecast curto prazo
    Forecast.SR <- mLag1  %*% (DX[idx[1], sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))] - DX[idx[2], sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))]) + 
      mLag2  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-2]), month(datelist[n-2]))] -DX[idx[2],sprintf("D%dM%02d", year(datelist[n-2]), month(datelist[n-2]))]) + 
      mLag3  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-3]), month(datelist[n-3]))] -DX[idx[2],sprintf("D%dM%02d", year(datelist[n-3]), month(datelist[n-3]))])+ 
      mLag4  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-4]), month(datelist[n-4]))] -DX[idx[2],sprintf("D%dM%02d", year(datelist[n-4]), month(datelist[n-4]))] )+ 
      mLag5  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-5]), month(datelist[n-5]))] -DX[idx[2],sprintf("D%dM%02d", year(datelist[n-5]), month(datelist[n-5]))] )+ 
      mLag6  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-6]), month(datelist[n-6]))] -DX[idx[2],sprintf("D%dM%02d", year(datelist[n-6]), month(datelist[n-6]))] )+ 
      mLag7  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-7]), month(datelist[n-7]))] -DX[idx[2],sprintf("D%dM%02d", year(datelist[n-7]), month(datelist[n-7]))] )+ 
      mLag8  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-8]), month(datelist[n-8]))] -DX[idx[2],sprintf("D%dM%02d", year(datelist[n-8]), month(datelist[n-8]))] )+ 
      mLag9  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-9]), month(datelist[n-9]))] -DX[idx[2],sprintf("D%dM%02d", year(datelist[n-9]), month(datelist[n-9]))] )+ 
      mLag10 %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-10]), month(datelist[n-10]))] -DX[idx[2],sprintf("D%dM%02d", year(datelist[n-10]), month(datelist[n-10]))] )+ 
      mLag11 %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-11]), month(datelist[n-11]))] -DX[idx[2],sprintf("D%dM%02d", year(datelist[n-11]), month(datelist[n-11]))] )+ 
      mLag12 %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-12]), month(datelist[n-12]))] -DX[idx[2],sprintf("D%dM%02d", year(datelist[n-12]), month(datelist[n-12]))] )+ 
      mLag13 %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-13]), month(datelist[n-13]))] -DX[idx[2],sprintf("D%dM%02d", year(datelist[n-13]), month(datelist[n-13]))] )+
      0
    
    Forecast.SR_ar1 <- mLag1_ar1  %*% (DX[idx[1], sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))] - DX[idx[2], sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))])
    
    
    # forecast constante e dummies
    Forecast.Dm <- as.matrix(mLagDm)
    Forecast.Dm_ar1 <- as.matrix(mLagDm_ar1)
    Forecast <- Forecast.SR + Forecast.Dm
    Forecast_ar1 <- Forecast.SR_ar1 + Forecast.Dm_ar1
    
    results.tbl[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] <- Forecast
    results.tbl_ar1[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] <- Forecast_ar1
    
    
    results.tbl[j,sprintf("Error_%dM%02d", year(datelist[n]), month(datelist[n]))] <- results.tbl[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] - results.tbl[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))]
    results.tbl_ar1[j,sprintf("Error_%dM%02d", year(datelist[n]), month(datelist[n]))] <- results.tbl_ar1[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] - results.tbl_ar1[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))]
    
  }
}

# Faz strip do numero da regiao e tipo
results.tbl$Regiao = str_split(results.tbl$variavel, "\\_", simplify = TRUE)[,1]
results.tbl$Tipo = str_split(results.tbl$variavel, "\\_", simplify = TRUE)[,2]

results.tbl_ar1$Regiao = str_split(results.tbl_ar1$variavel, "\\_", simplify = TRUE)[,1]
results.tbl_ar1$Tipo = str_split(results.tbl_ar1$variavel, "\\_", simplify = TRUE)[,2]

# Salva os dados para o futuro
readr::write_excel_csv(results.tbl, file = "./Excel Export/forecast_result_AR13 (2016).csv")
readr::write_excel_csv(results.tbl_ar1, file = "./Excel Export/forecast_result_AR1 (2016).csv")

