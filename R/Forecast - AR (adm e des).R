
# Setup -------------------------------------------------------------------
rm(list = ls())

library(readr)
library(readxl)
library(dplyr)
# library(urca)
# library(vars)
# library(tsDyn)
library(lubridate)
library(stringr)



file.name <- "forecast_result.csv"
dir1 <- "AR1"
dir13 <- "AR13"

export_file1 <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir1, file.name)
export_file13 <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir13, file.name)


# Dataload ----------------------------------------------------------------

# busca dados de estimacao
region.db <- read_csv("../Ox Metrics GVAR/Database/DatabaseDesAdm_RA_v1.csv")

selVector <- lubridate::ymd(region.db$X1, truncated = 1) < as.Date("2017-01-01")

region.db <- region.db[selVector, ]

head(region.db)


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

results.tbla <-  tibble(variavel=paste("R", 1:552, "_Admitidos", sep=""))
results.tbla_ar1 <-  tibble(variavel=paste("R", 1:552, "_Admitidos", sep=""))

results.tbld <-  tibble(variavel=paste("R", 1:552, "_Desligados", sep=""))
results.tbld_ar1 <-  tibble(variavel=paste("R", 1:552, "_Desligados", sep=""))



j <- 1
cols.sufix <- c("_Admitidos", "_Desligados")

# para cada regiao, estimar um VECM e fazer o forecast
for (j in 1:552) {
  # Busca dados especificos da regiao dados para estimacao do VECM
  tbl <- region.db[, paste("R", j, cols.sufix, sep="")]
  tbl[, paste("R", j, "_EmpLiquido", sep="")] <- tbl[, paste("R", j, "_Admitidos", sep="")] - tbl[, paste("R", j, "_Desligados", sep="")]
  
  y <- tbl[[paste("R", j, "_EmpLiquido", sep="")]]
  ya <- tbl[[paste("R", j, "_Admitidos", sep="")]]
  yd <- tbl[[paste("R", j, "_Desligados", sep="")]]
  
  dy <- diff(y)
  dya <- diff(ya)
  dyd <- diff(yd)
  
  mdl.ar1 <- stats::arima(dy, order = c(1,0,0), method = "CSS", include.mean = F)
  mdl.ar1a <- stats::arima(dya, order = c(1,0,0), method = "CSS", include.mean = F)
  mdl.ar1d <- stats::arima(dyd, order = c(1,0,0), method = "CSS", include.mean = F)
  
  mdl.ar13 <- stats::arima(dy, order = c(13,0,0), method = "CSS")
  mdl.ar13a <- stats::arima(dya, order = c(13,0,0), method = "CSS")
  mdl.ar13d <- stats::arima(dyd, order = c(13,0,0), method = "CSS")

  
  mLag1_ar1 <- mdl.ar1$coef["ar1"]
  mLag1_ar1a <- mdl.ar1a$coef["ar1"]
  mLag1_ar1d <- mdl.ar1d$coef["ar1"]
  
  mLagDm_ar1 <- 0#mdl.ar1$coef["intercept"]
  mLagDm_ar1a <- 0#mdl.ar1a$coef["intercept"]
  mLagDm_ar1d <- 0#mdl.ar1d$coef["intercept"]
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
  
  mLag1a <- mdl.ar13a$coef["ar1"]
  mLag2a <- mdl.ar13a$coef["ar2"]
  mLag3a <- mdl.ar13a$coef["ar3"]
  mLag4a <- mdl.ar13a$coef["ar4"]
  mLag5a <- mdl.ar13a$coef["ar5"]
  mLag6a <- mdl.ar13a$coef["ar6"]
  mLag7a <- mdl.ar13a$coef["ar7"]
  mLag8a <- mdl.ar13a$coef["ar8"]
  mLag9a <- mdl.ar13a$coef["ar9"]
  mLag10a <- mdl.ar13a$coef["ar10"]
  mLag11a <- mdl.ar13a$coef["ar11"]
  mLag12a <- mdl.ar13a$coef["ar12"]
  mLag13a <- mdl.ar13a$coef["ar13"]
  
  mLag1d <- mdl.ar13d$coef["ar1"]
  mLag2d <- mdl.ar13d$coef["ar2"]
  mLag3d <- mdl.ar13d$coef["ar3"]
  mLag4d <- mdl.ar13d$coef["ar4"]
  mLag5d <- mdl.ar13d$coef["ar5"]
  mLag6d <- mdl.ar13d$coef["ar6"]
  mLag7d <- mdl.ar13d$coef["ar7"]
  mLag8d <- mdl.ar13d$coef["ar8"]
  mLag9d <- mdl.ar13d$coef["ar9"]
  mLag10d <- mdl.ar13d$coef["ar10"]
  mLag11d <- mdl.ar13d$coef["ar11"]
  mLag12d <- mdl.ar13d$coef["ar12"]
  mLag13d <- mdl.ar13d$coef["ar13"]
  
  # Determina coeficientes de constante 
  mLagDm <- mdl.ar13$coef["intercept"]
  mLagDma <- mdl.ar13a$coef["intercept"]
  mLagDmd <- mdl.ar13d$coef["intercept"]
  # mLagDm <- 0
  
  # Determina qual a lina a regiao esta na matriz de forecast
  idx <-  which(DX.df$Variavel %in% paste("R", j, c("_Admitidos", "_Desligados"), sep = ""))
  idxa <-  which(DX.df$Variavel %in% paste("R", j, c("_Admitidos"), sep = ""))
  idxd <-  which(DX.df$Variavel %in% paste("R", j, c("_Desligados"), sep = ""))
  
  # i=0
  # para cada lag calcula o forecast
  for(i in 0:35){
    
    n <- 25+i
    cat(sprintf("%3d - %s\n",j,datelist[n]))
    
    # resultado atual 
    results.tbl[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))] <- DX[idx[1],sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))] - DX[idx[2],sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))]
    results.tbl_ar1[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))] <- DX[idx[1],sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))] - DX[idx[2],sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))]    

    results.tbla[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))] <- DX[idx[1],sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))]
    results.tbla_ar1[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))] <- DX[idx[1],sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))] 
    
    results.tbld[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))] <- DX[idx[2],sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))]
    results.tbld_ar1[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))] <- DX[idx[2],sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))]    
        
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
    
    Forecast.SRa <- mLag1  %*% (DX[idx[1], sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))]) + 
      mLag2  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-2]), month(datelist[n-2]))] ) + 
      mLag3  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-3]), month(datelist[n-3]))] )+ 
      mLag4  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-4]), month(datelist[n-4]))] )+ 
      mLag5  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-5]), month(datelist[n-5]))] )+ 
      mLag6  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-6]), month(datelist[n-6]))] )+ 
      mLag7  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-7]), month(datelist[n-7]))] )+ 
      mLag8  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-8]), month(datelist[n-8]))] )+ 
      mLag9  %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-9]), month(datelist[n-9]))] )+ 
      mLag10 %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-10]), month(datelist[n-10]))] )+ 
      mLag11 %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-11]), month(datelist[n-11]))] )+ 
      mLag12 %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-12]), month(datelist[n-12]))] )+ 
      mLag13 %*% (DX[idx[1],sprintf("D%dM%02d", year(datelist[n-13]), month(datelist[n-13]))] )+
      0
    
    
    Forecast.SRd <- mLag1  %*% (DX[idx[2], sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))]) + 
      mLag2  %*% ( DX[idx[2],sprintf("D%dM%02d", year(datelist[n-2]), month(datelist[n-2]))]) + 
      mLag3  %*% ( DX[idx[2],sprintf("D%dM%02d", year(datelist[n-3]), month(datelist[n-3]))])+ 
      mLag4  %*% ( DX[idx[2],sprintf("D%dM%02d", year(datelist[n-4]), month(datelist[n-4]))] )+ 
      mLag5  %*% ( DX[idx[2],sprintf("D%dM%02d", year(datelist[n-5]), month(datelist[n-5]))] )+ 
      mLag6  %*% ( DX[idx[2],sprintf("D%dM%02d", year(datelist[n-6]), month(datelist[n-6]))] )+ 
      mLag7  %*% ( DX[idx[2],sprintf("D%dM%02d", year(datelist[n-7]), month(datelist[n-7]))] )+ 
      mLag8  %*% ( DX[idx[2],sprintf("D%dM%02d", year(datelist[n-8]), month(datelist[n-8]))] )+ 
      mLag9  %*% ( DX[idx[2],sprintf("D%dM%02d", year(datelist[n-9]), month(datelist[n-9]))] )+ 
      mLag10 %*% ( DX[idx[2],sprintf("D%dM%02d", year(datelist[n-10]), month(datelist[n-10]))] )+ 
      mLag11 %*% ( DX[idx[2],sprintf("D%dM%02d", year(datelist[n-11]), month(datelist[n-11]))] )+ 
      mLag12 %*% ( DX[idx[2],sprintf("D%dM%02d", year(datelist[n-12]), month(datelist[n-12]))] )+ 
      mLag13 %*% ( DX[idx[2],sprintf("D%dM%02d", year(datelist[n-13]), month(datelist[n-13]))] )+
      0
    
    Forecast.SR_ar1 <- mLag1_ar1  %*% (DX[idx[1], sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))] - DX[idx[2], sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))])
  
    Forecast.SRa_ar1 <- mLag1_ar1  %*% (DX[idx[1], sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))] )
    
    Forecast.SRd_ar1 <- mLag1_ar1  %*% (DX[idx[2], sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))])
    
      
    
    # forecast constante e dummies
    Forecast.Dm <- as.matrix(mLagDm)
    Forecast.Dm_ar1 <- as.matrix(mLagDm_ar1)
    
    Forecast.Dma <- as.matrix(mLagDma)
    Forecast.Dma_ar1 <- as.matrix(mLagDm_ar1a)
    
    Forecast.Dmd <- as.matrix(mLagDmd)
    Forecast.Dmd_ar1 <- as.matrix(mLagDm_ar1d)

    
    Forecast <- Forecast.SR + Forecast.Dm
    
    Forecasta <- Forecast.SRa + Forecast.Dma
    Forecastd <- Forecast.SRd + Forecast.Dmd
    
    Forecast_ar1 <- Forecast.SR_ar1 + Forecast.Dm_ar1

    Forecast_ar1a <- Forecast.SRa_ar1 + Forecast.Dma_ar1
    
    Forecast_ar1d <- Forecast.SRd_ar1 + Forecast.Dmd_ar1
    
        
    results.tbl[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] <- Forecast
    results.tbl_ar1[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] <- Forecast_ar1

    results.tbla[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] <- Forecasta
    results.tbla_ar1[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] <- Forecast_ar1a
    
    
    results.tbld[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] <- Forecastd
    results.tbld_ar1[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] <- Forecast_ar1d
    

    results.tbl[j,sprintf("Error_%dM%02d", year(datelist[n]), month(datelist[n]))] <- results.tbl[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] - results.tbl[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))]
    results.tbl_ar1[j,sprintf("Error_%dM%02d", year(datelist[n]), month(datelist[n]))] <- results.tbl_ar1[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] - results.tbl_ar1[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))]

    results.tbla[j,sprintf("Error_%dM%02d", year(datelist[n]), month(datelist[n]))] <- results.tbla[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] - results.tbla[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))]
    results.tbla_ar1[j,sprintf("Error_%dM%02d", year(datelist[n]), month(datelist[n]))] <- results.tbla_ar1[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] - results.tbla_ar1[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))]
    
    results.tbld[j,sprintf("Error_%dM%02d", year(datelist[n]), month(datelist[n]))] <- results.tbld[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] - results.tbld[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))]
    results.tbld_ar1[j,sprintf("Error_%dM%02d", year(datelist[n]), month(datelist[n]))] <- results.tbld_ar1[j, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] - results.tbld_ar1[j, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))]
    
        
  }
}

# Faz strip do numero da regiao e tipo
results.tbl$Regiao = str_split(results.tbl$variavel, "\\_", simplify = TRUE)[,1]
results.tbl$Tipo = str_split(results.tbl$variavel, "\\_", simplify = TRUE)[,2]

results.tbla$Regiao = str_split(results.tbla$variavel, "\\_", simplify = TRUE)[,1]
results.tbla$Tipo = str_split(results.tbla$variavel, "\\_", simplify = TRUE)[,2]

results.tbld$Regiao = str_split(results.tbld$variavel, "\\_", simplify = TRUE)[,1]
results.tbld$Tipo = str_split(results.tbld$variavel, "\\_", simplify = TRUE)[,2]


results.tbl_ar1$Regiao = str_split(results.tbl_ar1$variavel, "\\_", simplify = TRUE)[,1]
results.tbl_ar1$Tipo = str_split(results.tbl_ar1$variavel, "\\_", simplify = TRUE)[,2]

results.tbla_ar1$Regiao = str_split(results.tbla_ar1$variavel, "\\_", simplify = TRUE)[,1]
results.tbla_ar1$Tipo = str_split(results.tbla_ar1$variavel, "\\_", simplify = TRUE)[,2]

results.tbld_ar1$Regiao = str_split(results.tbld_ar1$variavel, "\\_", simplify = TRUE)[,1]
results.tbld_ar1$Tipo = str_split(results.tbld_ar1$variavel, "\\_", simplify = TRUE)[,2]


# Salva os dados para o futuro
saveRDS(results.tbla_ar1, file = "./Database/AR13_ADM.rds")
saveRDS(results.tbld_ar1, file = "./Database/AR13_DES.rds")
saveRDS(results.tbl_ar1, file = "./Database/AR13_LIQ.rds")

saveRDS(results.tbla, file = "./Database/AR1_ADM.rds")
saveRDS(results.tbld, file = "./Database/AR1_DES.rds")
saveRDS(results.tbl, file = "./Database/AR1_LIQ.rds")

