
library(readxl)

DX.df <- read_excel("Excel Export/DatabaseDesAdm_RA_vForecast_v3.xlsx", 
                 range = "A1:AK1108")

DX <- DX.df[,-1] %>% data.matrix()

X.df <- read_excel("Excel Export/DatabaseDesAdm_RA_vForecast_v3.xlsx", 
                 range = "A1115:AK2222")

X <- X.df[,-1] %>% data.matrix()

mLag1 <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mGyL1.rds")
mLag2 <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mGyL2.rds")
mLag3 <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mGyL3.rds")

mLag4 <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mGyL4.rds")
mLag5 <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mGyL5.rds")
mLag6 <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mGyL6.rds")

mLag7 <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mGyL7.rds")
mLag8 <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mGyL8.rds")
mLag9 <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mGyL9.rds")

mLag10 <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mGyL10.rds")
mLag11 <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mGyL11.rds")
mLag12 <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mGyL12.rds")
mLag13 <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mGyL13.rds")


mLagLR <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mL.rds")


mLagDm <- readRDS("C:/Users/bteba/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/mGy_inv_X_mC.rds")

mLagDm <- mLagDm[, 1:12]
colnames(mLagDm) <- c("CONST", paste("M", 1:11))



datelist <- seq(from = as.Date("2017-01-01"),
    to = as.Date("2019-12-01"),
    by="month")
    
library(lubridate)
n <- 26
sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))
datelist[n-1]

actual <- DX[,sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))]

Forecast.SR <- mLag1 %*% DX[, sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))] + 
  mLag2 %*% DX[,sprintf("D%dM%02d", year(datelist[n-2]), month(datelist[n-2]))] + 
  mLag3 %*% DX[,sprintf("D%dM%02d", year(datelist[n-3]), month(datelist[n-3]))] + 
  mLag4 %*% DX[,sprintf("D%dM%02d", year(datelist[n-4]), month(datelist[n-4]))] + 
  mLag5 %*% DX[,sprintf("D%dM%02d", year(datelist[n-5]), month(datelist[n-5]))] + 
  mLag6 %*% DX[,sprintf("D%dM%02d", year(datelist[n-6]), month(datelist[n-6]))] + 
  mLag7 %*% DX[,sprintf("D%dM%02d", year(datelist[n-7]), month(datelist[n-7]))] + 
  mLag8 %*% DX[,sprintf("D%dM%02d", year(datelist[n-8]), month(datelist[n-8]))] + 
  mLag9 %*% DX[,sprintf("D%dM%02d", year(datelist[n-9]), month(datelist[n-9]))] + 
  mLag10 %*% DX[,sprintf("D%dM%02d", year(datelist[n-10]), month(datelist[n-10]))] + 
  mLag11 %*% DX[,sprintf("D%dM%02d", year(datelist[n-11]), month(datelist[n-11]))] + 
  mLag12 %*% DX[,sprintf("D%dM%02d", year(datelist[n-12]), month(datelist[n-12]))] + 
  mLag13 %*% DX[,sprintf("D%dM%02d", year(datelist[n-13]), month(datelist[n-13]))]
  

Forecast.LR <- mLagLR %*% X[,sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))]
  

Dummies <- matrix(NA, nrow = 12, ncol = 1)
rownames(Dummies) <- c("CONST", paste("M", 1:11, sep = ""))

Dummies[1,1] <- 1 # Constante
Dummies[2:12,1] <- 0-1/12 # Constante

Dummies[month(datelist[n])+1,1] <- 1-1/12 # mes de previsao

Forecast.Dm <- mLagDm %*% Dummies

Forecast <- Forecast.SR + Forecast.LR + Forecast.Dm

error <- actual- Forecast
boxplot(error)

nA=which(X.df$Variavel == "R346_Admitidos")
nD=nA+1

Forecast.SR[nA:nD]
Forecast.LR[nA:nD]
Forecast.Dm[nA:nD]

MSE = (mean( (actual - Forecast)^2 ))^0.5

MSE

DX.df[nA:nD,c("Variavel", "D2019M01")]











