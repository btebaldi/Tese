
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
library(dplyr)

results.tbl <-  tibble(variavel=X.df$Variavel)


for(i in 0:11){
  n <- 25+i
  cat(sprintf("%s\n",datelist[n]))
  
  # resultado atual 
  results.tbl[, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))] <- DX[,sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))]
  
  
  
  # forecast curto prazo
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
  
  # forecast longo prazo
  Forecast.LR <- mLagLR %*% X[,sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))]
  
  # forecast constante e dummies
  Dummies <- matrix(NA, nrow = 12, ncol = 1)
  rownames(Dummies) <- c("CONST", paste("M", 1:11, sep = ""))
  
  Dummies[1,1] <- 1 # Constante
  Dummies[2:12,1] <- 0-1/12 # Constante
  
  if (i != 11){
    Dummies[month(datelist[n])+1,1] <- 1-1/12 # mes de previsao
  }
  
  Forecast.Dm <- mLagDm %*% Dummies
  Forecast <- Forecast.SR + Forecast.LR + Forecast.Dm
  
  results.tbl[, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] <- Forecast
  
  results.tbl[,sprintf("Error_%dM%02d", year(datelist[n]), month(datelist[n]))] <- results.tbl[[sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))]] - results.tbl[[sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))]]
}



results.tbl$regiao = str_split(results.tbl$variavel, "\\_", simplify = TRUE)[,1]

readr::write_excel_csv(results.tbl, file = "./Excel Export/forecast_result.csv")

Relacao_Agregacao_Ox <- read_excel("Database/Relacao_Agregacao_Ox.xlsx")

dm <- Relacao_Agregacao_Ox %>% mutate(Reg2=sprintf("R%d",Id)) %>% right_join(results.tbl, by=c("Reg2"="regiao")) %>% 
  select(Id, Pop, starts_with("Error")) %>% filter(Id > 0) %>%
  dplyr::transmute(Id, 
                   Error_1 = Error_2019M01/Pop,
                   Error_2 = Error_2019M02/Pop,
                   Error_3 = Error_2019M03/Pop,
                   
                   Error_4 = Error_2019M04/Pop,
                   Error_5 = Error_2019M05/Pop,
                   Error_6 = Error_2019M06/Pop,
                   
                   Error_7 = Error_2019M07/Pop,
                   Error_8 = Error_2019M08/Pop,
                   Error_9 = Error_2019M09/Pop,
                   
                   Error_10 = Error_2019M10/Pop,
                   Error_11 = Error_2019M11/Pop,
                   Error_12 = Error_2019M12/Pop)

summary(dm)

library(ggplot2)
library(tidyr)
dm %>% pivot_longer(cols = starts_with("Error")) %>% 
  ggplot() +
  geom_boxplot(aes(x=name, y = value)) 


boxplot(t(dm[dm$Id == 404,-1]))

        