rm(list = ls())

library(dplyr)
library(tidyr)

RMSE.f <- function(x){
  return(x^2)
}

MAE.f <- function(x){
  return(abs(x))
}

tbl_adm_ar13 <- readRDS(file = "./Database/AR13_ADM.rds")
tbl_des_ar13 <- readRDS(file = "./Database/AR13_DES.rds")
tbl_liq_ar13 <- readRDS(file = "./Database/AR13_LIQ.rds")

tbl_adm_ar1 <- readRDS(file = "./Database/AR1_ADM.rds")
tbl_des_ar1 <- readRDS(file = "./Database/AR1_DES.rds")
tbl_liq_ar1 <- readRDS(file = "./Database/AR1_LIQ.rds")


tbl <- tbl_adm_ar13 %>% 
  select(starts_with("Error_") ) %>% 
  pivot_longer(everything()) %>% 
  mutate(SE=RMSE.f(value),
         AE=MAE.f(value)) %>% 
  summarise(RMSE = mean(SE)^0.5,
            MAE = mean(AE))

cat(sprintf("\nAR13 - ADM\nRMSE: %f\nMAE: %f",tbl$RMSE,tbl$MAE))


tbl <- tbl_des_ar13 %>% 
  select(starts_with("Error_") ) %>% 
  pivot_longer(everything()) %>% 
  mutate(SE=RMSE.f(value),
         AE=MAE.f(value)) %>% 
  summarise(RMSE = mean(SE)^0.5,
            MAE = mean(AE))

cat(sprintf("\nAR13 - DES\nRMSE: %f\nMAE: %f",tbl$RMSE,tbl$MAE))


tbl <- tbl_adm_ar1 %>% 
  select(starts_with("Error_") ) %>% 
  pivot_longer(everything()) %>% 
  mutate(SE=RMSE.f(value),
         AE=MAE.f(value)) %>% 
  summarise(RMSE = mean(SE)^0.5,
            MAE = mean(AE))

cat(sprintf("\nAR1 - ADM\nRMSE: %f\nMAE: %f",tbl$RMSE,tbl$MAE))


tbl <- tbl_des_ar1 %>% 
  select(starts_with("Error_") ) %>% 
  pivot_longer(everything()) %>% 
  mutate(SE=RMSE.f(value),
         AE=MAE.f(value)) %>% 
  summarise(RMSE = mean(SE)^0.5,
            MAE = mean(AE))

cat(sprintf("\nAR1 - DES\nRMSE: %f\nMAE: %f",tbl$RMSE,tbl$MAE))


tbl <- tbl_liq_ar1 %>% 
  select(starts_with("Error_") ) %>% 
  pivot_longer(everything()) %>% 
  mutate(SE=RMSE.f(value),
         AE=MAE.f(value)) %>% 
  summarise(RMSE = mean(SE)^0.5,
            MAE = mean(AE))

cat(sprintf("\nAR1 - liq\nRMSE: %f\nMAE: %f",tbl$RMSE,tbl$MAE))


tbl <- tbl_liq_ar13 %>% 
  select(starts_with("Error_") ) %>% 
  pivot_longer(everything()) %>% 
  mutate(SE=RMSE.f(value),
         AE=MAE.f(value)) %>% 
  summarise(RMSE = mean(SE)^0.5,
            MAE = mean(AE))

cat(sprintf("\nAR1 - liq\nRMSE: %f\nMAE: %f",tbl$RMSE,tbl$MAE))





