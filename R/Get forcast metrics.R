
# setup -------------------------------------------------------------------

rm(list = ls())

library(readr)
library(dplyr)
library(stringr)
library(tibble)
library(ggplot2)
library(tidyr)

# User defined function ---------------------------------------------------

Add.EmpLiq <- function(tbl){
  
  for(j in 1:552){
    # busca a linha do adm e des
    idx.adm <- which(tbl$variavel == sprintf("R%d_Admitidos", j))
    idx.des <- which(tbl$variavel == sprintf("R%d_Desligados", j))
    
    #  determina a linha de emprego liquido
    EmpLiq <- as.list(colnames(tbl))
    names(EmpLiq) <- colnames(tbl)
    
    EmpLiq["variavel"] <- sprintf("R%d_empLiquido", j)
    EmpLiq["Tipo"] <- "EmpLiq"
    EmpLiq["Regiao"] <- sprintf("R%d", j)
    
    col="Actual_2019M01"
    for(col in colnames(tbl)){
      
      if( stringr::str_detect(col, "^(Actual_)|(Forecast_)") ){
        EmpLiq[col] <-  tbl[[idx.adm, col]] - tbl[[idx.des, col]]
      } 
    }
    
    EmpLiq[4+c(0:11)*3] <- NA
    
    tbl <- add_row(tbl, 
                   variavel = EmpLiq$variavel,
                   Actual_2019M01 = EmpLiq$Actual_2019M01,
                   Forecast_2019M01 = EmpLiq$Forecast_2019M01,
                   Actual_2019M02 = EmpLiq$Actual_2019M02,
                   Forecast_2019M02 = EmpLiq$Forecast_2019M02,
                   Actual_2019M03 = EmpLiq$Actual_2019M03,
                   Forecast_2019M03 = EmpLiq$Forecast_2019M03,
                   Actual_2019M04 = EmpLiq$Actual_2019M04,
                   Forecast_2019M04 = EmpLiq$Forecast_2019M04,
                   Actual_2019M05 = EmpLiq$Actual_2019M05,
                   Forecast_2019M05 = EmpLiq$Forecast_2019M05,
                   Actual_2019M06 = EmpLiq$Actual_2019M06,
                   Forecast_2019M06 = EmpLiq$Forecast_2019M06,
                   Actual_2019M07 = EmpLiq$Actual_2019M07,
                   Forecast_2019M07 = EmpLiq$Forecast_2019M07,
                   Actual_2019M08 = EmpLiq$Actual_2019M08,
                   Forecast_2019M08 = EmpLiq$Forecast_2019M08,
                   Actual_2019M09 = EmpLiq$Actual_2019M09,
                   Forecast_2019M09 = EmpLiq$Forecast_2019M09,
                   Actual_2019M10 = EmpLiq$Actual_2019M10,
                   Forecast_2019M10 = EmpLiq$Forecast_2019M10,
                   Actual_2019M11 = EmpLiq$Actual_2019M11,
                   Forecast_2019M11 = EmpLiq$Forecast_2019M11,
                   Actual_2019M12 = EmpLiq$Actual_2019M12,
                   Forecast_2019M12 = EmpLiq$Forecast_2019M12,
                   Regiao = EmpLiq$Regiao,
                   Tipo = EmpLiq$Tipo,
                   .after = idx.des)
  }
  
  tbl$Error_2019M01 <- tbl$Forecast_2019M01 - tbl$Actual_2019M01
  tbl$Error_2019M02 <- tbl$Forecast_2019M02 - tbl$Actual_2019M02
  tbl$Error_2019M03 <- tbl$Forecast_2019M03 - tbl$Actual_2019M03
  tbl$Error_2019M04 <- tbl$Forecast_2019M04 - tbl$Actual_2019M04
  tbl$Error_2019M05 <- tbl$Forecast_2019M05 - tbl$Actual_2019M05
  tbl$Error_2019M06 <- tbl$Forecast_2019M06 - tbl$Actual_2019M06
  tbl$Error_2019M07 <- tbl$Forecast_2019M07 - tbl$Actual_2019M07
  tbl$Error_2019M08 <- tbl$Forecast_2019M08 - tbl$Actual_2019M08
  tbl$Error_2019M09 <- tbl$Forecast_2019M09 - tbl$Actual_2019M09
  tbl$Error_2019M10 <- tbl$Forecast_2019M10 - tbl$Actual_2019M10
  tbl$Error_2019M11 <- tbl$Forecast_2019M11 - tbl$Actual_2019M11
  tbl$Error_2019M12 <- tbl$Forecast_2019M12 - tbl$Actual_2019M12
  
  return(tbl)
}

# Dataload ----------------------------------------------------------------

GVAR.IIS <- read_csv("Excel Export/forecast_result_COM IIS.csv",
                     col_types = cols(
                       .default = col_double(),
                       variavel = col_character(),
                       Regiao = col_character(),
                       Tipo = col_character()
                     ))
GVAR.IIS <- GVAR.IIS[-c(1:3), ]
head(GVAR.IIS)


GVAR <- read_csv("Excel Export/forecast_result_SEM IIS.csv",
                 col_types = cols(
                   .default = col_double(),
                   variavel = col_character(),
                   Regiao = col_character(),
                   Tipo = col_character()
                 ))
GVAR <- GVAR[-c(1:3), ]
head(GVAR)


VECM <- read_csv("Excel Export/forecast_result_VECM.csv",
                 col_types = cols(
                   .default = col_double(),
                   variavel = col_character(),
                   Regiao = col_character(),
                   Tipo = col_character()
                 ))
VECM <- VECM[-c(1:3), ]
head(VECM)


PCA <- read_csv("Excel Export/forecast_result_PCA.csv",
                col_types = cols(
                  .default = col_double(),
                  variavel = col_character(),
                  Regiao = col_character(),
                  Tipo = col_character()
                ))
PCA <- PCA[-c(1:3), ]
head(PCA)


# Adiciona emprego liquido ------------------------------------------------
GVAR.IIS <- Add.EmpLiq(GVAR.IIS)
GVAR <- Add.EmpLiq(GVAR)
VECM <- Add.EmpLiq(VECM)
PCA <- Add.EmpLiq(PCA)



# s -----------------------------------------------------------------------

GVAR.IIS.error <- GVAR.IIS %>%  dplyr::select(variavel, Regiao, Tipo, starts_with("Error")) %>% mutate(source="GVAR_IIS")
GVAR.error <- GVAR %>% dplyr::select(variavel, starts_with("Error")) %>% mutate(source="GVAR")
VECM.error <- VECM %>% dplyr::select(variavel, starts_with("Error")) %>% mutate(source="VECM")
PCA.error <- PCA %>% dplyr::select(variavel, starts_with("Error")) %>% mutate(source="PCA")


# Contas para MSE ---------------------------------------------------------

tbl <- bind_rows(GVAR.IIS.error, GVAR.error, VECM.error, PCA.error)
# Faz strip do numero da regiao e tipo
tbl$Regiao = str_split(tbl$variavel, "\\_", simplify = TRUE)[,1]
tbl$Tipo = str_split(tbl$variavel, "\\_", simplify = TRUE)[,2]

fn <- function(x){
  return(x^2)
}

tbl.overall <- tbl %>% mutate_if(is.numeric, fn) %>% filter(Tipo != "empLiquido")
tbl.adm <- tbl %>% mutate_if(is.numeric, fn) %>% filter(Tipo == "Admitidos")
tbl.des <- tbl %>% mutate_if(is.numeric, fn) %>% filter(Tipo == "Desligados")
tbl.net <- tbl %>% mutate_if(is.numeric, fn) %>% filter(Tipo == "empLiquido")

tbl.overall <- tbl.overall %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.adm <- tbl.adm %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.des <- tbl.des %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.net <- tbl.net %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))

legend.names <- c("tbl.overall"="MSE Overall", "tbl.adm"="MSE Adm", "tbl.des"="MSE Des",  "tbl.net"="MSE Net")

for(table in c("tbl.overall", "tbl.adm", "tbl.des",  "tbl.net")){
  my_tbl <- get(table)
  
  erros <- my_tbl %>% filter(source == "GVAR_IIS") %>% pull(value)
  MSFE.GVAR_IIS <-  mean(erros)
  RMSE.GVAR_IIS <- MSFE.GVAR_IIS^0.5
  
  erros <- my_tbl %>% filter(source == "GVAR") %>% pull(value)
  MSFE.GVAR <-  mean(erros)
  RMSE.GVAR <- MSFE.GVAR^0.5
  
  erros <- my_tbl %>% filter(source == "VECM") %>% pull(value)
  MSFE.VECM <-  mean(erros)
  RMSE.VECM <- MSFE.VECM^0.5
  
  erros <- my_tbl %>% filter(source == "PCA") %>% pull(value)
  MSFE.PCA <-  mean(erros)
  RMSE.PCA <- MSFE.PCA^0.5
  
  metric <- c("RMSE.PCA" = RMSE.PCA,
              "RMSE.VECM" = RMSE.VECM,
              "RMSE.GVAR" = RMSE.GVAR,
              "RMSE.GVAR_IIS" = RMSE.GVAR_IIS)
  
  cat(sprintf("\n%s", table))
  cat(sprintf("\n%13s = %f", names(metric), metric))
  
  
  
  g <- my_tbl %>%
    filter(Regiao %in% c("R379", "R341", "R291", "R178", "R243")) %>% 
    mutate(Regiao = factor(Regiao,
                           levels = c("R379", "R341", "R291", "R178", "R243"),
                           labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Salvador") )) %>% 
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    labs(title = "Forecast error",
         subtitle = legend.names[table],
         y = NULL,
         x = "Error squared"
    ) + 
    theme_bw()
  
  print(g)
  ggsave(filename = sprintf("%s - main.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
  g <- my_tbl %>%
    filter(Regiao %in% c("R379")) %>% 
    mutate(Regiao = factor(Regiao,
                           levels = c("R379", "R341", "R291", "R178", "R243"),
                           labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Salvador") )) %>% 
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    labs(title = "Forecast error",
         subtitle = legend.names[table],
         y = NULL,
         x = "Error squared"
    ) + 
    theme_bw()
  
  print(g)
  ggsave(filename = sprintf("%s - SP.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
  g <- my_tbl %>%
    filter(Regiao %in% c("R341")) %>% 
    mutate(Regiao = factor(Regiao,
                           levels = c("R379", "R341", "R291", "R178", "R243"),
                           labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Salvador") )) %>% 
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    labs(title = "Forecast error",
         subtitle = legend.names[table],
         y = NULL,
         x = "Error squared"
    ) + 
    theme_bw()
  
  print(g)
  ggsave(filename = sprintf("%s - RJ.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
  g <- my_tbl %>%
    filter(Regiao %in% c("R291")) %>% 
    mutate(Regiao = factor(Regiao,
                           levels = c("R379", "R341", "R291", "R178", "R243"),
                           labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Salvador") )) %>% 
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    labs(title = "Forecast error",
         subtitle = legend.names[table],
         y = NULL,
         x = "Error squared"
    ) + 
    theme_bw()
  
  print(g)
  ggsave(filename = sprintf("%s - BH.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
  g <- my_tbl %>%
    # filter(source %in% c("GVAR_IIS", "R341", "R291", "R178", "R243")) %>% 
    # filter(Regiao %in% c("R379", "R341", "R291", "R178", "R243")) %>% 
    # mutate(Regiao = factor(Regiao,
    #                        levels = c("R379", "R341", "R291", "R178", "R243"),
    #                        labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Salvador") )) %>% 
    ggplot() +
    geom_histogram(aes(x=log(value), y=..density.., fill = source), position = "identity", alpha=0.5, bins = 50) +
    facet_wrap(.~source) +
    labs(title = "Forecast error - Histogram log(error^2)",
         subtitle = legend.names[table],
         y = NULL,
         x = "Error squared"
    ) + 
    theme_bw()
  
  print(g)
  ggsave(filename = sprintf("%s - Hist.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
  
  
  g <- my_tbl %>%
    # filter(source %in% c("GVAR_IIS", "R341", "R291", "R178", "R243")) %>% 
    # filter(Regiao %in% c("R379", "R341", "R291", "R178", "R243")) %>% 
    # mutate(Regiao = factor(Regiao,
    #                        levels = c("R379", "R341", "R291", "R178", "R243"),
    #                        labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Salvador") )) %>% 
    ggplot() +
    geom_boxplot(aes(x=log(value), y=source, colour = source), outlier.shape = NA) +
    # facet_wrap(.~source) +
    labs(title = "Forecast error - Boxplot log(error^2)",
         subtitle = legend.names[table],
         y = NULL,
         x = "Error squared"
    ) + 
    theme_bw()
  
  print(g)
  ggsave(filename = sprintf("%s - All box log.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
  g <- my_tbl %>%
    # filter(source %in% c("GVAR_IIS", "R341", "R291", "R178", "R243")) %>% 
    # filter(Regiao %in% c("R379", "R341", "R291", "R178", "R243")) %>% 
    # mutate(Regiao = factor(Regiao,
    #                        levels = c("R379", "R341", "R291", "R178", "R243"),
    #                        labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Salvador") )) %>% 
    ggplot() +
    geom_boxplot(aes(x=value, y=source, colour = source)) +
    # facet_wrap(.~source) +
    labs(title = "Forecast error - Boxplot",
         subtitle = legend.names[table],
         y = NULL,
         x = "Error squared"
    ) + 
    theme_bw()
  
  print(g)
  ggsave(filename = sprintf("%s - All box.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
}

# Contas para MAE ---------------------------------------------------------

tbl <- bind_rows(GVAR.IIS.error, GVAR.error, VECM.error, PCA.error)
# Faz strip do numero da regiao e tipo
tbl$Regiao = str_split(tbl$variavel, "\\_", simplify = TRUE)[,1]
tbl$Tipo = str_split(tbl$variavel, "\\_", simplify = TRUE)[,2]

fn <- function(x){
  return(abs(x))
}

tbl.overall <- tbl %>% mutate_if(is.numeric, fn) %>% filter(Tipo != "empLiquido")
tbl.adm <- tbl %>% mutate_if(is.numeric, fn) %>% filter(Tipo == "Admitidos")
tbl.des <- tbl %>% mutate_if(is.numeric, fn) %>% filter(Tipo == "Desligados")
tbl.net <- tbl %>% mutate_if(is.numeric, fn) %>% filter(Tipo == "empLiquido")

tbl.overall <- tbl.overall %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.adm <- tbl.adm %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.des <- tbl.des %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.net <- tbl.net %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))

legend.names <- c("tbl.overall"="MAE Overall", "tbl.adm"="MAE Adm", "tbl.des"="MAE Des",  "tbl.net"="MAE Net")

for(table in c("tbl.overall", "tbl.adm", "tbl.des",  "tbl.net")){
  my_tbl <- get(table)
  
  erros <- my_tbl %>% filter(source == "GVAR_IIS") %>% pull(value)
  MAFE.GVAR_IIS <-  mean(erros)
  MAFE.GVAR_IIS.sd <- sd(erros)
  
  erros <- my_tbl %>% filter(source == "GVAR") %>% pull(value)
  MAFE.GVAR <-  mean(erros)
  MAFE.GVAR.sd <- sd(erros)
  
  erros <- my_tbl %>% filter(source == "VECM") %>% pull(value)
  MAFE.VECM <-  mean(erros)
  MAFE.VECM.sd <- sd(erros)
  
  erros <- my_tbl %>% filter(source == "PCA") %>% pull(value)
  MAFE.PCA <-  mean(erros)
  MAFE.PCA.sd <- sd(erros)
  
  metric <- c("MAFE.PCA" = MAFE.PCA,
              "MAFE.VECM" = MAFE.VECM,
              "MAFE.GVAR" = MAFE.GVAR,
              "MAFE.GVAR_IIS" = MAFE.GVAR_IIS)
  
  cat(sprintf("\n%s", table))
  cat(sprintf("\n%13s = %f", names(metric), metric))
  
  metric <- c("MAFE.PCA.sd" = MAFE.PCA.sd,
              "MAFE.VECM.sd" = MAFE.VECM.sd,
              "MAFE.GVAR.sd" = MAFE.GVAR.sd,
              "MAFE.GVAR_IIS.sd" = MAFE.GVAR_IIS.sd)
  
  cat(sprintf("\n%s", table))
  cat(sprintf("\n%13s = %f", names(metric), metric))
  
  g <- my_tbl %>%
    filter(Regiao %in% c("R379", "R341", "R291", "R178", "R243")) %>% 
    mutate(Regiao = factor(Regiao,
                           levels = c("R379", "R341", "R291", "R178", "R243"),
                           labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Salvador") )) %>% 
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    # geom_point(size = 3, alpha = 0.15) +
    labs(title = "Forecast error - Mean ABSOLUT error",
         subtitle = legend.names[table],
         y = NULL,
         x = "Error"
    ) + 
    theme_bw()
  
  print(g)
  ggsave(filename = sprintf("%s - main.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
  g <- my_tbl %>%
    filter(Regiao %in% c("R379", "R341")) %>% 
    mutate(Regiao = factor(Regiao,
                           levels = c("R379", "R341", "R291", "R178", "R243"),
                           labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Salvador") )) %>% 
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    labs(title = "Forecast error",
         subtitle = legend.names[table],
         y = NULL,
         x = "Error squared"
    ) + 
    theme_bw()
  
  print(g)
  ggsave(filename = sprintf("%s - SP e RJ.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
}

# Avaliacao por tamanho da Populacao --------------------------------------

tbl <- bind_rows(GVAR.IIS.error, GVAR.error, VECM.error, PCA.error)
# Faz strip do numero da regiao e tipo
tbl$Regiao = str_split(tbl$variavel, "\\_", simplify = TRUE)[,1]
tbl$Tipo = str_split(tbl$variavel, "\\_", simplify = TRUE)[,2]

Relacao_Agregacao_Ox <- read_excel("Database/Relacao_Agregacao_Ox.xlsx")

tbl <- Relacao_Agregacao_Ox %>% 
  mutate(Regiao = sprintf("R%d", Id)) %>% 
  dplyr::select(Regiao, Pop) %>%
  right_join(tbl) %>% 
  mutate(
    cError_1 = Error_2019M01/Pop,
    cError_2 = Error_2019M02/Pop,
    cError_3 = Error_2019M03/Pop,
    
    cError_4 = Error_2019M04/Pop,
    cError_5 = Error_2019M05/Pop,
    cError_6 = Error_2019M06/Pop,
    
    cError_7 = Error_2019M07/Pop,
    cError_8 = Error_2019M08/Pop,
    cError_9 = Error_2019M09/Pop,
    
    cError_10 = Error_2019M10/Pop,
    cError_11 = Error_2019M11/Pop,
    cError_12 = Error_2019M12/Pop) %>% 
  dplyr::select(-starts_with("Error")) %>% 
  dplyr::select(-Pop)

fn <- function(x){
  return(abs(x))
}


tbl.overall <- tbl %>% mutate_if(is.numeric, fn) %>% filter(Tipo != "empLiquido")
tbl.adm <- tbl %>% mutate_if(is.numeric, fn) %>% filter(Tipo == "Admitidos")
tbl.des <- tbl %>% mutate_if(is.numeric, fn) %>% filter(Tipo == "Desligados")
tbl.net <- tbl %>% mutate_if(is.numeric, fn) %>% filter(Tipo == "empLiquido")

tbl.overall <- tbl.overall %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.adm <- tbl.adm %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.des <- tbl.des %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.net <- tbl.net %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))

legend.names <- c("tbl.overall"="MAE by pop. Overall", "tbl.adm"="MAE by pop. Adm", "tbl.des"="MAE by pop. Des",  "tbl.net"="MAE by pop. Net")

for(table in c("tbl.overall", "tbl.adm", "tbl.des",  "tbl.net")){
  my_tbl <- get(table)
  
  erros <- my_tbl %>% filter(source == "GVAR_IIS") %>% pull(value)
  FE.GVAR_IIS <-  mean(erros)
  FE.GVAR_IIS.sd <-  sd(erros)
  
  erros <- my_tbl %>% filter(source == "GVAR") %>% pull(value)
  FE.GVAR <-  mean(erros)
  FE.GVAR.sd <-  sd(erros)
  
  erros <- my_tbl %>% filter(source == "VECM") %>% pull(value)
  FE.VECM <-  mean(erros)
  FE.VECM.sd <-  sd(erros)
  
  erros <- my_tbl %>% filter(source == "PCA") %>% pull(value)
  FE.PCA <-  mean(erros)
  FE.PCA.sd <-  sd(erros)
  
  
  metric <- c("RMSE.PCA" = FE.PCA,
              "RMSE.VECM" = FE.VECM,
              "RMSE.GVAR" = FE.GVAR,
              "RMSE.GVAR_IIS" = FE.GVAR_IIS)
  
  metric.sd <- c("RMSE.PCA" = FE.PCA.sd,
                 "RMSE.VECM" = FE.VECM.sd,
                 "RMSE.GVAR" = FE.GVAR.sd,
                 "RMSE.GVAR_IIS" = FE.GVAR_IIS.sd)
  
  cat(sprintf("\n%s", table))
  cat(sprintf("\n%13s = %f (%f)", names(metric), metric, metric.sd))
  
  
  g <- my_tbl %>%
    filter(Regiao %in% c("R379", "R341", "R291", "R178", "R243")) %>% 
    mutate(Regiao = factor(Regiao,
                           levels = c("R379", "R341", "R291", "R178", "R243"),
                           labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Salvador") )) %>% 
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    # geom_point(size = 3, alpha = 0.15) +
    labs(title = "Forecast error - Mean Squared error",
         subtitle = legend.names[table],
         y = NULL,
         x = "Error squared"
    ) + 
    theme_bw()
  
  print(g)
  ggsave(filename = sprintf("%s - Main.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
  g <- my_tbl %>%
    filter(Regiao %in% c("R379", "R341")) %>% 
    mutate(Regiao = factor(Regiao,
                           levels = c("R379", "R341", "R291", "R178", "R243"),
                           labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Salvador") )) %>% 
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    # geom_point(size = 3, alpha = 0.15) +
    labs(title = "Forecast error - Mean Squared error",
         subtitle = table,
         y = NULL,
         x = "Error squared"
    ) + 
    theme_bw()
  
  print(g)
  ggsave(filename = sprintf("%s - SP e RJ.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
}

