# setup -------------------------------------------------------------------

rm(list = ls())

library(forecast)

library(readr)
library(dplyr)
library(stringr)
library(tibble)
library(ggplot2)
library(tidyr)

dir.COMIIS <- "COM IIS - Modelo 10"
dir.SEMIIS <- "SEM IIS - Modelo 0"
dir.VECM <- "VECM"
dir.PCA <- "PCA"

dir.AR1 <- "AR1"
dir.AR13 <- "AR13"

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
    
    EmpLiq[4+c(0:35)*3] <- NA
    
    tbl <- add_row(tbl, 
                   variavel = EmpLiq$variavel,
                   
                   Actual_2017M01 = EmpLiq$Actual_2017M01,
                   Forecast_2017M01 = EmpLiq$Forecast_2017M01,
                   Actual_2017M02 = EmpLiq$Actual_2017M02,
                   Forecast_2017M02 = EmpLiq$Forecast_2017M02,
                   Actual_2017M03 = EmpLiq$Actual_2017M03,
                   Forecast_2017M03 = EmpLiq$Forecast_2017M03,
                   Actual_2017M04 = EmpLiq$Actual_2017M04,
                   Forecast_2017M04 = EmpLiq$Forecast_2017M04,
                   Actual_2017M05 = EmpLiq$Actual_2017M05,
                   Forecast_2017M05 = EmpLiq$Forecast_2017M05,
                   Actual_2017M06 = EmpLiq$Actual_2017M06,
                   Forecast_2017M06 = EmpLiq$Forecast_2017M06,
                   Actual_2017M07 = EmpLiq$Actual_2017M07,
                   Forecast_2017M07 = EmpLiq$Forecast_2017M07,
                   Actual_2017M08 = EmpLiq$Actual_2017M08,
                   Forecast_2017M08 = EmpLiq$Forecast_2017M08,
                   Actual_2017M09 = EmpLiq$Actual_2017M09,
                   Forecast_2017M09 = EmpLiq$Forecast_2017M09,
                   Actual_2017M10 = EmpLiq$Actual_2017M10,
                   Forecast_2017M10 = EmpLiq$Forecast_2017M10,
                   Actual_2017M11 = EmpLiq$Actual_2017M11,
                   Forecast_2017M11 = EmpLiq$Forecast_2017M11,
                   Actual_2017M12 = EmpLiq$Actual_2017M12,
                   Forecast_2017M12 = EmpLiq$Forecast_2017M12,
                   
                   Actual_2018M01 = EmpLiq$Actual_2018M01,
                   Forecast_2018M01 = EmpLiq$Forecast_2018M01,
                   Actual_2018M02 = EmpLiq$Actual_2018M02,
                   Forecast_2018M02 = EmpLiq$Forecast_2018M02,
                   Actual_2018M03 = EmpLiq$Actual_2018M03,
                   Forecast_2018M03 = EmpLiq$Forecast_2018M03,
                   Actual_2018M04 = EmpLiq$Actual_2018M04,
                   Forecast_2018M04 = EmpLiq$Forecast_2018M04,
                   Actual_2018M05 = EmpLiq$Actual_2018M05,
                   Forecast_2018M05 = EmpLiq$Forecast_2018M05,
                   Actual_2018M06 = EmpLiq$Actual_2018M06,
                   Forecast_2018M06 = EmpLiq$Forecast_2018M06,
                   Actual_2018M07 = EmpLiq$Actual_2018M07,
                   Forecast_2018M07 = EmpLiq$Forecast_2018M07,
                   Actual_2018M08 = EmpLiq$Actual_2018M08,
                   Forecast_2018M08 = EmpLiq$Forecast_2018M08,
                   Actual_2018M09 = EmpLiq$Actual_2018M09,
                   Forecast_2018M09 = EmpLiq$Forecast_2018M09,
                   Actual_2018M10 = EmpLiq$Actual_2018M10,
                   Forecast_2018M10 = EmpLiq$Forecast_2018M10,
                   Actual_2018M11 = EmpLiq$Actual_2018M11,
                   Forecast_2018M11 = EmpLiq$Forecast_2018M11,
                   Actual_2018M12 = EmpLiq$Actual_2018M12,
                   Forecast_2018M12 = EmpLiq$Forecast_2018M12,
                   
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
  
  tbl$Error_2017M01 <- tbl$Forecast_2017M01 - tbl$Actual_2017M01
  tbl$Error_2017M02 <- tbl$Forecast_2017M02 - tbl$Actual_2017M02
  tbl$Error_2017M03 <- tbl$Forecast_2017M03 - tbl$Actual_2017M03
  tbl$Error_2017M04 <- tbl$Forecast_2017M04 - tbl$Actual_2017M04
  tbl$Error_2017M05 <- tbl$Forecast_2017M05 - tbl$Actual_2017M05
  tbl$Error_2017M06 <- tbl$Forecast_2017M06 - tbl$Actual_2017M06
  tbl$Error_2017M07 <- tbl$Forecast_2017M07 - tbl$Actual_2017M07
  tbl$Error_2017M08 <- tbl$Forecast_2017M08 - tbl$Actual_2017M08
  tbl$Error_2017M09 <- tbl$Forecast_2017M09 - tbl$Actual_2017M09
  tbl$Error_2017M10 <- tbl$Forecast_2017M10 - tbl$Actual_2017M10
  tbl$Error_2017M11 <- tbl$Forecast_2017M11 - tbl$Actual_2017M11
  tbl$Error_2017M12 <- tbl$Forecast_2017M12 - tbl$Actual_2017M12
  
  tbl$Error_2018M01 <- tbl$Forecast_2018M01 - tbl$Actual_2018M01
  tbl$Error_2018M02 <- tbl$Forecast_2018M02 - tbl$Actual_2018M02
  tbl$Error_2018M03 <- tbl$Forecast_2018M03 - tbl$Actual_2018M03
  tbl$Error_2018M04 <- tbl$Forecast_2018M04 - tbl$Actual_2018M04
  tbl$Error_2018M05 <- tbl$Forecast_2018M05 - tbl$Actual_2018M05
  tbl$Error_2018M06 <- tbl$Forecast_2018M06 - tbl$Actual_2018M06
  tbl$Error_2018M07 <- tbl$Forecast_2018M07 - tbl$Actual_2018M07
  tbl$Error_2018M08 <- tbl$Forecast_2018M08 - tbl$Actual_2018M08
  tbl$Error_2018M09 <- tbl$Forecast_2018M09 - tbl$Actual_2018M09
  tbl$Error_2018M10 <- tbl$Forecast_2018M10 - tbl$Actual_2018M10
  tbl$Error_2018M11 <- tbl$Forecast_2018M11 - tbl$Actual_2018M11
  tbl$Error_2018M12 <- tbl$Forecast_2018M12 - tbl$Actual_2018M12
  
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

GVAR.IIS.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir.COMIIS, "forecast_result.csv")
GVAR.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir.SEMIIS, "forecast_result.csv")
VECM.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir.VECM, "forecast_result.csv")
PCA.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir.PCA, "forecast_result.csv")

AR1.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir.AR1, "forecast_result.csv")
AR13.file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir.AR13, "forecast_result.csv")

GVAR.IIS <- read_csv(GVAR.IIS.file,
                     col_types = cols(
                       .default = col_double(),
                       variavel = col_character(),
                       Regiao = col_character(),
                       Tipo = col_character()
                     ))
GVAR.IIS <- GVAR.IIS[-c(1:3), ]
head(GVAR.IIS)

GVAR <- read_csv(GVAR.file,
                 col_types = cols(
                   .default = col_double(),
                   variavel = col_character(),
                   Regiao = col_character(),
                   Tipo = col_character()
                 ))
GVAR <- GVAR[-c(1:3), ]
head(GVAR)


VECM <- read_csv(VECM.file,
                 col_types = cols(
                   .default = col_double(),
                   variavel = col_character(),
                   Regiao = col_character(),
                   Tipo = col_character()
                 ))
VECM <- VECM[-c(1:3), ]
head(VECM)


PCA <- read_csv(PCA.file,
                col_types = cols(
                  .default = col_double(),
                  variavel = col_character(),
                  Regiao = col_character(),
                  Tipo = col_character()
                ))
PCA <- PCA[-c(1:3), ]
head(PCA)


AR13 <- read_csv(AR13.file,
                 col_types = cols(
                   .default = col_double(),
                   variavel = col_character(),
                   Regiao = col_character(),
                   Tipo = col_character()
                 ))
head(AR13)


AR1 <- read_csv(AR1.file,
                col_types = cols(
                  .default = col_double(),
                  variavel = col_character(),
                  Regiao = col_character(),
                  Tipo = col_character()
                ))
head(AR1)


# Adiciona emprego liquido ------------------------------------------------
GVAR.IIS <- Add.EmpLiq(GVAR.IIS)
GVAR <- Add.EmpLiq(GVAR)
VECM <- Add.EmpLiq(VECM)
PCA <- Add.EmpLiq(PCA)



# Cria tabelas de erro -----------------------------------------------------------------------

GVAR.IIS.error <- GVAR.IIS %>%  dplyr::select(variavel, Regiao, Tipo, starts_with("Error")) %>% mutate(source="GVAR_IIS")
GVAR.error <- GVAR %>% dplyr::select(variavel, Regiao, Tipo, starts_with("Error")) %>% mutate(source="GVAR")
VECM.error <- VECM %>% dplyr::select(variavel, Regiao, Tipo, starts_with("Error")) %>% mutate(source="VECM")
PCA.error <- PCA %>% dplyr::select(variavel, Regiao, Tipo, starts_with("Error")) %>% mutate(source="PCA")

AR1.error <- AR1 %>% dplyr::select(variavel, Regiao, Tipo, starts_with("Error")) %>% mutate(source="AR1")
AR13.error <- AR13 %>% dplyr::select(variavel, Regiao, Tipo, starts_with("Error")) %>% mutate(source="AR13")

# Fatores e nomes ---------------------------------------------------------

my.levels <- c("R404", "R346", "R193", "R552", "R280", "R151", "R191")
my.labels <- c("São Paulo",
            "Rio de Janeiro",
            "Serrana do Sertão Alagoano",
            "Brasilia",
            "Belo Horizonte",
            "Natal",
            "Recife")

# Contas para MSE ---------------------------------------------------------
rm(list = setdiff(ls(), c("GVAR.IIS.error", "GVAR.error", "VECM.error", "PCA.error", "AR1.error", "AR13.error",
                          "AR1", "AR13","PCA","GVAR.IIS","GVAR","VECM", "my.levels", "my.labels")))

tbl <- bind_rows(GVAR.IIS.error, GVAR.error, VECM.error, PCA.error)

summary(tbl)

fn <- function(x){
  return(x^2)
}

tbl.overall <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo != "EmpLiq")
tbl.adm <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo == "Admitidos")
tbl.des <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo == "Desligados")
tbl.net <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo == "EmpLiq")

tbl.overall <- tbl.overall %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.adm <- tbl.adm %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.des <- tbl.des %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.net <- tbl.net %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))

legend.names <- c("tbl.overall"="MSE Overall", "tbl.adm"="Admitidos", "tbl.des"="Desligados",  "tbl.net"="Emprego Liquido")

for(table in c("tbl.adm", "tbl.des",  "tbl.net")){
  my_tbl <- get(table)
  
  erros <- my_tbl %>% dplyr::filter(source == "GVAR_IIS") %>% pull(value)
  MSFE.GVAR_IIS <-  mean(erros)
  MSFE.GVAR_IIS.sd <-  sd(erros)
  RMSE.GVAR_IIS <- MSFE.GVAR_IIS^0.5
  RMSE.GVAR_IIS.sd <- MSFE.GVAR_IIS.sd^0.5
  
  erros <- my_tbl %>% dplyr::filter(source == "GVAR") %>% pull(value)
  MSFE.GVAR <-  mean(erros)
  MSFE.GVAR.sd <-  sd(erros)
  RMSE.GVAR <- MSFE.GVAR^0.5
  RMSE.GVAR.sd <- MSFE.GVAR.sd^0.5
  
  erros <- my_tbl %>% dplyr::filter(source == "VECM") %>% pull(value)
  MSFE.VECM <-  mean(erros)
  MSFE.VECM.sd <- sd(erros)
  RMSE.VECM <- MSFE.VECM^0.5
  RMSE.VECM.sd <- MSFE.VECM.sd^0.5
  
  erros <- my_tbl %>% dplyr::filter(source == "PCA") %>% pull(value)
  MSFE.PCA <-  mean(erros)
  MSFE.PCA.sd <- sd(erros)
  RMSE.PCA <- MSFE.PCA^0.5
  RMSE.PCA.sd <- MSFE.PCA.sd^0.5
  
  metric <- c("RMSE.PCA" = RMSE.PCA,
              "RMSE.VECM" = RMSE.VECM,
              "RMSE.GVAR" = RMSE.GVAR,
              "RMSE.GVAR_IIS" = RMSE.GVAR_IIS)
  
  metric.sd <- c("RMSE.PCA" = RMSE.PCA.sd,
                 "RMSE.VECM" = RMSE.VECM.sd,
                 "RMSE.GVAR" = RMSE.GVAR.sd,
                 "RMSE.GVAR_IIS" = RMSE.GVAR_IIS.sd)
  
  cat(sprintf("\n%s", table))
  cat(sprintf("\n%13s = %f (%f)", names(metric), metric, metric.sd))
  
  g <- my_tbl %>%
    dplyr::filter(Regiao %in% c("R404")) %>%
    mutate(Regiao = factor(Regiao,
                           levels = my.levels,
                           labels = my.labels )) %>%
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    labs(title = "Root mean square error",
         subtitle = sprintf("Series: %s", legend.names[table]),
         y = NULL,
         x = NULL
    ) +
    theme_bw()
  
  # print(g)
  ggsave(filename = sprintf("RMSE - SP - %s.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)

  
  g <- my_tbl %>%
    dplyr::filter(Regiao %in% c("R404")) %>%
    dplyr::filter(source %in% c("GVAR_IIS", "GVAR", "VECM")) %>%
    mutate(Regiao = factor(Regiao,
                           levels = my.levels,
                           labels = my.labels )) %>%
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    labs(title = "Root mean square error",
         subtitle = sprintf("Series: %s", legend.names[table]),
         y = NULL,
         x = NULL
    ) +
    theme_bw()
  
  # print(g)
  ggsave(filename = sprintf("RMSE - SP (NO PCA) - %s.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
  
  
  g <- my_tbl %>%
    dplyr::filter(Regiao %in% c("R346")) %>%
    mutate(Regiao = factor(Regiao,
                           levels = my.levels,
                           labels = my.labels )) %>%
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    labs(title = "Root mean square error",
         subtitle = sprintf("Series: %s", legend.names[table]),
         y = NULL,
         x = NULL
    ) +
    theme_bw()

  # print(g)
  ggsave(filename = sprintf("RMSE - RJ - %s.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)

  g <- my_tbl %>%
    dplyr::filter(Regiao %in% c("R280")) %>%
    mutate(Regiao = factor(Regiao,
                           levels = my.levels,
                           labels = my.labels )) %>%
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    labs(title = "Root mean square error",
         subtitle = sprintf("Series: %s", legend.names[table]),
         y = NULL,
         x = NULL
    ) +
    theme_bw()
  
  # print(g)
  ggsave(filename = sprintf("RMSE - BH - %s.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
  g <- my_tbl %>%
    dplyr::filter(Regiao %in% c("R552")) %>%
    mutate(Regiao = factor(Regiao,
                           levels = my.levels,
                           labels = my.labels )) %>%
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    labs(title = "Root mean square error",
         subtitle = sprintf("Series: %s", legend.names[table]),
         y = NULL,
         x = NULL
    ) +
    theme_bw()
  
  # print(g)
  ggsave(filename = sprintf("RMSE - DF - %s.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85) 
  
  g <- my_tbl %>%
    dplyr::filter(Regiao %in% c("R151")) %>%
    mutate(Regiao = factor(Regiao,
                           levels = my.levels,
                           labels = my.labels )) %>%
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    labs(title = "Root mean square error",
         subtitle = sprintf("Series: %s", legend.names[table]),
         y = NULL,
         x = NULL
    ) +
    theme_bw()
  
  # print(g)
  ggsave(filename = sprintf("RMSE - Natal - %s.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
  g <- my_tbl %>%
    dplyr::filter(Regiao %in% c("R191")) %>%
    mutate(Regiao = factor(Regiao,
                           levels = my.levels,
                           labels = my.labels )) %>%
    ggplot(aes(y=Regiao, x = value)) +
    geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
    labs(title = "Root mean square error",
         subtitle = sprintf("Series: %s", legend.names[table]),
         y = NULL,
         x = NULL
    ) +
    theme_bw()
  
  # print(g)
  ggsave(filename = sprintf("RMSE - Recife - %s.png", legend.names[table]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
}


# Contas para Erros simples ---------------------------------------------------------
rm(list = setdiff(ls(), c("GVAR.IIS.error", "GVAR.error", "VECM.error", "PCA.error", "AR1.error", "AR13.error",
                          "AR1", "AR13","PCA","GVAR.IIS","GVAR","VECM", "my.levels", "my.labels")))

tbl <- bind_rows(GVAR.IIS.error, GVAR.error, VECM.error, PCA.error)


fn <- function(x){
  return(x)
}

tbl.overall <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo != "EmpLiq")
tbl.adm <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo == "Admitidos")
tbl.des <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo == "Desligados")
tbl.net <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo == "EmpLiq")

tbl.overall <- tbl.overall %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.adm <- tbl.adm %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.des <- tbl.des %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.net <- tbl.net %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))

legend.names <- c("tbl.overall"="Overall", "tbl.adm"="Adm", "tbl.des"="Des",  "tbl.net"="Net")
# table <- "tbl.overall"
for(table in c("tbl.adm", "tbl.des",  "tbl.net")){
  my_tbl <- get(table)
  
  erros <- my_tbl %>% dplyr::filter(source == "GVAR_IIS") %>% pull(value)
  GVAR_IIS.single <-  mean(erros)
  GVAR_IIS.single.sd <-  sd(erros)
  
  
  erros <- my_tbl %>% dplyr::filter(source == "GVAR") %>% pull(value)
  GVAR.single <-  mean(erros)
  GVAR.single.sd <-  sd(erros)
  
  
  erros <- my_tbl %>% dplyr::filter(source == "VECM") %>% pull(value)
  VECM.single <-  mean(erros)
  VECM.single.sd <- sd(erros)
  
  erros <- my_tbl %>% dplyr::filter(source == "PCA") %>% pull(value)
  PCA.single <-  mean(erros)
  PCA.single.sd <- sd(erros)
  
  
  metric <- c("PCA" = PCA.single,
              "VECM" = VECM.single,
              "GVAR" = GVAR.single,
              "GVAR_IIS" = GVAR_IIS.single)
  
  metric.sd <- c("RMSE.PCA" = PCA.single.sd,
                 "RMSE.VECM" = VECM.single.sd,
                 "RMSE.GVAR" = GVAR.single.sd,
                 "RMSE.GVAR_IIS" = GVAR_IIS.single.sd)
  
  cat(sprintf("\n%s", table))
  cat(sprintf("\n%13s = %f (%f)", names(metric), metric, metric.sd))
  
  
  
  # g <- my_tbl %>%
  #   dplyr::filter(Regiao %in% c("R404", "R346", "R280", "R191", "R491")) %>%
  #   mutate(Regiao = factor(Regiao,
  #                          levels = c("R404", "R346", "R280", "R191", "R491"),
  #                          labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Porto Alegre") )) %>%
  #   ggplot(aes(y=Regiao, x = value)) +
  #   geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
  #   labs(title = "Forecast error",
  #        subtitle = legend.names[table],
  #        y = NULL,
  #        x = "Error"
  #   ) +
  #   theme_bw()
  # 
  # print(g)
  # ggsave(filename = sprintf("%s - main.png", legend.names[table]),
  #        path = "Graficos",
  #        plot = g,
  #        scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  # 
  # g <- my_tbl %>%
  #   dplyr::filter(Regiao %in% c("R404")) %>%
  #   mutate(Regiao = factor(Regiao,
  #                          levels = c("R404", "R346", "R280", "R191", "R491"),
  #                          labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Porto Alegre") )) %>%
  #   ggplot(aes(y=Regiao, x = value)) +
  #   geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
  #   labs(title = "Forecast error",
  #        subtitle = legend.names[table],
  #        y = NULL,
  #        x = "Error squared"
  #   ) +
  #   theme_bw()
  # 
  # print(g)
  # ggsave(filename = sprintf("%s - SP.png", legend.names[table]),
  #        path = "Graficos",
  #        plot = g,
  #        scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  # 
  # g <- my_tbl %>%
  #   dplyr::filter(Regiao %in% c("R346")) %>%
  #   mutate(Regiao = factor(Regiao,
  #                          levels = c("R404", "R346", "R280", "R191", "R491"),
  #                          labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Porto Alegre") )) %>%
  #   ggplot(aes(y=Regiao, x = value)) +
  #   geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
  #   labs(title = "Forecast error",
  #        subtitle = legend.names[table],
  #        y = NULL,
  #        x = "Error squared"
  #   ) +
  #   theme_bw()
  # 
  # print(g)
  # 
  # ggsave(filename = sprintf("%s - RJ.png", legend.names[table]),
  #        path = "Graficos",
  #        plot = g,
  #        scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  # 
  # g <- my_tbl %>%
  #   dplyr::filter(Regiao %in% c("R280")) %>% 
  #   mutate(Regiao = factor(Regiao,
  #                          levels = c("R404", "R346", "R280", "R191", "R491"),
  #                          labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Porto Alegre") )) %>%
  #   ggplot(aes(y=Regiao, x = value)) +
  #   geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
  #   labs(title = "Forecast error",
  #        subtitle = legend.names[table],
  #        y = NULL,
  #        x = "Error squared"
  #   ) +
  #   theme_bw()
  # 
  # print(g)
  # ggsave(filename = sprintf("%s - BH.png", legend.names[table]),
  #        path = "Graficos",
  #        plot = g,
  #        scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  # 
  # 
  # g <- my_tbl %>%
  #   dplyr::filter(Regiao %in% c("R552")) %>%
  #   mutate(Regiao = factor(Regiao,
  #                          levels = c("R404", "R346", "R280", "R191", "R552"),
  #                          labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Brasilia") )) %>%
  #   ggplot(aes(y=source, x = value)) +
  #   geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
  #   # geom_point(aes(colour = source), size = 3, alpha = 0.15) + 
  #   labs(title = "Forecast error",
  #        subtitle = paste("Brasilia", legend.names[table]),
  #        y = NULL,
  #        x = "Error squared"
  #   ) +
  #   theme_bw()
  # 
  # print(g)
  # 
  # 
  # g <- my_tbl %>%
  #   dplyr::filter(Regiao %in% c("R346")) %>%
  #   mutate(Regiao = factor(Regiao,
  #                          levels = c("R404", "R346", "R280", "R191", "R552"),
  #                          labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Brasilia") )) %>%
  #   ggplot(aes(y=source, x = value)) +
  #   geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
  #   geom_point(aes(colour = source), size = 3, alpha = 0.15) + 
  #   labs(title = "Forecast error",
  #        subtitle = paste("Brasilia", legend.names[table]),
  #        y = NULL,
  #        x = "Error squared"
  #   ) +
  #   theme_bw()
  # 
  # print(g)
  
}

# Contas para MAE ---------------------------------------------------------

rm(list = setdiff(ls(), c("GVAR.IIS.error", "GVAR.error", "VECM.error", "PCA.error", "AR1.error", "AR13.error",
                          "AR1", "AR13","PCA","GVAR.IIS","GVAR","VECM", "my.levels", "my.labels")))

tbl <- bind_rows(GVAR.IIS.error, GVAR.error, VECM.error, PCA.error)

fn <- function(x){
  return(abs(x))
}

tbl.overall <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo != "EmpLiq")
tbl.adm <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo == "Admitidos")
tbl.des <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo == "Desligados")
tbl.net <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo == "EmpLiq")

tbl.overall <- tbl.overall %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.adm <- tbl.adm %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.des <- tbl.des %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.net <- tbl.net %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))

legend.names <- c("tbl.overall"="MAE Overall", "tbl.adm"="MAE Adm", "tbl.des"="MAE Des",  "tbl.net"="MAE Net")

for(table in c("tbl.adm", "tbl.des",  "tbl.net")){
  my_tbl <- get(table)
  
  erros <- my_tbl %>% dplyr::filter(source == "GVAR_IIS") %>% pull(value)
  MAFE.GVAR_IIS <-  mean(erros)
  MAFE.GVAR_IIS.sd <- sd(erros)
  
  erros <- my_tbl %>% dplyr::filter(source == "GVAR") %>% pull(value)
  MAFE.GVAR <-  mean(erros)
  MAFE.GVAR.sd <- sd(erros)
  
  erros <- my_tbl %>% dplyr::filter(source == "VECM") %>% pull(value)
  MAFE.VECM <-  mean(erros)
  MAFE.VECM.sd <- sd(erros)
  
  erros <- my_tbl %>% dplyr::filter(source == "PCA") %>% pull(value)
  MAFE.PCA <-  mean(erros)
  MAFE.PCA.sd <- sd(erros)
  
  metric <- c("MAFE.PCA" = MAFE.PCA,
              "MAFE.VECM" = MAFE.VECM,
              "MAFE.GVAR" = MAFE.GVAR,
              "MAFE.GVAR_IIS" = MAFE.GVAR_IIS)
  
  metric.sd <- c("MAFE.PCA.sd" = MAFE.PCA.sd,
                 "MAFE.VECM.sd" = MAFE.VECM.sd,
                 "MAFE.GVAR.sd" = MAFE.GVAR.sd,
                 "MAFE.GVAR_IIS.sd" = MAFE.GVAR_IIS.sd)
  
  cat(sprintf("\n%s", table))
  cat(sprintf("\n%13s = %f  (%f)", names(metric), metric, metric.sd))
  
  # g <- my_tbl %>%
  #   dplyr::filter(Regiao %in% c("R404", "R346", "R280", "R191", "R491")) %>% 
  #   mutate(Regiao = factor(Regiao,
  #                          levels = c("R404", "R346", "R280", "R191", "R491"),
  #                          labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Porto Alegre") )) %>% 
  #   ggplot(aes(y=Regiao, x = value)) +
  #   geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
  #   # geom_point(size = 3, alpha = 0.15) +
  #   labs(title = "Forecast error - Mean ABSOLUT error",
  #        subtitle = legend.names[table],
  #        y = NULL,
  #        x = "Error"
  #   ) + 
  #   theme_bw()
  # 
  # print(g)
  # ggsave(filename = sprintf("%s - main.png", legend.names[table]),
  #        path = "Graficos",
  #        plot = g,
  #        scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  # 
  # g <- my_tbl %>%
  #   dplyr::filter(Regiao %in% c("R404", "R346")) %>% 
  #   mutate(Regiao = factor(Regiao,
  #                          levels = c("R404", "R346", "R280", "R191", "R491"),
  #                          labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Porto Alegre") )) %>% 
  #   ggplot(aes(y=Regiao, x = value)) +
  #   geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
  #   labs(title = "Forecast error",
  #        subtitle = legend.names[table],
  #        y = NULL,
  #        x = "Error squared"
  #   ) + 
  #   theme_bw()
  # 
  # print(g)
  # ggsave(filename = sprintf("%s - SP e RJ.png", legend.names[table]),
  #        path = "Graficos",
  #        plot = g,
  #        scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  # 
  # 
  # g <- my_tbl %>%
  #   dplyr::filter(Regiao %in% c("R66", "R151", "R552")) %>% 
  #   mutate(Regiao = factor(Regiao,
  #                          levels = c("R66", "R552", "R151"),
  #                          labels = c("Ma - São Luis", "Brasilia", "Natal") )) %>% 
  #   ggplot(aes(y=Regiao, x = value)) +
  #   geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
  #   labs(title = "Forecast error",
  #        subtitle = legend.names[table],
  #        y = NULL,
  #        x = "Error squared"
  #   ) + 
  #   theme_bw()
  # 
  # print(g)
  
}

# Avaliacao por tamanho da Populacao --------------------------------------
rm(list = setdiff(ls(), c("GVAR.IIS.error", "GVAR.error", "VECM.error", "PCA.error", "AR1.error", "AR13.error",
                          "AR1", "AR13","PCA","GVAR.IIS","GVAR","VECM", "my.levels", "my.labels")))


tbl <- bind_rows(GVAR.IIS.error, GVAR.error, VECM.error, PCA.error)
# Faz strip do numero da regiao e tipo

library(readxl)
Relacao_Agregacao_Ox <- read_excel("../Ox Metrics GVAR/Ox Scripts/Dicionario Ox-GVAR.xlsx")

tbl <- Relacao_Agregacao_Ox %>% 
  mutate(Regiao = Ox, Pop=pop) %>% 
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
    cError_12 = Error_2019M12/Pop,
    
    cError_21 = Error_2017M01/Pop,
    cError_22 = Error_2017M02/Pop,
    cError_23 = Error_2017M03/Pop,
    cError_24 = Error_2017M04/Pop,
    cError_25 = Error_2017M05/Pop,
    cError_26 = Error_2017M06/Pop,
    cError_27 = Error_2017M07/Pop,
    cError_28 = Error_2017M08/Pop,
    cError_29 = Error_2017M09/Pop,
    cError_210 = Error_2017M10/Pop,
    cError_211 = Error_2017M11/Pop,
    cError_212 = Error_2017M12/Pop,
    
    cError_31 = Error_2018M01/Pop,
    cError_32 = Error_2018M02/Pop,
    cError_33 = Error_2018M03/Pop,
    cError_34 = Error_2018M04/Pop,
    cError_35 = Error_2018M05/Pop,
    cError_36 = Error_2018M06/Pop,
    cError_37 = Error_2018M07/Pop,
    cError_38 = Error_2018M08/Pop,
    cError_39 = Error_2018M09/Pop,
    cError_310 = Error_2018M10/Pop,
    cError_311 = Error_2018M11/Pop,
    cError_312 = Error_2018M12/Pop
  ) %>% 
  dplyr::select(-starts_with("Error")) %>% 
  dplyr::select(-Pop)

fn <- function(x){
  # return(abs(x))
  return((x)^2)
}


tbl.overall <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo != "EmpLiq")
tbl.adm <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo == "Admitidos")
tbl.des <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo == "Desligados")
tbl.net <- tbl %>% mutate_if(is.numeric, fn) %>% dplyr::filter(Tipo == "EmpLiq")

tbl.overall <- tbl.overall %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.adm <- tbl.adm %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.des <- tbl.des %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.net <- tbl.net %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))

legend.names <- c("tbl.overall"="MAE by pop. Overall", "tbl.adm"="MAE by pop. Adm", "tbl.des"="MAE by pop. Des",  "tbl.net"="MAE by pop. Net")

for(table in c("tbl.adm", "tbl.des",  "tbl.net")){
  my_tbl <- get(table)
  
  erros <- my_tbl %>% dplyr::filter(source == "GVAR_IIS") %>% pull(value)
  FE.GVAR_IIS <-  mean(erros)
  FE.GVAR_IIS.sd <-  sd(erros)
  
  erros <- my_tbl %>% dplyr::filter(source == "GVAR") %>% pull(value)
  FE.GVAR <-  mean(erros)
  FE.GVAR.sd <-  sd(erros)
  
  erros <- my_tbl %>% dplyr::filter(source == "VECM") %>% pull(value)
  FE.VECM <-  mean(erros)
  FE.VECM.sd <-  sd(erros)
  
  erros <- my_tbl %>% dplyr::filter(source == "PCA") %>% pull(value)
  FE.PCA <-  mean(erros)
  FE.PCA.sd <-  sd(erros)
  
  
  metric <- c("RMSE.PCA" = FE.PCA^0.5,
              "RMSE.VECM" = FE.VECM^0.5,
              "RMSE.GVAR" = FE.GVAR^0.5,
              "RMSE.GVAR_IIS" = FE.GVAR_IIS^0.5)
  
  metric.sd <- c("RMSE.PCA" = FE.PCA.sd^0.5,
                 "RMSE.VECM" = FE.VECM.sd^0.5,
                 "RMSE.GVAR" = FE.GVAR.sd^0.5,
                 "RMSE.GVAR_IIS" = FE.GVAR_IIS.sd^0.5)
  
  cat(sprintf("\n%s", table))
  cat(sprintf("\n%13s = %f (%f)", names(metric), metric, metric.sd))
  
  
  # g <- my_tbl %>%
  #   dplyr::filter(Regiao %in% c("R404", "R346", "R280", "R191", "R491")) %>% 
  #   mutate(Regiao = factor(Regiao,
  #                          levels = c("R404", "R346", "R280", "R191", "R491"),
  #                          labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Porto Alegre") )) %>% 
  #   ggplot(aes(y=Regiao, x = value)) +
  #   geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
  #   # geom_point(size = 3, alpha = 0.15) +
  #   labs(title = "Forecast error - Mean Squared error",
  #        subtitle = legend.names[table],
  #        y = NULL,
  #        x = "Error squared"
  #   ) + 
  #   theme_bw()
  # 
  # print(g)
  # ggsave(filename = sprintf("%s - Main.png", legend.names[table]),
  #        path = "Graficos",
  #        plot = g,
  #        scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  # 
  # g <- my_tbl %>%
  #   dplyr::filter(Regiao %in% c("R404", "R346")) %>% 
  #   mutate(Regiao = factor(Regiao,
  #                          levels = c("R404", "R346", "R280", "R191", "R491"),
  #                          labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Porto Alegre") )) %>%    ggplot(aes(y=Regiao, x = value)) +
  #   geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
  #   # geom_point(size = 3, alpha = 0.15) +
  #   labs(title = "Forecast error - Mean Squared error",
  #        subtitle = legend.names[table],
  #        y = NULL,
  #        x = "Error squared"
  #   ) + 
  #   theme_bw()
  # 
  # print(g)
  # ggsave(filename = sprintf("%s - SP e RJ.png", legend.names[table]),
  #        path = "Graficos",
  #        plot = g,
  #        scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  # 
  # g <- my_tbl %>%
  #   dplyr::filter(Regiao %in% c("R151")) %>% 
  #   mutate(Regiao = factor(Regiao,
  #                          levels = c("R404", "R346", "R280", "R191", "R151"),
  #                          labels = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Recife", "Natal") )) %>%
  #   ggplot(aes(y=Regiao, x = value)) +
  #   geom_boxplot(aes(colour = source), outlier.alpha = 0.5) +
  #   # geom_point(size = 3, alpha = 0.15) +
  #   labs(title = "Forecast error - Mean Squared error",
  #        subtitle = legend.names[table],
  #        y = NULL,
  #        x = "Error squared"
  #   ) + 
  #   theme_bw()
  # 
  # print(g)
  # ggsave(filename = sprintf("%s - Natal.png", legend.names[table]),
  #        path = "Graficos",
  #        plot = g,
  #        scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
}



# Calculo de Diebold e Mariano --------------------------------------------
rm(list = setdiff(ls(), c("GVAR.IIS.error", "GVAR.error", "VECM.error", "PCA.error", "AR1.error", "AR13.error",
                          "AR1", "AR13","PCA","GVAR.IIS","GVAR","VECM", "my.levels", "my.labels")))

tbl <- bind_rows(GVAR.IIS.error, GVAR.error, VECM.error, PCA.error)

print_matrix <- function(x){
  
  length.max <- max(nchar(colnames(x)))
  
  # header <- paste(rep(paste("%", length.max, "s", sep=""), ncol(x)), collapse = " ")
  header <- paste(sprintf(paste("%", length.max, "s", sep = ""), c("",colnames(x))), collapse = " ")
  out.str <- "\n"
  out.str <- paste(out.str, header)
  
  for(i in seq_len(nrow(x))){
    line <- paste(c(sprintf(paste("%", length.max, "s", sep = ""), rownames(x)[i]),
                    sprintf(paste("%", length.max, ".4f", sep = ""), x[i,])), collapse = " ")
    out.str <- paste(out.str, "\n", line)
  }
  cat(out.str)
}


# dm.test(e1 = GVAR.IIS.error,  e2 = GVAR.error,  h = 1, power = 2)

tbl.overall <- tbl %>% dplyr::filter(Tipo != "EmpLiq")
tbl.adm <- tbl %>% dplyr::filter(Tipo == "Admitidos")
tbl.des <- tbl %>% dplyr::filter(Tipo == "Desligados")
tbl.net <- tbl %>% dplyr::filter(Tipo == "EmpLiq")

tbl.overall <- tbl.overall %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.adm <- tbl.adm %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.des <- tbl.des %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))
tbl.net <- tbl.net %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo", "source"))


Diebold.matrix <- matrix(NA, nrow = 4, ncol = 4)
rownames(Diebold.matrix) <- c("PCA", "VECM", "GVAR",  "GVAR_IIS")
colnames(Diebold.matrix) <- c("PCA", "VECM", "GVAR",  "GVAR_IIS")

for(table in c("tbl.adm", "tbl.des",  "tbl.net")){
  my_tbl <- get(table)
  
  for (i in 1:nrow(Diebold.matrix)) {
    for (j in 1:ncol(Diebold.matrix)) {
      
      row.label <- rownames(Diebold.matrix)[i]
      col.label <- colnames(Diebold.matrix)[j]
      
      tbl.e1 <- my_tbl %>% dplyr::filter(source == row.label)
      tbl.e2 <- my_tbl %>% dplyr::filter(source == col.label)
      
      if (row.label != col.label){
        # For alternative="greater", the alternative hypothesis is that method 2 is more accurate than method 1
        dm <- dm.test(e1 = tbl.e1$value,  e2 = tbl.e2$value,  h = 1, power = 2, alternative = "greater")
        
        Diebold.matrix[i, j] <- dm$p.value
      }
    }
    
  }
  
  cat(sprintf("\n\nTable: %s",table))
  cat(sprintf("\nAlternative hypothesis is that method on column is more accurate than method on row."))
  cat(sprintf("\nLow p-value means method on column is more accurate than method on row\n"))
  print_matrix(Diebold.matrix)
  
}




for(table in c("tbl.adm", "tbl.des",  "tbl.net")){
  my_tbl <- get(table)
  my_tbl <- my_tbl %>% dplyr::filter(Regiao == "R151")
  
  for (i in 1:nrow(Diebold.matrix)) {
    for (j in 1:ncol(Diebold.matrix)) {
      
      row.label <- rownames(Diebold.matrix)[i]
      col.label <- colnames(Diebold.matrix)[j]
      
      tbl.e1 <- my_tbl %>% dplyr::filter(source == row.label)
      tbl.e2 <- my_tbl %>% dplyr::filter(source == col.label)
      
      if (row.label != col.label){
        # For alternative="greater", the alternative hypothesis is that method 2 is more accurate than method 1
        dm <- dm.test(e1 = tbl.e1$value,  e2 = tbl.e2$value,  h = 1, power = 2, alternative = "greater")
        
        Diebold.matrix[i, j] <- dm$p.value
      }
    }
    
  }
  
  cat(sprintf("\n\nTable: %s",table))
  cat(sprintf("\nAlternative hypothesis is that method on column is more accurate than method on row."))
  cat(sprintf("\nLow p-value means method on column is more accurate than method on row\n"))
  print_matrix(Diebold.matrix)
}



# ANALISE BRASIL TODO -----------------------------------------------------
rm(list = setdiff(ls(), c("GVAR.IIS.error", "GVAR.error", "VECM.error", "PCA.error", "AR1.error", "AR13.error",
                          "AR1", "AR13","PCA","GVAR.IIS","GVAR","VECM", "my.levels", "my.labels")))

tbl <- bind_rows(GVAR.IIS.error, GVAR.error, VECM.error, PCA.error)
# Faz strip do numero da regiao e tipo


fn <- function(x){
  return(abs(x))
}

tbl.net <- tbl %>% dplyr::filter(Tipo == "EmpLiq") %>% 
  group_by(source) %>% summarise_if(.predicate = is.numeric, .funs = sum) %>% 
  mutate_if(.predicate = is.numeric, .funs = fn)

# tbl <- tbl %>% group_by(Tipo, source) %>% summarise_if(.predicate = is.numeric, .funs = sum) %>% ungroup()
# group_by(source) %>% 
# summarise_if(.predicate = is.numeric, .funs = sum) %>% ungroup()

erros <- AR1.error %>% summarise_if(.predicate = is.numeric, .funs = sum) %>% 
  mutate_if(.predicate = is.numeric, .funs = fn) %>% 
  mutate(source="AR1") %>% 
  pivot_longer(cols = -c("source")) %>% 
  pull(value)
MAFE.AR1 <-  mean(erros)
MAFE.AR1.sd <- sd(erros)


erros <- AR13.error %>% summarise_if(.predicate = is.numeric, .funs = sum) %>% 
  mutate_if(.predicate = is.numeric, .funs = fn) %>% 
  mutate(source="AR13") %>% 
  pivot_longer(cols = -c("source")) %>% 
  pull(value)
MAFE.AR13 <-  mean(erros)
MAFE.AR13.sd <- sd(erros)


tbl.net <- tbl.net %>%
  # dplyr::select(-variavel, -Regiao, -Tipo) %>% 
  # mutate_if(is.numeric, .funs = fn) %>% 
  pivot_longer(cols = -c("source"))

legend.names <- c("tbl.overall"="MAE Overall", "tbl.adm"="MAE Adm", "tbl.des"="MAE Des",  "tbl.net"="MAE Net")

table <- "tbl.net"
my_tbl <- get(table)

erros <- my_tbl %>% dplyr::filter(source == "GVAR_IIS") %>% pull(value)
MAFE.GVAR_IIS <-  mean(erros)
MAFE.GVAR_IIS.sd <- sd(erros)

erros <- my_tbl %>% dplyr::filter(source == "GVAR") %>% pull(value)
MAFE.GVAR <-  mean(erros)
MAFE.GVAR.sd <- sd(erros)

erros <- my_tbl %>% dplyr::filter(source == "VECM") %>% pull(value)
MAFE.VECM <-  mean(erros)
MAFE.VECM.sd <- sd(erros)

erros <- my_tbl %>% dplyr::filter(source == "PCA") %>% pull(value)
MAFE.PCA <-  mean(erros)
MAFE.PCA.sd <- sd(erros)

# metric <- c("MAFE.PCA" = MAFE.PCA^0.5,
#             "MAFE.VECM" = MAFE.VECM^0.5,
#             "MAFE.GVAR" = MAFE.GVAR^0.5,
#             "MAFE.GVAR_IIS" = MAFE.GVAR_IIS^0.5,
#             "MAFE.AR1" = MAFE.AR1^0.5,
#             "MAFE.AR13" = MAFE.AR13^0.5)

metric <- c("MAFE.PCA" = MAFE.PCA,
            "MAFE.VECM" = MAFE.VECM,
            "MAFE.GVAR" = MAFE.GVAR,
            "MAFE.GVAR_IIS" = MAFE.GVAR_IIS,
            "MAFE.AR1" = MAFE.AR1,
            "MAFE.AR13" = MAFE.AR13)

metric.sd <- c(MAFE.PCA.sd,
               MAFE.VECM.sd,
               MAFE.GVAR.sd,
               MAFE.GVAR_IIS.sd,
               MAFE.AR1.sd,
               MAFE.AR13.sd)

cat(sprintf("\n%s", table))
cat(sprintf("\n%13s = %f  (%f)", names(metric), metric, metric.sd))


my_tbl$Date <- str_split(my_tbl$name, "\\_", simplify = TRUE)[,2]
my_tbl$Date <- ymd(my_tbl$Date, truncated = 1)

my_tbl %>% dplyr::filter(source %in% c("GVAR", "GVAR_IIS", "VECM")) %>% 
  ggplot() + 
  geom_line(aes(x=Date, y=value, colour=source), size = 1) + 
  geom_point(aes(x=Date, y=value, colour=source)) + 
  geom_text(aes(x=Date, y=value, colour=source, label=month(Date)), position = position_jitter(width = 5, height = 30000)) +
  theme_bw()



my_tbl <- GVAR.IIS %>% pivot_longer(cols = -c("variavel", "Regiao", "Tipo"))
my_tbl$Date <- str_split(my_tbl$name, "\\_", simplify = TRUE)[,2]
my_tbl$Date <- ymd(my_tbl$Date, truncated = 1)
my_tbl$name2 <- str_split(my_tbl$name, "\\_", simplify = TRUE)[,1]


my_tbl <- my_tbl %>% dplyr::filter(name2 %in% c("Actual", "Forecast"))

my_tbl %>% group_by(Date, Tipo, name2) %>% summarise(value=sum(value)) %>% 
  pivot_wider(names_from = Tipo, values_from = value) %>% 
ggplot( ) + 
  geom_line(aes(x=Date, y = EmpLiq, colour=name2))
