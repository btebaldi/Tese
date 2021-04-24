
# Setup -------------------------------------------------------------------
rm(list=ls())

library(jsonlite)
library(dplyr)
library(lubridate)
library(readr)


# Aquisicao das series ----------------------------------------------------


url.mask <- "http://www.ipeadata.gov.br/api/odata4/Metadados('%s')/Valores"

# PIMPFN12_QIIGN12
# PIM-PF - Produção industrial - indústria geral - Índice de base fixa (média 2012 = 100)
# Macroeconomic
# IBGE/PIM-PF
# Monthly (2002.01-2021.02)
Serie.code1 <- "PIMPFN12_QIIGN12"

# PIMPFN12_QIIG12
# Produção industrial - indústria geral - quantum - índice (média 2002 = 100)
# Macroeconomic
# IBGE/PIM-PF antiga
# Monthly
# Monthly (1975.01-2014.02)
Serie.code2 <- "PIMPFN12_QIIG12"

url <- sprintf(url.mask, Serie.code1)
PIM.2021 <- jsonlite::fromJSON(url)

url <- sprintf(url.mask, Serie.code2)
PIM.2014 <- jsonlite::fromJSON(url)

# faz o download das series da PIM
PIM.2021 <- PIM.2021$value %>% as_tibble() %>% transmute(Data=as.Date(VALDATA), Valor = VALVALOR)
PIM.2014 <- PIM.2014$value %>% as_tibble() %>% transmute(Data=as.Date(VALDATA), Valor = VALVALOR)

# Constroi o range de datas
date.range <- range(range(PIM.2021$Data), range(PIM.2014$Data))

#  junta em uma unica tabela as duas series da PIM
tbl <- tibble(Data=seq(from=date.range[1], to=date.range[2], by="month"),
       Pim=as.numeric(NA))

tbl <- tbl %>%
  left_join(PIM.2021, by=c("Data"="Data")) %>%
  left_join(PIM.2014, by=c("Data"="Data"),suffix=c(".2021", ".2014")) %>% 
  mutate(factor=Valor.2014/dplyr::lead(Valor.2014))

# calcula uma serie unica da PIM considerando as variacoes da primeira 
select_vector <- tbl$Data < as.Date("2002-01-01")
firstValue <- tbl %>% filter(Data==as.Date("2002-01-01")) %>% pull(Valor.2021)

tbl$factor[select_vector] <- rev(exp(cumsum(log(rev(tbl$factor[select_vector])))))

tbl$Pim[!select_vector] <- tbl$Valor.2021[!select_vector]
tbl$Pim[select_vector] <- tbl$factor[select_vector] * firstValue

# Seleciona apenas a coluna que importa
tbl <- tbl %>% dplyr::select(Data, Pim)


# Salva os dados
readr::write_excel_csv(x = tbl, file = "PIM.csv")
