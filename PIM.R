
# Setup -------------------------------------------------------------------
rm(list=ls())

library(jsonlite)
library(dplyr)
library(lubridate)



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

PIM.2021 <- PIM.2021$value %>% as_tibble() %>% transmute(Data=as.Date(VALDATA), Valor = VALVALOR)
PIM.2014 <- PIM.2014$value %>% as_tibble() %>% transmute(Data=as.Date(VALDATA), Valor = VALVALOR)


date.range <- range(range(PIM.2021$Data), range(PIM.2014$Data))

tbl <- tibble(Data=seq(from=date.range[1], to=date.range[2], by="month"),
       Pim=as.numeric(NA))


tbl <- tbl %>%
  left_join(PIM.2021, by=c("Data"="Data")) %>%
  left_join(PIM.2014, by=c("Data"="Data"),suffix=c(".2021", ".2014")) %>% 
  mutate(factor=Valor.2014/dplyr::lead(Valor.2014))


select_vector <- tbl$Data < as.Date("2002-01-01")
firstValue <- tbl %>% filter(Data==as.Date("2002-01-01")) %>% pull(Valor.2021)

tbl$factor[select_vector] <- rev(exp(cumsum(log(rev(tbl$factor[select_vector])))))

tbl$Pim[!select_vector] <- tbl$Valor.2021[!select_vector]
tbl$Pim[select_vector] <- tbl$factor[select_vector] * firstValue

url <- sprintf(url.mask, Serie.code)

tbl1 = jsonlite::fromJSON(url)
str(tbl1)

tbl2 <- tbl1$value %>% as_tibble() 

tbl2 %>% transmute(Data=as.Date(VALDATA), Valor = VALVALOR) %>% 
  filter(Data == as.Date("2013-12-01")) %>% pull(Valor)









tbl2$VALDATA %>% transmute(Data=lubridate::ymd(VALDATA), valor=VALVALOR)
http://www.ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='ABATE_ABPESU')


2




library(ipeadatar)
library(stringr)

tbl <- available_series(language = c("en", "br"))
tbl2 <- tbl %>%
  filter(str_detect(name, "[Pp]rodu")) %>%
  filter(str_detect(name, "[iI]ndu")) %>% 
  filter(str_detect(name, "indústria geral"))
