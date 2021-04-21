# Setup -------------------------------------------------------------------
rm(list=ls())

library(jsonlite)
library(dplyr)
library(lubridate)


url <- "http://www.ipeadata.gov.br/api/odata4/Metadados"
tbl1 = jsonlite::fromJSON(url)
aa <- tbl1$value %>% as_tibble()


"http://www2.bmf.com.br/pages/portal/bmfbovespa/lumis/lum-taxas-referenciais-bmf-ptBR.asp?Data=18/04/2021"


