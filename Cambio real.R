
# Setup -------------------------------------------------------------------

rm(list = ls())

# library(stringr)
library(dplyr)
library(jsonlite)
library(readr)

geometric.mean <- function(x, na.rm=TRUE){
  ret <- exp(mean(log(x), na.rm=na.rm))
  return(ret)
}


# Dataload ----------------------------------------------------------------

url.mask <- "http://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=%s&dataInicial=%s&dataFinal=%s"

# 11752	Índice da taxa de câmbio real efetiva (IPCA) - Jun/1994=100
Serie.Id <- 11752

url <- sprintf(url.mask, Serie.Id, "json",
               stringr::str_replace_all(string = format(as.Date("1995-01-01"), "%d/%m/%Y"),
                                        pattern = "/",
                                        replacement = "%2F"),
               stringr::str_replace_all(string = format(as.Date("2021-02-01"), "%d/%m/%Y"),
                                        pattern = "/",
                                        replacement = "%2F"))

tbl1 <- jsonlite::fromJSON(url)

tbl1$data = as.Date(tbl1$data, format = "%d/%m/%Y")
tbl1$valor = as.numeric(tbl1$valor)

library(ggplot2)
tbl1 %>% 
ggplot() + geom_line(aes(x=data, y=valor))

tail(tbl1)

tbl1 <- tbl1 %>% as_tibble() %>% mutate(ln_cambio = log(valor))

write_excel_csv(x = tbl1, file = "Cambio real.csv")
