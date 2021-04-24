# Setup -------------------------------------------------------------------
rm(list = ls())

library(readr)
library(dplyr)
library(lubridate)

geometric.mean <- function (x, na.rm=TRUE) {
  ret = exp(mean(log(x)))
  return(ret)
}

# Dataload ----------------------------------------------------------------
url.mask <- "http://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=%s&dataInicial=%s"

# 11	Taxa de juros - Selic
Serie.Id <- 11
url <- sprintf(url.mask, Serie.Id, "csv", format(Sys.Date(), "%d/%m/%Y"))
destination.file <- "Selic.csv"
download.file(url = url, destfile = destination.file, mode = "w")

# Leitura do arquivo ------------------------------------------------------

tbl <- read_delim(file = destination.file, delim = ";", escape_double = FALSE,
                  col_types = cols(data = col_date(format = "%d/%m/%Y")),
                  locale = locale(decimal_mark = ",", grouping_mark = "."),
                  trim_ws = TRUE)

head(tbl)
tail(tbl)


# Construcao das taxas mensais --------------------------------------------

tbl2 <- tbl %>% mutate(Ano = year(data),
               Mes = month(data),
               valor2 = 1 + valor/100) %>% 
  group_by(Ano, Mes) %>% 
  summarise(selic = geometric.mean(valor2)) %>% 
  ungroup() %>% 
  mutate(Data = ymd(Ano*10000+Mes*100+1)) %>% 
  dplyr::filter(Ano >= 1995) %>% 
  mutate(ln_Selic_aa = log(selic)*252)


readr::write_excel_csv(x=tbl2, file = "selic.csv")
