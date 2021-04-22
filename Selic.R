
# Setup -------------------------------------------------------------------

rm(list = ls())

library(stringr)
library(dplyr)

geometric.mean <- function(x, na.rm=TRUE){
  ret <- exp(mean(log(x), na.rm=na.rm))
  return(ret)
}


# Dataload ----------------------------------------------------------------

url.mask <- "http://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=%s&dataInicial=%s&dataFinal=%s"

# 11 - Selic a.d.
# 11 - Selic a.a.
Serie.Id <- 1178

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

tbl1$valor2 = (1 + tbl1$valor/100)
tbl1$YearMonth <- lubridate::ymd(lubridate::year(tbl1$data)*1e4+lubridate::month(tbl1$data)*100+1)
tail(tbl1)


tbl2 <- tbl1 %>% 
  group_by(YearMonth) %>% 
  summarise(valor2 = geometric.mean(valor2))


library(ggplot2)

tbl1 %>% ggplot() +
  geom_line(aes(x=YearMonth, y=valor2)) + 
  geom_point(aes(x=YearMonth, y=valor2)) + 
  geom_point(aes(x=YearMonth, y=valor2), colour="red",data = tbl2)

# download.file(url = url, destfile = "BC.csv", mode = "w")

# Leitura do arquivo-------------------------------
library(readr)

tbl <- read_delim(file = "BC.csv", delim = ";", escape_double = FALSE,
                  col_types = cols(data = col_date(format = "%d/%m/%Y")),
                  locale = locale(decimal_mark = ",", grouping_mark = "."),
                  trim_ws = TRUE)

View(tbl)