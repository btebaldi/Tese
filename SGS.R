url.mask <- "http://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=%s&amp;dataInicial=%s"

# 24363 - IbcBr
Serie.Id <- 24363
url <- sprintf(url.mask, Serie.Id, "csv", format(Sys.Date(), "%d/%m/%Y"))

download.file(url = url, destfile = "BC.csv", mode = "w")

# Leitura do arquivo-------------------------------
library(readr)

tbl <- read_delim(file = "BC.csv", delim = ";", escape_double = FALSE,
                  col_types = cols(data = col_date(format = "%d/%m/%Y")),
                  locale = locale(decimal_mark = ",", grouping_mark = "."),
                  trim_ws = TRUE)

View(tbl)