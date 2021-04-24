# Script para download dos dados do IPCA

# Setup -------------------------------------------------------------------

library(sidrar)
library(stringi)
library(dplyr)
library(lubridate)

# Dataload ----------------------------------------------------------------

# help em http://api.sidra.ibge.gov.br/
IPCA.tbl <- get_sidra(api = "/t/1737/v/2266/p/all/N1/1")

# Data preparation --------------------------------------------------------

# padroniza o nome das colunas
colnames(IPCA.tbl) <- stringi::stri_replace_all(str = stringi::stri_trans_general(colnames(IPCA.tbl), "latin-ascii"),
                                                replacement = "_",
                                                regex = "\\s")


# seleciona apenas as colunas de interesse
IPCA.tbl <- IPCA.tbl %>% 
  dplyr::select(YearMonth="Mes_(Codigo)",
                Valor="Valor")

# acerta as datas de referencia
IPCA.tbl$Data <- lubridate::ymd(IPCA.tbl$YearMonth, truncated = 1)

# coloca as colunas em ordem
IPCA.tbl <- IPCA.tbl %>% dplyr::select(Data, YearMonth, Valor)


IPCA.tbl <- IPCA.tbl %>% dplyr::filter(Data >= as.Date("1995-01-01")) %>% 
  mutate(ln_ICPA = log(Valor))


readr::write_excel_csv(x = IPCA.tbl, file = "IPCA.csv")
