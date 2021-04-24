
# Setup -------------------------------------------------------------------

rm(list=ls())

library(dplyr)
library(readr)
library(fredr)

FRED_API_KEY="7e0e127fe6a68fc900e584736c80b168"

fredr_set_key(FRED_API_KEY)


tbl <- fredr(series_id = "POILBREUSDM",
             observation_start = as.Date("1995-01-01"),
             observation_end = as.Date("2021-01-01"))


tbl <- tbl %>% select(date, value) %>% mutate(ln_oil = log(value))

readr::write_excel_csv(x = tbl, file = "oil.csv")
