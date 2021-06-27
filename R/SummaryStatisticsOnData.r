
# Setup -------------------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2) 


# MACROVARIAVEIS ----------------------------------------------------------
rm(list = ls())

db <- read_csv("~/GitHub/Tese/Ox Metrics GVAR/Database/MacroVariables_for Summary.csv", 
               col_types = cols(Data = col_date(format = "%YM%m"), 
                                Dln_IPCA = col_skip()))
head(db)

library(moments)


db <- db %>%
  mutate(Pim_BR = exp(lpim_BR),
         Selic_aa = exp(ln_Selic_aa),
         IPCA = exp(ln_IPCA))

col <- "Selic_aa"
for (col in c("Pim_BR", "Selic_aa", "IPCA", "EmpLiq")) {
  
  mu <- mean(db[[col]])
  md <- median(db[[col]])
  max <- max(db[[col]])
  min <- min(db[[col]])
  sd <- sd(db[[col]])
  sk <- moments::skewness(db[[col]])
  ku <- moments::kurtosis(db[[col]])
  jb <- moments::jarque.test(db[[col]])
  
  line <- c(mu, md, max, min, sd, sk, ku)
  
  cat("\n", col, " & ", paste(format(line, digits = 1, nsmall = 2, scientific = FALSE, big.mark = " "), collapse = " & "), "\\\\")
}


ggplot(db) + 
  geom_line(aes(x=Data, y = EmpLiq))

ggplot(db) + 
  geom_line(aes(x=Data, y = IPCA))

ggplot(db) + 
  geom_line(aes(x=Data, y = Selic_aa))




# VARIAVEIS REGIONAIS -----------------------------------------------------
rm(list = ls())

db <- read_csv("~/GitHub/Tese/Ox Metrics GVAR/Database/DatabaseDesAdm_RA_v1.csv", 
               col_types = cols(X1 = col_date(format = "%YM%m")))

head(db)

library(lubridate)
library(tidyr)
library(stringr)

db <- db %>% pivot_longer(cols = -X1)

db$Serie <- str_split(db$name, "\\_", simplify = TRUE)[,2]
db$regiao <- str_split(db$name, "\\_", simplify = TRUE)[,1]

db.agregado <- db %>% group_by(X1,Serie) %>% summarise(q=sum(value)) %>%
  pivot_wider(names_from = "Serie", values_from = q)


ggplot(db.agregado) + 
  geom_line(aes(x=X1, y = Admitidos, colour="Admitted")) + 
  geom_line(aes(x=X1, y = Desligados, colour="Desligado")) + 
  labs()



Myseries <- c("Admitted", "Discharged")
names(Myseries) <- c("Admitidos", "Desligados")


tbl <- db.agregado
for (col in c("Admitidos", "Desligados")) {
  
  mu <- mean(tbl[[col]])
  md <- median(tbl[[col]])
  max <- max(tbl[[col]])
  min <- min(tbl[[col]])
  sd <- sd(tbl[[col]])
  sk <- moments::skewness(tbl[[col]])
  ku <- moments::kurtosis(tbl[[col]])
  jb <- moments::jarque.test(tbl[[col]])
  
  line <- c(mu, md, max, min, sd, sk, ku)
  
  cat("\n", "Global", "&", Myseries[col], " & ", paste(format(line, digits = 2, nsmall = 2, scientific = FALSE, big.mark = " "), collapse = " & "), "\\\\")
}


Mynames <- c("Sp-São Paulo", "Rj-Rio de Janeiro", "Mg-Belo Horizonte", "Df-Brasília")
names(Mynames) <- c(404, 346, 280, 552)




for (reg_number in c(404, 346, 280, 552)){
  tbl <- db %>%
    filter(regiao == sprintf("R%d",reg_number)) %>%
    select(Date=X1, Serie, value) %>% 
    pivot_wider(names_from = Serie, values_from = value)
  for (col in c("Admitidos", "Desligados")) {
    
    mu <- mean(tbl[[col]])
    md <- median(tbl[[col]])
    max <- max(tbl[[col]])
    min <- min(tbl[[col]])
    sd <- sd(tbl[[col]])
    sk <- moments::skewness(tbl[[col]])
    ku <- moments::kurtosis(tbl[[col]])
    jb <- moments::jarque.test(tbl[[col]])
    
    line <- c(mu, md, max, min, sd, sk, ku)
    
    cat("\n",Mynames[as.character(reg_number)], "&", Myseries[col], " & ", paste(format(line, digits = 2, nsmall = 2, scientific = FALSE, big.mark = " "), collapse = " & "), "\\\\")
  }
  
}


