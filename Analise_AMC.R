# Clear all
rm(list = ls())

# Carrega dados dos municipios
load(file = "./dados.full.Rdata")

# libraries
library(readxl)
library(dplyr)
library(tibble)
library(tidyr)
library(urca)

# mostra dados dos municipios
head(dados.full)

# carrega dados das AMCs
# amc <- read_excel("Database/Amcs_70_91_00_v4.xlsx", range = cell_limits(c(1,1), c(NA,3)))
amc <- read_excel("Database/Amcs_70_91_00_v5.xlsx",
                  sheet = 2,
                  range = cell_limits(c(2,1), c(NA,3)))


# Retirar codigos ignorados, 99 e 0
dados.full <- dados.full %>% filter(!(Municipio %in% c(99, 0)))

# Tabela com relacao de dados ignorados
table <- tibble(cod=0, desc="NA", .rows = 27)
table$cod <- c(119999, 129999, 139999, 149999, 159999, 169999, 
               179999, 219999, 229999, 239999, 249999, 259999,
               269999, 279999, 289999, 299999, 319999, 329999, 
               339999, 359999, 419999, 429999, 439999, 509999,
               519999, 529999, 539999)

table$desc <- c("Ro-Ignorado","Ac-Ignorado","Am-Ignorado","Rr-Ignorado","Pa-Ignorado","Ap-Ignorado",
                "To-Ignorado","Ma-Ignorado","Pi-Ignorado","Ce-Ignorado","Rn-Ignorado","Pb-Ignorado",
                "Pe-Ignorado","Al-Ignorado","Se-Ignorado","Ba-Ignorado","Mg-Ignorado","Es-Ignorado",
                "Rj-Ignorado","Sp-Ignorado","Pr-Ignorado","Sc-Ignorado","Rs-Ignorado","Ms-Ignorado",
                "Mt-Ignorado","Go-Ignorado","Df-Ignorado")

# Retira da amostra os dados ignorados
dados.full <- dados.full %>% filter(!(Municipio %in% table$cod))

# Junta os dados com os codigos das amcs
dados.full <- dados.full %>% left_join(amc, by = c("Municipio" = "munic"))

# Agrupa os dados em uma unica tabela
dados.full.group <- dados.full %>%
  group_by(amc7000, ano) %>%
  summarise(N_ini=sum(Nao_admitido_ano), N_fim=sum(Nao_desligado_ano))

# Calcula os erros de divulgaçao a cada ano
dados.full.wide <- pivot_wider(dados.full.group,
            names_from = "ano",
            values_from = c("N_ini", "N_fim"),
            id_cols = amc7000,
            names_sep = ".") %>% 
   mutate(erro.1986 = N_ini.1986 - N_fim.1985,
                                                erro.1987 = N_ini.1987 - N_fim.1986,
                                                erro.1988 = N_ini.1988 - N_fim.1987,
                                                erro.1989 = N_ini.1989 - N_fim.1988,
                                                erro.1990 = N_ini.1990 - N_fim.1989,
                                                
                                                erro.1991 = N_ini.1991 - N_fim.1990,
                                                erro.1992 = N_ini.1992 - N_fim.1991,
                                                erro.1993 = N_ini.1993 - N_fim.1992,
                                                erro.1994 = N_ini.1994 - N_fim.1993,
                                                erro.1995 = N_ini.1995 - N_fim.1994,
                                                erro.1996 = N_ini.1996 - N_fim.1995,
                                                erro.1997 = N_ini.1997 - N_fim.1996,
                                                erro.1998 = N_ini.1998 - N_fim.1997,
                                                erro.1999 = N_ini.1999 - N_fim.1998,
                                                erro.2000 = N_ini.2000 - N_fim.1999,
                                                
                                                erro.2001 = N_ini.2001 - N_fim.2000,
                                                erro.2002 = N_ini.2002 - N_fim.2001,
                                                erro.2003 = N_ini.2003 - N_fim.2002,
                                                erro.2004 = N_ini.2004 - N_fim.2003,
                                                erro.2005 = N_ini.2005 - N_fim.2004,
                                                erro.2006 = N_ini.2006 - N_fim.2005,
                                                erro.2007 = N_ini.2007 - N_fim.2006,
                                                erro.2008 = N_ini.2008 - N_fim.2007,
                                                erro.2009 = N_ini.2009 - N_fim.2008,
                                                erro.2010 = N_ini.2010 - N_fim.2009,
                                                
                                                erro.2011 = N_ini.2011 - N_fim.2010,
                                                erro.2012 = N_ini.2012 - N_fim.2011,
                                                erro.2013 = N_ini.2013 - N_fim.2012,
                                                erro.2014 = N_ini.2014 - N_fim.2013,
                                                erro.2015 = N_ini.2015 - N_fim.2014,
                                                erro.2016 = N_ini.2016 - N_fim.2015,
                                                erro.2017 = N_ini.2017 - N_fim.2016,
                                                erro.2018 = N_ini.2018 - N_fim.2017)



dados.deltas <- dados.full.wide %>% 
  select( c("amc7000", paste("erro", 1986:2018, sep = ".")) )

summary(dados.deltas)

# Analise de dados com 
sum(!complete.cases(dados.deltas))


i=1
for(i in 1:nrow(dados.deltas)){
  x <- ts(t(as.matrix(dados.deltas[i, -1])), start = 1986, end=2018, frequency = 1)
  
  # se existe NA na serie, considero como erro zerado.
   if(sum(is.na(x)) > 0){
     cat(sprintf("\nSerie %d contem NA", i));
     x[is.na(x)] <- 0
   }
  
  # Se a serie so tem zeros entao não ha problemas.
  if( sum(x^2) == 0) { cat(sprintf("\nSerie %d esta zerada", i)); next(); }
  
  ADF.none <- ur.df(x, type = "none", lags = 0)
  ADF.drift <- ur.df(x, type = "drift", lags = 0)
  
  dados.deltas[i, "tau1"] = ADF.none@teststat
  dados.deltas[i, "tau1.p"] = ADF.none@teststat > -1.95
  
  dados.deltas[i, "tau2"] = ADF.drift@teststat[1]
  dados.deltas[i, "tau2.p"] = ADF.drift@teststat[1] > - 2.89
  
  dados.deltas[i, "phi1"] = ADF.drift@teststat[2] 
  dados.deltas[i, "phi1.p"] = ADF.drift@teststat[2] < 4.71
  
  rm(list = c("x", "ADF.none", "ADF.drift"))
}


cat(sprintf("\ndrift is not null: %d\n", sum(dados.deltas$phi1.p)))
cat(sprintf("\ntau is not null (drift test): %d\n", sum(dados.deltas$tau2.p)))
cat(sprintf("\ntau and drift is not null: %d\n", sum(dados.deltas$tau2.p & dados.deltas$phi1.p) ))
cat(sprintf("\ntau is not null: %d\n", sum(dados.deltas$tau1.p)))
