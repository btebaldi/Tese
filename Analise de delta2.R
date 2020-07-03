# Clear all
rm(list = ls())

# load file
load(file = "./dados.full.Rdata")

# libraries
library(dplyr)
library(tidyr)
library(urca)

# Cria coluna de lag de ano
dados.full <- dados.full %>% dplyr::mutate(nivel_ant = 0)
colnames(dados.full) = c("Municipio", "N_ini", "N_fim", "ano")

dados.full.wide <- pivot_wider(dados.full, names_from = "ano", values_from = c("N_ini", "N_fim"), id_cols = Municipio, names_sep = ".")

dados.full.wide <- dados.full.wide %>% mutate(erro.1986 = N_ini.1986 - N_fim.1985,
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

summary(dados.full.wide)


deltas <- dados.full.wide[,c("Municipio", paste("erro", 1986:2018, sep = "."))]

i=1
for(i in 1:nrow(deltas)){
  x <- ts(t(as.matrix(deltas[i, paste("erro", 1986:2018, sep = ".")])), start = 1985, end=2018, frequency = 1)
  
  x[is.na(x)] <- 0 
  if( sum(x^2) == 0) { cat(sprintf("%d\n", i)); next(); }
  
  ADF.none <- ur.df(x, type = "none", lags = 0)
  ADF.drift <- ur.df(x, type = "drift", lags = 0)
  
  deltas[i, "tau1"] = ADF.none@teststat
  deltas[i, "tau1.p"] = ADF.none@teststat > -1.95
  
  deltas[i, "tau2"] = ADF.drift@teststat[1]
  deltas[i, "tau2.p"] = ADF.drift@teststat[1] > - 2.89
  
  deltas[i, "phi1"] = ADF.drift@teststat[2] 
  deltas[i, "phi1.p"] = ADF.drift@teststat[2] < 4.71
  
  rm(list = c("x", "ADF.none", "ADF.drift"))
}

summary(deltas)

deltas.noNa = deltas[!is.na(deltas$tau1.p), ]

cat(sprintf("\nTrend is not null: %d\n", sum(deltas.noNa$phi1.p)))
cat(sprintf("\ntau in trend is not null: %d\n", sum(deltas.noNa$tau2.p)))
cat(sprintf("\ntau is not null: %d\n", sum(deltas.noNa$tau1.p)))



