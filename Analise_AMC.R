# Clear all
rm(list = ls())

# Carrega dados dos municipios
load(file = "./Database/dados.1995plus.Rdata")

# libraries
library(readxl)
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(urca)

# mostra dados dos municipios
head(dados.1995plus)

# carrega dados das AMCs
# amc <- read_excel("Database/Amcs_70_91_00_v4.xlsx", range = cell_limits(c(1,1), c(NA,3)))
amc <- read_excel("Database/Amcs_91_v1.xlsx",
                  sheet = 1,
                  range = "A1:F5661")

# Retirar codigos ignorados, 99 e 0
dados.1995plus <- dados.1995plus %>% filter(!(Municipio %in% c(99, 0)))

# dados.1995plus %>% filter(Municipio %in% c(99, 0))

# Tabela com relacao de dados ignorados
tbl.ignorados <- tibble(cod=0, desc="NA", .rows = 29)
tbl.ignorados$cod <- c(119999, 129999, 139999, 149999, 159999, 169999, 
                       179999, 219999, 229999, 239999, 249999, 259999,
                       269999, 279999, 289999, 299999, 319999, 329999, 
                       339999, 359999, 419999, 429999, 439999, 509999,
                       519999, 529999, 539999,
                       99, 0)

tbl.ignorados$desc <- c("Ro-Ignorado","Ac-Ignorado","Am-Ignorado","Rr-Ignorado","Pa-Ignorado","Ap-Ignorado",
                        "To-Ignorado","Ma-Ignorado","Pi-Ignorado","Ce-Ignorado","Rn-Ignorado","Pb-Ignorado",
                        "Pe-Ignorado","Al-Ignorado","Se-Ignorado","Ba-Ignorado","Mg-Ignorado","Es-Ignorado",
                        "Rj-Ignorado","Sp-Ignorado","Pr-Ignorado","Sc-Ignorado","Rs-Ignorado","Ms-Ignorado",
                        "Mt-Ignorado","Go-Ignorado","Df-Ignorado",
                        "Total1","Total2")

# save(tbl.ignorados, file = "./Database/tblIgnorados.RData")
# Retira da amostra os dados ignorados
dados.1995plus <- dados.1995plus %>% filter(!(Municipio %in% tbl.ignorados$cod))
# dados.1995plus %>% filter(Municipio %in% tbl.ignorados$cod)


# ADICIONO MUNICIPIOS QUE NAO DECLARARAM NADA
# Essa adicao é decorrente de investigacao de regiao que nao declarou nada.
# O codigo para deteccao desta regiao pode ser encontrado mais abaixo.
dados.1995plus <- dados.1995plus %>% add_row(Municipio=293245,
                                             Nao_admitido_ano=0,
                                             Nao_desligado_ano=0,
                                             ano=1998)

dados.1995plus <- dados.1995plus %>% add_row(Municipio=292303,
                                             Nao_admitido_ano=0,
                                             Nao_desligado_ano=0,
                                             ano=c(1997, 1998))

# Junta os dados com os codigos das amcs
dados.1995plus <- dados.1995plus %>% left_join(amc, by = c("Municipio" = "munic"))

# Verifica se há regioes sem AMC associada
unique(dados.1995plus[is.na(dados.1995plus$amc), "Municipio"])

# Mostra o total de amc em cada ano
dados.1995plus %>%
  group_by(ano) %>%
  summarize(dist_amc = n_distinct(amc))


# Procedimento prar investigar municipios que nao declararam nada
for (j in 1995:2018) {
  anoj <- dados.1995plus %>% filter(ano == j) %>% select(amc)
  
  for (i in 1995:2018) {
    ano <- dados.1995plus %>% filter(ano == i) %>% select(amc)  
    
    cat(sprintf("\nNao esta em %d: %d", i, dplyr::setdiff(anoj$amc, ano$amc)))
    cat(sprintf("\nNao esta em %d: %d", j, dplyr::setdiff(ano$amc, anoj$amc)))
  }
}
rm(list = c("anoj", "ano", "i", "j"))

# Detectado duas AMCS que nao tem informacao declarada
# AMCs detectada como ausente
# 293245 Umburanas      AMC: 293245 (População estimada [2019]	19.222 pessoas)
# 292303 Novo Horizonte AMC: 292303 (População estimada [2019]	12.385 pessoas)

# Essas informações foram consideradas como declaração zerada nos anos faltantes.


# --------- Continuar analise ------------



# Agrupa os dados em uma unica tabela
dados.full.group <- dados.1995plus %>%
  group_by(amc, ano) %>%
  summarise(N_ini=sum(Nao_admitido_ano), N_fim=sum(Nao_desligado_ano), pop = sum(Populacao))



# Padronizo para a populacao de 2018
for (amc in unique(dados.full.group$amc)) {
  dados.full.group[dados.full.group$amc==amc, "pop"]<- dados.full.group[dados.full.group$amc==amc & dados.full.group$ano==2018, "pop"]
}


# Calcula os erros de divulgaçao a cada ano
dados.full.wide <- pivot_wider(dados.full.group,
                               names_from = "ano",
                               values_from = c("N_ini", "N_fim"),
                               id_cols = c("amc", "pop"),
                               # id_cols = "amc",
                               names_sep = ".") %>% 
  mutate(
    # erro.1986 = N_ini.1986 - N_fim.1985,
    # erro.1987 = N_ini.1987 - N_fim.1986,
    # erro.1988 = N_ini.1988 - N_fim.1987,
    # erro.1989 = N_ini.1989 - N_fim.1988,
    # erro.1990 = N_ini.1990 - N_fim.1989,
    # erro.1991 = N_ini.1991 - N_fim.1990,
    # erro.1992 = N_ini.1992 - N_fim.1991,
    # erro.1993 = N_ini.1993 - N_fim.1992,
    # erro.1994 = N_ini.1994 - N_fim.1993,
    # erro.1995 = N_ini.1995 - N_fim.1994,
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
  select( c("amc", "pop", paste("erro", 1996:2018, sep = ".")) )

summary(dados.deltas)


# Analise de dados com 
sum(!complete.cases(dados.deltas))

# Estimativa da popuulacao brasileira
sum(dados.deltas$pop)

i=1
for(i in 1:nrow(dados.deltas)){
  x <- ts(t(as.matrix(dados.deltas[i, -1])), start = 1996, end=2018, frequency = 1)
  
  # plot(x)
  
  # se existe NA na serie, considero como erro zerado.
  if(sum(is.na(x)) > 0){
    cat(sprintf("\nSerie %d contem NA", i));
    x[is.na(x)] <- 0
  }
  
  # Se a serie so tem zeros entao não ha problemas.
  if( sum(x^2) == 0) { cat(sprintf("\nSerie %d esta zerada", i)); next(); }
  
  ADF.none <- ur.df(x, type = "none", lags = 1)
  ADF.drift <- ur.df(x, type = "drift", lags = 1)
  ADF.trend <- ur.df(x, type = "trend", lags = 1)
  
  # print(ADF.none@lags)
  # print(ADF.drift@lags)
  
  dados.deltas[i, "Unit.Root"] = as.logical(NA)
  dados.deltas[i, "Criterio"] = as.numeric(NA)
  
  if(ADF.trend@teststat[1] < (-3.60)){
    # se temos rejeicao da nula entao nao tem os unit root
    dados.deltas[i, "Unit.Root"] = FALSE
    dados.deltas[i, "Criterio"] = 1
  } else {
    if(ADF.trend@teststat[3] < 7.24) {
      # aqui a trend nao é significante, entao devemos estimar o modelo drift
      
      if(ADF.drift@teststat[1] < (-3.00)){
        #  temos rejeicao da nula e com isso nao temos raiz unitária
        dados.deltas[i, "Unit.Root"] = FALSE
        dados.deltas[i, "Criterio"] = 2
      } else{
        # temos de testar a presenca de drift nao significante
        if(ADF.drift@teststat[2] < 5.18){
          # aqui nao temos drift, temos de estimar o modelo sem nada
          if(ADF.none@teststat[1] < (-1.95)){
            #  temos rejeicao da nula e com isso nao temos raiz unitaria
            dados.deltas[i, "Unit.Root"] = FALSE
            dados.deltas[i, "Criterio"] = 3
          } else {
            dados.deltas[i, "Unit.Root"] = TRUE
            dados.deltas[i, "Criterio"] = 4
          }
        } else {
          # temos presenca de drift. Realizamos o teste utilizando a distribuicao normal.
          if(ADF.drift@teststat[1] < (-1.645)){
            # o coeficiente de y_t-1 é diferente de zero, logo nao tem unit root
            dados.deltas[i, "Unit.Root"] = FALSE
            dados.deltas[i, "Criterio"] = 5
          } else {
            dados.deltas[i, "Unit.Root"] = TRUE
            dados.deltas[i, "Criterio"] = 6
          }
        }
        
      }
    } else {
      # aqui a trend é significante, entao devemos estimar gamma com distrib normal
      if(ADF.drift@teststat[1] < (-1.645)){
        dados.deltas[i, "Unit.Root"] = FALSE
        dados.deltas[i, "Criterio"] = 7
      }else{
        dados.deltas[i, "Unit.Root"] = TRUE  
        dados.deltas[i, "Criterio"]=8
      }
    }
  }
  
  # dados.deltas[i, "tau1"] = ADF.none@teststat
  # 
  # dados.deltas[i, "tau2"] = ADF.drift@teststat[1]
  # dados.deltas[i, "tau2.p"] = ADF.drift@teststat[1] > - 2.89
  # 
  # dados.deltas[i, "phi1"] = ADF.drift@teststat[2] 
  # dados.deltas[i, "phi1.p"] = ADF.drift@teststat[2] < 4.71
  
  rm(list = c("x", "ADF.none", "ADF.drift", "ADF.trend"))
}

cat(sprintf("\n Total de Raiz Unitaria: %d\n", sum(dados.deltas$Unit.Root)))

dados.deltas$Criterio[dados.deltas$Unit.Root]
dados.deltas[dados.deltas$Criterio ==8, ]

unit.root.data <- dados.1995plus %>% 
  filter(amc %in% dados.deltas$amc[dados.deltas$Unit.Root]) %>% 
  select(Municipio, Nome_Municipio, Populacao) %>% 
  distinct() %>% 
  arrange(desc(Populacao))
write_excel_csv(x=unit.root.data,path = "./Excel Export/Dados1.csv")

summary(unit.root.data$Populacao/sum(dados.deltas$pop) *100)

boxplot(unit.root.data$Populacao/sum(dados.deltas$pop) *100)

plot(unlist(dados.deltas[dados.deltas$Criterio==8, paste("erro", 1996:2018, sep = ".")]), type="l")








