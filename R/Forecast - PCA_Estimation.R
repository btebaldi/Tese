
# Setup -------------------------------------------------------------------

rm(list = ls())

library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(factoextra)
library(lubridate)

# User defined function ---------------------------------------------------


fn <- function(x){
  ret <- x - lag(x)
  return(ret)
}


# Leitura do banco de dados -----------------------------------------------


# busca dados de estimacao
region.db <- read_csv("../Ox Metrics GVAR/Database/DatabaseDesAdm_RA_v1.csv")
head(region.db)

selVector <- lubridate::ymd(region.db$X1, truncated = 1) < as.Date("2017-01-01")
region.db <- region.db[selVector, ]
# macro.db <- read_excel("~/GitHub/Tese/Ox Metrics GVAR/Database/MacroVariables_forR_20210604.xlsx",
#                        na = "#N/A")
# head(macro.db)

# data para forecast em diferenca
DX.df <- read_excel("Excel Export/DatabaseDesAdm_RA_vForecast_v3.xlsx", 
                    range = "A1:BR1108")

DX <- DX.df[,-1] %>% data.matrix()



# Faça uma diagrama de caixa (boxplot) das variaveis quantitativas.
data.both <- region.db %>% dplyr::select(contains("Adm"), contains("Des")) %>%
  mutate_all(.funs = fn) %>% filter(row_number() > 1)

# Faça uma padronização das variaveis quantitativas.
m=1


medias <- apply(data.both, 2, mean)
sds <- apply(data.both, 2, sd)

DX_t <- t(DX[-c(1:3),])
colnames(DX_t) <- 
  paste("R",
        sort(rep(1:552,2)),
        rep(c("_Admitidos","_Desligados"), 552),
        sep=""
  )
DX_t <- DX_t[,colnames(data.both)]

data.both.c <- data.both
# Normalizacao
for(i in 1:length(medias)){
  data.both.c[,i] <- (data.both[,i] - medias[i])/sds[i]
  DX_t[,i] <- (DX_t[,i] - medias[i])/sds[i]
}


pca.both <- prcomp(data.both.c, center = FALSE, scale. = FALSE, rank. = 2*m)

factoextra::fviz_screeplot(pca.both, addlabels = TRUE)

Forecast.PCA <- DX_t %*% pca.both$rotation



# Vetor de datas
datelist <- seq(from = as.Date("2015-01-01"),
                to = as.Date("2019-12-01"),
                by="month")

# Matriz com resultados
results.tbl <-  tibble(variavel=DX.df$Variavel)

j=1
for (j in 1:552) {
  
  reg.tbl <- as_tibble(pca.both$x)
  
  
  # plot(reg.tbl[[1]], type="l")
  
  reg.tbl$Des <- data.both[[sprintf("R%d_Desligados", j)]]
  reg.tbl$Adm <- data.both[[sprintf("R%d_Admitidos", j)]]
  
  
  mdl.adm <- lm(Adm ~  1 +
                  lag(PC1, 1) +
                  lag(PC1, 2) +
                  lag(PC1, 3) +
                  lag(PC1, 4) +
                  lag(PC1, 5) +
                  lag(PC1, 6) +
                  lag(PC1, 7) +
                  lag(PC1, 8) +
                  lag(PC1, 9) +
                  lag(PC1, 10) +
                  lag(PC1, 11) +
                  lag(PC1, 12) +
                  lag(PC1, 13) +
                  lag(PC2, 1) +
                  lag(PC2, 2) +
                  lag(PC2, 3) +
                  lag(PC2, 4) +
                  lag(PC2, 5) +
                  lag(PC2, 6) +
                  lag(PC2, 7) +
                  lag(PC2, 8) +
                  lag(PC2, 9) +
                  lag(PC2, 10) +
                  lag(PC2, 11) +
                  lag(PC2, 12) +
                  lag(PC2, 13) +
                  1
                , data = reg.tbl)
  
  summary(mdl.adm)
  
  mdl.des <- lm(Des ~  1 +
                  lag(PC1, 1) +
                  lag(PC1, 2) +
                  lag(PC1, 3) +
                  lag(PC1, 4) +
                  lag(PC1, 5) +
                  lag(PC1, 6) +
                  lag(PC1, 7) +
                  lag(PC1, 8) +
                  lag(PC1, 9) +
                  lag(PC1, 10) +
                  lag(PC1, 11) +
                  lag(PC1, 12) +
                  lag(PC1, 13) +
                  lag(PC2, 1) +
                  lag(PC2, 2) +
                  lag(PC2, 3) +
                  lag(PC2, 4) +
                  lag(PC2, 5) +
                  lag(PC2, 6) +
                  lag(PC2, 7) +
                  lag(PC2, 8) +
                  lag(PC2, 9) +
                  lag(PC2, 10) +
                  lag(PC2, 11) +
                  lag(PC2, 12) +
                  lag(PC2, 13) +
                  1
                , data = reg.tbl)
  
  
  summary(mdl.des)
  
  idx <-  which(DX.df$Variavel %in% paste("R", j, c("_Admitidos", "_Desligados"), sep = ""))
  
  i=0
  # para cada lag calcula o forecast
  for(i in 0:35){
    
    n <- 25+i
    cat(sprintf("%d - %s\n",j,datelist[n]))
    
    
    # resultado atual 
    results.tbl[idx, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))] <- DX[idx,sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))]
    
    
    Forecast.PCA.adm = mdl.adm$coefficients["(Intercept)"]
    Forecast.PCA.des = mdl.des$coefficients["(Intercept)"]
    for (lagPca in 1:13) {
      ii=lagPca
      idx2 <- which(rownames(Forecast.PCA) == sprintf("D%dM%02d", year(datelist[n-ii]), month(datelist[n-ii])))
      
      values <- Forecast.PCA[idx2, ]
      Forecast.PCA.adm=Forecast.PCA.adm + mdl.adm$coefficients[sprintf("lag(PC1, %d)", ii)] * values[1] + mdl.adm$coefficients[sprintf("lag(PC2, %d)", ii)] * values[2]
      Forecast.PCA.des=Forecast.PCA.des + mdl.des$coefficients[sprintf("lag(PC1, %d)", ii)] * values[1] + mdl.des$coefficients[sprintf("lag(PC2, %d)", ii)] * values[2]
    }
    
    
    # forecast constante e dummies
    
    results.tbl[idx, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] <- c(Forecast.PCA.adm, Forecast.PCA.des)
    
    results.tbl[idx,sprintf("Error_%dM%02d", year(datelist[n]), month(datelist[n]))] <- results.tbl[idx, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))] - results.tbl[idx, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))]
  }
  
}


# Faz strip do numero da regiao e tipo
results.tbl$Regiao = str_split(results.tbl$variavel, "\\_", simplify = TRUE)[,1]
results.tbl$Tipo = str_split(results.tbl$variavel, "\\_", simplify = TRUE)[,2]

# Salva os dados para o futuro
readr::write_excel_csv(results.tbl, file = "./Excel Export/forecast_result_PCA (2016).csv")
