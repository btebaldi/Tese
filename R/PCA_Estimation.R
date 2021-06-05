
# Setup -------------------------------------------------------------------

rm(list = ls())

library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(factoextra)



# User defined function ---------------------------------------------------


fn <- function(x){
  ret <- x - lag(x)
  return(ret)
}


# Leitura do banco de dados -----------------------------------------------


region.db <- read_csv("~/GitHub/Tese/Ox Metrics GVAR/Database/DatabaseDesAdm_RA_v1.csv")
head(region.db)

# macro.db <- read_excel("~/GitHub/Tese/Ox Metrics GVAR/Database/MacroVariables_forR_20210604.xlsx",
#                        na = "#N/A")
# head(macro.db)

# tbl <- dplyr::bind_cols(region.db, macro.db)
tbl <- region.db

# Faça uma diagrama de caixa (boxplot) das variaveis quantitativas.
data.both <- tbl %>% dplyr::select(contains("Des"),
                                   contains("Adm")) %>%
  mutate_all(.funs = fn) %>% filter(row_number()>2)

# Faça uma padronização das variaveis quantitativas.
m=1
pca.both <- prcomp(data.both, center = TRUE, scale. = TRUE, rank. = 2*m)

# factoextra::fviz_screeplot(pca.both, addlabels = TRUE, ncp=10)
# 
# factoextra::get_eigenvalue(pca.both)

# variável se relaciona aos Componentes principais.
# loadings <- pca$rotation
# head(loadings)


i=1

# reg.tbl <- bind_cols(as_tibble(pca.des$x), as_tibble(pca.adm$x))
reg.tbl <- as_tibble(pca.both$x)

# plot(reg.tbl[[1]], type="l")

reg.tbl$Des <- data.both[[sprintf("R%d_Desligados", i)]]
reg.tbl$Adm <- data.both[[sprintf("R%d_Admitidos", i)]]

# e) Determine os valores dos componentes principais.

mdl.des <- lm(Des ~  1 +
            lag(PC1, 1) +
            lag(PC1, 2) +
            lag(PC1, 3) +
            # lag(PC1, 4) +
            # lag(PC1, 5) +
            # lag(PC1, 6) +
            # lag(PC1, 7) +
            # lag(PC1, 8) +
            # lag(PC1, 9) +
            # lag(PC1, 10) +
            # lag(PC1, 11) +
            # lag(PC1, 12) +
            # lag(PC1, 13) +
            lag(PC2, 1) +
            lag(PC2, 2) +
            lag(PC2, 3) +
            # lag(PC2, 4) +
            # lag(PC2, 5) +
            # lag(PC2, 6) +
            # lag(PC1, 7) +
            # lag(PC2, 8) +
            # lag(PC2, 9) +
            # lag(PC2, 10) +
            # lag(PC2, 11) +
            # lag(PC2, 12) +
            # lag(PC2, 13) +
            1
          , data = reg.tbl)
summary(mdl.des)

# get coeficients
# calculate the new PCA's from the loadings
# calculate the model 

acf(mdl$residuals)
pacf(mdl$residuals)


# FAZER MESMAS CONTAS PARA OS ADMINITDOS


# c) Um método alternativo para determinar o número de componentes principais é
# olhar para um Scree Plot, que é o gráfico de autovalores ordenados do maior
# para o menor. Faça um Scree Plot utiliznao o comando "fviz_screeplot()" do
# pacote "factoextra".
factoextra::fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 50), ncp=20)


# d) Determine os carregamentos de cada variavel (loadings). Ou seja como cada
# variável se relaciona aos Componentes principais.
loadings <- pca$rotation
head(loadings)


# e) Determine os valores dos componentes principais.
PC.values <- pca.both$x
head(PC.values)

