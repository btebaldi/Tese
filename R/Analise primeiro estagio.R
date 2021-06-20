
# Setup -------------------------------------------------------------------
rm(list = ls())
library(readr)
library(tidyr)
library(ggplot2)

dir.SEM <- "SEM IIS - Modelo 0"
dir.COM <- "COM IIS - Modelo 7"

SEM_IIS.filepath = file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir.SEM, "Criterios_de_Inforacao.csv")
COM_IIS.filepath = file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir.COM, "Criterios_de_Inforacao.csv")

file.exists(SEM_IIS.filepath)
file.exists(COM_IIS.filepath)


# Abrindo os dados --------------------------------------------------------
Crit.s <- read_delim(SEM_IIS.filepath, ";", escape_double = FALSE, 
                     locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
Crit.s$Source = "SEM"

Crit.c <- read_delim(COM_IIS.filepath, ";", escape_double = FALSE, 
                     locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
Crit.c$Source = "COM"


# Criando os graficos -----------------------------------------------------
tbl <- bind_rows(Crit.c, Crit.s)

g <- tbl |>
  pivot_longer(cols = c("AIC", "BIC", "HaQ")) |>
  ggplot() + 
  geom_boxplot(aes(x=name, y=value, colour=Source), outlier.alpha = 0.25) + 
  labs() +
  theme_bw()

print(g)

grafic.filename <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir.COM, "Graficos", sprintf("Boxplot - Analise 1o Estagio.png"))

g.path <- paste("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir.COM, sep = "/")

ggsave(filename = sprintf("Boxplot - Analise 1o Estagio.png"),
       path = g.path,
       plot = g,
       scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)



sink(paste(g.path, "Analise 1o Estagio - estatisticas descritivas.txt", sep = "/"))
summary(Crit.s)
summary(Crit.c)

sink()
