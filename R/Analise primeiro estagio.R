
# Setup -------------------------------------------------------------------
rm(list = ls())
library(readr)
library(tidyr)
library(ggplot2)

dir.SEM <- "SEM IIS - Modelo 0"
dir.COM <- "COM IIS - Modelo 8"

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





library(readxl)
Relacao_Agregacao_Ox <- read_excel("../Ox Metrics GVAR/Ox Scripts/Dicionario Ox-GVAR.xlsx")

tbl <- Crit.c %>%
  dplyr::inner_join(Crit.s, by = c("region"="region")) %>% 
  dplyr::inner_join(Relacao_Agregacao_Ox, by = c("region"="Contagem"))
  
tbl$ganho <- log(tbl$pop)/(tbl$BIC.x/tbl$BIC.y)

tbl <- tbl %>% arrange(desc(ganho))


library(writexl)
writexl::write_xlsx(tbl, path = paste(g.path, "Ganho 1o Estagio - estatisticas descritivas.xlsx", sep = "/"))


