library(readxl)
library(ggplot2)

Crit.s <- read_excel("ANALISE DE Criterios_de_Inforacao.xlsx", 
                     range = "A2:D554")
Crit.s$Source = "SEM"

Crit.c <- read_excel("ANALISE DE Criterios_de_Inforacao.xlsx", 
                     range = "F2:I554")
Crit.c$Source = "COM"
 
tbl <- bind_rows(Crit.c, Crit.s)

tbl |>
  pivot_longer(cols = c("AIC", "BIC", "HaQ")) |>
  ggplot() + 
  geom_boxplot(aes(x=name, y=value, colour=Source), outlier.alpha = 0.25) + 
  labs() +
  theme_bw()


summary(Crit.s)
summary(Crit.c)
