# clear all
rm(list = ls())

# Bibliotecas utilizadas
library(readxl)

# Nome e numero da regiao
currentMeso.Name <- "Metropolitana de Belo Horizonte"
currentMeso.Number <- 3107

# carrega dados da imagem da mesoregiao de BH
load(file = "./Database/Meso BH v1/BH_mesoDataNivel.Rdata")

# Carrega tabela com dummies
Dummies <- read_excel("Database/Meso BH v1/Dummies.xlsx", 
                      range = "K1:L23",
                      col_types = c("date","numeric"))

# pre-view
head(Dummies)

# Converte coluna para formato de data
Dummies$Dummies = as.Date(Dummies$Dummies)
Dummies$DummiesFim = Dummies$Dummies + 30


# gero o grafico da mesoregiao
g1 <- 
  ggplot(meso.nivel) +
  # geom_vline(xintercept = Dummies$Dummies[Dummies$Ligado == 1], color="#0066CC") +
  # geom_ribbon(aes(xmin=Dummies, xmax = DummiesFim, y=Inf), data = Dummies) +
  geom_rect(aes(xmin=Dummies, xmax=DummiesFim, ymin=-Inf, ymax=Inf), data = Dummies,  fill = "steelblue", alpha = 0.3) +
  geom_line(aes(x = Data, y = nivel)) +
  labs(title = "Nível de emprego - 1995 a 2018",
       subtitle = sprintf("Mesoregião de %s (%d)", currentMeso.Name, currentMeso.Number),
       # caption = "1995-2018",
       y="Nível de emprego",
       x="Data") +
  scale_x_date(minor_breaks = c(as.Date(paste(1995:2019, "-01-01", sep = "")))) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M") )


# salvo o grafico no diretorio
ggsave(sprintf("Dummies (%d) %s.png", currentMeso.Number, stringr::str_replace(currentMeso.Name, "[*.\"/\\:;|,]", "_")),
       plot=g1,
       device = "png",
       width = 12.10,
       height = 7.62,
       units = "in",
       dpi = 300,
       path = "./Plots/Mesoregioes",
       scale = 1)


