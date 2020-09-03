# clear all ----
rm(list = ls())

# Bibliotecas ----
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)


# Importacao de dados ----

# --- ADIMITIDOS ---
tbl.adm.md <- read_excel("./Database/GVAR Data/graphs_bs Admitido.xls", 
                         sheet = "Median Estimates", range = "A4:AY141")
tbl.adm.lb <- read_excel("Database/GVAR Data/graphs_bs Admitido.xls", 
                         sheet = "Lower Bounds", range = "A4:AY141")
tbl.adm.ub <- read_excel("Database/GVAR Data/graphs_bs Admitido.xls", 
                         sheet = "Upper Bounds", range = "A4:AY141")


# --- DESLIGADOS ---
tbl.des.md <- read_excel("./Database/GVAR Data/graphs_bs Desligado.xls", 
                         sheet = "Median Estimates", range = "A4:AY141")
tbl.des.lb <- read_excel("Database/GVAR Data/graphs_bs Desligado.xls", 
                         sheet = "Lower Bounds", range = "A4:AY141")
tbl.des.ub <- read_excel("Database/GVAR Data/graphs_bs Desligado.xls", 
                         sheet = "Upper Bounds", range = "A4:AY141")


# --- Producao Industrial ---
tbl.pim.md <- read_excel("./Database/GVAR Data/graphs_bs ProdIndustrial.xls", 
                         sheet = "Median Estimates", range = "A4:AY5")
tbl.pim.lb <- read_excel("Database/GVAR Data/graphs_bs ProdIndustrial.xls", 
                         sheet = "Lower Bounds", range = "A4:AY5")
tbl.pim.ub <- read_excel("Database/GVAR Data/graphs_bs ProdIndustrial.xls", 
                         sheet = "Upper Bounds", range = "A4:AY5")



# padroniza o nome das colunas ----
colnames(tbl.adm.md) <- c("Meso", "Serie", paste("Per_", 0:48, sep = ""))
colnames(tbl.adm.lb) <- c("Meso", "Serie", paste("Per_", 0:48, sep = ""))
colnames(tbl.adm.ub) <- c("Meso", "Serie", paste("Per_", 0:48, sep = ""))

colnames(tbl.des.md) <- c("Meso", "Serie", paste("Per_", 0:48, sep = ""))
colnames(tbl.des.lb) <- c("Meso", "Serie", paste("Per_", 0:48, sep = ""))
colnames(tbl.des.ub) <- c("Meso", "Serie", paste("Per_", 0:48, sep = ""))

colnames(tbl.pim.md) <- c("Meso", "Serie", paste("Per_", 0:48, sep = ""))
colnames(tbl.pim.lb) <- c("Meso", "Serie", paste("Per_", 0:48, sep = ""))
colnames(tbl.pim.ub) <- c("Meso", "Serie", paste("Per_", 0:48, sep = ""))


# Transformacao das colunas em series----
# --- Admitidos ---
tbl.adm.md <- tbl.adm.md %>% tidyr::pivot_longer(cols = starts_with("Per_"),
                                                 names_to = "periodo",
                                                 names_prefix = "Per_",
                                                 names_transform = list(periodo = as.integer),
                                                 values_to = "MD")

tbl.adm.lb <- tbl.adm.lb %>% tidyr::pivot_longer(cols = starts_with("Per_"),
                                                 names_to = "periodo",
                                                 names_prefix = "Per_",
                                                 names_transform = list(periodo = as.integer),
                                                 values_to = "LB")

tbl.adm.ub <- tbl.adm.ub %>% tidyr::pivot_longer(cols = starts_with("Per_"),
                                                 names_to = "periodo",
                                                 names_prefix = "Per_",
                                                 names_transform = list(periodo = as.integer),
                                                 values_to = "UB")


# --- Desligados ---
tbl.des.md <- tbl.des.md %>% tidyr::pivot_longer(cols = starts_with("Per_"),
                                                 names_to = "periodo",
                                                 names_prefix = "Per_",
                                                 names_transform = list(periodo = as.integer),
                                                 values_to = "MD")

tbl.des.lb <- tbl.des.lb %>% tidyr::pivot_longer(cols = starts_with("Per_"),
                                                 names_to = "periodo",
                                                 names_prefix = "Per_",
                                                 names_transform = list(periodo = as.integer),
                                                 values_to = "LB")

tbl.des.ub <- tbl.des.ub %>% tidyr::pivot_longer(cols = starts_with("Per_"),
                                                 names_to = "periodo",
                                                 names_prefix = "Per_",
                                                 names_transform = list(periodo = as.integer),
                                                 values_to = "UB")

# --- Producao Industrial ---
tbl.pim.md <- tbl.pim.md %>% tidyr::pivot_longer(cols = starts_with("Per_"),
                                                 names_to = "periodo",
                                                 names_prefix = "Per_",
                                                 names_transform = list(periodo = as.integer),
                                                 values_to = "MD")

tbl.pim.lb <- tbl.pim.lb %>% tidyr::pivot_longer(cols = starts_with("Per_"),
                                                 names_to = "periodo",
                                                 names_prefix = "Per_",
                                                 names_transform = list(periodo = as.integer),
                                                 values_to = "LB")

tbl.pim.ub <- tbl.pim.ub %>% tidyr::pivot_longer(cols = starts_with("Per_"),
                                                 names_to = "periodo",
                                                 names_prefix = "Per_",
                                                 names_transform = list(periodo = as.integer),
                                                 values_to = "UB")


# Juncao de Upper bound, loer bound e median values ----

tbl.adm <- tbl.adm.md %>% inner_join(tbl.adm.lb, by=c("Meso"="Meso", "periodo"="periodo", "Serie"="Serie")) %>%
  inner_join(tbl.adm.ub, by=c("Meso"="Meso", "periodo"="periodo", "Serie"="Serie"))

tbl.des <- tbl.des.md %>% inner_join(tbl.des.lb, by=c("Meso"="Meso", "periodo"="periodo", "Serie"="Serie")) %>%
  inner_join(tbl.des.ub, by=c("Meso"="Meso", "periodo"="periodo", "Serie"="Serie"))

tbl.pim <- tbl.pim.md %>% inner_join(tbl.pim.lb, by=c("Meso"="Meso", "periodo"="periodo", "Serie"="Serie")) %>%
  inner_join(tbl.pim.ub, by=c("Meso"="Meso", "periodo"="periodo", "Serie"="Serie"))


tbl <- tbl.des %>% inner_join(tbl.adm, by=c("Meso"="Meso", "periodo"="periodo")) %>% 
  select(Meso, periodo, des_Md = MD.x, des_Lb=LB.x, des_Ub=UB.x, adm_Md=MD.y, adm_Lb=LB.y, adm_Ub=UB.y)


# Remove dados nao utilizados ----
rm(list = c("tbl.adm", "tbl.des"))
rm(list = c("tbl.adm.lb", "tbl.des.lb", "tbl.adm.ub", "tbl.des.ub", "tbl.adm.md", "tbl.des.md"))
rm(list = c("tbl.pim.lb", "tbl.pim.ub", "tbl.pim.md"))

# Plotagem dos graficos ----

for(region.name in unique(tbl$Meso)){
  
  # if(region.name != "Sp - Metropol Sao Paulo")
  # {next();}
  
  # Gero o grafico
  g1 <- tbl %>% filter(Meso == region.name) %>% 
    ggplot()+
    geom_line(aes(x=periodo, y=adm_Md, color="Adm"), linetype = "solid", size=1) +
    geom_ribbon(aes(x=periodo, ymin = adm_Lb, ymax = adm_Ub),alpha=0.3, fill="#529EFF") +
    geom_line(aes(x=periodo, y=des_Md, color="Des"), linetype = "solid", size=1) +
    geom_ribbon(aes(x=periodo, ymin = des_Lb, ymax = des_Ub),alpha=0.3, fill="#FC717F") +
    labs(
      title = region.name,
      subtitle = "Resposta a um choque positivo na produção industrial",
      caption = NULL,
      x=NULL,
      y=NULL
    ) + 
    scale_color_manual(NULL, values = c("Adm"="blue", "Des"="red")) +
    theme_bw() + 
    theme(legend.position="bottom") + 
    xlim(0,45)
    
  
  
  # salvo o grafico no diretorio
  ggsave(sprintf("%s.png", stringr::str_replace(region.name, "[*.\"/\\:;|,]", "_")),
         plot=g1,
         device = "png",
         path = "./Plots/ImpulsoResposta/",
         scale = 2)
  
}


g2 <- tbl.pim %>%  
  ggplot()+
  geom_line(aes(x=periodo, y=MD, color="Pim"), linetype = "solid", size=1) +
  geom_ribbon(aes(x=periodo, ymin = LB, ymax = UB),alpha=0.3, fill="#529EFF") +
  labs(
    title = "Produção industrial (log)",
    subtitle = "Choque positivo",
    caption = NULL,
    x=NULL,
    y=NULL
  ) + 
  scale_color_manual("Legenda", values = c("Pim"="blue")) +
  theme_bw() +
  theme(legend.position="none") + 
  xlim(0,45)
  


# salvo o grafico no diretorio
ggsave(sprintf("%s.png", stringr::str_replace("DomUnit - Producao Industrial", "[*.\"/\\:;|,]", "_")),
       plot=g2,
       device = "png",
       path = "./Plots/ImpulsoResposta/",
       scale = 2)
