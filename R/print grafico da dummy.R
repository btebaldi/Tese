rm(list=ls())
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(cowplot)




dir <- "COM IIS - Modelo 8"
dummy.info.filepath <- file.path("../Ox Metrics GVAR", "Ox Scripts", 
                                 "mat_files", "Result_Matrix",
                                 dir, "Dummies by region.rds")


# Fatores e nomes ---------------------------------------------------------

my.levels <- c("R404", "R346", "R193", "R552", "R280", "R151", "R191", "R33", "R15")
my.labels <- c("São Paulo",
               "Rio de Janeiro",
               "Serrana do Sertão Alagoano",
               "Brasilia",
               "Belo Horizonte",
               "Natal",
               "Recife",
               "Pa-Almeirim","Am-Japura")

dummy.tbl <- read_rds(file = dummy.info.filepath)

# busca dados de estimacao
region.db <- read_csv("../Ox Metrics GVAR/Database/DatabaseDesAdm_RA_v1.csv")
selVector <- lubridate::ymd(region.db$X1, truncated = 1) < as.Date("2017-01-01")
region.db <- region.db[selVector, ]

for (region.number in c(404, 346, 193, 552, 280, 151, 191, 33, 15)) {
  
  idx <- which(my.levels == paste("R", region.number, sep=""))
  
  
  tbl <- region.db[, paste("R", region.number, c("_Admitidos", "_Desligados"), sep="")]
  colnames(tbl) <- c("Adm", "Des", "Date")
  tbl$Date <- ymd(region.db$X1, truncated = 1)
  
  dummy.tbl.region <- dummy.tbl %>% filter(region == region.number)

  # Grafico de Admitidos e Demitidos (SEM DUMMY)
  g <- ggplot(tbl) +
    geom_line(aes(x=Date, y = Adm, colour="Adm")) + 
    geom_line(aes(x=Date, y = Des, colour="Des")) + 
    # geom_vline(xintercept = dummy.tbl.region$Dummy, colour="blue", alpha =0.7)+
    theme_bw() +
    labs(title = "Admitidos e Desligados",
         subtitle = sprintf("Região: %s", my.labels[idx]),
         y="Emprego Líquido")
  
  print(g)
  ggsave(filename = sprintf("Dummy-1 - %s.png", my.labels[idx]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  
  
  
  # Grafico de Admitidos e Demitidos (COM DUMMY)
  g <- ggplot(tbl) +
    geom_line(aes(x=Date, y = Adm, colour="Adm")) + 
    geom_line(aes(x=Date, y = Des, colour="Des")) + 
    geom_vline(xintercept = dummy.tbl.region$Dummy, colour="blue", alpha =0.7)+
    theme_bw() +
    labs(title = "Admitidos e Desligados",
         subtitle = sprintf("Região: %s", my.labels[idx]),
         y="Emprego Líquido")
  
  print(g)
  ggsave(filename = sprintf("Dummy-2 - %s.png", my.labels[idx]),
         path = "Graficos",
         plot = g,
         scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
  

  
  # Grafico Emprego liquido (SEM DUMMY)
  g1 <- ggplot(tbl) +
    geom_line(aes(x=Date, y = Adm-Des)) + 
    # geom_vline(xintercept = dummy.tbl.region$Dummy, colour="blue", alpha =0.7)+
    theme_bw() +
    labs(title = "Net Employment",
         subtitle = sprintf("Region: %s", my.labels[idx]),
         y="Net Employment",
         caption = "Source: elaborated by the author")

    print(g1)
    ggsave(filename = sprintf("Dummy-3 - %s.png", my.labels[idx]),
           path = "Graficos",
           plot = g1,
           scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
    
  
    # Grafico Emprego liquido (COM DUMMY)
    g2 <- ggplot(tbl) +
      geom_line(aes(x=Date, y = Adm-Des)) + 
      geom_vline(xintercept = dummy.tbl.region$Dummy, colour="blue", alpha =0.7)+
      theme_bw() +
      labs(title = "Net Employment with Dummies",
           subtitle = sprintf("Region: %s", my.labels[idx]),
           y="Net Employment",
           caption = "Source: elaborated by the author")
    
    print(g2)
    ggsave(filename = sprintf("Dummy-4 - %s.png", my.labels[idx]),
           path = "Graficos",
           plot = g2,
           scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)
    

    # default ggplot2 theme
    g3 <- plot_grid(g1, g2)
    ggsave(filename = sprintf("Dummy-5 - %s.png", my.labels[idx]),
           path = "Graficos",
           plot = g3,
           scale=1, units = "in", dpi = 300, width = 20.8, height = 5.85)
    
    
}
