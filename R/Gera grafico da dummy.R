rm(list=ls())
library(readr)
library(ggplot2)
library(lubridate)

dummy.tbl <- read_rds(file = "./Database/dummy_by_region.rds")

# busca dados de estimacao
region.db <- read_csv("../Ox Metrics GVAR/Database/DatabaseDesAdm_RA_v1.csv")
selVector <- lubridate::ymd(region.db$X1, truncated = 1) < as.Date("2017-01-01")
region.db <- region.db[selVector, ]

for (region.number in c(15, 66, 552, 151, 404, 346, 280, 191)) {
  
  tbl <- region.db[, paste("R", region.number, c("_Admitidos", "_Desligados"), sep="")]
  colnames(tbl) <- c("Adm", "Des", "Date")
  tbl$Date <- ymd(region.db$X1, truncated = 1)
  
  dummy.tbl.region <- dummy.tbl %>% filter(region == region.number)
  
  g <-   ggplot(tbl) +
    geom_line(aes(x=Date, y = Adm-Des)) + 
    geom_vline(xintercept = dummy.tbl.region$Dummy, colour="blue", alpha =0.7)+
    theme_bw() +
    labs(title = "Gráfico do Emprego liquido e dummies",
         subtitle = sprintf("Região %d", region.number),
         y="Emprego Líquido")
  
  print(g)
  
  g <-   ggplot(tbl) +
    geom_line(aes(x=Date, y = Adm-Des)) + 
    # geom_vline(xintercept = dummy.tbl.region$Dummy, colour="blue", alpha =0.7)+
    theme_bw() +
    labs(title = "Gráfico do Emprego liquido sem dummies",
         subtitle = sprintf("Região %d", region.number),
         y="Emprego Líquido")
  
  print(g)
  
  g <-   ggplot(tbl) +
    geom_line(aes(x=Date, y = Adm, colour="Adm")) + 
    geom_line(aes(x=Date, y = Des, colour="Des")) + 
    # geom_vline(xintercept = dummy.tbl.region$Dummy, colour="blue", alpha =0.7)+
    theme_bw() +
    labs(title = "Gráfico de Admitidos e dummies",
         subtitle = sprintf("Região %d", region.number),
         y="Emprego Líquido")
  
  print(g)
  
  g <-   ggplot(tbl) +
    geom_line(aes(x=Date, y = Adm, colour="Adm")) + 
    geom_line(aes(x=Date, y = Des, colour="Des")) + 
    geom_vline(xintercept = dummy.tbl.region$Dummy, colour="blue", alpha =0.7)+
    theme_bw() +
    labs(title = "Gráfico de Admitidos e dummies",
         subtitle = sprintf("Região %d", region.number),
         y="Emprego Líquido")
  
  print(g)
}
