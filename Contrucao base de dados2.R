# Clear all
rm(list=ls())

# bibliotecas utilizadas
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)

# Carrego a lista dos municipios
load(file = "./Database/codigos_municipios.RData")
colnames(full) = "Muni"

# carrego a lista de amcs
amc <- read_excel("Database/Amcs_91_v1.xlsx",
                  sheet = 1,
                  range = "A1:F5661")

# carrego a lista de regioes ignoradas
load(file = "./Database/tblIgnorados.RData")

# faço o join entre municipios e amcs
full <- left_join(full, amc, by=c("Muni"="munic"))

# retiro os casos que nao estao completos
full <- na.omit(full)

# determino a lista de amcs
full.amc <- unique(full[,"amc"])


# caminho dos aquivos
path <- "C:/Users/bteba/Dropbox/bruno-tebaldi/RAIS Base de dados/XLS/"


for(ano in 1995:2018){
  # nome do arquivo
  file.adm <- paste(path, "consulta_adm_", ano," (todos).xlsx", sep = "")
  file.des <- paste(path, "consulta_des_", ano," (todos).xlsx", sep = "")
  
  # load database
  data.adm <- read_excel(file.adm, range = cell_limits(c(1, 1), c(NA, 14)))
  data.des <- read_excel(file.des, range = cell_limits(c(1, 1), c(NA, 14)))
  
  # padroniza as colunas da serie de adimissao
  colnames(data.adm) <- c("Muni", "Nivel", paste("col", sprintf("%02d",1:12), sep=""))
  
  # filtro regioes que nao interessam
  data.adm <- data.adm %>% filter(!(Muni %in% tbl.ignorados$cod))
  
  # agrupo nas amcs
  data.adm <- data.adm %>% left_join(amc, by=c("Muni"="munic")) %>% 
    group_by(amc) %>% 
    summarise(col_0 = sum(Nivel),
              col_1 = sum(col01),
              col_2 = sum(col02),
              col_3 = sum(col03),
              col_4 = sum(col04),
              col_5 = sum(col05),
              col_6 = sum(col06),
              col_7 = sum(col07),
              col_8 = sum(col08),
              col_9 = sum(col09),
              col_10 = sum(col10),
              col_11 = sum(col11),
              col_12 = sum(col12)
    )
  
  # padroniza as colunas da serie de admissao no nivel AMC
  colunas <- paste("ADM_", ano, "M", sprintf("%02d",1:12), sep="")
  colnames(data.adm) <- c("amc", paste("NIV_", ano, "M00", sep=""), colunas)
  
  # padroniza as colunas da serie de desligamentos
  colnames(data.des) <- c("Muni", "Nivel", paste("col", sprintf("%02d",1:12), sep=""))
  
  # filtro regioes que nao interessam
  data.des <- data.des %>% filter(!(Muni %in% tbl.ignorados$cod))
  
  # agrupo nas amcs
  data.des <- data.des %>% left_join(amc, by=c("Muni"="munic")) %>% 
    group_by(amc) %>% 
    summarise(col_0 = sum(Nivel),
              col_1 = sum(col01),
              col_2 = sum(col02),
              col_3 = sum(col03),
              col_4 = sum(col04),
              col_5 = sum(col05),
              col_6 = sum(col06),
              col_7 = sum(col07),
              col_8 = sum(col08),
              col_9 = sum(col09),
              col_10 = sum(col10),
              col_11 = sum(col11),
              col_12 = sum(col12)
    )
  
  
  # padroniza as colunas da serie de desligamento no nivel AMC
  colunas <- paste("DES_", ano, "M", sprintf("%02d",1:12), sep="")
  colnames(data.des) <- c("amc", paste("NIV_", ano, "M13", sep=""), colunas)  
  
  # inicializo a tabela do nivel
  full.amc <- data.adm %>% full_join(full.amc, by=c("amc"="amc"))
  full.amc <- data.des %>% full_join(full.amc, by=c("amc"="amc"))
  
  data.adm[!complete.cases(data.adm), ]
  data.des[!complete.cases(data.des), ]
  full.amc[!complete.cases(full.amc), ]
  
  # inicializa o nivel naquele ano
  
  # Acerto do nivel
  if(ano==1995){
    load("./amc.negativa.Rdata")
    
    for (j in 1:nrow(amc.negativa2)) {
      index = match(amc.negativa2$amc[j], full.amc$amc)
      
      full.amc$NIV_1995M00[index] = full.amc$NIV_1995M00[index] - amc.negativa2$minimo[j]
    }
    rm(list = c("j", "amc.negativa2"))
  }
    
  
  
  # i=1
  # calcula ao nivel para cada mes
  for(i in 1:12){
    myCol.adm = paste("ADM_", ano, "M", sprintf("%02d",i), sep="")
    myCol.des = paste("DES_", ano, "M", sprintf("%02d",i), sep="")
    
    myCol.Niv = paste("NIV_", ano, "M", sprintf("%02d",i), sep="")
    
    if(i==1){
      if(ano==1995){
        myCol_1.Niv = paste("NIV_", ano, "M", sprintf("%02d",i-1), sep="")
      } else
      {
        myCol_1.Niv = paste("NIV_", ano-1, "M", sprintf("%02d",12), sep="")
      }
      
    } else {
      myCol_1.Niv = paste("NIV_", ano, "M", sprintf("%02d",i-1), sep="")
    }
    
    
    # para cada coluna calcula-se o nivel atual
    full.amc[, myCol.Niv] = full.amc[, myCol_1.Niv] +
      full.amc[, myCol.adm] - full.amc[, myCol.des]
  }
  
  
} # fim do for de ano.



full.amc %>% select(amc, starts_with("NIV"))


# Remove arquivos que nao serao mais autlizados
rm(list = c("data.adm", "data.des", "colunas"))
rm(list = c("i", "myCol.Niv", "myCol.adm", "myCol.des", "myCol_1.Niv"))
rm(list = c("file.adm", "file.des"))




full.nivel <- full.amc %>% select(amc,
                            c(paste("NIV_", 1995, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 1996, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 1997, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 1998, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 1999, "M", sprintf("%02d", 1:12), sep=""),
                              
                              paste("NIV_", 2000, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2001, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2002, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2003, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2004, "M", sprintf("%02d", 1:12), sep=""),
                              
                              paste("NIV_", 2005, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2006, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2007, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2008, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2009, "M", sprintf("%02d", 1:12), sep=""),
                              
                              paste("NIV_", 2010, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2011, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2012, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2013, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2014, "M", sprintf("%02d", 1:12), sep=""),
                              
                              paste("NIV_", 2015, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2016, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2017, "M", sprintf("%02d", 1:12), sep=""),
                              paste("NIV_", 2018, "M", sprintf("%02d", 1:12), sep=""))
)

# verifica o menor nivel de emprego
min(apply(full.nivel, 2, min))

# Seleciona top 10 regioes por populacao.
amc.top10 <- amc %>% filter(munic %in% c(355030, 330455, 292740, 530010, 230440, 310620, 130260,  410690, 261160, 431490))

# para cada regiao imprime o grafico
for (i in 1:nrow(amc.top10)) {

    full.nivel.top10 <- full.nivel %>%
    filter(amc == amc.top10$amc[i]) %>% 
    pivot_longer(cols = -amc, names_to = "periodo", values_to = "nivel")
  
  
  full.nivel.top10$Data <- as.Date(NA)
  
  full.nivel.top10$Data <- full.nivel.top10$periodo %>%
    stringr::str_replace(pattern = "NIV\\_", replacement = "") %>% 
    stringr::str_replace(pattern = "M", replacement = "-") %>% 
    stringr::str_replace(pattern = "$", replacement = "-01") %>% as.Date()
  
  g1 <- ggplot(full.nivel.top10) +
    geom_line(aes(x = Data, y = nivel)) +
    labs(title = "Nível de emprego - 1995 a 2018",
         subtitle = sprintf("%s (%d)", amc.top10$Nome_Municipio[i], amc.top10$amc[i]),
         # caption = "1995-2018",
         y="Nível populacional",
         x="Data") +
    scale_x_date(minor_breaks = c(as.Date(paste(1995:2019, "-01-01", sep = "")))) +
    scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M") )
  
  print(g1)
  ggsave(sprintf("%d-%s.png",amc.top10$amc[i],amc.top10$Nome_Municipio[i]),
         plot=g1,
         device = "png",
         path = "./Plots/",
         scale = 2)
}

