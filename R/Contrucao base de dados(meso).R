# Clear all
rm(list=ls())

# bibliotecas utilizadas
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)

# Turn On or off the grafical print
PrintGrafs <- TRUE

# Carrego a lista dos municipios
load(file = "./Database/codigos_municipios.RData")
colnames(full) = "Muni"

# carrego a lista de amcs e mesoregioes
meso.data <- read_excel("Database/Amcs_91_v1.xlsx",
                        sheet = 1,
                        range = "A1:G5661")


# carrego as informacoes de mesoregioes.
meso.info <- read_excel("Database/Cadastro Municipios.xlsx", 
                        sheet = "Mesoregioes")


# carrego a lista de regioes ignoradas
load(file = "./Database/tblIgnorados.RData")

# faço o join entre municipios e as mesoregioes
full <- left_join(full, meso.data, by=c("Muni"="munic"))

# retiro os casos que nao estao completos
full <- na.omit(full)


# determino a lista de Mesorerioes
full.meso <- unique(full[,"Meso"])


# caminho dos aquivos
path <- "C:/Users/bteba/Dropbox/bruno-tebaldi/RAIS Base de dados/XLS/"
# path <- "C:/Users/Teo/Dropbox/bruno-tebaldi/RAIS Base de dados/XLS/"


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
  
  # agrupo nas mesoregioes
  data.adm <- data.adm %>% left_join(meso.data, by=c("Muni"="munic")) %>% 
    group_by(Meso) %>% 
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
  
  # padroniza as colunas da serie de admissao no nivel de mesoregiao
  colunas <- paste("ADM_", ano, "M", sprintf("%02d",1:12), sep="")
  colnames(data.adm) <- c("Meso", paste("NIV_", ano, "M00", sep=""), colunas)
  
  # padroniza as colunas da serie de desligamentos
  colnames(data.des) <- c("Muni", "Nivel", paste("col", sprintf("%02d",1:12), sep=""))
  
  # filtro regioes que nao interessam
  data.des <- data.des %>% filter(!(Muni %in% tbl.ignorados$cod))
  
  # agrupo nas mesoregioes
  data.des <- data.des %>% left_join(meso.data, by=c("Muni"="munic")) %>% 
    group_by(Meso) %>% 
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
  
  
  # padroniza as colunas da serie de desligamento no nivel das mesoregioes
  colunas <- paste("DES_", ano, "M", sprintf("%02d",1:12), sep="")
  colnames(data.des) <- c("Meso", paste("NIV_", ano, "M13", sep=""), colunas)  
  
  # inicializo a tabela do nivel
  full.meso <- data.adm %>% full_join(full.meso, by=c("Meso"="Meso"))
  full.meso <- data.des %>% full_join(full.meso, by=c("Meso"="Meso"))
  
  data.adm[!complete.cases(data.adm), ]
  data.des[!complete.cases(data.des), ]
  full.meso[!complete.cases(full.meso), ]
  
  # inicializa o nivel naquele ano
  
  # Acerto do nivel
  # if(ano==1995){
  #   load("./amc.negativa.Rdata")
  #   
  #   for (j in 1:nrow(amc.negativa2)) {
  #     index = match(amc.negativa2$amc[j], full.amc$amc)
  #     
  #     full.amc$NIV_1995M00[index] = full.amc$NIV_1995M00[index] - amc.negativa2$minimo[j]
  #   }
  #   rm(list = c("j", "amc.negativa2"))
  # }
  
  
  
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
    full.meso[, myCol.Niv] = full.meso[, myCol_1.Niv] +
      full.meso[, myCol.adm] - full.meso[, myCol.des]
  }
  
  
} # fim do for de ano.



full.meso %>% select(Meso, starts_with("NIV"))



# Remove arquivos que nao serao mais autlizados
rm(list = c("data.adm", "data.des", "colunas"))
rm(list = c("i", "myCol.Niv", "myCol.adm", "myCol.des", "myCol_1.Niv"))
rm(list = c("file.adm", "file.des"))



full.nivel <- full.meso %>% select(Meso,
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
min(apply(full.nivel, 1, min))

# tem uma regiao com -82 empregos no nivel.


# mostra o grafico do nivel para todas as mesoregioes.
# para cada regiao imprime o grafico
if(PrintGrafs){
  for (i in 1:nrow(full.nivel)) {
    
    currentMeso.Number <- full.nivel$Meso[i]
    currentMeso.Name <- meso.info$Nome_Mesorregiao[meso.info$ID_Meso == currentMeso.Number]
    
    meso.nivel <- full.nivel %>% filter(Meso == currentMeso.Number) %>% 
      pivot_longer(cols = -Meso, names_to = "periodo", values_to = "nivel")
    
    # adiciono uma coluna para data
    meso.nivel$Data <- as.Date(NA)
    
    # parse da data a partir da coluna de periodo
    meso.nivel$Data <- meso.nivel$periodo %>%
      stringr::str_replace(pattern = "NIV\\_", replacement = "") %>% 
      stringr::str_replace(pattern = "M", replacement = "-") %>% 
      stringr::str_replace(pattern = "$", replacement = "-01") %>% as.Date()
    
    # gero o grafico da mesoregiao
    g1 <- ggplot(meso.nivel) +
      geom_line(aes(x = Data, y = nivel)) +
      labs(title = "Nível de emprego - 1995 a 2018",
           subtitle = sprintf("Mesoregião de %s (%d)", currentMeso.Name, currentMeso.Number),
           # caption = "1995-2018",
           y="Nível de emprego",
           x="Data") +
      scale_x_date(minor_breaks = c(as.Date(paste(1995:2019, "-01-01", sep = "")))) +
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M") )
    
    # faço o print em tela
    # print(g1)
    
    # salvo o grafico no diretorio
    ggsave(sprintf("(%d) %s.png", currentMeso.Number, stringr::str_replace(currentMeso.Name, "[*.\"/\\:;|,]", "_")),
           plot=g1,
           device = "png",
           path = "./Plots/Mesoregioes",
           scale = 2)
    
    
    # Se a regiao for de BH salva os dados
    if(currentMeso.Number == 3107)
    {
      save(meso.nivel, file = "./Database/BH_mesoDataNivel.Rdata")
    }
    
  }
}


# Calculo do emprego liquido para exportacao para o excel
full.EmpLiqu <- full.meso %>% mutate(Liq_1995M01 = ADM_1995M01 - DES_1995M01,
                                     Liq_1995M02 = ADM_1995M02 - DES_1995M02,
                                     Liq_1995M03 = ADM_1995M03 - DES_1995M03,
                                     Liq_1995M04 = ADM_1995M04 - DES_1995M04,
                                     Liq_1995M05 = ADM_1995M05 - DES_1995M05,
                                     Liq_1995M06 = ADM_1995M06 - DES_1995M06,
                                     Liq_1995M07 = ADM_1995M07 - DES_1995M07,
                                     Liq_1995M08 = ADM_1995M08 - DES_1995M08,
                                     Liq_1995M09 = ADM_1995M09 - DES_1995M09,
                                     Liq_1995M10 = ADM_1995M10 - DES_1995M10,
                                     Liq_1995M11 = ADM_1995M11 - DES_1995M11,
                                     Liq_1995M12 = ADM_1995M12 - DES_1995M12,
                                     
                                     Liq_1996M01 = ADM_1996M01 - DES_1996M01,
                                     Liq_1996M02 = ADM_1996M02 - DES_1996M02,
                                     Liq_1996M03 = ADM_1996M03 - DES_1996M03,
                                     Liq_1996M04 = ADM_1996M04 - DES_1996M04,
                                     Liq_1996M05 = ADM_1996M05 - DES_1996M05,
                                     Liq_1996M06 = ADM_1996M06 - DES_1996M06,
                                     Liq_1996M07 = ADM_1996M07 - DES_1996M07,
                                     Liq_1996M08 = ADM_1996M08 - DES_1996M08,
                                     Liq_1996M09 = ADM_1996M09 - DES_1996M09,
                                     Liq_1996M10 = ADM_1996M10 - DES_1996M10,
                                     Liq_1996M11 = ADM_1996M11 - DES_1996M11,
                                     Liq_1996M12 = ADM_1996M12 - DES_1996M12,
                                     
                                     Liq_1997M01 = ADM_1997M01 - DES_1997M01,
                                     Liq_1997M02 = ADM_1997M02 - DES_1997M02,
                                     Liq_1997M03 = ADM_1997M03 - DES_1997M03,
                                     Liq_1997M04 = ADM_1997M04 - DES_1997M04,
                                     Liq_1997M05 = ADM_1997M05 - DES_1997M05,
                                     Liq_1997M06 = ADM_1997M06 - DES_1997M06,
                                     Liq_1997M07 = ADM_1997M07 - DES_1997M07,
                                     Liq_1997M08 = ADM_1997M08 - DES_1997M08,
                                     Liq_1997M09 = ADM_1997M09 - DES_1997M09,
                                     Liq_1997M10 = ADM_1997M10 - DES_1997M10,
                                     Liq_1997M11 = ADM_1997M11 - DES_1997M11,
                                     Liq_1997M12 = ADM_1997M12 - DES_1997M12,
                                     
                                     Liq_1998M01 = ADM_1998M01 - DES_1998M01,
                                     Liq_1998M02 = ADM_1998M02 - DES_1998M02,
                                     Liq_1998M03 = ADM_1998M03 - DES_1998M03,
                                     Liq_1998M04 = ADM_1998M04 - DES_1998M04,
                                     Liq_1998M05 = ADM_1998M05 - DES_1998M05,
                                     Liq_1998M06 = ADM_1998M06 - DES_1998M06,
                                     Liq_1998M07 = ADM_1998M07 - DES_1998M07,
                                     Liq_1998M08 = ADM_1998M08 - DES_1998M08,
                                     Liq_1998M09 = ADM_1998M09 - DES_1998M09,
                                     Liq_1998M10 = ADM_1998M10 - DES_1998M10,
                                     Liq_1998M11 = ADM_1998M11 - DES_1998M11,
                                     Liq_1998M12 = ADM_1998M12 - DES_1998M12,
                                     
                                     Liq_1999M01 = ADM_1999M01 - DES_1999M01,
                                     Liq_1999M02 = ADM_1999M02 - DES_1999M02,
                                     Liq_1999M03 = ADM_1999M03 - DES_1999M03,
                                     Liq_1999M04 = ADM_1999M04 - DES_1999M04,
                                     Liq_1999M05 = ADM_1999M05 - DES_1999M05,
                                     Liq_1999M06 = ADM_1999M06 - DES_1999M06,
                                     Liq_1999M07 = ADM_1999M07 - DES_1999M07,
                                     Liq_1999M08 = ADM_1999M08 - DES_1999M08,
                                     Liq_1999M09 = ADM_1999M09 - DES_1999M09,
                                     Liq_1999M10 = ADM_1999M10 - DES_1999M10,
                                     Liq_1999M11 = ADM_1999M11 - DES_1999M11,
                                     Liq_1999M12 = ADM_1999M12 - DES_1999M12,
                                     
                                     Liq_2000M01 = ADM_2000M01 - DES_2000M01,
                                     Liq_2000M02 = ADM_2000M02 - DES_2000M02,
                                     Liq_2000M03 = ADM_2000M03 - DES_2000M03,
                                     Liq_2000M04 = ADM_2000M04 - DES_2000M04,
                                     Liq_2000M05 = ADM_2000M05 - DES_2000M05,
                                     Liq_2000M06 = ADM_2000M06 - DES_2000M06,
                                     Liq_2000M07 = ADM_2000M07 - DES_2000M07,
                                     Liq_2000M08 = ADM_2000M08 - DES_2000M08,
                                     Liq_2000M09 = ADM_2000M09 - DES_2000M09,
                                     Liq_2000M10 = ADM_2000M10 - DES_2000M10,
                                     Liq_2000M11 = ADM_2000M11 - DES_2000M11,
                                     Liq_2000M12 = ADM_2000M12 - DES_2000M12,
                                     
                                     Liq_2001M01 = ADM_2001M01 - DES_2001M01,
                                     Liq_2001M02 = ADM_2001M02 - DES_2001M02,
                                     Liq_2001M03 = ADM_2001M03 - DES_2001M03,
                                     Liq_2001M04 = ADM_2001M04 - DES_2001M04,
                                     Liq_2001M05 = ADM_2001M05 - DES_2001M05,
                                     Liq_2001M06 = ADM_2001M06 - DES_2001M06,
                                     Liq_2001M07 = ADM_2001M07 - DES_2001M07,
                                     Liq_2001M08 = ADM_2001M08 - DES_2001M08,
                                     Liq_2001M09 = ADM_2001M09 - DES_2001M09,
                                     Liq_2001M10 = ADM_2001M10 - DES_2001M10,
                                     Liq_2001M11 = ADM_2001M11 - DES_2001M11,
                                     Liq_2001M12 = ADM_2001M12 - DES_2001M12,
                                     
                                     Liq_2002M01 = ADM_2002M01 - DES_2002M01,
                                     Liq_2002M02 = ADM_2002M02 - DES_2002M02,
                                     Liq_2002M03 = ADM_2002M03 - DES_2002M03,
                                     Liq_2002M04 = ADM_2002M04 - DES_2002M04,
                                     Liq_2002M05 = ADM_2002M05 - DES_2002M05,
                                     Liq_2002M06 = ADM_2002M06 - DES_2002M06,
                                     Liq_2002M07 = ADM_2002M07 - DES_2002M07,
                                     Liq_2002M08 = ADM_2002M08 - DES_2002M08,
                                     Liq_2002M09 = ADM_2002M09 - DES_2002M09,
                                     Liq_2002M10 = ADM_2002M10 - DES_2002M10,
                                     Liq_2002M11 = ADM_2002M11 - DES_2002M11,
                                     Liq_2002M12 = ADM_2002M12 - DES_2002M12,
                                     
                                     Liq_2003M01 = ADM_2003M01 - DES_2003M01,
                                     Liq_2003M02 = ADM_2003M02 - DES_2003M02,
                                     Liq_2003M03 = ADM_2003M03 - DES_2003M03,
                                     Liq_2003M04 = ADM_2003M04 - DES_2003M04,
                                     Liq_2003M05 = ADM_2003M05 - DES_2003M05,
                                     Liq_2003M06 = ADM_2003M06 - DES_2003M06,
                                     Liq_2003M07 = ADM_2003M07 - DES_2003M07,
                                     Liq_2003M08 = ADM_2003M08 - DES_2003M08,
                                     Liq_2003M09 = ADM_2003M09 - DES_2003M09,
                                     Liq_2003M10 = ADM_2003M10 - DES_2003M10,
                                     Liq_2003M11 = ADM_2003M11 - DES_2003M11,
                                     Liq_2003M12 = ADM_2003M12 - DES_2003M12,
                                     
                                     Liq_2004M01 = ADM_2004M01 - DES_2004M01,
                                     Liq_2004M02 = ADM_2004M02 - DES_2004M02,
                                     Liq_2004M03 = ADM_2004M03 - DES_2004M03,
                                     Liq_2004M04 = ADM_2004M04 - DES_2004M04,
                                     Liq_2004M05 = ADM_2004M05 - DES_2004M05,
                                     Liq_2004M06 = ADM_2004M06 - DES_2004M06,
                                     Liq_2004M07 = ADM_2004M07 - DES_2004M07,
                                     Liq_2004M08 = ADM_2004M08 - DES_2004M08,
                                     Liq_2004M09 = ADM_2004M09 - DES_2004M09,
                                     Liq_2004M10 = ADM_2004M10 - DES_2004M10,
                                     Liq_2004M11 = ADM_2004M11 - DES_2004M11,
                                     Liq_2004M12 = ADM_2004M12 - DES_2004M12,
                                     
                                     Liq_2005M01 = ADM_2005M01 - DES_2005M01,
                                     Liq_2005M02 = ADM_2005M02 - DES_2005M02,
                                     Liq_2005M03 = ADM_2005M03 - DES_2005M03,
                                     Liq_2005M04 = ADM_2005M04 - DES_2005M04,
                                     Liq_2005M05 = ADM_2005M05 - DES_2005M05,
                                     Liq_2005M06 = ADM_2005M06 - DES_2005M06,
                                     Liq_2005M07 = ADM_2005M07 - DES_2005M07,
                                     Liq_2005M08 = ADM_2005M08 - DES_2005M08,
                                     Liq_2005M09 = ADM_2005M09 - DES_2005M09,
                                     Liq_2005M10 = ADM_2005M10 - DES_2005M10,
                                     Liq_2005M11 = ADM_2005M11 - DES_2005M11,
                                     Liq_2005M12 = ADM_2005M12 - DES_2005M12,
                                     
                                     Liq_2006M01 = ADM_2006M01 - DES_2006M01,
                                     Liq_2006M02 = ADM_2006M02 - DES_2006M02,
                                     Liq_2006M03 = ADM_2006M03 - DES_2006M03,
                                     Liq_2006M04 = ADM_2006M04 - DES_2006M04,
                                     Liq_2006M05 = ADM_2006M05 - DES_2006M05,
                                     Liq_2006M06 = ADM_2006M06 - DES_2006M06,
                                     Liq_2006M07 = ADM_2006M07 - DES_2006M07,
                                     Liq_2006M08 = ADM_2006M08 - DES_2006M08,
                                     Liq_2006M09 = ADM_2006M09 - DES_2006M09,
                                     Liq_2006M10 = ADM_2006M10 - DES_2006M10,
                                     Liq_2006M11 = ADM_2006M11 - DES_2006M11,
                                     Liq_2006M12 = ADM_2006M12 - DES_2006M12,
                                     
                                     Liq_2007M01 = ADM_2007M01 - DES_2007M01,
                                     Liq_2007M02 = ADM_2007M02 - DES_2007M02,
                                     Liq_2007M03 = ADM_2007M03 - DES_2007M03,
                                     Liq_2007M04 = ADM_2007M04 - DES_2007M04,
                                     Liq_2007M05 = ADM_2007M05 - DES_2007M05,
                                     Liq_2007M06 = ADM_2007M06 - DES_2007M06,
                                     Liq_2007M07 = ADM_2007M07 - DES_2007M07,
                                     Liq_2007M08 = ADM_2007M08 - DES_2007M08,
                                     Liq_2007M09 = ADM_2007M09 - DES_2007M09,
                                     Liq_2007M10 = ADM_2007M10 - DES_2007M10,
                                     Liq_2007M11 = ADM_2007M11 - DES_2007M11,
                                     Liq_2007M12 = ADM_2007M12 - DES_2007M12,
                                     
                                     Liq_2008M01 = ADM_2008M01 - DES_2008M01,
                                     Liq_2008M02 = ADM_2008M02 - DES_2008M02,
                                     Liq_2008M03 = ADM_2008M03 - DES_2008M03,
                                     Liq_2008M04 = ADM_2008M04 - DES_2008M04,
                                     Liq_2008M05 = ADM_2008M05 - DES_2008M05,
                                     Liq_2008M06 = ADM_2008M06 - DES_2008M06,
                                     Liq_2008M07 = ADM_2008M07 - DES_2008M07,
                                     Liq_2008M08 = ADM_2008M08 - DES_2008M08,
                                     Liq_2008M09 = ADM_2008M09 - DES_2008M09,
                                     Liq_2008M10 = ADM_2008M10 - DES_2008M10,
                                     Liq_2008M11 = ADM_2008M11 - DES_2008M11,
                                     Liq_2008M12 = ADM_2008M12 - DES_2008M12,
                                     
                                     Liq_2009M01 = ADM_2009M01 - DES_2009M01,
                                     Liq_2009M02 = ADM_2009M02 - DES_2009M02,
                                     Liq_2009M03 = ADM_2009M03 - DES_2009M03,
                                     Liq_2009M04 = ADM_2009M04 - DES_2009M04,
                                     Liq_2009M05 = ADM_2009M05 - DES_2009M05,
                                     Liq_2009M06 = ADM_2009M06 - DES_2009M06,
                                     Liq_2009M07 = ADM_2009M07 - DES_2009M07,
                                     Liq_2009M08 = ADM_2009M08 - DES_2009M08,
                                     Liq_2009M09 = ADM_2009M09 - DES_2009M09,
                                     Liq_2009M10 = ADM_2009M10 - DES_2009M10,
                                     Liq_2009M11 = ADM_2009M11 - DES_2009M11,
                                     Liq_2009M12 = ADM_2009M12 - DES_2009M12,
                                     
                                     Liq_2010M01 = ADM_2010M01 - DES_2010M01,
                                     Liq_2010M02 = ADM_2010M02 - DES_2010M02,
                                     Liq_2010M03 = ADM_2010M03 - DES_2010M03,
                                     Liq_2010M04 = ADM_2010M04 - DES_2010M04,
                                     Liq_2010M05 = ADM_2010M05 - DES_2010M05,
                                     Liq_2010M06 = ADM_2010M06 - DES_2010M06,
                                     Liq_2010M07 = ADM_2010M07 - DES_2010M07,
                                     Liq_2010M08 = ADM_2010M08 - DES_2010M08,
                                     Liq_2010M09 = ADM_2010M09 - DES_2010M09,
                                     Liq_2010M10 = ADM_2010M10 - DES_2010M10,
                                     Liq_2010M11 = ADM_2010M11 - DES_2010M11,
                                     Liq_2010M12 = ADM_2010M12 - DES_2010M12,
                                     
                                     Liq_2011M01 = ADM_2011M01 - DES_2011M01,
                                     Liq_2011M02 = ADM_2011M02 - DES_2011M02,
                                     Liq_2011M03 = ADM_2011M03 - DES_2011M03,
                                     Liq_2011M04 = ADM_2011M04 - DES_2011M04,
                                     Liq_2011M05 = ADM_2011M05 - DES_2011M05,
                                     Liq_2011M06 = ADM_2011M06 - DES_2011M06,
                                     Liq_2011M07 = ADM_2011M07 - DES_2011M07,
                                     Liq_2011M08 = ADM_2011M08 - DES_2011M08,
                                     Liq_2011M09 = ADM_2011M09 - DES_2011M09,
                                     Liq_2011M10 = ADM_2011M10 - DES_2011M10,
                                     Liq_2011M11 = ADM_2011M11 - DES_2011M11,
                                     Liq_2011M12 = ADM_2011M12 - DES_2011M12,
                                     
                                     Liq_2012M01 = ADM_2012M01 - DES_2012M01,
                                     Liq_2012M02 = ADM_2012M02 - DES_2012M02,
                                     Liq_2012M03 = ADM_2012M03 - DES_2012M03,
                                     Liq_2012M04 = ADM_2012M04 - DES_2012M04,
                                     Liq_2012M05 = ADM_2012M05 - DES_2012M05,
                                     Liq_2012M06 = ADM_2012M06 - DES_2012M06,
                                     Liq_2012M07 = ADM_2012M07 - DES_2012M07,
                                     Liq_2012M08 = ADM_2012M08 - DES_2012M08,
                                     Liq_2012M09 = ADM_2012M09 - DES_2012M09,
                                     Liq_2012M10 = ADM_2012M10 - DES_2012M10,
                                     Liq_2012M11 = ADM_2012M11 - DES_2012M11,
                                     Liq_2012M12 = ADM_2012M12 - DES_2012M12,
                                     
                                     Liq_2013M01 = ADM_2013M01 - DES_2013M01,
                                     Liq_2013M02 = ADM_2013M02 - DES_2013M02,
                                     Liq_2013M03 = ADM_2013M03 - DES_2013M03,
                                     Liq_2013M04 = ADM_2013M04 - DES_2013M04,
                                     Liq_2013M05 = ADM_2013M05 - DES_2013M05,
                                     Liq_2013M06 = ADM_2013M06 - DES_2013M06,
                                     Liq_2013M07 = ADM_2013M07 - DES_2013M07,
                                     Liq_2013M08 = ADM_2013M08 - DES_2013M08,
                                     Liq_2013M09 = ADM_2013M09 - DES_2013M09,
                                     Liq_2013M10 = ADM_2013M10 - DES_2013M10,
                                     Liq_2013M11 = ADM_2013M11 - DES_2013M11,
                                     Liq_2013M12 = ADM_2013M12 - DES_2013M12,
                                     
                                     Liq_2014M01 = ADM_2014M01 - DES_2014M01,
                                     Liq_2014M02 = ADM_2014M02 - DES_2014M02,
                                     Liq_2014M03 = ADM_2014M03 - DES_2014M03,
                                     Liq_2014M04 = ADM_2014M04 - DES_2014M04,
                                     Liq_2014M05 = ADM_2014M05 - DES_2014M05,
                                     Liq_2014M06 = ADM_2014M06 - DES_2014M06,
                                     Liq_2014M07 = ADM_2014M07 - DES_2014M07,
                                     Liq_2014M08 = ADM_2014M08 - DES_2014M08,
                                     Liq_2014M09 = ADM_2014M09 - DES_2014M09,
                                     Liq_2014M10 = ADM_2014M10 - DES_2014M10,
                                     Liq_2014M11 = ADM_2014M11 - DES_2014M11,
                                     Liq_2014M12 = ADM_2014M12 - DES_2014M12,
                                     
                                     Liq_2015M01 = ADM_2015M01 - DES_2015M01,
                                     Liq_2015M02 = ADM_2015M02 - DES_2015M02,
                                     Liq_2015M03 = ADM_2015M03 - DES_2015M03,
                                     Liq_2015M04 = ADM_2015M04 - DES_2015M04,
                                     Liq_2015M05 = ADM_2015M05 - DES_2015M05,
                                     Liq_2015M06 = ADM_2015M06 - DES_2015M06,
                                     Liq_2015M07 = ADM_2015M07 - DES_2015M07,
                                     Liq_2015M08 = ADM_2015M08 - DES_2015M08,
                                     Liq_2015M09 = ADM_2015M09 - DES_2015M09,
                                     Liq_2015M10 = ADM_2015M10 - DES_2015M10,
                                     Liq_2015M11 = ADM_2015M11 - DES_2015M11,
                                     Liq_2015M12 = ADM_2015M12 - DES_2015M12,
                                     
                                     Liq_2016M01 = ADM_2016M01 - DES_2016M01,
                                     Liq_2016M02 = ADM_2016M02 - DES_2016M02,
                                     Liq_2016M03 = ADM_2016M03 - DES_2016M03,
                                     Liq_2016M04 = ADM_2016M04 - DES_2016M04,
                                     Liq_2016M05 = ADM_2016M05 - DES_2016M05,
                                     Liq_2016M06 = ADM_2016M06 - DES_2016M06,
                                     Liq_2016M07 = ADM_2016M07 - DES_2016M07,
                                     Liq_2016M08 = ADM_2016M08 - DES_2016M08,
                                     Liq_2016M09 = ADM_2016M09 - DES_2016M09,
                                     Liq_2016M10 = ADM_2016M10 - DES_2016M10,
                                     Liq_2016M11 = ADM_2016M11 - DES_2016M11,
                                     Liq_2016M12 = ADM_2016M12 - DES_2016M12,
                                     
                                     Liq_2017M01 = ADM_2017M01 - DES_2017M01,
                                     Liq_2017M02 = ADM_2017M02 - DES_2017M02,
                                     Liq_2017M03 = ADM_2017M03 - DES_2017M03,
                                     Liq_2017M04 = ADM_2017M04 - DES_2017M04,
                                     Liq_2017M05 = ADM_2017M05 - DES_2017M05,
                                     Liq_2017M06 = ADM_2017M06 - DES_2017M06,
                                     Liq_2017M07 = ADM_2017M07 - DES_2017M07,
                                     Liq_2017M08 = ADM_2017M08 - DES_2017M08,
                                     Liq_2017M09 = ADM_2017M09 - DES_2017M09,
                                     Liq_2017M10 = ADM_2017M10 - DES_2017M10,
                                     Liq_2017M11 = ADM_2017M11 - DES_2017M11,
                                     Liq_2017M12 = ADM_2017M12 - DES_2017M12,
                                     
                                     Liq_2018M01 = ADM_2018M01 - DES_2018M01,
                                     Liq_2018M02 = ADM_2018M02 - DES_2018M02,
                                     Liq_2018M03 = ADM_2018M03 - DES_2018M03,
                                     Liq_2018M04 = ADM_2018M04 - DES_2018M04,
                                     Liq_2018M05 = ADM_2018M05 - DES_2018M05,
                                     Liq_2018M06 = ADM_2018M06 - DES_2018M06,
                                     Liq_2018M07 = ADM_2018M07 - DES_2018M07,
                                     Liq_2018M08 = ADM_2018M08 - DES_2018M08,
                                     Liq_2018M09 = ADM_2018M09 - DES_2018M09,
                                     Liq_2018M10 = ADM_2018M10 - DES_2018M10,
                                     Liq_2018M11 = ADM_2018M11 - DES_2018M11,
                                     Liq_2018M12 = ADM_2018M12 - DES_2018M12
                                     
) %>% select(Meso, starts_with("Liq"))


full.adm <- full.meso %>% select(Meso,
                                 c(paste("ADM_", 1995, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 1996, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 1997, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 1998, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 1999, "M", sprintf("%02d", 1:12), sep=""),
                                   
                                   paste("ADM_", 2000, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2001, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2002, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2003, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2004, "M", sprintf("%02d", 1:12), sep=""),
                                   
                                   paste("ADM_", 2005, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2006, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2007, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2008, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2009, "M", sprintf("%02d", 1:12), sep=""),
                                   
                                   paste("ADM_", 2010, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2011, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2012, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2013, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2014, "M", sprintf("%02d", 1:12), sep=""),
                                   
                                   paste("ADM_", 2015, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2016, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2017, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("ADM_", 2018, "M", sprintf("%02d", 1:12), sep=""))
)


full.des <- full.meso %>% select(Meso,
                                 c(paste("DES_", 1995, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 1996, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 1997, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 1998, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 1999, "M", sprintf("%02d", 1:12), sep=""),
                                   
                                   paste("DES_", 2000, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2001, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2002, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2003, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2004, "M", sprintf("%02d", 1:12), sep=""),
                                   
                                   paste("DES_", 2005, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2006, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2007, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2008, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2009, "M", sprintf("%02d", 1:12), sep=""),
                                   
                                   paste("DES_", 2010, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2011, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2012, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2013, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2014, "M", sprintf("%02d", 1:12), sep=""),
                                   
                                   paste("DES_", 2015, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2016, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2017, "M", sprintf("%02d", 1:12), sep=""),
                                   paste("DES_", 2018, "M", sprintf("%02d", 1:12), sep=""))
)


# Salva as planilhas de Nivel e emprego liquido.
readr::write_excel_csv(full.nivel, path = "./Excel Export/Meso_Nivel.csv")
readr::write_excel_csv(full.EmpLiqu, path = "./Excel Export/Meso_EmpLiq.csv")

readr::write_excel_csv(full.adm, path = "./Excel Export/Meso_adm.csv")
readr::write_excel_csv(full.des, path = "./Excel Export/Meso_des.csv")

