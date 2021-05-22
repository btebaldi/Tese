# Clear all
rm(list=ls())

# bibliotecas utilizadas
library(readxl)
library(dplyr)
library(tidyr)

ano = 2019

# nome do arquivo
path <- "C:/Users/bteba/Dropbox/bruno-tebaldi/RAIS Base de dados/XLS/"
file.adm <- paste(path, "consulta_adm_", ano," (todos).xlsx", sep = "")
file.des <- paste(path, "consulta_des_", ano," (todos).xlsx", sep = "")

# load database
data.adm <- read_excel(file.adm, range = cell_limits(c(1, 1), c(NA, 14)))
data.des <- read_excel(file.des, range = cell_limits(c(1, 1), c(NA, 14)))

# padroniza as colunas da serie de adimissao
colunas <- paste("ADM_", ano, "M", sprintf("%02d",1:12), sep="")
colnames(data.adm) <- c("Muni", paste("NIV_", ano, "M00", sep=""), colunas)

# padroniza as colunas da serie de desligamentos
colunas <- paste("DES_", ano, "M", sprintf("%02d",1:12), sep="")
colnames(data.des) <- c("Muni", paste("NIV_", ano, "M13", sep=""), colunas)

# inicializo a tabela do nivel
data.nivel <- data.des %>% full_join(data.adm, by=c("Muni"="Muni"))

# calcula ao nivel para cada mes
for(i in 1:12){
  myCol.adm = paste("ADM_", ano, "M", sprintf("%02d",i), sep="")
  myCol.des = paste("DES_", ano, "M", sprintf("%02d",i), sep="")
  
  myCol.Niv = paste("NIV_", ano, "M", sprintf("%02d",i), sep="")
  myCol_1.Niv = paste("NIV_", ano, "M", sprintf("%02d",i-1), sep="")
  
  # para cada coluna calcula-se o nivel atual
  data.nivel[, myCol.Niv] = data.nivel[, myCol_1.Niv] +
    data.nivel[, myCol.adm] - data.nivel[, myCol.des]
  
}

data.nivel %>% select(Muni, NIV_2018M00,
                      NIV_2018M01,
                      NIV_2018M02,
                      NIV_2018M03,
                      NIV_2018M04,
                      NIV_2018M12,
                      NIV_2018M13)

# Remove arquivos que nao serao mais autlizados
rm(list = c("data.adm", "data.des", "colunas"))
rm(list = c("i", "myCol.Niv", "myCol.adm", "myCol.des", "myCol_1.Niv"))
rm(list = c("file.adm", "file.des"))



for(ano in 2017:1995){
  
  # para o ano de 2017
  # ano = 2017
  
  # arquivos 
  file.adm <- paste(path, "consulta_adm_", ano," (todos).xlsx", sep = "")
  file.des <- paste(path, "consulta_des_", ano," (todos).xlsx", sep = "")
  
  # load database
  data.adm <- read_excel(file.adm, range = cell_limits(c(1, 1), c(NA, 14)))
  data.des <- read_excel(file.des, range = cell_limits(c(1, 1), c(NA, 14)))
  
  # padroniza as colunas da serie de adimissao
  colunas <- paste("ADM_", ano, "M", sprintf("%02d",1:12), sep="")
  colnames(data.adm) <- c("Muni", paste("NIV_", ano, "M00", sep=""), colunas)
  
  # padroniza as colunas da serie de desligamentos
  colunas <- paste("DES_", ano, "M", sprintf("%02d",1:12), sep="")
  colnames(data.des) <- c("Muni", paste("NIV_", ano, "M13", sep=""), colunas)
  
  
  # inicializo a tabela do nivel para o ano corrente
  data.nivel.temp <- data.des %>% full_join(data.adm, by=c("Muni"="Muni"))
  
  # junto a tabela com a tabela de nivel que tinha anteriormente
  data.nivel <- data.nivel %>%
    full_join(data.nivel.temp, by=c("Muni"="Muni"))
  
  
  # forco o nivel de 2017 a ser igua ao ano seguinte no inicio do ano
  col1 <- paste("NIV_", ano, "M13", sep="")
  col2 <- paste("NIV_", ano+1, "M00", sep="")
  
  print(min(data.nivel[[col2]]))
  
  data.nivel[, col1] <- data.nivel[, col2]
  rm(list = c("col1", "col2"))
  
  # data.nivel.temp %>% select(Muni, NIV_2017M13, NIV_2018M00)
  
  # data.nivel[!complete.cases(data.nivel), ]
  
  # calcula ao nivel para cada mes
  for(i in 12:1){
    
    # seleciona as colunas a serem trabalhadas
    myCol.adm = paste("ADM_", ano, "M", sprintf("%02d",i), sep="")
    myCol.des = paste("DES_", ano, "M", sprintf("%02d",i), sep="")
    
    myCol.Niv = paste("NIV_", ano, "M", sprintf("%02d",i), sep="")
    myCol_1.Niv = paste("NIV_", ano, "M", sprintf("%02d",i-1), sep="")
    
    if(i==12){
      myCol_13.Niv <- paste("NIV_", ano, "M", sprintf("%02d",i+1), sep="")
      data.nivel[, myCol.Niv] <- data.nivel[, myCol_13.Niv]
    } 
    
    # Verifica se existem regioes com NA, caso afirmativo, estabelece as mesmas como zero!
    data.nivel[is.na(data.nivel[[myCol.des]]), myCol.des] <- 0
    data.nivel[is.na(data.nivel[[myCol.adm]]), myCol.adm] <- 0
      
    # para cada coluna calcula-se o nivel atual
    data.nivel[, myCol_1.Niv] <- data.nivel[, myCol.Niv] +
      data.nivel[, myCol.des] - data.nivel[, myCol.adm]
  }
  
  # remove aruivos nao utilizados
  rm(list = c("i", "myCol.Niv", "myCol.adm", "myCol.des", "myCol_1.Niv"))
  rm(list = c("file.adm", "file.des"))
  rm(list = c("data.adm", "data.des", "colunas"))
  rm(list = c("myCol_13.Niv", "data.nivel.temp"))
  
}

# 
# data.nivel %>% select(Muni, NIV_2017M00, NIV_2017M01, NIV_2017M12, NIV_2017M13,
#                       NIV_2018M00, NIV_2018M01, NIV_2018M12, NIV_2018M13)
# 
# data.nivel %>% select(Muni, NIV_2016M00, NIV_2016M01, NIV_2016M12, NIV_2016M13, NIV_2017M00)
# data.nivel %>% select(Muni, NIV_2016M13, NIV_2016M00)
# data.nivel %>% select(Muni, NIV_2015M13, NIV_2015M00)
# data.nivel %>% select(Muni, NIV_2014M13, NIV_2014M00)
# 
# data.nivel %>% select(Muni, NIV_2013M13, NIV_2013M00)
# data.nivel %>% select(Muni, NIV_2012M13, NIV_2012M00)
# data.nivel %>% select(Muni, NIV_2011M13, NIV_2011M00)
# data.nivel %>% select(Muni, NIV_2010M13, NIV_2010M00)


# ANALISE DE CASOS INCOMPLETOS
imcomplete <- data.nivel[!complete.cases(data.nivel), ]


summary(imcomplete)


imcomplete[imcomplete$Muni == 110175, ]

data.nivel$NIV_2010M13

aa <- data.nivel %>% select(Muni,
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


amc <- read_excel("Database/Amcs_91_v1.xlsx",
                  sheet = 1,
                  range = "A1:F5661")

aa <- aa %>% filter(!(Muni %in% c(99, 0)))



# Tabela com relacao de dados ignorados
tbl.ignorados <- tibble(cod=0, desc="NA", .rows = 27)
tbl.ignorados$cod <- c(119999, 129999, 139999, 149999, 159999, 169999, 
                       179999, 219999, 229999, 239999, 249999, 259999,
                       269999, 279999, 289999, 299999, 319999, 329999, 
                       339999, 359999, 419999, 429999, 439999, 509999,
                       519999, 529999, 539999)

tbl.ignorados$desc <- c("Ro-Ignorado","Ac-Ignorado","Am-Ignorado","Rr-Ignorado","Pa-Ignorado","Ap-Ignorado",
                        "To-Ignorado","Ma-Ignorado","Pi-Ignorado","Ce-Ignorado","Rn-Ignorado","Pb-Ignorado",
                        "Pe-Ignorado","Al-Ignorado","Se-Ignorado","Ba-Ignorado","Mg-Ignorado","Es-Ignorado",
                        "Rj-Ignorado","Sp-Ignorado","Pr-Ignorado","Sc-Ignorado","Rs-Ignorado","Ms-Ignorado",
                        "Mt-Ignorado","Go-Ignorado","Df-Ignorado")


# Retira da amostra os dados ignorados
aa <- aa %>% filter(!(Muni %in% tbl.ignorados$cod))

# min(aa$NIV_1995M01)
# 
# plot(serie, type = "l")


# Junta os dados com os codigos das amcs
ab <- aa %>% left_join(amc, by = c("Muni" = "munic"))

ac <- ab %>%
  group_by(amc) %>%
  summarise(NIV_1995M01_2=sum(NIV_1995M01))

ac[ac$NIV_1995M01_2 < -70000, ]

1 172100
2 250750

plot(unlist(aa[aa$Muni==250750,]))
abline(a=0, b=0)



cc <- aa[aa$Muni==250750,]

cd <- cc %>% pivot_longer(cols=-Muni, names_to = "name")

plot(sapply(aa[,-1], sum))

10.339.626
46.631.115

sum(aa[aa$NIV_1995M01<0, 2])

2.199.252