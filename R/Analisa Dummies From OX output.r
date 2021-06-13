
# Setup -------------------------------------------------------------------
rm(list=ls())
# You should take care with readLines(...) and big files. Reading all lines at
# memory can be risky. Below is a example of how to read file and process just
# one line at time:


# Bibliotecas -------------------------------------------------------------

library(stringr)
library(dplyr)


# Variaveis internas ------------------------------------------------------

fileName = "Gvar_Passo1_v4 (COM IIS 2021-06-07).out"
# fileName = "Gvar_Passo1_v4 (saida modelo SEM IIS).txt" <- <- <- <- %>% %>% 
filepath = file.path("..", "Ox Metrics GVAR","Ox Scripts", fileName)


file.exists(filepath)

ReadCon  <- file(description = filepath, open = "r")


pattern0 <- "-* Ox at .* on .* -*"
pattern2 <- "SKIP: Regiao "
pattern3 <- "             Regiao "

pattern_dummy <- "(?<=I:)\\d{4}\\(\\d{1,2}\\)(?=@D_R\\d{1,3})"


tbl.results <- tibble(region = 1:552, Dummies="")


while ( TRUE ) {
  
  # inicializa as variaveis
  # region_number <- 0  # Numero da Regiao
  DUMMY <- ""           # DUMMIE
  WriteInfo <- FALSE
  
  #  Faz leitura da linha
  line = readLines(ReadCon, n = 1)
  
  # Se a linha tem tamanho zero entao para o processamento
  if ( length(line) == 0 ) {
    break
  }
  
  # verifica a regiao atual
  if(stringr::str_detect(line, stringr::regex(pattern3))){
    region_number <- stringr::str_match(line,  stringr::regex("(?<=             Regiao )\\d*"))
    # WriteInfo <- TRUE
    print(line)
  } 
  
  # verifica e o comeco do processamento
  if(stringr::str_detect(line, stringr::regex(pattern0))){
    print(line)
  }
  
  # verifica a regiao que foi skiped
  if(stringr::str_detect(line, stringr::regex(pattern2))){
    region_number <- stringr::str_match(line,  stringr::regex("(?<=SKIP: Regiao )\\d*"))
    WriteInfo <- TRUE
    
    print(line)
  }
  
  
  # verifica o criterio de Akaike
  if(stringr::str_detect(line, stringr::regex(pattern_dummy))){
    DUMMY <- stringr::str_match(line,  stringr::regex(pattern_dummy))
    WriteInfo <- TRUE
    
    print(line)
  }
  
  
  
  # Escreve as informacoes
  if(WriteInfo)
  {
    selctVector <- tbl.results$region == as.integer(region_number)
    
    if(DUMMY != ""){
      tbl.results$Dummies[selctVector] <- paste(tbl.results$Dummies[selctVector], DUMMY)
      cat(sprintf(" => Region: %d \tDUMMY: %s\n", as.integer(region_number), DUMMY))
    }
    
  }
  
}

close(ReadCon)

readr::write_excel_csv2(x=tbl.results,
                        file = sprintf("Dummies by region(%s) (ref 2016).csv", fileName))



tbl.results2 <- tibble(region = as.integer(NA), Dummy=as.Date(NA))

library(lubridate)
library(stringr)
library(dplyr)
i=1
for (i in 1:nrow(tbl.results)) {
  
  dates.str <- unique(unlist(str_split(str_trim(tbl.results$Dummies[i]), " ")))
  
  date.list <- lubridate::ymd(dates.str, truncated = 1)
  
  for(j in seq_along(date.list)){
    tbl.results2 <- tbl.results2 %>% dplyr::add_row(region = i, Dummy=date.list[j])
  }
  
}


write_rds(tbl.results2, "./Database/dummy_by_region.rds")









