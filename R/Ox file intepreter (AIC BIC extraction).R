
# Setup -------------------------------------------------------------------
rm(list=ls())
# You should take care with readLines(...) and big files. Reading all lines at
# memory can be risky. Below is a example of how to read file and process just
# one line at time:


# Bibliotecas -------------------------------------------------------------

library(stringr)
library(dplyr)


# Variaveis internas ------------------------------------------------------

fileName = "Gvar_Passo1_v4.out"

dir <- "COM IIS - Modelo 17"

filepath = file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir, fileName)
file.out.path = file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir, "Criterios_de_Inforacao.csv")

file.exists(filepath)

ReadCon  <- file(description = filepath, open = "r")


pattern0 <- "-* Ox at .* on .* -*"
pattern2 <- "SKIP: Regiao "
pattern3 <- "             Regiao "

pattern_AIC <- ">>> AIC: .*"
pattern_BIC <- ">>> SC: .*"
pattern_HaQ <- ">>> HQ: .*"


tbl.results <- tibble(region = 1:552,
                      AIC = as.numeric(NA),
                      BIC = as.numeric(NA),
                      HaQ = as.numeric(NA))


while ( TRUE ) {
  
  # inicializa as variaveis
  # region_number <- 0  # Numero da Regiao
  AIC <- 0           # Akaike 
  BIC <- 0           # Schwartz
  HaQ <- 0           # Hanna Queen
  WriteInfo <- FALSE
  teste7 <- 0
  
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
  if(stringr::str_detect(line, stringr::regex(pattern_AIC))){
    AIC <- stringr::str_match(line,  stringr::regex("(?<=>>> AIC: )\\d*.{0,1}\\d*"))
    WriteInfo <- TRUE
    
    print(line)
  }
  
  # verifica o criterio de Schwartz
  if(stringr::str_detect(line, stringr::regex(pattern_BIC))){
    BIC <- stringr::str_match(line,  stringr::regex("(?<=>>> SC: )\\d*.{0,1}\\d*"))
    WriteInfo <- TRUE
    
    print(line)
  }
  
  # verifica o criterio de Hanna Queen
  if(stringr::str_detect(line, stringr::regex(pattern_HaQ))){
    HaQ <- stringr::str_match(line,  stringr::regex("(?<=>>> HQ: )\\d*.{0,1}\\d*"))
    WriteInfo <- TRUE
    
    print(line)
  }
  
  
  
  
  # Escreve as informacoes
  if(WriteInfo)
  {
    selctVector <- tbl.results$region == as.integer(region_number)
    
    if(AIC != 0){
      tbl.results$AIC[selctVector] <- as.numeric(AIC)  
      cat(sprintf("%s => Region: %d \tAIC: %f\n", line, as.integer(region_number), as.numeric(AIC)))
    }
    
    if(BIC != 0){
      tbl.results$BIC[selctVector] <- as.numeric(BIC)
      cat(sprintf("%s => Region: %d \tBIC: %f\n", line, as.integer(region_number), as.numeric(BIC)))
    }
    
    if(HaQ != 0){
      tbl.results$HaQ[selctVector] <- as.numeric(HaQ)
      cat(sprintf("%s => Region: %d \tHaQ: %f\n", line, as.integer(region_number), as.numeric(HaQ)))
    }
  }
  
}

close(ReadCon)

readr::write_excel_csv2(x=tbl.results, file = file.out.path)

