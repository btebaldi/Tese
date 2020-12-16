
# Setup -------------------------------------------------------------------
rm(list=ls())
# You should take care with readLines(...) and big files. Reading all lines at
# memory can be risky. Below is a example of how to read file and process just
# one line at time:


# Bibliotecas -------------------------------------------------------------

library(stringr)
library(dplyr)
library(here)


# Variaveis internas ------------------------------------------------------

fileName = "Gvar_Determina WeakExo_v1(regioes problema)-2020-12-14 pibpercapta.out"
filepath = file.path("..", "Ox Metrics GVAR","Ox Scripts", fileName)


file.exists(filepath)

ReadCon  <- file(description = filepath, open = "r")
WriteCon <- file(description = "./Ox_Results.txt", open = "w")

ncount <- 0
ncount_2S  <- 0
pattern_2S <- "Test of restrictions on alpha:\\s*.*\\[.*\\]\\*{2}"

pattern0 <- "-* Ox at .* on .* -*"
pattern1 <- "Test of restrictions on alpha:"
pattern2 <- "SKIP: Regiao "
pattern3 <- "             Regiao "
pattern4 <- "RANK TOTAL: "
pattern5 <- "Test of restrictions on alpha and beta:"
pattern6 <- "RANK ZERO DETECTADO"

tbl.results <- tibble(region = 1:552,
                      rank = as.integer(NA),
                      A_test = as.numeric(NA),
                      AB_test = as.numeric(NA),
                      A_test_Logical = as.logical(NA),
                      AB_test_Logical = as.logical(NA),
                      Zero_Rank = FALSE)


while ( TRUE ) {
  
  # inicializa as variaveis
  # region_number <- 0  # Numero da Regiao
  rank <- 0             # Rank test
  teste1 <- 0           # Weak Exo Test
  teste2 <- 0           # Alpha Beta Test
  zerorank <- 0         # rank zero detectado
  WriteInfo <- FALSE
  
  #  Faz leitura da linha
  line = readLines(ReadCon, n = 1)
  
  
  # Se a linha tem tamanho zero entao para o processamento
  if ( length(line) == 0 ) {
    break
  }
  
  
  # verifica e o comeco do processamento
  if(stringr::str_detect(line, stringr::regex(pattern0))){
    writeLines( text = line, con = WriteCon)
    print(line)
  }
  
  # verifica a regiao que foi skiped
  if(stringr::str_detect(line, stringr::regex(pattern2))){
    writeLines( text = line, con = WriteCon)
    writeLines( text = "No data", con = WriteCon)
    writeLines( text = "No data", con = WriteCon)
    writeLines( text = "No data", con = WriteCon)
    
    region_number <- stringr::str_match(line,  stringr::regex("(?<=SKIP: Regiao )\\d*"))
    WriteInfo <- TRUE
    
    print(line)
    print("No data")
    print("No data")
  }
  
  
  # verifica a regiao que foi detectado rank zero
  if(stringr::str_detect(line, stringr::regex(pattern6))){
    writeLines(text = line, con = WriteCon)

    zerorank <- 1
    WriteInfo <- TRUE
    
    print(line)
  }
  
  
  # verifica a regiao atual
  if(stringr::str_detect(line, stringr::regex(pattern3))){
    writeLines( text = line, con = WriteCon)
    
    region_number <- stringr::str_match(line,  stringr::regex("(?<=             Regiao )\\d*"))
    WriteInfo <- TRUE
    print(line)
  }
  
  # verifica o Rank
  if(stringr::str_detect(line, stringr::regex(pattern4))){
    writeLines( text = line, con = WriteCon)
    
    rank <- stringr::str_match(line,  stringr::regex("(?<=RANK TOTAL: )\\d"))
    WriteInfo <- TRUE
    print(line)
    # stop()
  }
  
  # Verifica a restrição em alpha e beta
  if(stringr::str_detect(line, stringr::regex(pattern5))){
    writeLines( text = line, con = WriteCon)
    teste2 <- stringr::str_match(line,  stringr::regex("(?<=\\[).*(?=\\])"))
    WriteInfo <- TRUE
    print(line)
  }
  
  
  # Verifica a restrição em alpha (2020-11-18: A principio tera dois testes por regiao)
  if(stringr::str_detect(line, stringr::regex(pattern1))){
    writeLines(text = line, con = WriteCon)
    teste1 <- stringr::str_match(line,  stringr::regex("(?<=\\[).*(?=\\])"))
    WriteInfo <- TRUE
    print(line)
    
    # Detecta se o teste passou ou nao.
    if(stringr::str_detect(line, stringr::regex(pattern_2S))) {
      ncount_2S = ncount_2S + 1
    }
    
    ncount = ncount + 1
    
  }
  
  if(WriteInfo)
  {
    selctVector <- tbl.results$region == as.integer(region_number)
    
    if(rank != 0){
      tbl.results$rank[selctVector] <- as.integer(rank)  
    }
    
    if(teste1 != 0){
      tbl.results$A_test[selctVector] <- as.numeric(teste1)  
    }
    
    if(teste2 != 0){
      tbl.results$AB_test[selctVector] <- as.numeric(teste2)  
    }

    if(zerorank != 0){
      tbl.results$Zero_Rank[selctVector] <- TRUE  
    }
    
  }
  
}

close(ReadCon)
close(WriteCon)

cat(sprintf("Total de 1S: %d\nTotal de 2S: %d", ncount - ncount_2S, ncount_2S))

tbl.results <- tbl.results %>% 
  mutate(A_test_Logical = if_else(A_test >= 0.01, TRUE, FALSE, missing = NA)) %>% 
  mutate(AB_test_Logical = if_else(AB_test >= 0.01, TRUE, FALSE, missing = NA))

readr::write_excel_csv(x=tbl.results, file = sprintf("Ox_Results_Analise(%s).csv", fileName))

