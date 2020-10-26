# You should take care with readLines(...) and big files. Reading all lines at
# memory can be risky. Below is a example of how to read file and process just
# one line at time:
  
library(stringr)


path = "C:/Users/bteba/Downloads/Gvar_Determina WeakExo_v2.txt"







processFile = function(filepath) {
    ReadCon  <- file(description = filepath, open = "r")
    
    WriteCon <- file(description = "./Ox_Results.txt", open = "w")
    
    ncount <- 0
    ncount_2S  <- 0
    pattern_2S <- "Test of restrictions on alpha:\\s*.*\\[.*\\]\\*{2}"
    
    pattern0 <- "-* Ox at .* on .* -*"
    pattern <- "Test of restrictions on alpha:"
    
    while ( TRUE ) {
      line = readLines(ReadCon, n = 1)
      if ( length(line) == 0 ) {
        break
      }

      teste <- stringr::str_detect(line, stringr::regex(pattern))
      
      if(stringr::str_detect(line, stringr::regex(pattern0))){
        writeLines( text = line, con = WriteCon)
        print(line)
      }
      
      if(teste){
        writeLines( text = line, con = WriteCon)
        print(line)
        
        if(stringr::str_detect(line, stringr::regex(pattern_2S))) {
          ncount_2S = ncount_2S + 1
        }
        
        ncount = ncount + 1
        
        
      }
    }
    
    close(ReadCon)
    close(WriteCon)
    
    cat(sprintf("Total de 1S: %d\nTotal de 2S: %d", ncount - ncount_2S, ncount_2S))
}


# --- Salva a matrix em arquivo .mat ----
processFile(path)

