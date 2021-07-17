rm(list = ls())

library(zip)

OxOutFile <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "SEM IIS - Modelo 0", "Gvar_Passo1_v4.out")

filesToZip <- c( "mGy",
                 "mGy_inv_X_mC",
                 "mGy_inv_X_mGyL1",
                 "mGy_inv_X_mGyL2",
                 "mGy_inv_X_mGyL3",
                 "mGy_inv_X_mGyL4",
                 "mGy_inv_X_mGyL5",
                 "mGy_inv_X_mGyL6",
                 "mGy_inv_X_mGyL7",
                 "mGy_inv_X_mGyL8",
                 "mGy_inv_X_mGyL9",
                 "mGy_inv_X_mGyL10",
                 "mGy_inv_X_mGyL11",
                 "mGy_inv_X_mGyL12",
                 "mGy_inv_X_mGyL13",
                 "mGy_inv_X_mL")

# # zip(zipfile = "MatFiles", files = file.path(dirname(OxOutFile), filesToZip))
# zip(zipfile = "MatFiles.zip", files = "C:/Users/Teo/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/COM IIS - Modelo 4/mGy_inv_X_mGyL13.mat")
# getwd()

for(FileBaseName in filesToZip){
  ZipFile <- file.path(dirname(OxOutFile), paste(FileBaseName, ".zip", sep = ""))
  
  ToZip <- file.path(dirname(OxOutFile), paste(FileBaseName, ".mat", sep = ""))
  zip(
    zipfile = ZipFile,
      files = ToZip,
    include_directories = FALSE,
    mode = "cherry-pick",
  )
  
  #Check its existence
  if (file.exists(ZipFile)) {
    #Delete file if it exists
    file.remove(ToZip)
  }
}
