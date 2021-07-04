rm(list = ls())

library(zip)

OxOutFile <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", "COM IIS - Modelo 14", "Gvar_Passo1_v4.out")

filesToZip <- c( "mGy.mat",
                 # "mGy_inv_X_mC.mat",
                 # "mGy_inv_X_mGyL1.mat",
                 # "mGy_inv_X_mGyL2.mat",
                 # "mGy_inv_X_mGyL3.mat",
                 # "mGy_inv_X_mGyL4.mat",
                 # "mGy_inv_X_mGyL5.mat",
                 # "mGy_inv_X_mGyL6.mat",
                 # "mGy_inv_X_mGyL7.mat",
                 # "mGy_inv_X_mGyL8.mat",
                 # "mGy_inv_X_mGyL9.mat",
                 # "mGy_inv_X_mGyL10.mat",
                 # "mGy_inv_X_mGyL11.mat",
                 # "mGy_inv_X_mGyL12.mat",
                 # "mGy_inv_X_mGyL13.mat",
                 "mGy_inv_X_mL.mat")

# # zip(zipfile = "MatFiles", files = file.path(dirname(OxOutFile), filesToZip))
# zip(zipfile = "MatFiles.zip", files = "C:/Users/Teo/Documents/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/COM IIS - Modelo 4/mGy_inv_X_mGyL13.mat")
# getwd()


zip(
  zipfile = "../MatFiles.zip",
  files = file.path(dirname(OxOutFile), filesToZip),
  include_directories = FALSE,
  mode = "cherry-pick",
  )
