# clear all
rm(list = ls())

# bibliotecas
library(readxl)
library(readr)
library(dlm)

# Leitura dos dados 
dados <- read_excel("Database/DatabaseDesAdm.xlsx")
head(dados)


# Funcao que cria um polinomio de Dynamic Linear Model (DLM) de ordem 1.
# utiliza exp nas variancias para garantir que serao sempre maior que zero.
fn <- function(params){
  # Create an n-th order polynomial DLM
  dlm::dlmModPoly(order = 1, dV = exp(params[1]) , dW = exp(params[2]))
}


# Faz a interpolacao dos dados com zero
for (coluna in colnames(dados)) {
  
  if(coluna == "Data")  {
    cat(sprintf("Pulando coluna %s", coluna))
  } else {
    minimo <- dados %>%  pull(coluna) %>% min()
    
    if(minimo == 0){    
      cat(sprintf("\n%s : %d ", coluna, minimo))
      
      # retira o zero e coloca NA para a interpolacao
      dados[dados[[coluna]] == 0, coluna] <- NA
      
      # Determina qual a serie que sera utilizada para o local level model
      y <- dados[[coluna]]
      
      # chute iniicial da variancia
      a0 <- log(var(y, na.rm = T))
      
      # Estima os parametros do modelo de nível local
      # Recall: local level deve ser especificado duas varaincias
      fit <- dlm::dlmMLE(y, c(0,0), fn)
      
      # The local level model
      mod <- fn(fit$par)
      
      obs.error.var <- V(mod)
      state.error.var <- W(mod)
      
      cat(sprintf("Observation error variance: %f\nState error variance: %f", obs.error.var, state.error.var))
      
      # Applies Kalman filter to compute filtered values of the state vectors,
      # together with their variance/covariance matrices
      filtered <- dlmFilter(y, mod)
      
      # Apply Kalman smoother to compute smoothed values of the state vectors, 
      # together with their variance/covariance matrices.
      smoothed <- dlmSmooth(filtered)
      
      # Obtem a serie de mu
      mu <- dropFirst(smoothed$s)
      
      # Adiciona serie de nivel local a base de dados
      values <- trunc(mu[is.na(dados[[coluna]])])
      
      # Preenche os dados faltantes
      cat(sprintf("\n%s: %d", coluna, values))
      
      dados[is.na(dados[[coluna]]), coluna] <- values
    }
  }
}


# Salva os dados para conferencia
write_excel_csv(x = dados, path = "./Database/DatabaseDesAdm_v2.csv")



# Passa o log nas colunas
for (coluna in colnames(dados)) {
  if(coluna == "Data")  {
    cat(sprintf("\nPulando coluna %s", coluna))
  } else {
    cat(sprintf("\nPassando o log em %s", coluna))
    dados[, coluna] = log(dados[[coluna]])
  }
}


# Salva os dados para conferencia
write_excel_csv(x = dados, path = "./Database/DatabaseDesAdm_v3.csv")

