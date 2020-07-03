# clear all
rm(list = ls())

source(file = "./helper fucntions/matrix_Functions.r")

library(urca)

# tamanho da simulacao
n = 100

# determinação de alpha e beta de cointegracao
Dx1 <- rnorm(n)
Dx2 <- rnorm(n)
Dx3 <- rnorm(n)


X1 = rep(0, n)
X2 = rep(0, n)
X3 = rep(0, n)

for (i in 2:n) {
  X1[i] <- X1[i-1] + rnorm(1)
  X2[i] <- X2[i-1] + rnorm(1)
  X3[i] <- X3[i-1] + rnorm(1)
}

X4 = X1 + X2 + X3 + rnorm(1)
X5 = X1 - X2 + rnorm(1)

data = cbind(X1, X2, X3, X4, X5)

Y <- diff(data)
X <- data[1:99,]


PI.estimated = solve(t(X)%*%X)%*%t(X)%*%Y


data2 <-  as.data.frame(data)

ca.jo(data2, type = "trace", K=2, ecdet = "none", spec = "transitory")


ggplot(data2) +
  geom_line(aes(x=1:n, y = X1), color="red")+
  geom_line(aes(x=1:n, y = X2), color="blue")+
  geom_line(aes(x=1:n, y = X3))+
  geom_line(aes(x=1:n, y = X4))+
  geom_line(aes(x=1:n, y = X5))

# Calculo da matrix de longo prazo
PI = alpha %*% t(beta)

PI = matrix(c(0,0,0,1,1,0,0,0,1,-1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol=5)
eigen(PI)

  data <- matrix(c(0, 0, 0), nrow = n, ncol = 3, byrow = T)
colnames(data) <- c("x1", "x2", "x3")


for(i in 2:n){
  e = matrix(rnorm(3), ncol = 1, nrow = 3)
  data[i, ] = (eye(3)+PI)%*%data[i-1, ] + e
}


head(data)

ggplot(as_tibble(data))+
  geom_line(aes(x=1:n, y=x1), color="red") +
  geom_line(aes(x=1:n, y=x2), color="blue") +
  geom_line(aes(x=1:n, y=x3), color="black")


write.csv(data, file = "coint_1.csv")




aa = matrix(c(-0.37286, 0.071547,
0.23594,     -0.52673,
0.14372 ,     0.43813,
0.031540 ,    0.064711), ncol = 2, nrow = 4, byrow = T)





aa[,1]/aa[1,1]

aa[,2]/aa[2,2]




