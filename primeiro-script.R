# Dados para trabalho
idade = c(24, 48, 32, 65, 38, 56, 74, 19, 29, 22)
peso = c(80, 67, 49, 55, 89, 72, 45, 88, 56, 74)
altura = c(180, 165, 162, 175, 172, 165, 168, 185, 172, 168)
sexo = c('M', 'F', 'M', 'F', 'M', 'F', 'M', 'F', 'M', 'F')

n = length(tabela$altura)
alturaOrdenada = sort(tabela$altura, decreasing = F)
(alturaOrdenada[(n/2)] + alturaOrdenada[(n/2 + 1)])/2

median(tabela$altura) # media em R
quantile(tabela$altura)
quantile(tabela$altura, 0.10)
summary(tabela$altura)
summary(tabela)
var(tabela$altura)
sd(tabela$altura)
(sd(tabela$altura)/mean(tabela$altura))*100
cor(tabela$idade, tabela$peso, method = "pearson")
cor(tabela$idade, tabela$altura, method = "pearson")
cor(tabela$peso, tabela$altura, method = "pearson")


# IMPORTANDO GGPLOT P/ GERAR GRAFICOS :)

library(ggplot2)
x = seq(from=-5, to=5, length.out = 100)
f = dnorm(x)

#gerando um grafico de distribuicao normal
ggplot(data.frame(x=x,y=f), aes(x=x, y=y)) + geom_line() 

#gerando um grafico com 2 curvas 
u = rnorm(1000)
ggplot(data.frame(x=u), aes(x=x)) + geom_density() + geom_line(data=data.frame(x=x, y=f), aes(x=x, y=y), linetype=2)

# gerando um grafico com distribuicao lognormal
x = seq(from=-5, to=5, length.out = 100)
f = dlnorm(x)
u = rlnorm(1000)
ggplot(data.frame(x=u), aes(x=x)) + geom_density() + geom_line(data=data.frame(x=x, y=f), aes(x=x, y=y), linetype=2)

# gerando um grafico com lognormal e normal
f2 = dnorm(x)
u = rlnorm(1000)
  ggplot(data.frame(u=u), aes(x=log(u))) + geom_density() + geom_line(data=data.frame(x=x, y=f2), aes(x=x, y=y), linetype=2)

  # vetores
  v = c(1, 2, 3)
  print(v)
  
  # multiplicacao de vetores
  a = c(1, 2, 3)
  b = c(4, 5, 6)
  print(a*b)
  
  # criando uma matriz
  A = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
  print(A)
  
  B = as.matrix(data.frame(x = c(1, 0), y = c(0, 1)))
  print(B)
  
  # extraindo elementos
  A = matrix(c(1, 2, 3 , 4, 5, 6), nrow = 3, ncol = 2, byrow = TRUE)
  print(A)
  
  R = matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrow = 4, ncol = 3, byrow = TRUE)
  
  # combinando matrizes
  
  A = matrix(c(1, 2, 3, 4), nrow = 10, ncol = 10)
  B = matrix(c(5, 6, 7, 8), nrow = 10, ncol = 10)
  
  rbind(A,B)
  cbind(A,B)  

  # matriz diagonal
  diag(nrow = 100, ncol = 100)
  
  # multiplicacao entre matrizes
  A = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2, byrow = TRUE)
  B = matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
  
  print(A %*% B)
  print(B %*% A)
  print(t(A)) #transposicao de matriz = trocar linha por colunau
  
  
# matriz inversa
A = matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
print(solve(A))
solve(A) %*% A == diag(nrow = nrow(A), ncol = ncol(A))


# decomposicao de matriz
library(Matrix)
A = matrix(c(1, 2, 2, 1), nrow = 2, byrow = TRUE)
luA = lu(A)
expand(luA)