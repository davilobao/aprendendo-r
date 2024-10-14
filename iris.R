# novo script para o cap 4

dados = read.table('https://raw.githubusercontent.com/uiuc-cse/data-fa14/gh-pages/data/iris.csv', sep =',', header = T)

# tipo de objeto
class(dados)

# distribuicao de cada variavel
summary(dados)

# linhas e colunas
dim(dados)

# criando um grafico histograma
par(mfrow=c(1, 4))
for(i in 1:4) {
  hist(dados[,i], main = names(dados)[i])
}

# criando um grafico densidade
library(lattice)
par(mfrow=c(1,4))
for(i in 1:4) {
  plot(density(dados[,i]), main=names(dados)[i])
}

# criando um grafico boxplot
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(dados[,i], main=names(dados)[i])
}

# criando um grafico de correlacao
install.packages("corrplot")
library(corrplot)

correlations = cor(dados[,1:4])
corrplot(correlations, method = "circle")

# criando um grafico de dispersao
# nesse trecho do livro está somente pairs(dados), mas deu um erro de
# argumento nao numerico para essa funcao, entao tive que buscar uma solucao no stackoverflow
# entendi que se eu especificar que quero puxar todas as linhas e o intervalo de colunas, ele entende como tipo numero,
# caso nao ele puxa apenas a primeira linha (que e a que descreve as colunas) e da esse erro
pairs(dados[,1:4])  

# fica dando erro de argumento nao numerico, preciso encontrar a solucao para isso
pairs(species~., data = dados, col = dados$species)


# criando um grafico de dispersao separado por classes

