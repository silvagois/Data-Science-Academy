# Medidas de Posi��o Relativa

# Definindo a pasta de trabalho
# Substitua o caminho abaixo pela pasta no seu computador
setwd("D:/Dropbox/DSA/PowerBI-DataScience/Cap11/04-Medidas-Posicao-Relativa")
getwd()


# Carregando o dataset
carros <- read.csv("carros.csv")

# Resumo dos dados
head(carros)
str(carros)

# Medidas de Tend�ncia Central
summary(carros$ano)
summary(carros[c('preco', 'kilometragem')]) # buscando por colunas vetor de colunas


## Explorando vari�veis num�ricas
mean(carros$preco)
median(carros$preco)
quantile(carros$preco)
quantile(carros$preco, probs = c(0.01, 0.99)) # definindo o percentual do quartil
quantile(carros$preco, seq(from = 0, to = 1, by = 0.20))# definindo de 20 em 20%
IQR(carros$preco) # Diferen�a entre Q3 e Q1
range(carros$preco)
summary(carros$preco)
diff(range(carros$preco))