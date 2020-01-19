######################### Projeto Machine Learning


getwd()
setwd("C:\Users\marco\Documents\Repositorio-Git\Data-Science-Academy\Power-BI-Data-Science\cap-10-PowerBi-LinguagemR\ProjetoInadiplenciaR")

########################## Pacotes do R ######################################

# Instalando pacotes para o projeto(os pacotes precisam ser instalados apenas uma vez)

install.packages("Amelia")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape")
install.packages("randomForest")

# CArregando pacotes
library(Amelia)
library(caret)
library(ggplot2)
library(dplyr)
library(reshape)
library(randomForest)

# Carregando o DataSet
dataset <- read.csv("credit-card.csv")
#Visualizando o dataset
print(dataset)

#Visualizando dados de sua estrutura
View(dataset)
str(dataset)
head(dataset)


######################### ETL LIMPANDO E TRANSFORMANDO OS DADOS ##################

# Convertendo os atributos idade, sexo, escolaridade e estado civil para fatores 
# do tipo (categoria) os mesmo estão inteiros 

#Idade
head(dataset$AGE)
dataset$AGE <- cut(dataset$AGE, c(0, 30, 50, 100), labels=c("Jovem","Adulto","Idoso"))
head(dataset$AGE)

# Sexo
head(dataset$SEX)
dataset$SEX <- cut(dataset$SEX, c(0,1,2), labels=c("Masculino","Feminino"))
head(dataset$SEX)


# Escolaridade
head(dataset$EDUCATION)
dataset$EDUCATION <- cut(dataset$EDUCATION, c("Desconhecido","Casado","Solteiro","Outros"), labels=c("Pos Graduado","Graduado","Ensino Medio","Outros"))
head(dataset$EDUCATION)

# Estado Civil
head(dataset$MARRIAGE)
dataset$MARRIAGE <- cut(dataset$MARRIAGE, c(-1,0,1,2,3), labels=c("Desconhecido","Casado","Solteiro","Outros"))
head(dataset$MARRIAGE)

# Pagamentos
dataset$PAY_0 <- as.factor(dataset$PAY_0)
dataset$PAY_2 <- as.factor(dataset$PAY_2)
dataset$PAY_3 <- as.factor(dataset$PAY_3)
dataset$PAY_4 <- as.factor(dataset$PAY_4)
dataset$PAY_5 <- as.factor(dataset$PAY_5)
dataset$PAY_6 <- as.factor(dataset$PAY_6)

# Alterando a variável depedente para o tipo fator
dataset$default.payment.next.month <- as.factor(dataset$default.payment.next.month)
head(dataset)

# Renomeando a Coluna de Classe de pagamento para Inadiplente 
colnames(dataset)
colnames(dataset)[25] <- "inadiplente"
colnames(dataset)

# Verificando e removendo valores missing ou null
sapply(dataset, function(x) sum(is.na(x)))
missmap(dataset, main="Valores Missing Observados")#gera um grafico
dataset <- na.omit(dataset)

# Removendo a coluna ID
dataset$ID <- NULL


# Total de inadiplentes verus no-inadiplentes
table(dataset$inadiplente) #usando a função table chamando a variavel depedente

# Plotando a distribuição de inadiplente e não inadiplentes 
qplot(inadiplente, data = dataset, geom = "bar")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Set see
set.seed(12345)

# Amostragem estratificada. Selecione as linhas de acordo com a variable defalt.payment.next.month como strata
# Dividindo o conjunto de dados em treino e teste

TrainingDataIndex <-createDataPartition(dataset$inadiplente, p = 0.45, list = FALSE)
TrainingDataIndex




