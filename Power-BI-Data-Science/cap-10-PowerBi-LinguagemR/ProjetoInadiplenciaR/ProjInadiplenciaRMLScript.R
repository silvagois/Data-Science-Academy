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
library(caret) #Pacote usado para machine learning 
library(ggplot2)
library(dplyr)
library(reshape)
library(randomForest)

# Carregando o DataSet
dataset <- read.csv("credit-card.csv")
#Visualizando o dataset
print(dataset)

#Visualizando dados de sua estrutura
View(dataset$EDUCATION)
str(dataset)
head(dataset)


######################### ETL LIMPANDO E TRANSFORMANDO OS DADOS ##################

# Convertendo os atributos idade, sexo, escolaridade e estado civil para fatores 
# do tipo (categoria) os mesmo estÃ£o inteiros 

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
dataset$EDUCATION <- cut(dataset$EDUCATION, c(0,1,2,3,4), labels=c("Pos Graduado","Graduado","Ensino Medio","Outros"))
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

# Alterando a variÃ¡vel depedente para o tipo fator
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
table(dataset$inadiplente) #usando a funÃ§Ã£o table chamando a variavel depedente

# Plotando a distribuiÃ§Ã£o de inadiplente e nÃ£o inadiplentes 
qplot(inadiplente, data = dataset, geom = "bar")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Set see
set.seed(12345)

# Amostragem estratificada. Selecione as linhas de acordo com a variable defalt.payment.next.month como strata

# Dividindo o conjunto de dados em treino e teste
TrainingDataIndex <-createDataPartition(dataset$inadiplente, p = 0.45, list = FALSE)
TrainingDataIndex

#Criando uma variável para inserir os dados de treino
#Conjunto de dados de treino trainData
trainData <- dataset[TrainingDataIndex,]
table(trainData$inadiplente)

#Mostrando a porcentagem do dado de treino
prop.table(table(trainData$inadiplente))

#Numero de linhas no dataset
nrow(trainData)

# Comparando os dados de treino com o dados do dataset original
DistributionCompare <- cbind(prop.table(table(trainData$inadiplente)),prop.table(table(dataset$inadiplente)))
colnames(DistributionCompare) <-c("Treinamento","Original")
DistributionCompare

#Melt Data -  Converte colunas em linhas
meltedDcomp <- melt(DistributionCompare)
meltedDcomp

# Plot para ver a diferença dos dados de treino vs dados original
ggplot(meltedDcomp, aes(x = X1, y = value)) + geom_bar( aes(fill = X2), stat = "identity", position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# agora data set de teste 
#tudo que não esta no de treino ira para o de teste
testData <- dataset[-TrainingDataIndex,]
testData
View(testData)

# Validação dos dados de teste cross validation
# Validar o modelo 
trainingParameters <- trainControl(method = "cv", number = 10)

####################################################################################
################## RandomForest Classification MOdel ##########################

#Construindo o modelo
rf_model <- randomForest(inadiplente ~ ., data = trainData)







