dataset <- read.csv("Vendas.csv")
print(dataset)

str(dataset)

plot(dataset$Custo, dataset$Valor)


#instalando ggplot2
install.packages("ggplot2")
#usando na instância
library(ggplot2)

#função qplot, uma das funções do pacote ggplot2
qplot(Valor, Custo, data= dataset, geom = "point")



