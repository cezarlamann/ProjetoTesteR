install.packages("devtools")
install.packages("arules")
install.packages("ggplot2")
install.packages("caret")
install.packages("iterators")
install.packages("geosphere")

library(caret)
library(devtools)
# library(ggbiplot)
library(arules)
library(ggplot2)
library(iterators)
library(geosphere)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

baseSolo = read.csv("datasets/baseSolo.csv")
baseColheita = read.csv("datasets/baseColheita.csv")
baseFinal = read.csv("datasets/dataset.csv")

# #Remove colunas com pouca variancia na baseSolo
baseSolo.nzv = nearZeroVar(baseSolo, saveMetrics = TRUE)
baseSolo = baseSolo[c(rownames(baseSolo.nzv[baseSolo.nzv$nzv == FALSE, ])) ]
rm(baseSolo.nzv)


# par(mfrow=c(4,5), mar=c(2,5,2,1), las=1, bty="n")
boxplot(baseSolo$Calcio, main="Calcio", ylab="Valores")
boxplot(baseSolo$Magnesio, main="Magnesio", ylab="Valores")
boxplot(baseSolo$Magnesio, main="Aluminio", ylab="Valores")
boxplot(baseSolo$Potassio, main="Potassio", ylab="Valores")
boxplot(baseSolo$Fosforo, main="Fosforo", ylab="Valores")
boxplot(baseSolo$Enxofre, main="Enxofre", ylab="Valores")
boxplot(baseSolo$MO, main="MO", ylab="Valores")
boxplot(baseSolo$CTC, main="CTC", ylab="Valores")
boxplot(baseSolo$SaturacaoPorBases, main="SaturacaoPorBases", ylab="Valores")
boxplot(baseSolo$CalcioSaturacaoDeBases, main="CalcioSaturacaoDeBases", ylab="Valores")
boxplot(baseSolo$MagnesioSaturacaoDebases, main="MagnesioSaturacaoDebases", ylab="Valores")
boxplot(baseSolo$PotassioSaturacaoDeBases, main="PotassioSaturacaoDeBases", ylab="Valores")
boxplot(baseSolo$pH, main="pH", ylab="Valores")

dataset = baseFinal
dataset.new <- baseFinal[, -(17:18)]	#Retira Latitude e Longitude da baseSolo
dataset.pca <- prcomp(dataset.new[, 5:16], center = TRUE, scale. = TRUE) #Retira Latitude e Longitude da baseColheita para Calcular PCA

g <- ggbiplot(dataset.pca, groups = dataset$Produtividade, obs.scale = 1, var.scale = 1, circle = TRUE, ellipse = TRUE)
print(g)

#(6) MAPA
summary(dataset$Rendimento.seco)

dataset[, 'Produtividade'] <- ifelse(dataset$Rendimento.seco >= median(dataset$Rendimento.seco), 1, 0)
dataset$Produtividade[dataset$Rendimento.seco<9.3] <- 0 #baixa
dataset$Produtividade[dataset$Rendimento.seco>10.9] <- 1 #alta

dfDataSet = data.frame(dataset)
dfAgregado = setNames(aggregate(dfDataSet$Rendimento.seco, by=list(PontoAmostral=dfDataSet$PontosAmostrais), FUN=median), c("PontoAmostral", "Rendimento"))
dfAgregado = dfAgregado[order(dfAgregado$Rendimento, decreasing = TRUE),]

PontoAmostralMaisProdutivo = dfAgregado[1,1]
PontoAmostralMenosProdutivo = dfAgregado[length(dfAgregado[,1]),1]

colunas <- c("Calcio","Magnesio","Al","Potassio","Fosforo","Enxofre","MO","CTC","SaturacaoPorBases","CalcioSaturacaoDeBases","MagnesioSaturacaoDebases","PotassioSaturacaoDeBases","pH")
baseAlta.new <- dataset[which(dataset$PontosAmostrais == PontoAmostralMaisProdutivo), colunas]
baseAlta.new[, colunas] <- lapply(baseAlta.new[, colunas], factor)
baseAlta.tr <- apriori(
  baseAlta.new,
  parameter = list(support = 1, minlen = 13, maxlen = 13, target = "frequent")
)

baseBaixa.new <- dataset[which(dataset$PontosAmostrais == PontoAmostralMenosProdutivo), colunas]
baseBaixa.new[, colunas] <- lapply(baseBaixa.new[, colunas], factor)
baseBaixa.tr <- apriori(
  baseBaixa.new,
  parameter = list(support = 1, minlen = 13, maxlen = 13, target = "frequent")
)

inspect(baseAlta.tr)
inspect(baseBaixa.tr)