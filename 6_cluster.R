
## ---- Cluster aglomerativo hierarquico ---- ##

# Pacotes necessários
library(ade4)
library(vegan)
library(gclus)
library(cluster)
library(RColorBrewer)
library(labdsv)

# Planilha de dados (livro do Boccard)
spe=read.csv("DoubsSpe.csv")
env=read.csv("DoubsEnv.csv")
spa=read.csv("DoubsSpa.csv")

# Observando a planilha de dados 
spe
env
spa

## --> Função Hclust
## Deconstand: Comando do pacote Vegan para transformação/normalização dos dados
spe.norm <- decostand(spe[,-1], "normalize")
spe.norm #dados transformados

## Para a criação de uma matriz de distâncias podemos usar tanto o comando "dist" como o "vegdist"
spe.ch <- vegdist(spe.norm, "euc") # Aplicando a normalização com a distância euclidiana temos o 
                                   # método de 'chord' para aglomeração.          
spe.ch

## O comando 'hclust' monta o cluster com uso do método 'single'
spe.ch.single <- hclust(spe.ch, method="single")

## Mostrando o cluster no formato gráfico
plot(spe.ch.single)

## Arrumando o gráfico
dev.new(title="Fish - Chord - Single linkage", width=12, height=8) # Abre uma janela extra 
                                                                   # com opções extras para salvar fig
plot(spe.ch.single, main="Cluster - Single", ylab="Distância euclidiana", 
     xlab="", labels=rownames(spe), hang=-1, xpd=NA, sub ="")

# main: Título do gráfico
# ylab: nome do eixo Y
# xlab: nome do eixo x
# labels: nomes dos agrupados
# hang=-1: para deixar os nomes dos agrupados pareados
# sub="": para retirar a legenda em baixo

# E com outros métodos de ligação?
# Método de ligação completa
spe.ch.complete <- hclust(spe.ch, method="complete")
summary(spe.ch.complete)
dev.new(title="Fish - Chord - Complete linkage", width=12, height=8)
plot(spe.ch.complete, main="Cluster - Completa", ylab="Distância euclidiana", 
     xlab="", labels=rownames(spe), hang=-1, xpd=NA, sub ="")

# Comparando os dois métodos
dev.new(title="Fish - Chord - Complete linkage", width=12, height=8)
par(mfrow=c(1,2))
plot(spe.ch.single, main="Cluster - Single", ylab="Distância euclidiana", 
     xlab="", labels=rownames(spe), hang=-1, xpd=NA, sub ="")
plot(spe.ch.complete, main="Cluster - Completa", ylab="Distância euclidiana", 
     xlab="", labels=rownames(spe), hang=-1, xpd=NA, sub ="")
dev.off()

## !! Métodos completos tendem a formar grupos pequenos em função de sua natureza matemática
##    exigir a ligação entre todos os objetos em análise !!

## Método aglomerátivo atraves da média (UPGMA - Unweighted Pair Group Method with Arithmetic Mean)
spe.ch.UPGMA <- hclust(spe.ch, method="average")
dev.new(title="Fish - Chord - UPGMA", width=12, height=8)
plot(spe.ch.UPGMA, main="Cluster - UPGMA", ylab="Distância euclidiana", 
     xlab="", labels=rownames(spe), hang=-1, xpd=NA, sub ="")

## Método aglomerátivo atráves do centróide
spe.ch.centroid <- hclust(spe.ch, method="centroid")
dev.new(title="Fish - Chord - Centroid", width=12, height=8)
plot(spe.ch.centroid, main="Cluster - Centróide", ylab="Distância euclidiana", 
     xlab="", labels=rownames(spe), hang=-1, xpd=NA, sub ="")
## !! Boccard: "Resultado deste método é o pesadelo do ecólogo, e por isso é pouco utilizado"

## Método aglomerátivo de Ward
spe.ch.ward <- hclust(spe.ch, method="ward.D2")
dev.new(title="Fish - Chord - Ward", width=12, height=8)
plot(spe.ch.ward, main="Cluster - Wars", ylab="Distância euclidiana", 
     xlab="", labels=rownames(spe), hang=-1, xpd=NA, sub ="")

## --- Mas qual método é o melhor? Em demonstrar a realidade? --- ##

## --> Coeficiente cofenético
# Single
spe.ch.single.coph <- cophenetic(spe.ch.single)
cor(spe.ch, spe.ch.single.coph) # 0.5015116
# Complete
spe.ch.comp.coph <- cophenetic(spe.ch.complete)
cor(spe.ch, spe.ch.comp.coph) # 0.7567998
# UPGMA
spe.ch.UPGMA.coph <- cophenetic(spe.ch.UPGMA)
cor(spe.ch, spe.ch.UPGMA.coph) # 0.8537529
# Ward 
spe.ch.ward.coph <- cophenetic(spe.ch.ward)
cor(spe.ch, spe.ch.ward.coph) # 0.7821555

## -- Diagrama de shepard
dev.new(title="Cophenetic correlation", width=8, height=8)
par(mfrow=c(2,2))
plot(spe.ch, spe.ch.single.coph, xlab="Chord distance", 
     ylab="Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)),
     main=c("Single linkage", paste("Cophenetic correlation =",
                                    round(cor(spe.ch, spe.ch.single.coph),3))))
abline(0,1)
lines(lowess(spe.ch, spe.ch.single.coph), col="red")
plot(spe.ch, spe.ch.comp.coph, xlab="Chord distance", 
     ylab="Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)),
     main=c("Complete linkage", paste("Cophenetic correlation =",
                                      round(cor(spe.ch, spe.ch.comp.coph),3))))
abline(0,1)
lines(lowess(spe.ch, spe.ch.comp.coph), col="red")
plot(spe.ch, spe.ch.UPGMA.coph, xlab="Chord distance", 
     ylab="Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)),
     main=c("UPGMA", paste("Cophenetic correlation =",
                           round(cor(spe.ch, spe.ch.UPGMA.coph),3))))
abline(0,1)
lines(lowess(spe.ch, spe.ch.UPGMA.coph), col="red")
plot(spe.ch, spe.ch.ward.coph, xlab="Chord distance", 
     ylab="Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), 
     ylim=c(0,max(spe.ch.ward$height)),
     main=c("Ward clustering", paste("Cophenetic correlation =",
                                     round(cor(spe.ch, spe.ch.ward.coph),3))))
abline(0,1)
lines(lowess(spe.ch, spe.ch.ward.coph), col="red")
dev.off()

# Distância de Gower (1983)
(gow.dist.single <- sum((spe.ch-spe.ch.single.coph)^2)) #  95.41391
(gow.dist.comp <- sum((spe.ch-spe.ch.comp.coph)^2)) # 43.46711
(gow.dist.UPGMA <- sum((spe.ch-spe.ch.UPGMA.coph)^2)) # 12.22276
(gow.dist.ward <- sum((spe.ch-spe.ch.ward.coph)^2)) # 1286.697

## ---- Fazendo um looping para calcular todos os métodos de correlação de uma única vez ---- ##
 
met=c('single', 'complete', 'average') # vetor com os métodos de ligação
ccc<-numeric(0) # vetor númerico para armazenar as correlações cofenéticas

for (i in 1:3){ # inicia um loop para realização do cluster para cada métodos de ligação
  cluster <- hclust(spe.ch, met[i])
  coph <- cophenetic(cluster)
  ccc[i] <- cor(spe.ch, coph)}

cluster
coph
ccc ## qual é o melhor?


