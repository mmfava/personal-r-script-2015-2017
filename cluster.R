# Estatística Avançada: Cluster Aglomerativo Hierárquico
# Marília Melo Favalesso
# mariliabioufpr@gmail.com

# Pacotes necessários
library(ade4)
library(vegan)
library(gclus)
library(cluster)
library(RColorBrewer)
library(labdsv)

# Carregando os dados
spe <- read.csv("DoubsSpe.csv")
env <- read.csv("DoubsEnv.csv")
spa <- read.csv("DoubsSpa.csv")

# Observando a planilha de dados 
head(spe)
head(env)
head(spa)

# Normalização dos dados usando o comando 'decostand' do pacote 'vegan'
spe.norm <- decostand(spe[,-1], "normalize")

# Criação de uma matriz de distâncias usando a distância euclidiana ('chord' method)
spe.ch <- vegdist(spe.norm, "euc")

# Criação de clusters usando diferentes métodos de ligação
spe.ch.single <- hclust(spe.ch, method="single")
spe.ch.complete <- hclust(spe.ch, method="complete")
spe.ch.UPGMA <- hclust(spe.ch, method="average")
spe.ch.centroid <- hclust(spe.ch, method="centroid")
spe.ch.ward <- hclust(spe.ch, method="ward.D2")

# Plotando os clusters
plot_cluster <- function(cluster, method) {
  dev.new(title = paste("Fish - Chord -", method, "linkage"), width = 12, height = 8)
  plot(cluster, main = paste("Cluster -", method), ylab = "Distância Euclidiana", 
       xlab = "", labels = rownames(spe), hang = -1, xpd = NA, sub = "")
}

plot_cluster(spe.ch.single, "Single")
plot_cluster(spe.ch.complete, "Complete")
plot_cluster(spe.ch.UPGMA, "UPGMA")
plot_cluster(spe.ch.centroid, "Centroid")
plot_cluster(spe.ch.ward, "Ward")

# Comparando os métodos 'single' e 'complete'
dev.new(title = "Fish - Chord - Single vs Complete linkage", width = 12, height = 8)
par(mfrow = c(1, 2))
plot(spe.ch.single, main = "Cluster - Single", ylab = "Distância Euclidiana", 
     xlab = "", labels = rownames(spe), hang = -1, xpd = NA, sub = "")
plot(spe.ch.complete, main = "Cluster - Complete", ylab = "Distância Euclidiana", 
     xlab = "", labels = rownames(spe), hang = -1, xpd = NA, sub = "")
dev.off()

# Coeficiente cofenético para avaliar os métodos de clustering
coph_corr <- function(cluster, dist_matrix) {
  coph <- cophenetic(cluster)
  corr <- cor(dist_matrix, coph)
  return(corr)
}

single_coph_corr <- coph_corr(spe.ch.single, spe.ch)
complete_coph_corr <- coph_corr(spe.ch.complete, spe.ch)
UPGMA_coph_corr <- coph_corr(spe.ch.UPGMA, spe.ch)
ward_coph_corr <- coph_corr(spe.ch.ward, spe.ch)

# Plotando os diagramas de Shepard
plot_shepard <- function(cluster, method, dist_matrix) {
  coph <- cophenetic(cluster)
  corr <- round(cor(dist_matrix, coph), 3)
  plot(dist_matrix, coph, xlab = "Chord distance", ylab = "Cophenetic distance", 
       asp = 1, xlim = c(0, sqrt(2)), ylim = c(0, sqrt(2)), 
       main = paste(method, "linkage", "\nCophenetic correlation =", corr))
  abline(0, 1)
  lines(lowess(dist_matrix, coph), col = "red")
}

dev.new(title = "Cophenetic correlation", width = 8, height = 8)
par(mfrow = c(2, 2))
plot_shepard(spe.ch.single, "Single", spe.ch)
plot_shepard(spe.ch.complete, "Complete", spe.ch)
plot_shepard(spe.ch.UPGMA, "UPGMA", spe.ch)
plot_shepard(spe.ch.ward, "Ward", spe.ch)
dev.off()

# Distância de Gower (1983) para cada método
gower_dist <- function(cluster, dist_matrix) {
  coph <- cophenetic(cluster)
  dist <- sum((dist_matrix - coph)^2)
  return(dist)
}

gower_dist_single <- gower_dist(spe.ch.single, spe.ch)
gower_dist_complete <- gower_dist(spe.ch.complete, spe.ch)
gower_dist_UPGMA <- gower_dist(spe.ch.UPGMA, spe.ch)
gower_dist_ward <- gower_dist(spe.ch.ward, spe.ch)

# Looping para calcular correlações cofenéticas para diferentes métodos
met <- c('single', 'complete', 'average')
ccc <- numeric(0)

for (i in 1:length(met)) {
  cluster <- hclust(spe.ch, method = met[i])
  coph <- cophenetic(cluster)
  ccc[i] <- cor(spe.ch, coph)
}

names(ccc) <- met
ccc # Melhor método com base na correlação cofenética
