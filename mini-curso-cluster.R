
# Mini-curso em ecologia númerica
# XXV Semana acadêmica de BIOLOGIA - UNIOESTE Cascavel

# Prática 1: Análise de agrupamento - CLUSTER
#            Métodos hierarquicos aglomerativos

# O cluster hierarquico (função hcluster) está no diretório padrão do R não sendo necessário,
# portanto, realizar o download de qualquer pacote para execução dessa função. Para a constru-
# -ção de cluster via R, é necessária a construção de uma matriz de distância. O R possui
# o comando padrão chamado "dist" para a construção de matrizes de distância, porém esse comando
# não contém todos os indíces ecológicos que podem ser usados como medidas de distância. Em razão
# deste é comum os pesquisadores utilizarem o comando para distâncias "vegdist" disponível no 
# pacote vegan.
# Os índices ecológicos são uteis por apresentarem valor máximo = 1 para absoluta diferença entre 
# locais (ou seja, '1' significa 'não compartilha espécies'). Em contraste, a distância padrão mais 
# utilizada, euclidiana, não possui limite superior, mas varia com a soma das abundâncias totais dos
# locais quando não há espécies compartilhadas, e utiliza quadrado das diferenças das abundâncias.
# Na atividade de hoje iremos prosseguir com a distância euclidiana, porém vocês podem tentar outras
# medidas em casa com os dados usados neste minicurso. 

# Carregando os pacotes necessários 
library(vegan)

# A atividade:

# Cinco locais foram amostrados e o pesquisador deseja saber, com base na frequência absoluta
# das espécies amostradas, qual é o mais próximamente parecida com qual.

## --> Passo 1: Inseririndo os dados amostrados.

# Criando a matriz com os dados (comando 'matrix')
P1=matrix(c(0,0,10,8,0,0,0,12,9,0,0,0,13,5,10,2,3,0,4,12,5,10,0,0,16,15,20,0,0,0),nrow=6,byrow=5)
P1
# Colocando nome nas colunas/linhas da matriz 
# colunas - comando 'colnames(nome da matriz)'
# linhas - comando 'rownames (nome das linhas)'
colnames(P1)=c("A", "B", "C", "D","E")
rownames(P1)=c("Local 1", "Local 2", "Local 3", "Local 4", "Local 5","Local 6")
P1 # versão final da nossa matriz/tabela


## --> Passo 2: Criar uma matriz de distâncias

# Criar a matriz de similaridade/dissimilaridade - mas qual distância utilizar?
# Existem dois comandos do pacote vegan bem utilizados - "dist" e o "vegdist"

P1 #  Matriz original

eu=vegdist(P1, method="euclidean", binary=F) # Matriz com distância de bray-curtis - comando "vegdist"
eu

# --> Passo 3: O cluster

# 1. Método do vizinho mais próximo - method ="single"

# Comando de criação de hirarquia cluster via metodo do vizinho mais próximo
vizinhop=hclust(eu, method="single") 

# Visualização gráfica
plot(vizinhop) # o cluster

plot(vizinhop, main="Método do vizinho mais próximo", sub="",
     ylab="Distância euclidiana", xlab="Locais", hang = -1) # Deixando mais paupavel 

rect.hclust(vizinhop, k=3, border="red") # divisão de grupos

cutree(vizinhop, 3) # visualização dos indivíduos nos grupos


# 2. Método da ligação completa ou do vizinho mais distante ("Complete linkage") 
#    method="complete"

# Comando de criação de hirarquia cluster via metodo do vizinho mais distante
vizinhod=hclust(eu, method="complete") 

# Visualização gráfica
plot(vizinhod) # o cluster

plot(vizinhod, main="Método do vizinho mais distante", sub="",
     ylab="Distância euclidiana", xlab="Locais", hang = -1) # Deixando mais paupavel 

rect.hclust(vizinhod, k=3, border="red") # divisão de grupos

cutree(vizinhod, 3) # visualização dos indivíduos nos grupos

# 3. Método da ligação média (average linkage):

# Comando de criação de hirarquia cluster via metodo do vizinho mais distante
media=hclust(eu, method="average") 

# Visualização gráfica
plot(media) # o cluster

plot(media, main="Método da ligação média", sub="",
     ylab="Distância euclidiana", xlab="Locais", hang = -1) # Deixando mais paupavel 

rect.hclust(media, k=3, border="red") # divisão de grupos

cutree(media, 3) # visualização dos indivíduos nos grupos

# --> Passo 4: Mas qual cluster? Avaliação!
cor(eu, cophenetic(vizinhop)) # 0,8985643
cor.test(eu, cophenetic(vizinhop)) # t= 7,3827; df= 13, P < 0,001
plot(eu, cophenetic(vizinhop), main="Coeficiente cofenético = 0,90",
     xlab="Valores da matriz", ylab="Valores do cluster")
abline(0, 1)

cor(eu, cophenetic(vizinhod)) # 0,9070452
cor.test(eu, cophenetic(vizinhod)) # t= 7,7676; df= 13, P < 0,001
plot(eu, cophenetic(vizinhod), main="Coeficiente cofenético = 0,91",
     xlab="Valores da matriz", ylab="Valores do cluster" )
abline(0, 1)

cor(eu, cophenetic(media)) # 0,9236304
cor.test(eu, cophenetic(media)) # t= 8,6886; df= 13, P < 0,001
plot(eu, cophenetic(media), main="Coeficiente cofenético = 0,92",
     xlab="Valores da matriz", ylab="Valores do cluster")
abline(0, 1)

