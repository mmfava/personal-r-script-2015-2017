

## XXV SEMANA ACADÊMICA DE CIÊNCIAS BIOLÓGICAS
## UNIVERSIDADE ESTADUAL DO OESTE DO PARANÁ
## PRÁTICA 2 - ANÁLISE DE COMPONENTES PRINCIPAIS

## A análise de componentes principais (PCA) é uma forma de simplificar uma base de dados multivari-
## ada. Está análise auxilia na representação de uma variável estatística reduzida, baseada na vari-
## ação de um banco de dados complexo.

## O banco de dados deve conter apenas variáveis quantitativas. Se a base de dados contiver uma vari-
## ávelqualitativa (não-númerica), você deve excluí-la

## A função que usaremos é a 'princomp', a qual calculará os devios-padrões dos componentes princi-
## pais. Entretanto, existem outras informações que devem ser geradas em uma PCA como, por exemplo,
## as cargas fatoriais e os escores padronizados.

## Passo 1: Carregando o pacote necessário a execução da PCA
library(vegan)
library(ggfortify)
library(psych)
library(MVN)

## Passo 2: Banco de dados 

## Abrir a base de dados
dados=read.csv2(file.choose(),sep=",",dec=".",header=TRUE,strip.white=TRUE)
dados
names(dados)

## Dados sem a linha nominal
dados1=dados[,-1]
dados1
attach(dados1)

## Pressupostos para a aplicação da Análise de componentes principais

## a. Todas as variáveis são númericas?
dados1

## b. Existe associação entre as variáveis?

## Correlação linear de Pearson
cor(dados1)
cor=cor(dados1)
cor
pairs(cbind(dados1), pch="+", col="blue")

## Teste de esferécidade de Bartlett
## Teste de esfericidade de bartlett entre a matriz de correlação (cor) e o tamanho da amostra (n).
## Ele apresenta a significância da associação entre pelo menos algumas
## das variáveis amostradas (P<0,05).
bartlett.test(dados1)
# Resultado: X² =  , P = 

## c. Critério de Kayser-Meyer-Olkin (KMO)
## Adequação amostral segundo a medida KMO.
## > 0,9       Excelente
## (0,8; 0,9]  Meritória
## (0,7; 0,8]  Intermediária
## (0,6; 0,7]  Medíocre
## (0,5; 0,6]  Mísera
## < 0,5       Inaceitável
KMO(cor)

names(dados)

dados=dados[,-7]
dados
names(dados)
attach(dados)

dados1=dados[,-1]
dados1

cor=cor(dados1)
cor

pairs(cbind(dados1), pch="+", col="blue")

bartlett.test(dados1)

KMO(cor)

## f. normalidade multivariada
mardiaTest(dados1)

## e. Caso as variáveis estejam de acordo, vamos realizar uma análise descritiva para observar
##    o comportamento de cada variável entre os pontos. 

par(mfrow=c(2,4))
boxplot(Temperatura~dados$ponto, ylab="Temperatura")
boxplot(pH~dados$ponto, ylab="pH")
boxplot(Potêncial.de.oxirredução~dados$ponto, ylab="Potêncial de oxirredução")
boxplot(Turbidez~dados$ponto, ylab="Turbidez")
boxplot(Oxigênio.dissolvido~dados$ponto, ylab="Oxigênio dissolvido")
boxplot(Espécie.B~dados$ponto, ylab="Espécie B")
dev.off()

## e. Análise de Componentes Principais - PCA
pca.dados=princomp(dados[,-1], cor=T)
pca.dados

## Resultados da análise: 
## Visualiza a proporção da variância total explicativa de cada componente principal.
summary(pca.dados)
## Standard deviation = Autovalor
## Proportion of Variance = o quanto cada componente explica a variação dos dados
## Cumulative Proportion = % acumulada de explicabilidade de todos os fatores

## Cargas fatoriais: coeficientes das combinações lineares das variáveis contínuas
pca.dados$loadings

## Escores padronizados
pca.dados$scores

## Método Plot: 
## este plot representa as variâncias (y) associadas com os componentes principais (x). 
## É um método interessante para decidir quantos componentes principais serio utilizados na 
## análise. 
plot(pca.dados)
## Critério de Broken-stick: Autovalor > 1

## Diagrama de ordenação: representação gráfica da PCA
attach(dados)
autoplot(pca.dados, data=dados, colour='ponto') # o comando colour coloriu os pontos amostrados

autoplot(pca.dados, data=dados, colour = 'ponto', label=TRUE) # o comando 'label=T' dá nomes númeri-
                                                              # cos na ordem da tabela.

autoplot(pca.dados, data=dados, colour = 'ponto', loadings=TRUE) # inclui os auto-vetores

autoplot(pca.dados, data=dados, colour = 'ponto',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3) # agora nós colocamos os nomes dos autove-
                                                         # tores.
