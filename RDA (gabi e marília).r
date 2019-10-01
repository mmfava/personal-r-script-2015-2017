
## Universidade Estadual do Oeste do Paraná (UNIOESTE), Campus Cascavel
## Disciplina: Análise multivariada
## Profa. Luciana Pagliosa Carvalho Guedes
## Alunas: Gabriela Medeiros e Marília Melo Favalesso

## -- ANÁLISE DE REDUNDÂNCIA (RDA) -- ##

## Conceitualmente, a RDA é uma técnica multivariada que realiza uma regressão linear
## entre duas matrizes (resposta Y ~ preditora X), seguida de uma análise de componentes
## principais com a tabela de valores ajustados. 

## Ela é uma importante ferramenta dentro da ecológia, ajudando em análises de composição
## de comunidades.

## - Pacotes necessários para realização da análise
library(vegan) # O comando utilizado para a análise de redundância está contido no pacote
               # vegan.

## - Carregando os dados

## Dados das espécies
sp<-read.table('clipboard',header=T, row.names=1, dec = ",")###abrir planilha sp
sp

## Dados abióticos
abio<-read.table('clipboard',header=T, row.names=1, dec = ",")###abrir planilha abio
abio

## - Transformação nos dados
## Usamos o comando decostand, também do pacote vegan.
## Fazemos a transformação de Hellinge - transformação padrão da rda.
spe.hel<-decostand(sp,"hel") 

## - Aplicação da RDA

## Para realização da RDA usamos o comando rda() do pacote vegan
spe.rda<-rda(spe.hel~.,abio) 
## !! Lembrando que usamos a matriz transformada aqui!

## Resultado da RDA
summary(spe.rda)

## - Coeficiente de determinação 

## Existem dois meios de se calcular o R² e o R ajustado

## Primeiro: 
(R2<-RsquareAdj(spe.rda)$r.squared)

(R2adj<-RsquareAdj(spe.rda)$adj.r.squared)

## Segundo:
RsquareAdj(spe.rda)

# Também podemos fazer a RDA incluindo as variáveis abióticos escritas como em
# um modelo de regressão. Esse modo de descrição de modelo é idealizado para
# aplicação de post-testes de seleção de variáveis. 
attach(abio)
names(abio)
spe.rda<-rda(spe.hel~vaz+no3+no2+nh4+nid+pt+st+prec+temp+zmax+ph+od+cond) 

## - Índice VID: Fator de inflação da variância
## Ele faz a seleção das var abio - tem que ser menor que 0.10 pra saber se não tem colinearidade entre as variáveis
vif.cca(spe.rda)

## - Plotando a RDA em formato gráfico

## A maneira mais simples de representar uma RDA com o pacote vegan
## é com o comando plot seguida do nome da RDA.
plot(spe.rda)

## Mas também é possível customizar os gráficos:
colvec <- c("orange", "green4") # Escolhe as cores que vai usar
plot(spe.rda) # plota o gráfico
with(abio, points(spe.rda, display = "sites",cex=1, col = colvec, pch = 21, bg = colvec))#adiciona os pontos no estilo e cor que escolheu

## - Testando a significância da RDA
## Também é possível testar a significância da RDA, dos eixos da RDA e 
## das variáveis utilizadas.

anova.cca(spe.rda) # Testa a significância da RDA

anova.cca(spe.rda,by='axis') # Testa  a significância dos eixos

anova.cca(spe.rda,by='terms') # Testa a significância das variâveis

## - Salvando os ecores em .txt em sua pasta para você abrir e editar
## no excel ou no statistica posteriormente.
scores<-spe.rda$CCA
write.table(scores$wa,'scoreslocais.txt', sep='\t')
write.table(scores$biplot,'biplot.txt', sep='\t')
write.table(scores$v,'especies.txt', sep='\t')

