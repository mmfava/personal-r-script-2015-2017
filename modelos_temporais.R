## Modelos temporais 
## Script de Marília Melo Favalesso
## E-mail: mariliabioufpr@gmail.com
## Instituto Nacional de Medicina Tropical - Argentina

####################################################################

## SERIES TEMPORAIS NO R:

## --> Series temporais correspondem a dados coletados em um intervalo de tempo regular, ex: segundos, hora, dia ou ano. 
## --> Eles dependem de ordem, ou seja, os dados devem ser interpretados de acordo com a ordem em que os dados foram amostrados.
##     A ordem influência nos aspectos análiticos. 
## --> As séries temporais (ST) podem ser resumidas em: Medidas + Fatos + Unidade de tempo. 
## --> Podem ser univariadas (uma unica variável utilizada para suas próprias predições) ou multivariada (além da autocorrelação
##     é usada a variação de outros dados). 
## --> Usado para compreender fenomenos e gerar predições. 

## COMPONENTES DE UMA SÉRIE TEMPORAL:

## - Tendência
##   São os aumentos ou redução de valores ao longo do tempo. Ex. Se vemos o y indo de 0 até 100 em 10 anos. 
##   Séries sem tendências apresentão uma variação uniforme ao longo de zero. 

## - Sazonalidade
##   Períodos (x) com diferentes valores para a variável resposta (y); ou variação de y ao longo do tempo x. 
##   Padrões que ocorrem em interlos fixo de tempo. Ex. Temperatura ao longo dos meses, que vai variando entre as estações
##   do ano. 

## - Ciclo
##   Aumento ou redução de frequências sem intervalos fixos. Ex. O evento La ninã que modificam a variável climática.
##   Também pode ser uma crise econômica.

## - Erro (restante)
##   Tudo o que não é explicado pela 'tendência' e pela 'sazonalidade' é considerado erro, e é considerado que ele ocorra por 
##   eventos aleatórios. 

####################################################################

## PACOTES:

## carregar o pacote que faz as análises temporais no R (Forecast)
## e também o pacote para os gráficos: 
library(forecast)
library(ggplot2)

####################################################################

## SIMULNDO PASSEIOS ALEATÓRIOS
## É uma simulação que produz valores ao acaso para uma série temporal. Eles partem de um determinado ponto e vão "caminhando"
## de maneira aleatória. É um evento no qual não sabemos qual valor está por vir, apenas sabendo que ele partira do valor/ponto
## anterio; por isso sempre tem relação com o valor anterior. 
## São eventos não estacionários, não autocorrelacionados e imprevisivéis. 

# Passeio aleatório sem Drift. 
# Fixar a raíz para que a simulação gere os mesmos dados em qualquer computador 
set.seed(123) 
# Simulação de um passeio aleatório
n <- 1000
p0 <- 10
phi1 <- 1
pt <- rep(p0,n)
for (i in 1:(n-1)) {
  pt[i+1] <- phi1*pt[i] + rnorm(1)
}
# Visualização do passeio aleatório.
plot(pt, type = "l", xlab = "observações", ylab = "contagem de formigas", main = "Simulação Passeio Aleatório")

# Passeio aleatório com Drift.  
# Simulação de um passeio aleatório com drift (pt = mi + phi1*pt-1 + at)
n <- 1000
p0 <- 10
mi <- 0.2
phi1 <- 1
pt_drift <- rep(p0,n)
for (i in 1:(n-1)) {
  pt_drift[i+1] <- mi + phi1*pt_drift[i] + rnorm(1)
}
  
# Visualização do passeio aleatório com drift
plot(pt_drift, type = "l", xlab = "observações", ylab = "preço", main = "Simulação Passeio Aleatório com Drift")

# Simulação de um passeio aleatório com drift e tendência (pt = mi + trend + phi1*pt-1 + at)
n <- 1000
p0 <- 10
mi <- 0.2
phi1 <- 1
trend <- 1:n
pt_drift_trend <- rep(p0,n)
for (i in 1:(n-1)) {
  pt_drift_trend[i+1] <- mi + trend + phi1*pt_drift_trend[i] + rnorm(1)
}
# Visualização do passeio aleatório com drift
plot(pt_drift_trend, type = "l", xlab = "observações", ylab = "preço", main = "Simulação Passeio Aleatório com Drift e Tendência Determinística")

## Para chegar se os dados correspondem (ou não) a dados de caminhantes ao azar é necessário empregar um teste
## para o R temos o adf.test() que é análogo ao tsa.stattools.adfuller() do Python. 

# Pacote aTSA:
library(aTSA)
adf.test(pt_drift)

####################################################################

## Carregar os dados "índice NDVI" para as práticas: NDVI e NDVI 2. 

####################################################################

## INDICAR QUE OS DADOS SÃO SÉRIES TEMPORAIS:
## Indicar que esses dados são séries temporais. Para tal usamos a função 'ts' do pacote forecast.
## Para os dados de NDVI foi possível nominar o período de inicio da série - comando 'start=c(ano_inicial, ano_final)' -
## e também o período que ela acaba - comando 'end=c(ano_final,mes_final). Também podemos colocar o intervalor que os valores 
## aparecem com a função 'frequency=c(valor_intervalo)'. 
## Para o comando frequency: frequência = ciclo (ex. anual) x coleta (ex. mensal). 

tempo1=ts(NDVI1, star=c(2000,1), end=c(2020,2), frequency=c(12)) # transformamos NDVI1 em uma série temporal associada ao nome 'tempo1'
plot(tempo1) # plotamos a série de tempo 1. 

tempo2=ts(NDVI2, star=c(2000,1), end=c(2020,2), frequency=c(12)) # transformamos NDVI2 em uma série temporal associada ao nome 'tempo1'
plot(tempo2) # plotamos a série de tempo 2.

####################################################################

## ESTATÍSTICA DESCRITIVA E GRÁFICA DAS SÉRIES DE TEMPO: 

# Estatística descritiva:
summary(tempo1) 
summary(tempo2)

# Boxplot:
boxplot(tempo1, tempo2)

# fazendo as séries pelo ggplot
autoplot(tempo1)
autoplot(tempo2)

# plotando as séries por sazonalidade
ggseasonplot(tempo1, year.labels = T, year.labels.left = T) 
ggseasonplot(tempo2, year.labels = T, year.labels.left = T) 

ggseasonplot(tempo1, polar = T) 
ggseasonplot(tempo2, polar = T) 

gglagplot(tempo1)
gglagplot(tempo2)

# Análises descritivas para os períodos de tempo:
start(tempo1) # onde começa o tempo 1?
start (tempo2) # onde começa o tempo 2?

end(tempo1) # onde termina o tempo 1?
end(tempo2) # onde termina o tempo 2?

frequency(tempo1) # qual é a frequencia entre intervalos de tempo 1?
frequency(tempo2) # qual é a frequencia entre intervalos de tempo 2?

window(tempo1) # todos os descritores da frequência de tempo em um só comando para tempo 1. 
window(tempo2) # todos os descritores da frequência de tempo em um só comando para tempo 2.
# a função window também pode ser utilizada para recortar uma série temporal e mostrar só um pedaço dela.
# a configuração é feita igual da função ts, mas no lugar de 'dados = NDVI1' vai o nome da série temporal, que neste
# caso é 'tempo1'. 

## Para sobrepor os períodos de tempo (dados completos) em um único gráfico:
plot(tempo1)
lines(tempo2, col="blue")

## limitando o período de tempo para ambos:
tempo1.lim<-window(tempo1, star=c(2000,1), end=c(2016,5))
plot(tempo1.lim)

tempo2.lim<-window(tempo2, star=c(2000,1), end=c(2016,5))
plot(tempo2.lim)

plot(tempo1.lim, col="red")
lines(tempo2.lim, col="blue")

## Selecionar somente os valores que NDVI que são maiores que 0.3
## Preciso descobrir como fazer. 
NDVI>0.30

####################################################################3

## AULA 2:
## Carregar o pacote que faz o 'detrand'
library(pracma)
## detrend(x, tt = 'linear', bp = c())

## Multiplicadores e somas
vector1=c(1,2,3,1)
vector2=c(0,9,8,6)
sum(vector1*vector2) # soma de vectores

## Soma dos vectores da nossa variável temporal
sum(tempo1*tempo2)
(tempo1*tempo2)*sum()
sum(NDVI1*NDVI2)

## Para fazer covolução dos meus valores - é usado para calcular as covariáveis 
## no curso vamos usar somente a de Pearson. 
## Os correlogramas saem desses covoluções. 
convolve(tempo1, tempo2, conj = F, type = c("circular"))
convolve(tempo1, tempo2, conj = T, type = c("circular"))

convolve(tempo1, tempo2, conj = F, type = c("open"))
convolve(tempo1, tempo2, conj = T, type = c("open"))

convolve(tempo1, tempo2, conj = F, type = c("open"))
convolve(tempo1, tempo2, conj = T, type = c("open"))

convolve(tempo1, tempo2, conj = F, type = c("filter"))
convolve(tempo1, tempo2, conj = T, type = c("filter"))

## com os vetores que criamos como exemplo:

## Função 'detrend'
## Remove o valor médio ou a tendência linear (por partes) de um vetor ou de cada coluna de uma matriz.
v1d<-detrend(vector1)
v2d<-detrend(vector2)
convolve(v1d, v2d)

##############################################################

## MÉDIAS MOVÉIS:
## Transformação de dados
## Suaviazação de séries
## Remoção de outliers
## Identificação de tendências
## --> método simples e explicativo
## Quando vamos aplica as médias movéis sempre temos que eleger um número de ordem para o qual será feita a média
## movel. 

# Função 'ma' - "order" coloca a ordem com o qual deseja que seja realizado a média movel
mmtempo1.1<-ma(tempo1, order = 5) 
mmtempo1.2<-ma(tempo1, order = 10) 

# plotando todas as linhas juntas para ver o resultado. 
plot(tempo1)
lines(mmtempo1.1, col="red")
lines(mmtempo1.2, col="blue")

# Também temos a função "tsclean" que não 'suavisa' mas faz uma limpeza de outliers na 
# série de dados.

tsclean.tempo1<-tsclean(tempo1)

# Plotando tudo em gráfico
plot(tempo1)
lines(mmtempo1.1, col="red")
lines(mmtempo1.2, col="blue")
lines(tsclean.tempo1, col="pink")

#####################################################################

## Teste de Estacionariedade 
## Estacionariedade = média, variância e autocorrelação são constantes, portanto não mudam ao longo do tempo. 
## Ela pode ser definida em termos matemáticos precisos --> é uma série parecida com um plano liso, sem tendências, 
## variâncias constante no decorrer do tempo, autocorrelação constante e nenhuma flutuação períodica. 

autoplot(tempo1) # uma série estacionária geralmente se parece com a série do tempo 1, pq elas estão variando para 'cima'
                 # e para baixo ao longo de uma 'média'. 

autoplot(pt_drift) # essa série já apresenta uma tendência positiva, tornando-a não estacionária. 

## Pq estacionáriedade é importante?
## Pq existem uma seire de analises que se baseiam nessa suposição. 

# Testando a estacionalidade.
# Carregar o pacote "Urca" para o teste:
library(urca)

# aplicando o teste:
x<-ur.kpss(tempo1)
print(x) # segundo o teste de hipóteses, existe evidência de que a série é estacionária (p<0,05).

y<-ur.kpss(tempo2)
print(y) # segundo o teste de hipóteses, existe evidência de que a série é estacionária (p<0,05).

# Caso as séries não fossem estacionárias, é possível corrigir com o comando diferenciação no R. 
# Primeiro usa o comando 'ndiffs()' para saber quantas transformações são necessárias para transformar uma série em 
# estacionária. Feito a transforação, aplica-se a função 'diff()' o número de vezes indicado por 'ndiffs()'.
# O 'diff()' é um processo de diferênciação que transforma a série em estacionária. 

# Além da 'diff()' existem outras transformações que podem ser feitas, como o Boxcox etc. 

# Testes que podem ser usados para encontrar a estacionalidade: Dickey-fuller, kpss, Phillips-Perron. 

# !!  ** Um comportamento não estacionário é observar um crescimento ou descrescimento (tendência) nos dados !! * ##

#####################################################################

## Ergodicidade:
## Quando a média é igual a média móvel de uma função temporal. 
## A amplitude de variação é a mesma - não varia (os dados aumentam e diminui de maneira equilibrada ao longo do tempo).

# Toda ergodicidade é estacionárias, mas nem sempre um ST estacionário apresenta ergodicidade. 

#####################################################################

## AUTOCORRELAÇÃO

## Autocorrelação 
## É a correlação de valores de uma mesma variável ordenada no tempo ou no espaço; ou, mede se existe uma relação matemática
## entre os intervalos da série temporal. 
## Cov(et, et+s)= E(et.et-s) ≠ 0.
## Mostra a força e a direção da variação da variável (entre -1 e +1; 0 é ausência de autocorrelação). 

## Medidas são entre intervalos (ou lags):
## -> de 1 intervalo: mede como valores de 1 período distantes estão correlacionados (ou seja, de valores vizinhos)
## -> de 2 intervalos: mede como valores de 2 períodos de distância estão correlacionados (ou seja, de valores 2 lags de 
##    distância).

## ACF
## No R podemos análisar a série a partir do diagrama de autocorrelação (Auto-Correlation Funcition - ACF).
## O diagrama terá valores entre -1 e +1 e apresentará uma linha tracejada que mostram o nível de significância 
## (ou intervalo de confiança). 
## No R a primeira lag sempre terá valor 1. 
## Cada traço mostra um intervalo no tempo e o seu valor de autocorrelação. 
## Ele mostra como o intervalo atual está relacionado com os intervalos anteriores. 
## valores indiretos + diretos de autocorrelação = ACF
acf(tempo1) # os limites pontilhados são os erros e tudo o que está dentro dele é considerado como 'não direferente de zero'. 

## PACF
## Diagrama de autocorrelação parcial - ele mede a autocorrelação diretamente com o mês anterior, e não entre 
## todos os antigos valores, como faz o ACF. 
## Aqui análisamos o efeito direto de um período de tempo estipulado em outro, e não o efeito ao longo de uma série de tempo. 
## apenas valores diretos de autocorrelação = PACF.
pacf(tempo1) # os limites pontilhados são os erros e tudo o que está dentro dele é considerado como 'não direferente de zero'.

## *É possível observar a sazonalidade ou tendência atraves destes gráficos. 
##  Gráficos sazonais apresentam padrões de barras que variam entre valores positivos e negativos.
##  Gráficos com tendências + vão apresentar alta correlação e que vai diminuindo ao longo do tempo. 

## *É possível aplicar o quadrado ao r da autocorrelação e assim obter o coeficiente de determinação (R²).

## RUÍDO BRANCO (White noise): Se mais de 5% dos dados estiverem fora do intervalo do ACF ou PACF, não é considerado 
## White Noise. Lembrando que autocorrelações podem aparecem ao acaso, e por isso é necessário análisar a partir de 
## um umbral. 

## Caracteristicas de um ruído branco: média = zero, desvio padrão constante e ausência de autocorrelação entre os 'lags'. 
## Pq é importante avaliar isso? Pq só será possível modelar ST se houver autocorrelaçaõ. 
## Como testar 'ruído branco'? Visualizando o ACF e o PACF como foi dito acima. 

#####################################################################

## PREVISÕES PARA O FUTURO

## - Em ST prever significa/resulta em extrapolação. 
## - Só é possível prever se existirem padrões na série de dados. 
## - Quanto maior (>) o valor de y na minha série de dados, menor (<) peso esse valor
##   terá no modelo.
## - Quanto maior (>) a distância prevista na série de tempo, menor (<) o poder de prever corretamente y. 
## - As técnicas são: Modelos puros, modelos explanatórios e modelos mistos. 

## Bom modelo: Avaliar resíduos, avaliar a performance (MAE, RMSE, etc.) e avaliar as métricas. 

## Dicas: Bons resíduos não indicam por sí só bons modelos.
##        Os modelos sempre podem melhorar, não confie no primeiro. 
##        Sempre escolher preditores relevantes. 

#####################################################################

## MODELOS SIMPLES:

## Naive - projeta o último valor. 
## Para o naive colocamos o período de previsão para a frente no temp (parâmetro 'h').
## É possivel mudar o intervalo de confiança (original de 80 e 95%) com o parâmetro 'level=c(valor, valor)'.
## Por exemplo:level=c(.95, .99) --> para 95 e 99% de confiança. 
naivemodel<-naive(tempo1, h=12) # h= 12 meses. 
naivemodel #resultados do modelo. 

naivemodel$fitted # valores ajustados pela previsão. 
naivemodel$residuals # valores dos nossos resíduos

autoplot(naivemodel) # previsão gerada pelo modelo
                     # -: ponto médio de previsão
                     # intervalo mais escuro: 80% do intervalo de confiança predito. 
                     # intervalo mais claro: 95$ do intervalo de confiança. 

## Também podemos usar o naive sazonal quando identificamos sazonalidade em nossos dados. 
## Para tal usamos a função 'snaive()'.
snaivemodel<-snaive(tempo1, h=12, level=c(.95,.99)) # modelo
autoplot(snaivemodel) # plot do modelo 
                      # é possível verificar que ele repete o ultimo ciclo dos dados com um intervalo de confiança. 

# têm mais uma função que no livro da Tamara diz que é equivalente ao Naive Model. 
rwf(tempo1)

## Mean - cálcula a média dos dados históricos e utiliza como previsão para o futuro. 
##        parece o snaive mas faz uma previsão linear. 

mean(tempo1) # a previsão de 'mean' será baseada nesse valor médio. 
meanfmodel<-meanf(tempo1, h=12) # o modelo
meanfmodel
autoplot(meanfmodel)

## Drift - Ela faz uma previsão que acompanha a tendência da série. 
##         Ela é equivalente a pegar o primeiro e o último ponto da reta para extrapolar para o futuro.
driftmodel<-rwf(pt_drift, h=200, drift= T) ## ~~ !! usei os dados gerados pelo passeio aleatório com drift!! ~~##
driftmodel
autoplot(driftmodel)

## Plotando todos os modelos feitos até agora em um único gráfico: 
## obs: h= 12 meses.
autoplot(tempo1) +
  autolayer(meanf(tempo1, h=12),
            series="Mean", PI=FALSE) +
  autolayer(naive(tempo1, h=12),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(tempo1, h=12),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts para média de NDVI 1") +
  xlab("Year") + ylab("NDVI") +
  guides(colour=guide_legend(title="Forecast"))

## Incluíndo o modeo DRIFT (quando existe uma tendência?)
## h= 48 meses. 

autoplot(tempo1) +
  autolayer(meanf(tempo1, h=48),
            series="Mean", PI=FALSE) +
  autolayer(rwf(tempo1, h=48),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(tempo1, drift=TRUE, h=48),
            series="Drift", PI=FALSE) +
  ggtitle("NDVI 1") +
  xlab("year") + ylab("NDVI 1") +
  guides(colour=guide_legend(title="Forecast"))


## Resíduos dos modelos:

## Para o método Naive (exemplo)

res <- residuals(naive(tempo1)) # cria um objeto somente para os resíduos 
res

autoplot(res) + xlab("years") + ylab("Residuals") + # aqui conseguimos gráficas os resíduos por ano
  ggtitle("Residuals from naïve method")

# Autocorrelação nos resíduos: - Se existir autocorrelação, quer dizer que o modelo não está bem ajustado
#                              pq ainda existe variação não aportado pelo modelo.
ggAcf(res) + ggtitle("ACF of residuals") # autocorrelação parcial dos resíduos.
                                         # alguns caem na área de significância. 
# Especialmente em 2015 temos uma grande variação dos resíduos. Fora isso, os demais estão variando de maneira 
# uniforme ao redor da média zero (0).

gghistogram(res) + ggtitle("Histogram of residuals") # Histograma com os resíduos 
# Aparenta estar em distribuição normal, com média zero e varianças constantes (?). 

# Em geral as previsões parecem ok, com excessão do ano de 2015.

## !! Rodando os gráficos para análise de resíduos de uma só vez. 
checkresiduals(naive(tempo1)) ## é possível colocar o modelo diretamente. 
                              ## Também sai o resultado do teste de Ljung-Box para autocorr dos resíduos.

#####################################################################

## SÉRIES TEMPORAIS DECOMPOSTAS
## Baseado em processo de decomposição? 
modelodecomposto<-stlf(tempo1)
autoplot(modelodecomposto)

#####################################################################

## Suavização Exponencial

## Este é um esquema muito popular para produzir uma Série Temporal suavizada. Enquanto na Média Móvel Simples as 
## observações passadas são ponderadas igualmente, a Suavização Exponencial atribui pesos decrescentes exponencialmente 
## quando a observação fica mais velha.
## Em outras palavras, observações recentes são tomadas com mais peso relativamente na previsão que as observações 
## mais antigas.

## No caso de médias móveis, os pesos que são atribuídos às observações são os mesmos e iguais a 1/N. 
## Na suavização exponencial, entretanto, existem um ou mais parâmetros de suavização a serem determinados (ou estimados)
## e estas escolhas determinam os pesos atribuídos Às observações.

# Principios básicos:
# - As observações passadas possuem pesos
# - Quanto mais recente as observações, maiores seus pesos para as previsões
# - Ultiliza médias que reduzem quanto mais distantes são as observações
# - O parâmetro alfa determina o índice de redução: valor entre 0 e 1; próximo de 0 a observação mais 
#   antiga tem maior peso, e próximo de 1 as observações recentes tem maior peso. 
# Existem várias técnicas, inclusive uma simples que se parece com o NAIVE (sem tendência ou sazonalidade). 

## TENDÊNCIA LINEAR DE HOLD:
## Suavização exponencial para dados com tendência. Ela gera uma tendência linear para o futuro. Tende ao infinito. 
modtlh<-holt(pt_drift, h=400) # !! novamente usei a tendência com drift. 
autoplot(modtlh) ## É possível observar que ele segue a tendência de maneira linear. 
modtlh$model # Para observar os parâmetros do modelo. 
             # Ele elegeu automaticamente um alpha de 0.8739 --> lembrando que quanto mais próximo de 1 maior o peso
             # para as variáveis recentes. Esse parâmetro pode ser elegido pelo 'modelador' com a função 'alpha='. 

## exemplo com diferentes alphas e já plotados em gráfico:
plot(holt(pt_drift, h= 400, alpha= 0.2)) 
lines(modtlh$mean, col="red")

## TENDÊNCIA AMORTECIDA
## Na tendência amortecida existe um parâmetro que amortece a tendência conforme a previsão avança para o futuro. 
## Não é linear para o infinito como a Linear de Hold. 

## exemplo com diferentes alphas e já plotados em gráfico:
plot(holt(pt_drift, h= 400, alpha= 0.2)) 
lines(modtlh$mean, col="red")

## Parametro "damped" é o 'phi' ou padrão de amortecimento. 
## Em baixo é plotado dois modelos - com e sem o padrão. 
plot(holt(pt_drift, h= 400, alpha= 0.8, damped= T)) 
lines(holt(pt_drift, h= 400, alpha= 0.8, damped= F)$mean, col="red")
## !! Vemos claramente que houve uma suavização nos modelos. 

## MÉTODO DE HOLT-WITERS SAZONAL
## - Inclui captura de sazonalidade e;ou tendência.
## Possui duas formas: 
## - Aditivo: para variação sazonal constante.
## - Multiplicativo: variação sazonal varia na serie. 
## !! Precisa fazer uma decomposição na série e observar a sazonalidade para escolha entre os dois métodos. 
## !! Pode deixar a função eleger também qual método ficará melhor aos dados. 

## Parâmetros: Erro, Tendência e Sazonalidade. 
## - Cada parâmetro pode ser aditivo, multiplicativo, nenhum ou automático.

# Modelo aditivo:
hwmodel<-hw(tempo1, seasonal = "additive", h= 24)
autoplot(hwmodel) # captura tanto a tendência quanto a sazonalidade. 

# Modelo multiplicativo:
hwmodelmulti<-hw(tempo1, seasonal = "multiplicative", h= 24)
autoplot(hwmodelmulti) # captura tanto a tendência quanto a sazonalidade. 

# Para observar a diferença de previsão entre aditivo e multiplicativo:
plot(hwmodel)
lines(hwmodelmulti$mean, col="red")

# Também é possível adicionar armotecimento nessa forma de modelo.
hwmodelmulti2<-hw(tempo1, seasonal = "multiplicative", h= 48, damped=T)
autoplot(hwmodelmulti2) 

# A função 'ets' gera os parâmetros para o modelo, algo semelhante aos bayesianos.
parametros<-ets(tempo1)
parametros

prev<-forecast(parametros, levels=c(85,90))

autoplot(prev) # vendo a previsão

autoplot(prev$residuals) # resíduos do modelo

autoplot(prev$fitted) # valores ajustados

## Mas qual modelo usar? Aditivo ou multiplicativo?
## Fazer uma decomposição da ST:
autoplot(decompose(tempo1)) # observar se a sazonalidade é constante ou se ela varia ao longo da série.
                            # existe uma sazonalidade constante mas sem tendência - melhor o modelo aditivo. 

## Criar os modelos utilizando os parâmetros análisados em gráficos:
# A função 'ets' gera os parâmetros para o modelo, algo semelhante aos bayesianos.

mdl00<-ets(tempo1, model="ZAA", damped=T)
# Podemos dizer como queremos os parâmetros do modelo em "model":
# z (erro) --> erro automatico
# A (tendência) --> aditivo
# A (sazonalidade) --> Aditivo
mdl00
autoplot(mdl00)


#####################################################################

## MODELOS ARIMA: AR, MA e ARMA. 

## i) ARIMA(p,0,0) = AR(p);
## ii)ARIMA(0,0,q) = MA(q);
## iii)ARIMA(p,0,q) = ARMA(p,q).

## Esta metodologia consiste em ajustar modelos autorregressivos integrados de médias móveis, ARIMA(p,d,q), a um conjunto
## de dados. Para a construção do modelo seguimos um algorítimo no qual a escolha da estrutura do modelo é baseado nos próprios
## dados. Podemos descrever o algorítimo através dos seguintes passos:
  
## 1. Considera-se uma classe geral de modelos para a análise;
## 2. identifica-se um modelo com base na análise de autocorrelações, autocorrelações parciais e outros critérios;
## 3. estima-se os parâmetros do modelo identificado;
## 4. verificar se o modelo ajustado é adequado aos dados através de uma análise de resíduos.
## 5. Caso o modelo não seja adequado o algoritmo é repetido, voltando à fase de identificação.

## Existem vários critérios para identificação de um modelo, por isso, é possível identificar modelos diferentes dependendo do
## critério que foi escolhido para identificação.

# ARIMA NÃO SAZONAL
# - Robusto: Pode ser usado em patricamente qualquer tipo de ST
# - Dados estáveis, com poucos outliers
# - Requer dados estacionários; caso não sejam é possível transformar os dados usando diferenciação para remover tendências.

# Diferenciação:
# --> Subtrai a observação do período atual do período anterior
# --> A diferenciação pode ser feita 1x: diferenciação de primeira ordem.
# --> Ou pode ser necessário mais de uma vez (de segunda ordem). 

# O ARIMA não-sazonal é composto por três elementos:
# --> AR - Autoregressivo: avalia a relação entre os períodos (lags): autocorrelação. Extrai essa influência.
#                          calcula um modelo de regressão em que os períodos de tempo possuem betas. 
# --> I - Integrated: Aplica a diferenciação, se necessária.
# --> MA - Moving Average: Avalia erros entre períodos e extrai estes erros (não tem relação com MA usados para suavização 
#     de st). 

## Nos modelos ARIMA sempre serão utilizadas as letras (p, d, q):

## !! No caso o AR I MA é: AR = p, I = d e MA = q !! ##

## -- p: ordem da parte autoregressiva.
##       Exemplo: p = 1, significa que uma determinada observação pode ser explicada pela observação prévia + erro; e um p = 2 
##       significa que uma terminada observação pode ser explicada por duas observações prévias + erro. 
 
## -- d: grau de diferenciação.
##       Exemplo: d=0, significa que não é aplicada diferenciação; d=1 significa que será aplicada diferenciação de primeira
##       ordem; e um d= 2 significa que será aplicada uma diferenciação de segunda ordem. 

## -- q: ordem da média móvel.
##       Exemplo: q=1 significa que uma determinada observação pode ser explicada pelo erro da observação prévia; e um 
##       q= 2 significa que uma determinada observação pode ser explicada pelo erro de duas observações prévias. 

## Modelos AR (1) ou ARIMA (1,0,0) --> temos apenas elementos autoregressivos de 1a ordem; 
## Modelos AR (2) ou ARIMA (2,0,0) --> temos apenas elementos autoregressivos de 2a ordem.
## MA (1) ou ARIMA (0,0,1) --> Temos apenas média model.
## ARMA (1,1) ou ARIMA (1,0,1) --> Autoregressão e média móvel de 1a ordem. 

## Também é possível usar ARIMA para dados de SAZONALIDADE! Neste caso, além dos elementos (p, d, q) também teremos os 
## elementos (P, D, Q), o que torna o modelo mais complexo. 
## !! Uma opção é remover o elemento sazonal por um processo de decomposição e usar o Arima padrão !! ##. 

## Como definir os valores de p, d e q?
## 'p' é ordem da parte autoregressiva (PACF); é possível observar ela a partir de um diagrama PACF. 
## 'd' é o grau de diferenciação e é possível obter ele a partir de um teste de estacionariedade. 
## 'q' é a ordem da média móvel - análisamos pelo ACF para ter a ordem da média móvel. 

## Como saber qual é o melhor modelo?
## Podemos comparar pelas métricas de AIC, AICc e BIC.
## O objetivo é ter o menor valor desses indices. 

## Qual é o processo para o ARIMA?
## Exploração dos dados (est. desc. e decomposção) > Estabilização (se necessário, remover outliers ou diferenciação ) > 
## Cria o Modelo ARIMA: tem o autoarima para identificar o melhor modelo > 
## Faz a avaliação dos Residuos (técnicas de análise) > Usa o Forecast para fazer a Previsão. 

## Funções no R: Existem dois - 'arima' (R base) e 'Arima' (pacote Forecast). 

## Definir os parâmetros p, d e q?
## - Não é um processo linear
## - É dificil. 
## - Nem sempre o modelo intuíto é o melhor. 

## !!! A função Auto.arima() testa diferentes combinações de p, d e r para ter certeza qual é a melhor configurações !!! ##.

## Para potencializar a busca pelo melhor modelo a partir do Auto.arima() é possível definir alguns parâmetros (ver o help). 
## Ex: ver se o modelo é sasonal, se não definir 'seasonal=FALSE'; também é possível definir os valores máximos para testar 
## os parâmetros (max.p, max.q, etc) e também os valores iniciais (start.p, start. q, etc).  

## Exemplo de Auto.arima:
a= auto.arima(tempo1, trace= T) # argumento 'trace=T' é só para poder visualizar os modelos que ele vai ajustando. 
a # Melhor modelo com as seguintes configurações: ARIMA(1,0,2)(2,0,1)[12] with non-zero mean. 
  # Ele usa os indices AIC, AICc e BIC como indicativo para os melhores modelos. 
summary(a)
a$coef
## Mas o que podemos fazer para tentar melhorar esses modelos?
## O auto.arima() possui algumas limitações, como ter um stepiwise como default.
## Em razão disso, retirar algumas dessas limitações pode amplificar a gama de modelos possíveis. 
b<-auto.arima(tempo1, trace=T, stepwise = F, approximation = F)

## Comparando melhor modelo 'a' e 'b'
## a= AIC=-271.21   AICc=-270.59   BIC=-243.3
## b= AIC=-272.71   AICc=-272.36   BIC=-251.78
## !! O modelo 'a' enconstrou combinações melhores que o 'b'. 

## Vamos fazer as previsões para os dois e compara-las:
prev.a<- forecast(a, h= 24)
autoplot(prev.a)

prev.b<-forecast(b, h=24)
autoplot(prev.b)

plot(prev.a)
lines(prev.b$mean, col="red")

## SARIMA
## Modelos ARMA e ARIMA: Para séries em que quase sempre oscilam entre um intervalo fixo (estacionário ou não; com ou 
## sem tendência), mas que não é sazonal. Porém, algumas séries temporais exibem comportamentos cíclicos e periódicos. 
## Ex. Dependendo do tipo de negócio da empresa, períodos específicos do ano tendem a apresentar resultados melhores 
## (safras agrícolas, férias, clima ou data especiais como, por exemplo, Natal) que por conseguinte impactam os ganhos 
## com a ação. Essa tal componente sazonal pode aparecer quando são feitas observações intra-anuais para a série de interesse, 
## isto é, os dados são registrados mensalmente, trimestralmente ou semanalmente, por exemplo.
## Este tipo de série é chamada de série temporal sazonal. 
## Em algumas aplicações a sazonalidade tem segunda importância e é removida dos dados, resultando em uma série temporal 
## ajustada sazonalmente (extraída a sazonalidade) que é então usada para fazer inferência. O procecimento de remover a
## sazonalidade de uma série temporal é conhecido como ajustamento sazonal. Em outras aplicações tal como previsão, a 
## sazonalidade é tão importante quanto outra característica dos dados e deve ser tratada. 


## Processo de estimação dos modelos SARIMA:
## 1. Visualizar os dados para identificar observações fora do padrão (outliers ou dados faltantes), examinar se existe 
##    tendência e/ou sazonalidade por meio de gráficos específicos.
autoplot(decompose(tempo1)) 
ggtsdisplay(tempo1)

## 2. Se necessário, transformar os dados para estabilizar a variância (logaritmo dos dados, variação ou retorno, por exemplo).
autoplot(decompose(log(tempo1))) 
ggtsdisplay(log(tempo1))

## 3. Testar se os dados são estacionários. Caso tenha raiz unitária é preciso diferenciar os dados até se tornarem 
##    estacionários. Para isso, testa-se novamente se a série diferenciada se tornou estacionária.
##    --> Se existe apenas sazonalidade, faça a diferenciação sazonal (por exemplo, para dados mensais faça a diferenciação de 
##        ordem 12). 
##    --> Se existe apenas tendência, faça apenas a primeira diferença.
##    --> Se existe tendência e sazonalidade, aplique a primeira diferença nos dados para tratar a tendência e após isso a 
##        diferenciação sazonal (o caso dos ganhos trimestrais da Coca-Cola).
##    --> Se não existe tendência ou sazonalidade não faça qualquer diferenciação.

# Carregar o pacote "Urca" para o teste:
library(urca)

# aplicando o teste:
x<-ur.kpss(tempo1)
print(x) 

x1<-ur.kpss(log(tempo1))
x1

## 4. Examinar as funções de autocorrelação parcial (FAC) e autocorrelação parcial (FACP) para determinar as ordens máximas 
##    P e Q para os componentes AR e MA tanto da parte regular quanto da parte sazonal da série estacionária:
##    --> Termos não sazonais: examine as primeiras defasagens (1,2,3,..) da FAC e FACP. Mantemos o mesmo padrão já estudado, 
##        ou seja, a FAC define termos MA e a FACP termos AR 
##    --> Termos sazonais: examine padrões em defasagens que são múltiplas da periodicidade da série. Por exemplo, para dados
##        mensais, verifique nas defasagens 12, 24, 36 (provavelmente será preciso verificar as primeiras duas ou três 
##        defasagens múltiplas da periodicidade). Avalie a FAC e FACP nas defasagens sazonais da mesma forma que você fez nas 
##        defasagens não sazonais.
ggtsdisplay(tempo1)

## 5. Estimar todas as combinações para p, d e q na parte regular e P, D e Q na parte sazonal. Aqui, tanto d quanto D serão 
##    fixos e igual ao número de vezes necessárias para tornar a série original estacionária na parte regular e na parte 
##    sazonal, respectivamente.
##    !! No caso, a função 'auto.arima()' já faz isso. 
c<-auto.arima(log(tempo1), trace=T, stepwise = F, approximation = F)
c

(fit <- Arima(log(tempo1), order=c(1,0,0 ), seasonal=c(2,0,0)))
autoplot(fit)

## 6. Escolher dentre todos os modelos estimados no passo anterior, o modelo com menor AIC e/ou BIC.
##    !! No caso, a função 'auto.arima()' também faz isso. 

## 7. Examinar se os resíduos se comportam como ruído branco. Caso contrário, retornar ao passo 3 ou 4.
checkresiduals(fit)

## 8. Testar autocorrelação nos resíduos.
checkresiduals(fit)

## 9. Testar se tem heterocedasticidade condicional.

## 10. Verificar a distribuição de probabilidade.
##     !! Não achei como. 

## 11.Uma vez que os resíduos são ruído branco, obter as previsões.
log(tempo1) %>%
  Arima(order=c(1,0,0), seasonal=c(2,0,0)) %>%
  forecast() %>%
  autoplot() +
  ylab("NDVI") + xlab("Year")

prev.c<- forecast(c, h= 48) # dá pra ir mudando o 'h=' e ir vendo o comportamento da predição. 
autoplot(prev.c)
summary(c)

c$coef
c$sigma2
c$var.coef
c$arma
c$model
c$fitted

######################################################################33

## KRIGAGEM
## Kriging, também muitas vezes traduzido como Krigagem, é um método de regressão usado em geoestatística para aproximar 
## ou interpolar dados. Na comunidade estatística, também é conhecido como “Processo Gaussiano de Regressão”.
## Krigagem poderá ser entendido como uma predição linear ou uma forma da Inferência bayesiana. Parte do princípio que pontos 
## próximos no espaço tendem a ter valores mais parecidos do que pontos mais afastados. A técnica de Krigagem assume que os 
## dados recolhidos de uma determinada população se encontram correlacionados no espaço. Isto é, se num aterro de resíduos 
## tóxicos e perigosos a concentração de Zinco num ponto p é x, é muito provável que se encontrem resultados muito próximos 
## de x quanto mais próximos se estiver do ponto p (princípio da geoestatística). Porém, a partir de determinada distância 
## de p, certamente não se encontrarão valores aproximados de x porque a correlação espacial pode deixar de existir.

## Considera-se o método de Krigagem do tipo BLUE (Best Linear Unbiased Estimator - Melhor Estimador Linear não-Viciado):
## é linear porque as suas estimativas são combinações lineares ponderadas dos dados existentes; é não enviesada pois 
## procura que a média dos erros (desvios entre o valor real e o valor estimado) seja nula; é a melhor porque os erros de 
## estimação apresentam uma variância (variância de estimação) mínima. O termo Krigagem abrange um conjunto de métodos, 
## sendo os mais usuais os seguintes:

## - Krigagem Simples: Assume que as médias locais são relativamente constantes e de valor muito semelhante à média da 
## população que é conhecida. A média da população é utilizada para cada estimação local, em conjunto com os pontos vizinhos 
## estabelecidos como necessários para a estimação.

## - Krigagem Normal: As médias locais não são necessariamente próximas da média da população usando-se apenas os pontos 
## vizinhos para a estimação. É o método mais usado em problemas ambientais.

## - Co-krigagem: É uma extensão do anterior a situações em que duas ou mais variáveis são espacialmente dependentes e a 
## variável que se quer estimar não está amostrada com a intensidade com que estão as outras variáveis dependentes, 
## utilizando-se os valores destas e as suas dependências para estimar a variável requerida.

## CONCEITOS MATEMÁTICOS:
## A semi-variância é a medida do grau de dependência espacial entre duas amostras. A magnitude da semi-variância entre 
## dois pontos depende da distância entre eles, implicando em semi-variâncias menores para distâncias menores e 
## semi-variâncias maiores para distâncias maiores. O gráfico das semi-variâncias em função da distância a um ponto é 
## chamado de Semi-variograma. A partir de uma certa distância a semi-variância não mais aumentará com a distância e 
## estabilizar-se-á num valor igual à variância média, dando a esta região o nome de silo ou patamar (sill). 
## A distância entre o início do semi-variograma e o começo do silo recebe o nome de range ou amplitude ou alcance. 
## Ao extrapolarmos a curva do semi-variograma para a distância zero, podemos chegar a um valor não-nulo de semi-variância. 
## Este valor recebe o nome de Efeito Pepita (Nugget Effect).

## No Método de Krigagem normalmente são usados quatro tipos de variogramas. Neles, são usadas as seguintes variáveis:
## V: variância
## C0: nugget
## alfa: silo ou patamar (sill)
## c0 + c: variação assintótica 
## h: distância

## Tipos de modelos:
## 1. Linear: Modelo sem silo (sill), sendo mais simples e com curva representada pela equação: 
## v = c0 + c * h
## 2. Esférico: A forma esférica é a mais utilizada e possui silo. Sua forma é definida por:
## v= {c0 + c [1.5(h/a)-0.5(h/a)³], se alfa < 0
##    {c0 + c, alfa > 0
## 3. Exponencial: A curva do variograma exponencial respeita a seguinte equação:
## v= c0 + c(1-e exp(-h/b))
## 4. Gaussiano: 
## 


######################################################################
## REFERÊNCIAS:
## ARIMA
## https://www.sciencedirect.com/science/article/pii/S1995764512600959
## https://otexts.com/fpp2/VAR.html

## AUTOCORRELAÇÃO:
## https://www.youtube.com/watch?v=DeORzP0go5I
## https://medium.com/one-datum-at-a-time/time-series-and-autocorrelation-an-exploration-3cbe414e537a

## ESTACIONARIEDADE:
## https://towardsdatascience.com/detecting-stationarity-in-time-series-data-d29e0a21e638

## REGRESSÃO
## https://otexts.com/fpp2/regression.html

## WHITE NOISE
## https://www.youtube.com/watch?v=cr4zIXAmSRI

## YOUTUBE com diversas referências:
## https://www.youtube.com/channel/UCUcpVoi5KkJmnE3bvEhHR0Q/videos


#####################################################################

