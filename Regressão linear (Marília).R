
## Regressão e Correlação 
## Marília Melo Favalesso 
## Mestranda do programa de conservação e manejo de ambientes naturais
## Universidade Estadual do Oeste do Paraná

## VAR. QUANTI ~ VAR. QUANTI

## -- A correlação -- ##

# A correlação têm como objetivo avaliar se existe associação entre duas variáveis quantitativas.
# A correlação deve ser feita com variáveis (características) de um mesmo objeto de em estudo 
# (podem ser variáveis medidas em um mesmo indivíduo, me um mesmo local, etc).

# Existem duas formas de avaliar a associação entre variáveis: a partir de um gráfico de correlação
# e por indices de correlação.

## --> Banco de dados
cor=matrix(c(8,7,6,3,3,6,5,2,10,8,6,8,6,9,7,4), nrow = 8, ncol=2)
cor
colnames(cor)=c("x (horas)", "y (notas)")
rownames(cor)=c("Hermione", "Cho","Rony", "Draco", "Harry", "Neville", "Gina", "Luna")
cor
attach(cor)

## --> Gráfico de dispersão
plot(cor)
plot(cor, main="Gráfico de dispersão", ylab="Notas dos alunos", xlab="horas de estudo", col="blue")
# main = título do gráfico
# ylab = adicionar legenda no eixo y
# xlab = adicionar legenda no eixo x
# col = cor dos pontos no gráfico de dispersão

## --> Coeficiente de correlação (r)
# Uma outra maneira de se avaliar a correlação é usar o coeficiente, que tem a vantagem de ser um 
# número puro, isto é, independente da unidade de medida das variáveis. Esse coeficiente r é uma me-
# dida da intensidade de associação existente entre duas variáveis quantitativas.

# Variação no coeficiente de correlação
# O coeficiente de correlação pode variar entre -1 e +1. Valores negativos de r indicam uma correla-
# ção do tipo inversa, isto é,  quando x aumenta, y em média diminui (ou vice-versa). Valores positi-
# vos para r ocorrem quando a correlação direta, isso é, quando x e y varam no mesmo sentido. O
# valor máximo de correlação (+1 ou -1) são obtidos quando todos os pontos do diagrama de dispersão 
# formam uma linha reta inclinada. Quando não existe associação entre as variáveis, no diagrama de 
# dispersão as nuvens estarão em formato circular.

## --> Pressupostos para a correlação linear de pearson

## Normalidade
shapiro.test(cor[,1])
shapiro.test(cor[,2])
# !! Lembrando que o nome da nossa matriz era "cor"
# !! Nós usamos as chaves [] para dizer "quero apenas os valores da primeira coluna --> ,1"
#    ou "quero apenas os valores da segunda coluna --> ,2". Lembrando que a primeira coluna
#    conteve os valores de 'horas de estudo (x)' e a segundo as notas dos alunos 'notas (y)'.

## Homocedasticidade
var.test(cor[,1],cor[,2])

## --> Aplicação da correlação de Pearson

# Interpretação do coeficiente r segundo Callegari-Jacques (olhar para os valores em módulo):
# r = 0: A correlação é nula (ou inexistente)
# 0-0,3: A correlação é muito fraca
# 0,3|-0,6: Correlação moderada
# 0,6 |-0,9: Correlação forte entre as variáveis
# 0,9|-1: Correlação muito forte entre as variáveis
# r = 1: A correlação é plena ou perfeita

cor(cor[,1],cor[,2], method="pearson")
cor(cor, method="pearson")
# !! Se as variáveis não estivessem em normalidade ou homocedasticidade, teria que ter utilizado
#    o metódo de correlação de postos de Spearman. No lugar de 'method = "pearson"' você colocaria
#    'method = "spearman"'. 

## -->  Mas a associação é verdade?
t.test(cor[,1],cor[,2], method="pearson")

## --> Interpretação:
# ESCREVA AQUI O RESULTADO DA SUA ANÁLISE!!

## ----------------------------------------------------------------------------------------##

## --> A regressão linear

# O estudo de regressão aplica-se àquelas situações em que há razões para supor
# uma relação de causa-efeito entre duas variáveis quantitativas e se deseja
# expressar matematicamente essa relação. Ou seja:

# Y ~ X (Y depende de X, ou Y é função de X)

# Em um estudo de regressão, os valores da variável independente (X) geralmen-
# te são escolhidos, e para cada valor escolhido observa-se o valor de y corre
# -spondente. 

# Dessa forma, o objetivo de um estudo de regressão é:

# a) Avaliar uma possível dependência de y em relação a x;
# b) Expressar matematicamente esta relação por meio de uma equação:
#    y = B0 + B1.x

# Para encontrar os coeficientes B0 e B1 é utilizado o método dos minimos
# quadrados.

## Passo-a-passo da regressão linear no R

## --> 1 - Fazer o gráfico de dispersão entre as variáveis e testar a correlação 

# Todo estudo de regressão deve iniciar pela elaboração de um gráfico de 
# dispersão de pontos e pela aplicação de um teste de associação de Pearson.
# Esse passo é fundamental, pois o gráfico já dá uma boa idéia da existência
# ou não de regressão, e a correlação evita o erro de aplicar a técnica a da-
# dos para os quais não é adequada. 

# !! Lembrando que esse passo já foi feito lá em cima!

# Plot:
plot(cor, main="Gráfico de dispersão", ylab="Notas dos alunos", xlab="horas de estudo", col="blue")

# Pressupostos da correlação (que também valem para a regressão linear)
shapiro.test(cor[,1])
shapiro.test(cor[,2])
var.test(cor[,1],cor[,2], method="pearson")

# Testando a correlação
t.test(cor[,1],cor[,2], method="pearson")


##-->  Regredindo y ~ x

## Como dito anteriormente, a equação da reta da LM pode ser dada por:

## y = B0 + B1.x

## Onde: 
## y = variável dependente
## B0 = Parâmetro ou coeficiente linear (valor de y quando x = 0)
## B1 = Parâmetro ou coeficiente angular (inclinação da reta, acréscimo ou
## decréscimo em y para cada acréscimo de uma unidade em x)
## x = variável independente

regressao=lm(cor[,2]~cor[,1]) # Ajuste da regressão
regressao
# !! Lembrando que cor[,2] = variável dependente "notas"
# !! Lembrando que cor[,1] = Variável independente (ou preditora) "horas de estudos"

# Com base nesse modelo ajustados, temos duas informações: O valor do inter-
# cepto (B0-valor em que a reta de regressão intercepta o eixo das ordenadas)
# e o valor que representa o coeficiente de inclinação da reta (B1), ou seja, a
# relação entre o peso e a altura (o quanto o peso varia para cada variação
# unitária de altura). 

# Onde: Intercept = Valor de B0 = ???????
#       Altura = Valor de B1 = ??????

## --> Teste de significância da Regressão 

# A dependência de y em relação a x é representada pelo coeficiente B1. No 
# entando ele quase sempre é determinado com base em uma amostra de dados. 
# Não se trata do valor verdadeiro do coeficiente de regressão,
# mas de sua estimativa. No caso do nota~tempo de estudo, tal coeficiente foi obtido
# com base em uma amostra de oito pessoas. Para se afirmar que o valor B1 = 
# 0,6562 representa uma dependendência real de y ~ x e justificar previsões
# para y com base na equação obtida, deve-se realizar um teste de hipótese 
# sobre a existência de regressão na população. 

# Raciocínio do teste:

# Quando não existe dependência de y em relação a x, o coeficiente de regre-
# ssão populacionar, B1, é igual a zero. A distribuição de B1 em torno de
# zero será gaussiana se a distribuição de y for normal. Para testar a hi-
# pótese de que B1 não é zero, determina-se o número crítico de erros padrão
# permitido para um afastamento observado (b1 - B1), em unidades de erro pa-
# drão (tcalculado). A decisão sobre a significância do desvio é semelhante 
# àquelas vistas nas comparações entre médias e no teste de significância
# de r: se o valor calculadode t exceder o valor crítico, rejeita-se H0
# e conclui-se pela regressão de y em relação a x.

## Teste do coeficiente de regressão
summary(regressao)

## Desenrolando os resultados:
# 1. Estimate Std = Valor dos interceptos β0 e β1
# 2. Error = Erro padrão da média estandardizado
# 3. T-valor = T calculado
# 4. Pr(>|t|)  = P-valor do teste
# * Multiple R-squared: Coeficiente de determinação (R²) - O quanto da variação das notas
#   é explicada pelas horas de estudo.

## !!O Parâmetro no teste T é o valor do intercept 
#    Logo, o parâmetro para B1 será 0,6562 ± 0,2471 (que é o erro). 
#    Transformando esses valores será 0 ± 2 erros.
#    Os valores de T transformados ficam 0 ± 2,45 para alfa = 5% (tabela T crítico)

## !!Calculando o T, que tem o valor de 2,656, vemos que este valor está 
#    fora do intervalo de confiança (que é de ± 2,45), portando é 
#    aceita a Hipótese alternativa do teste (B1 é diferente de 0).

## Resposta:
## ESCREVA AQUI A SUA INTERPRETAÇÃO DOS RESULTADOS

## -- Os pontos experimentais -- ##

# No modelo matemática recém-indicado, a letra y (ou nota) representa um 
# valor que é fixo e dependente de um determinado x (horas de estudo), isto é, y é
# uma quantidade que não pode variar quando x assume determinado valor. Com 
# dados biológicos, no entanto, é comum verificar-se variação  na variável
# dependente quando ela é medida para um certo valor da variável independen-
# te. Por exemplo, diferentes valores de peso (kg) são observados em indivi-
# duos de um mesmo tamanho. Assim sendo, os pontos obtidos por um experimen-
# to dificilmente se colocam exatamente em uma linha, embora se possa obser-
# var, muitas vezes, que os dados tendem a um alinhamento. Os "desalinhamen-
# tos" são interpretados como desvios, ao acaso, do comportamento geral do
# fenômeno. É por isso está razão que se pensa em ajustar uma linha reta a 
# pontos que não estão perfeitamentes alinhados: A reta vai representar o 
# comportamento médio dos valores de y à medida que x aumenta de valor. O
# modelo matemático proposto, neste caso, é y = B0 + B1.x + E (erro). o E
# representa a diferença entre o valor observado e o esperado, segundo a re-
# ta de y. 

# Na regressão, a linha reta representa o comportamento de valores de y mé-
# dios esperados para distintos valores de x, isto é, a reta representa uma
# média que se modifica à medida que os valores de x aumentam. Em cada uma
# dessas "subpopulações" os valores de y variam ao redor da média deste
# grupo. 

## --> Normalidade dos resíduos 

# Uma pressuposição importante para o teste estatístico da re-
# gressão é a de que a variação dos pontos em relaçaõ a média (reta) seja
# a mesma nas várias "subpopulações", ou para cada valor de X.

# Dificilmente testamos a homocedasticidade e a normalidade para cada
# um dos valores de y em relação a x, sendo substituida esse tipo de 
# teste por uma normalidade dos resíduos. 

# Para testar tal suposição, nos extraimos os valores de resíduos da amostra
# e então aplicamos um teste de normalidade dos resíduos.

## Preditos e resíduos
preditos=predict(regressao) # Valores esperados de y para cada valor de x
# ou média dos valores de y para cada x.
residuos=resid(regressao) # É o "E", ou a diferença entre os valores espera-
# dos (média y) e o que foi observado (cada valor).

# A seguinte apresentação tabular pode ser usada para resumir as informações
result=data.frame(cor[,2], cor[,1], preditos, residuos)
colnames(result)=c("notas", "horas estudadas", "valores preditos", "resíduos (predito - observado)")
result

## Normalidade dos resíduos
shapiro.test(residuos) 
## A normalidade dos resíduos garante que o teste de significância apresente um resultado fidedigno

## --> Gráfico de regressão linear

## Para finalizar a análise, caso essa seja verdadeira, é feito um 
## gráfico plot com a linha de regressão e os resíduos.

plot(cor[,2]~cor[,1], ylab="Notas", xlab="Horas de estudo") # Faço o gráfico de dispersão
abline(regressao, col="red") # A reta de regressão
segments(cor[,1],cor[,2],cor[,1],preditos, col="blue") # Resíduos (distância prevista - observada)

## --> INTEREPRETAÇÃO
## ESCREVE AQUI A CONCLUSÃO DA ANÁLISE.

## ----------------------------------------------------------------------------------------##

## -- Atividades para aplicação de correlação e regressão -- ##

# 1 - Foram medidas 6 árvores de uma mesma espécie e o pesquisador deseja saber
#     se existe associação entre a altura e o diametro da árvore (ou seja, quando uma cresce 
#     a outra cresce também)

# variáveis
arvores=matrix(c(1.74, 1.93, 2.41, 5.94, 7.33, 9.27, 10, 12, 14, 23, 40, 59), nrow = 6, ncol = 2)
colnames(arvores)=c("Altura", "Diâmetro")
rownames(arvores)=c("Arvore 1", "Arvore 2", "Arvore 3", "Arvore 4", "Arvore 5", "Arvore 6")
arvores

# 1. Pressupostos avaliados:

# 1.a. Normalidade
shapiro.test() # Avaliar normalidade da altura
shapiro.test() # Avaliar a normalidade do diâmetro

# 1.b. Homocedasticidade
var.test() # Avaliar de altura e diâmetro variam da mesma forma

# 2. Gráfico de dispersão
plot() 

# 3. Correlação de Pearsoon
# 3.1 Coeficiente de correlação
cor()
# 3.2 é significativo?
cor.test()

# 4. Escreva a resposta

## --------------------------------------------------------------

## 2 - Existe relação funcional entre a altura da árvore o seu diâmetro? Ou seja, será que
##     quanto maior o indivíduo obrigatoriamente mais largo ele será?

## diâmetro ~ altura

# 1. Criando o modelo
m1=lm()
m1 # resultado do modelo

# 2. Coeficientes do modelo
m1 # Intercept (B0) = , altura (B1) = 

# 3. Validação do modelo
# 3.a Valores de preditos e resíduos
predito=fitted()
residuo=resid()

# 3.b Normalidade dos resíduos
shapiro.test() 

# 4. Plot dos dados
plot()
abline()
segments() # independente, predito, independente, dependente

# 5. A regressão é significativa?
summary(m1)
## Multiple R-squared = é interpretado quando ambos os coeficiente são 
## significativos (B0 e B1)
## Adjusted R-squared = quando eu aceito somente o coeficiente angular 

## ESCREVA A SUA RESPOSTA:
## ...
   
## Vamos brincar? diametro=5,832*altura-1,49

# altura 1,74m
5.832*1.74-1.49

# altura 2,41m
5.832*2.41-1.49

# altura 9,27m
5.832*9.27-1.49

# altura 10m
5.832*15-1.49









