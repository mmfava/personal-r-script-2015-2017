
## ANOVA - Análise da variância
## Var. quanti ~ var. quali

## Pequeno glossário:
# --> Fator = Em delineamento de ANOVA, fator é um conjunto de níveis de um
#             único trtamento experimental. 

# --> Nível = O valor individual de um dado tratamento ou fator experimental
#             de uma análise de variância.

# --> Blocos = Uma área, um intervalo de tempo ou um indivíduo, dentro da
#              qual fatores não manipulados experimentalmente são considera
#              -dos homogêneos. 
#
# --> Variável resposta = y (quantitativa) --> meu único fator
# --> Variável preditora/ explicativa/ independente/fator = x1, x2, x3... (qualitativa)

# A análise da variância (ou ANOVA) é uma poderosa técnica estatística desenvo-
# lvida por R.A Fisher. Ela consiste em um procedimento que decompõe, em vários
# componentrs identificáveis, a variação total entre os valores obtidos no ex-
# perimento. Cada componente atribui a variação a uma causa ou fonte de varia-
# ção diferente; o número de causas de variação ou "fatores" depende do deline-
# amento da investigação. 

# A Anova apresenta diferentes tipos de aplicações de acordo com o delinea-
# mentos de pesquisa utilizado. Os principais tipos são os seguintes:

# ANOVA - fator único
# ANOVA - fator duplo
# ANOVA - fatorial
# ANOVA - para medidas repetidas
# ANOVA aninhada (nested ANOVA)

# Todas estas análises dependem dos fatores que estão sendo estudados nos 
# delineamentos de pesquisa. Entende-se como fatores os tratamentos realiza-
# dos em experimentos, ou coleta de dados de levantamento biológicos. Tais
# fatores são subdivididos em níveis ou categorias (e.g. controle, tratame-
# nto 1 e tratamento 2, pontos de coleta 1, 2 e 3).

## !! PRESSUPOSTOS IMPORTANTÍSSIMOS:
## As amostras são k grupos de observações independentes (k amostras aleató-
## rias) sendo os grupos independentes entre si. Estas amostras devem provir
## de uma população em distribuição normal e as variâncias dos grupos devem
## ser semelhantes para poderem ser comparadas. A garantia destes pressupos-
## tos estatísticos garante que a ANOVA é um teste estatístico paramétrico.

## -- ANOVA COM UM CRITÉRIO DE CLASSIFICAÇÃO (ONE-WAY) -- ##

# Um dos modelos mais simples de ANOVA é o que analisa os dados de um delinea-
# mento completamente casualizado ou ANOVA a um critério de classificação (One
# way ANOVA). Neste modelo, a variação global é subdividida em duas frações.
# A primeira é a varição entre as médias dos vários grupos, quando comparadas 
# com a média geral de todos os indivíduos do experimento e representa o efei-
# to dos diferentes tratamentos. A outra é a variação observada entre as unida-
# des experimentais de um mesmo grupo ou tratamento, com relaçaõ à média de-
# sse grupo: tratam-se das diferenças individuais, ou aleatorias, nas respo-
# stas.

# ANOVa one-way = Variação entre tratamentos + Variação dentro dos tratamen-
#                 tos.

# A variação entre grupos experimentais ou tratamentos é estimada pela vari-
# ância entre tratamentos ou simplesmente variância entre. A variação dentro
# do mesmo tratamento é estimada pela média das variâncias de cada grupo: é
# por isso chamada variância média dentro dos grupos ou variância dentro. 
# Como ela representa também a fração da variabilidade que não é explicada 
# pelo efeito dos tratamentos, é também chamada de variação residual ou, 
# ainda, variância do erro experimental.

# O teste de comparação entre os efeitos dos tratamentos baseia-se na pressu-
# posição de que os k tratamentos (A, B, C..) podem originar médias diferen-
# tes, mas a variação entre os indivíduos (S²) é igual em todas as populaçõ-
# es que estão sendo comparadas. Em outras palavras, deseja-se testar a hipó
# -tese de igualdade entre médias...

# H0: Média A = Média B = Média C... ou S²(A) = S²(B) = S² (C)..

# ... Supondo homocedasticidade, isto é, supondo que S²A = S²B = S²C...

# Deduz-se dai que se houver efeito diferencial entre tratamentos, a varia-
# ção entre eles deve ser maior que a variação dentro do mesmo tratamento.
# Ou seja, a variância entre entre deve ser maior do que a dentro. Isso eq-
# uivale a dizer que se houver diferença entre grupos, o resultado da divisão
# da variância entre pela variância dentro deve ser maior do que 1. Esse cál-
# culo é chamado de razão F de variâncias, em homenagem a Fisher, e seu resu-
# ltado é comparado com um valor tabelado para então se rejeitar ou não H0.

# !Os valores sempre serão positivos. 
# !Quando um dos quadrados dos tratamentos apresenta uma variação acima da 
# variação entre os tratamentos, acabamos refutando a igualdade entre os 
# tratamentos. 
# !Comparo F calculado com F critico: Se o F calculado cai no nível de confi-
# aça então as amostras são iguais, ou seja, o quadrado médio dos tratamen-
# tos é igual ao quadrado médio dos resíduos. 

## -- ANOVA one-way --##

# A ANOVA fator único apresenta sempre um fator em análise, o qual apresenta
# mais de dois grupos (níveis) para comparação. Nesta análise, se os grupos
# (níveis) são pré-determinados no início do experimento, então tem-se uma
# análise com efeitos fixos. Se os grupos são escolhidos aleatoriamente a 
# partir de um conjunto de possibilidades, então tem-se uma análise de efe-
# itos aleatórios. 
# Considera-se um planejamento equilibrado quando o número de indivíduos
# em cada grupo é equivalente, e os indivíduos são escolhidos aleatoriamen-
# te e a distribuição dos grupos também é aleatória, caracterizando assim
# um delineamento inteiramente casualizado (DIC). 

# Pressupostos do Delineamento inteiramente casualizados (DIC):

# 1. Aleatóriedade
# 2. Homogeneidade entre os tratamentos (homocedasticidade)
# 3. Tratamentos apresentam repetições equilibradas
# 4. Normalidades dos resíduos 
# 5. Coeficiente de variação ≤ 20\%. 

# O Delineamento inteiramente casualizado (DIC) trata do experimento em que
# os dados não são pré-separados ou classificados em categorias mais conhe-
# cidas como blocos. A ANOVA, associada a esse tipo de experimento, é muitas
# vezes chamada de ONE-WAY ANOVA.

# No modelo linear generalizado da One-way ANOVA considera-se:
# Yij = µ + erroij = µ + ti + erro ij
# onde:
# µi representa a média de cada grupo
# µ representa a média de todos os grupos
# ti representa a diferença entre a média total e a média de cada grupo
# eij representa um erro aleatório de cada observação sendo estes erros 
# independentes entre si.

# Com erros normais e independnetes
modelo=(lm(respostaquanti~varquali)) # Criação do modelo y (quanti)~x quali
ANOVA=aov(modelo) # Aplicação do teste ANOVA para o modelo
summary(ANOVA) # Resultado da anova

# Com estrutura de erros especificada
modelo1=glm(resposta~varquali)

# Com isso temos uma representação usual do quadoro da ANOVA, com fontes de
# variação (tratamento e residuals), graus de liberdade (Df = "degree free-
# dom"), somas de quadrado (Sum sq), quadrados médios (Mean Sq), valor do
# F calculado (F Value) e o nível de significância do teste. Uma forma de 
# verificar se o objeto que descreve os tratamentos foi criado corretamente
# é conferindo os graus de liberdade no quado da anova. Caso o objeto "trat-
# amento" não fosse um fator, temos apenas um grau de liberdade, indicando
# que a análise não procedeu da maneira correta.

# A ANOVA pode ser interpretada da seguinde maneira:
# H0:
# COmo o p-valor > 5%, não existe diferença entre as médias dos tratamentos.
# H1:
# Como o P-valor < 1%, então existe diferença significativa entre as médias
# de pelo menos dois tratamentos, a 1% de significância.

## -- Condição para uso da Anova -- ##

# - Homocedasticidade multivariada
bartlett.test()
leveneTest()

# - Normalidade dos resíduos
shapiro.test(RESIDUOS)

## ~~ Gráficos ~~ ##

plot(modelo)

##!! OBSERVAÇÃO:
## Em todos os tipos de análises de variância, para todas as variáveis qua-
## litativas, devem ser criados fatores e não vetores, ou seja, o objeto 
## que contém os nomes (ou números) dos tratamentos, dos blocos, etc. devem
## ser fatores e nãos vetores. Para criar fatores ou para a conversão de um 
## vetor em um fator podemos usar as funções factor () ou as.factor().
## É importante citar que a entrada de dados no R poderia ser realizada usa-
## ndo-se os recursos do R para a leitura de dados já existentes. 

## -- Comparação múltiplas entre médias -- ##

# Um valor de F significativo na ANOVA não indica quais são os tratamentos 
# significativamente diferentes entre sí quando comparados dois a dois;
# ele apenas mostra que existe ao menos uma diferença entre os grupos estu-
# dados. A identificação de diferenças particulares entre médias, tomando-as
# duas a duas, deve ser feita usando um dos vários testes de Comparações 
# multiplas entre Médias existentes. Esses testes são semelhantes ao t, com
# a diferença de que controlam o nível de significância ao levar em conta o
# número de comparações feitas no experimento. Além  disso, nesta técnica
# estatística, a variância dentro dos grupos é estimada usand o QM resíduo,
# que é baseado em todas as amostras, enquanto em um teste t a variância
# dentro do grupo é estimada com base em duas amostras apenas. 

## ~~ TESTE DE TUKEY ~~ ##

# O procedimeto de Tukey é um complemento à ANOVA e visa a identificar quais
# as médias que, tomadas duas a duas, diferem significativamente entre si. O
# método de Tukey protege os testes de um aumento no nível de significância
# devido ao grande número de comparações efetuadas. Note que se forem usados
# K grupos experimentais, é possível realzar k(k-1)/2 comparações de médias
# duas a duas.

# No R, o teste de Tukey é apresentado através de intervalos de confiança.
# A interpretação é: se o intervalo de confiança para a diferença entre duas
# médias não incluir o valor zero, significa que se rejeita a hipótese nula,
# caso contrário, não se rejeita. O resultado pode ser visto através de uma 
# tabela e/ou graficamente:

TukeyHSD(ANOVA, "tratamento")
# * O dms calcula a diferença entre as médias.

plot(TukeyHSD(ANOVA))
# A interpretação gráfica é a seguinte: Se o intervalo definido por lwr 
# e upr contiver o zero, a diferença entre as médias dos níveis do fator
# será não significativa para aquele par de médias em teste. Caso esse 
# intervalo não contenha o zero, a diferença, portanto, será significativa
# ao nível de confiança em que o teste foi realizado. 


## ~~ TESTE DE DUNNETT ~~ ## 

# Este teste é utilizado para comparar uma média, geralmente a do grupo-con-
# trole, com as demais. Aplica-se ao caso em que o pesquisados não está 
# interessado em realizar todas as compraçãoes possíveis, mas apenas as 
# (k-1) de cada tratamento com o controle, aproveitando a vantagem de maior 
# poder de análise de variância. 

## ~~ Como apresentar os resultados? ~~ ##

# Existem duas formas para apresentar os resultados de uma comparação 
# múltipla entre médias. A primeira consiste em ordenar os valores das
# médias, anotar a que tratamentos se referem e sublinhar as médias que não
# diferem significativamente entre si. Na segunda, colocam-se letras ao la-
# -do das médias: letras iguais indicam médias que não diferem significamente
# entre si.

## -- ANOVA DOIS CRITÉRIOS DE CLASSIFICAÇÃO -- ##

# O pareamento de amostras é desejável em muitos experimentos, uma vez que
# tende a diminuir as diferenças indivíduais. Com isso, facilita a identi-
# ficação de diferenças pequenas, porém reais, entre os tratamentos. O mes-
# mo princípio do pareamento pode ser estendido para mais de dois tratamen-
# tos organizando-se blocos de três ou mais valores relacionados entre si. 
# O bloco representa, então, um conjunto de unidades experimentais homogê-
# neas entre sí. O par é um bloco particular, contendo apenas dois elemen-
# tos. 

# Um modelo experimental desse tipo é denominado Delineamento em Blocos
# Casualizados e a análise chama-se ANOVA a dois critérios de classifica-
# ção (Two way ANOVA). É importante ressaltar que o critério de aleatoriza-
# ção, que faz parte de todo planejamnento experimental bem feito, também
# deve ser observado aqui. Por exemplo, em um experimento onde colocamos
# três diferentes tratamentos e as pessoas divididas em blocos a partir da
# idade, os tratamentos foram atribuídos ao acaso entre as pessoas perten-
# centes ao mesmo bloco e todos os tratamentos foram estudados em cada 
# bloco.

# O delineamento em Blocos Casualizados (DBC) abrange os três princípios
# básicos da experimentação: Repetição, casualização e controle local. 
# Este delineamento é bastante utilizado quando já heterogeneidade nas 
# condições experimentais. Nesse caso, divide-se o material experimental, 
# ou amostras, em blocos homogêneos, de forma a contemplar as diferenças
# entre os grupos. O que importa é a homogeneidade dentro de cada grupo e
# não entre os grupos. 


# RESUMO ANOVA TWO-WAY = Variação entre tratamentos + Variação entre blocos
#                        + Variação residual (entre indivíduos)

# Uma consequência do tratamento em blocos é que se pode agora estimar a va-
# riação devido à idade, que pode ser subtraída da variação residual. Assim,
# o valor númerico do QM resíduo diminuirá, restando apenas uma variação
# aleatória entre pessoas, a qual não pode ser atribuída nem à droga nem à
# idade, que já foram levados em consideração. 

# Os graus de liberdade para este teste são: GL numerados = GL e GL deno-
# minador = GL resíduo, que podem ser obtidos diretamente da tabela da 
# ANOVA. 

# A diferença entre tratamentos pode ser tomada, dois a dois, pelos testes
# de Tukeym SNK ou Bonferroni. A variação entre blocos também pode ser tes-
# tada. Em muitos casos, ela não é de particular interesse, pois está sendo 
# considerada uma característica que reconhecidamente determina variaçaõ en-
# tre os indivíduos e que se deseja controlar. Se, no entrando, for intere-
# ssante testar a variação ebtre blocos, é emprego outro teste considerando
# o bloco como um fator (anova 2 fatores).

# Em geral, o ganho de eficiência no experimento é mais interessante para o 
# pesquisador do que o próprio resultado do teste F para os blocos. Se o 
# teste para blocos não for significativo, pode indicar que o pesquisador 
# teve sucesso em reduzir a variação residual ou que, na verdade, as unida-
# des experimentais já eram homogêneas desde o início, não havendo necessi-
# dade, portanto, de reuni-las em blocos. 


## -- ANOVA two-way --##

# Two-way ANOVA
modelo2=aov(resposta~tratam+blocos) 
summary(modelo2)

# Resíduos
residuos=resid(modelo2)
residuos

# Soma dos resíduos e normalidade dos resíduos
sum(residuos)
shapiro.wilk(residuos)

# Soma dos quadrados dos resíduos
sum(residuos^2)

# Os totais dos tratamentos
tapply(resposta, tratamento, sum)

# Médias dos tratamentos
tapply(resposta, tratamento, mean)

# Os totais dos blocos
tapply(resposta, bloco, sum)

# As médias dos blocos
tapply(resposta, bloco, mean)

## ANOVA em esquema fatorial

# Em muitas experiências interessa estudar o efeito de mais do que um fac-
# tor sobre uma variável de interesse. Quando uma experiência envolve dois 
# ou mais factores diz-se que temos uma ANOVA múltipla. Uma ANOVA em que 
# todas as combinações de todos os níveis de todos os factores são conside-
# radas diz-se ANOVA factorial. Na maioria das situações, quando estamos in-
# teressados em estudar a influência de dois ou mais factores numa variável, 
# utilizamos uma ANOVA factorial.

# Em geral o número de níveis de cada factor bem como o seu valor não
# depende dos restantes factores. Quando o número de níveis ou o seu valor
# varia consoante os níveis considerados nos restantes factores diz-se que 
# temos uma ANOVA hierárquica. Nestes casos deixamos de ter uma ANOVA factorial.
# Enquanto numa ANOVA factorial os factores são cruzados (dando origem a
# todas as possíveis combinações dos seus níveks), numa ANOVA hieárquica os
# factores são encaixados uns nos outros (dando origem a uma estrutura tipo
# árvore).

# Vimos que numa ANOVA simples o factor em causa podia ter os efeitos fixos
# ou os efeitos aleatórios. O mesmo se vai passar com os modelos de ANOVA
# com dois ou mais factores.
# Quando um modelo têm todos os factores com efeitos fixos diz-se que temos
# uma ANOVA de efeitos fixos ou um Modelo I de ANOVA. Quando um modelo tem 
# todos os factores com efeitos aleatórios diz-se que temos uma ANOVA de 
# efeitos aleatórios ou um Modelo II de ANOVA. Quando um modelo tem alguns 
# factores com efeitos fixos e outros com efeitos aleatórios diz-se que 
# temos uma ANOVA de efeitos mistos ou um Modelo III de ANOVA.

## -- DOIS FATORES COM O DIC -- ## 

## Criando o modelo
modelo3=aov(resposta~fator1+fator2+fator1:fator2)

## Resposta
summary(modelo3)

## Neste caso, se os fatores forem significativos mas a interação 
## (fator1:fator2) não, pode-se dizer que os fatores atuam de maneira 
## independente. Dessa forma, pode-se dizer que a variação encontrada nos
## nos valores dos erros se deu ao acaso.

## !! obs: Os termos dist+ang+dist:ang o modelo podem ser substituídos pelo 
## equivalente dist*ang. Este último soma cada elemento separadamente 
## (fator 1 e depois fator 2), tornando-se assim equivalente primeiro. 

## Um comando gráfico muito útil em experimentos fatoriais pode ser usado
## para ajudar na interpretação:
interaction.plot(fator1, fator2, resposta, fixed=T)

## -- Dois fatores usando o DBC -- ##

## Esquemas fatoriais também podem ser conduzidos segundo um delineamento
## em blocos casualizados.

## O modelo
modelo4=aov(resposta~bloco+fator1+fator2+fator1:fator2)

## Resultado anova
summary(modelo4)

## Caso os fatores sejam significativos e a interação entre eles não, dize-
## mos que estes atuam de maneira independente. 

## -- ANOVA COM O PACOTE ExpDes -- ##

## Pacotes:
library(ExpDes.pt)
library(car)


## -- ANOVA - DIC Fator único ou One-way -- #

## Pergunta: A Largura (m) é igual entre os diferentes substratos?
## H0: Subs grosso = Subs Fino = Subs Folhiço

# Dados:
abioticos=read.csv(file.choose(),header=TRUE,sep=";",dec=",")
attach(abioticos)
names(abioticos)

## ~ Estatística descritiva ~ ##

# Começamos avaliando pela estatística descritiva
summary(Largura..m.[Substrato=="Grosso"])
summary(Largura..m.[Substrato=="Fino"])
summary(Largura..m.[Substrato=="Folhiço"])
boxplot(Largura..m.[Substrato=="Grosso"], Largura..m.[Substrato=="Fino"], Largura..m.[Substrato=="Folhiço"], names=c("Grosso", "Fino", "Folhiço"))
boxplot(Largura..m.~Substrato)

## ~ MODELO ~ ##

# Modelando a largura ~ do substrato
modelolarg=lm(Largura..m.~Substrato)
modelolarg
## !! Largura = -0.3567 * Folhiço - 0,0540 * grosso + 3,8613
## !! Mas e o fino?
## !! Se tivermos 1 do folhiço teremos 0 dos outros substratos
## !! Se tivermos 1 grosso teremos 0 dos outros 
## !! E se tivermos 0 de folhiço e 0 de grosso, nós teremos 1 de fino (mes-
## !! mo ele não aparecendo na função)

## ~ Pressupostos ~~ #

## 1) Homocedasticidade multivariada

# Depois avaliamos a homocedasticidade. Para tanto podemos usar os testes 
# de:
# - Bartlett
bartlett.test(Largura..m.~Substrato) # K-squared = 2,67, df=2, P = 0,26

# - Levine* (mais robusto entre os testes)
leveneTest(Largura..m.~Substrato) # F = 2,21, df=2, P = 0,12
# !! Em ambos os testes as variâncias foram consideradas iguais !! #

## 2) Normalidade dos resíduos

## Preditos e resíduos
predito=fitted(modelolarg)
predito
resido=resid(modelolarg)
resido

## Normalidade dos resíduos
shapiro.test(resido) # P = 0,34

## !! Como foram aceitos os pressupostos de normalidade e homocedasticidade
## !! eu posso usar um teste parámetrico --> neste caso a ANOVA.

## ~~ Anova DIC 1 fator - pacote ExpDes ~~ ##

## dic(trat, resp, quali = TRUE, mcomp = "tukey", sigT = 0.05, sigF = 0.05)

# trat = Vetor númerico ou complexo contendo os tratamentos
# resp = Vetor númerico ou complexo contendo a variável resposta
# quali = Se TRUE (verdadeiro e default), os tratamentos são entendidos 
#         como qualitativos, se FALSE como quantitativos
# mcomp = Escolha do teste "Post hoc" de comparação multipla. O default
#         é o teste de Tukey, contudo existem outras opções como o LSD
#         ou o SNK (vide help).
# sigT = Significância que será adotada no teste de comparação multipla 
#        de médias (default = 5%)
# sigF = Significância a ser adotada pelo Teste F da ANAVA (default = 5%)

## Testando a ação do substrato sobre a variável resposta Largura
dic(Substrato, Largura..m., quali=TRUE, sigF=0.05, mcomp="tukey", 
    sigT=0.05)
# Tratamento = substrato
# GL = grau de liberdade
# SQ = Soma dos quadrados 
# QM = Soma dos quadrados médios
# Fc = Valor do teste F
# P = P-valor do teste F
# CV% = coeficiente e variação dos resíduos em relação ao predito
#       CV < 20\% = homogênero (resíduos variam pouco em relação aos preditos).

# !! Resposta:
# Os diferentes substratos apresentaram médias de largura estatísticamente 
# iguais (Tabela 1).

# Tabela 1 - Resultado da análise "One-way Anova"
# considerando os diferentes substratos como tratamento
# e tendo como variável resposta a largura (m), onde:
# GL = Graus de liberdade, SQ = Soma dos quadrados, 
# QM = Quadrado médio, Fc = Resultado do teste F de Sneadcor,
# P = P-valor e CV% = Coeficiente de variação dos resíduos em 
# relação aos valores preditos. 
# ----------------------------------------------
#  Quadro da analise de variancia
# ----------------------------------------------
#            GL   SQ      QM      Fc    Pr>Fc
# Tratamento  2  1.1087 0.55434 0.8392 0.43917
# Residuo    42 27.7432 0.66055               
# Total      44 28.8519                       
# CV = 21.82 %
# -----------------------------------------------

## ~~ Outro exemplo:

## Pergunta: 

## Existe variação do índice glicemico em relação ao consumo de farinha
## da polpa de yacon?

## Banco de dados
data(ex1)
attach(ex1)

## Pressupostos

# 1. Homocedasticidade
# Quando o tratamento é quanti, não há necessidade de avaliar a homocedasti-
# cidade multivariada

# 2. Normalidade dos resíduos  
shapiro.test(resid(lm(ig~trat))) # P = 0,71

## Aplicando a ANOVA DIC
dic(trat, ig, quali = FALSE, sigF = 0.05, mcomp="tukey", sigT=0.05) 
## !! Quando a variável preditora é quanti, o dic também faz a regressão
##!! para os valores. 

## Resposta:
## A média de índice glicemico entre os diferentes tratamentos (consumo de
## farinha yacon) é igual (Tabela 2). Quanto a construção de um modelo,
## não foi encontrado um valor significativo de coeficiente "b".

# Tabela 2 - Resultado da análise "One-way Anova"
# considerando o efeito de diferentes níveis da farinha yacon
# sobre o índice glicemico, onde: GL = Graus de liberdade, 
# SQ = Soma dos quadrados, QM = Quadrado médio, 
# Fc = Resultado do teste F de Sneadcor,
# P = P-valor e CV% = Coeficiente de variação dos resíduos em 
# relação aos valores preditos. 
# ----------------------------------------------
#  Quadro da analise de variancia
# ----------------------------------------------
#            GL SQ       QM     Fc     Pr>Fc
# Tratamento  3 214.88 71.626 6.5212 0.0029622
# Resíduos    20 219.67 10.984                 
# Total      23 434.55                        
# CV = 3.41 %
# ------------------------------------------------
    
## -- ANOVA DBC - one-way -- ##

## Pergunta: Algumas barras alimenticias foram avaliadas quanto a aparên-
## cia, sendo questionada qual delas seriam as mais comercializaveis. 

## ~ Banco de dados ~ ##
data(ex2)
attach(ex2)

# O data é composto de "provador", "tratamento" e "aparência". Esses dados 
# são dados de engenharia de alimentos, sendo uma prática realizada para 
# avaliação de aspectos de determinados alimentos. O provador são as pes-
# soas, o tratamento são os alimentos, o bloco é a pessoa logo que cada 
# pessoa terá uma persepção para cada alimento. O fator é o tratamento e a
# variável resposta é a aparência. 

## ~ Anova DBC 1 fator ~ ##

# dbc(trat, bloco, resp, quali = TRUE, mcomp = "tukey", sigT = 0.05, 
# sigF = 0.05)

# Na função do DBC primeiro é colocado o fator (tratamento), depois o 
# bloco (no caso do exemplo, pessoa) e então a var. resposta (aparência). 

dbc(trat, provador, aparencia, quali = TRUE, mcomp="lsd", sigT = 0.05, sigF = 0.05)

## !! Para a DBC 1 fator, melhor usar o teste "post hoc" LSD


## Resposta:
# A aparência das barras alimenticias difere em pelo menos uma das marcas
# avaliadas (Tabela 3). A percepção dos avaliadores (blocos) também foi
# significativa (Tabela 3). Os resíduos variaram de maneira heterogênea
# quando comparadados aos preditos (CV > 20% - Tabela 3).
# As marcas "C", "D" e "E" foram consideradas como iguais quanto a sua 
# aparência e apresentaram maiores médias de pontuação(Tabela 4 e figura 1),
# seguido da marca "B" e então marca "A" (Tabela 4 e figura 1). 

# Figura 1.
boxplot(aparencia~trat)

# Tabela 3.
# ----------------------------------------------------
#  Quadro da analise de variancia
# ----------------------------------------------------
#             GL   SQ      QM     Fc      Pr>Fc
# Tratamento   4  720.38 180.096 71.156 0.00000000
# Bloco       69  324.00   4.696  1.855 0.00025852
# Residuo    276  698.55   2.531                  
# Total      349 1742.94                          
#  CV = 29.14 %
# ----------------------------------------------------

# Tabela 4.
# ------------------------------------------------------------------------
# Teste t (LSD)
# ------------------------------------------------------------------------
# Grupos  Tratamentos  Medias
#      a 	 D 	         6.657143 
#      a 	 E 	         6.6 
#      a 	 C 	         6.271429 
#      b 	 B 	         4.871429 
#      c 	 A 	         2.9 
# ------------------------------------------------------------------------

## -- ANOVA DIC dois fatores -- ##

## ~ Pergunta: A quantidade de zinco é influênciado pela compostagem com 
## casca de café? Existe diferença se ocorrer revolvimento desse material?

# É um DIC esquema fatorial duplo tipo 2x3.
# Fator 1 = 2 níveis
# Fator 2 = 3 níveis

## ~ Banco de dados
data(ex4)
attach(ex4)
names(ex4)

## ~ Pressupostos:

## 1. homocedasticidade:
leveneTest(zn~esterco) # F = 0,64, df = 1, P = 0,43
leveneTest(zn~as.factor(revol)) # F = 1,95, df=3, P = 0,15

## 2. Normalidade dos resíduos:
shapiro.test(resid(lm(zn~esterco))) # W=0,93, P = 0,71
shapiro.test(resid(lm(zn~revol))) # W=0,89, P = 0,71

## ~ Anova 2 fatores
# fat2.dic(fator1, fator2, resp, quali = c(TRUE, TRUE), 
# mcomp = "tukey", fac.names = c("F1", "F2"), sigT = 0.05, sigF = 0.05)

# onde:
# quali= Na primeira posição eu coloco TRUE/FALSE para o fator 1
#        Na segunda posição eu coloco TRUE/FALSE para o fator 2
#...
# fac.names = Permite noemar os fatores 1 e 2

fat2.dic(revol,esterco,zn,quali=c(TRUE,TRUE),mcomp="tukey",fac.names=c("Revolvimento","Esterco"),sigT = 0.05, sigF = 0.05)

## ~ Resposta:
# O quadro da ANOVA mostra que o revolvimento e o esterco atuam independen-
# temente sobre a variável resposta, uma vez que a interação representada
# por revolvimento x esterco não foi significativa (Tabela 5). Podemos verifi-
# car também que só houve diferença significativa para zn quanto ao fator
# esterco (Tabela 5). O restante da variação encontrada nos valores do erro
# se deu ao acaso. 
# A média de Zn foi signficativamente maior no grupo tratado com esterco
# quando comparado ao grupo sem o esterco (Figura 2).

interaction.plot(revol,esterco,zn)


## -- ANOVA DBC dois fatores -- ##

## Pergunta: Qual sabor de barra alimenticia foi melhor avaliado?

## ~ Banco de dados:
data(ex5)
attach(ex5)
names(ex5)

## ~ Pressupostos:
## 1. homocedasticidade:
leveneTest(sabor~trat) # F = 0,97, df = 3, P = 0,41
leveneTest(sabor~genero) # F = 0,77, df = 1, P = 0,08

## 2. Normalidade dos resíduos:
shapiro.test(resid(lm(sabor~trat))) # W = 0,92, P < 0,001
shapiro.test(resid(lm(sabor~genero))) # W = 0,91, P < 0,001

## !! Os dados obtidos para cada variável foram analisados verificando o 
## padrão de normalidade dos resíduos por meio do teste de Shapiro-Wilk. 
## Mesmo com os resíduos não apresentando normalidade, foi aplicado o teste
## de ANOVA-fator duplo, uma vez que em experimentação inteiramente casua-
## lizada o poder de análise é superior quando comparado com testes não pa-
## ramétricos !!

## ~ ANOVA DBC fator duplo

# fat2.dbc(fator1, fator2, bloco, resp, quali = c(TRUE, TRUE), 
# mcomp = "tukey", fac.names = c("F1", "F2"), sigT = 0.05, sigF = 0.05)

fat2.dbc(trat, genero, bloco, sabor, quali=c(TRUE, TRUE), mcomp="lsd", 
         fac.names=c("Amostras", "Gênero"))

## Resposta: 
# Não houve diferença de percepção de sabor entre os avaliadores (F=1,55; 
# P = 0,08), bem como entre as amostras (F= 1,94; P=0,13) e entre os 
# gêneros (F=2,30, P=0,13). A interação entre os gêneros e as amostras 
# também não mostratam diferenças significativas (F= 0,39; P = 0,76). 

# Tabela 6. 
# ------------------------------------------------------------------------
# Quadro da analise de variancia
# ------------------------------------------------------------------------
#                GL     SQ     QM      Fc   Pr>Fc
# Bloco            19  97.82 5.1484 1.55116 0.07832
# Amostras          3  19.37 6.4563 1.94522 0.12537
# Gênero            1   7.66 7.6563 2.30677 0.13118
# Amostras*Gênero   3   3.92 1.3062 0.39356 0.75783
# Residuo         133 441.43 3.3190                
# Total           159 570.19            
# CV = 27.58 %
# ------------------------------------------------------------------------

interaction.plot(trat, genero, sabor)

##########################################################################

## PRATICANDO ANOVA ##

library(ExpDes.pt)
library(car)

# Questões livro Sidia M. Callegari-Jacques, pg. 198.

# 71. O dado ecológico devido ao despejo de substâncias produzidas por cer-
# ta fábrica foi medido em quatro pontos de um curso d'agua: Antes da saída
# do efluente da fábrica (ponto A), na saída do efluente (B) e em dois ou-
# tros pontos situados após o local B (C e D). Com os dados (fictício)
# apresentados a seguir, teste a hipótese que que há diferentes índices de 
# dano ecológico nos locais examinados (alfa = 0,05). 

A=c(1,2)
B=c(5,4,5)
C=c(3,0,2)
D=c(3,3,2,2)

dado=c(1,2,5,4,5,3,0,2,3,3,2,2)
ponto=c("A", "A", "B", "B", "B", "C", "C", "C", "D", "D", "D", "D")

dano=data.frame(ponto, dado)
dano

## Pressupostos

# 1. Homocedasticidade:
bartlett.test(dado~ponto) # K-squared = 2,60, df = 3, P = 0,46

# 2. Normalidade dos resíduos
shapiro.test(resid(lm(dado~ponto))) # w = 0,91, P = 0,25

## Anova

## ~ Criando o modelo 
modelo=lm(dado~ponto)

## ~ Teste ANOVA
Anova=aov(modelo)

## ~ Resultado ANOVA
summary(Anova)

## ~ Teste Post-Hoc
TukeyHSD(Anova)

## Gráfico para o teste post-hoc
plot(TukeyHSD(Anova))
boxplot(dado~ponto)

## Resposta:
## As médias de dano ecológico foram significativamente diferentes entre os
## pontos A-B e C-B (F= 6,96, P = 0,01 - resultado Tukey).

#

# 72. Os próximos dados referem-se à redução no peso corporal de animais de 
# laboratório submetidos a diferentres dietas. Os animais foram previamente
# divididos em cinco grupos, por faixa de peso no início do experimento. 
# Compare as dietas entre sí e verifique também se a redução no peso varia
# entre as faixas de peso. Use um nível de sig. de 5%. 

trat=c("dieta 1", "dieta 1", "dieta 1", "dieta 1", "dieta 1", 
       "dieta 2", "dieta 2", "dieta 2", "dieta 2", "dieta 2",
       "dieta 3", "dieta 3", "dieta 3", "dieta 3", "dieta 3")
faixa=c("I", "II", "III", "IV", "V", "I", "II", "III", "IV", 
        "V", "I", "II", "III", "IV", "V")
peso=c(15, 17, 20, 24, 19, 10, 8, 12, 16, 18, 12, 16, 16, 15, 22)
dieta=data.frame(trat, faixa, peso)

dbc(trat, faixa, peso, quali = TRUE, mcomp="lsd", sigT = 0.05, sigF = 0.05)

## Resposta:
# Pelo menos uma das dietas apresentou médias de perca de peso diferente
# das demais (Fc = 7,10 e P = 0,01). Os blocos também apresentaram dife-
# renças estatísticas entre sí (Fc= 4,18, P=0,04). Os resíduos variaram de
# maneira homogênea quando comparadados aos preditos (CV < 20%).
# Quanto a perca de peso em sí, a dieta A apresentou uma média de 
# perca de peso superior a dieta 2, enquanto a dieta 3 foi intermediaria
# em relação a dieta 1 e 2 para perca de peso (Tabela Tukey).  

# ------------------------------------------------------------------------
#  Quadro da analise de variancia
# ------------------------------------------------------------------------
#          GL      SQ     QM     Fc    Pr>Fc
# Tratamento  2  96.400 48.200 7.1057 0.016829
# Bloco       4 113.333 28.333 4.1769 0.040720
# Residuo     8  54.267  6.783                
# Total      14 264.000                       
# CV = 16.28 %
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Teste t (LSD)
# ------------------------------------------------------------------------
# Grupos Tratamentos  Medias
# a   	   dieta 1 	 19 
# ab    	 dieta 3 	 16.2 
# b        dieta 2 	 12.8 
# ------------------------------------------------------------------------

boxplot(peso~trat)

# 

# 75. Deseja-se comparar quatro procedimentos usados para estimar o tempo de 
# gestação em mulheres grávidas: Data da última mestrução (UM), exame vagi-
# nal (EV), ultra-som (US) e nível sanguíneo de diamina oxidase (DO). To-
# dos os quatro procedimentos foram usados na mesma pessoa e o número de
# mulheres estudadas foi sete. Verifique se existe diferença entre procedi-
# mentos quanto ao tempo estimado de gestação (alfa = 5%).

gestante=c("1","2","3","4","5","6","7", "1","2","3","4","5","6","7", "1","2","3","4","5","6","7", "1","2","3","4","5","6","7")
tempo=c(275, 292, 281, 284, 285, 283, 290, 273, 283, 274, 275, 294, 279,
        265, 273, 285, 270, 272, 278, 276, 292, 244, 329, 252, 258, 275, 
        279, 295)
trat=c("UM", "UM", "UM", "UM", "UM", "UM", "UM", "EV", "EV", "EV", "EV", "EV",
       "EV", "EV", "US", "US", "US", "US", "US", "US", "US", "DO", "DO", "DO", 
       "DO", "DO", "DO", "DO")

data=data.frame(gestante, trat, tempo)

# ~ Pressupostos:

# Homocedasticidade
bartlett.test(tempo~trat) # P <0,01

# Normalidade
dbc(trat, gestante, tempo, quali = TRUE, mcomp="lsd", sigT = 0.05, sigF = 0.05)
# Resíduos normais

# ~ Anova:
dbc(trat, gestante, tempo, quali = TRUE, mcomp="lsd", sigT = 0.05, sigF = 0.05)

# Resposta: 
# Não houve diferença entre os tratamentos para o tempo de gestação estima-
# do (em dias) (F = 0,49, P = 0,69), bem como entre as gestantes (blocos)
# (F = 2,46, P = 0,06). 

boxplot(tempo~trat)


## Revisão prof. Ana

## ANOVA: Usamos quando a variável preditora = fator = qualitativa
##        Resposta= quantitativa

## DIC - Delineamente inteiramente casualizado
## Nela, toda a distribuição dos grupos é feita de maneira aleatória

## DBC - Delineamento em blocos casualizados
## Delineamento inteiramente casualizado mas com controle local 

## Esquema - relacionada a variável independente = FATOR.
## 1 Fator = Anova fator único
## 2 Fatores = Anova fator duplo
## = ou > 3 Fatores = Anova multivariada

## Níveis - Número de tratamentos por fator

## Anova = F = QMTtratamentos/QMresíduos

## Caso o P da anova apresente um valor < 0,05, nós 
## vamos para os testes de acompanhamento.

## Teste de acompanhamento ou Post-Hoc Test
## - Mais usado: TukeyHSD - Uso quando eu tenho n'a iguais (tratamentos 
## homogêneos entre os níveis). 
## - TukeyNHSD - Uso quando os n's são diferentes
## - LSD
##- Bonferroni
## - Skt-Knot

## Exemplo: Todos os delinamentos da tabela são casualizados.

ANOVA=read.csv2(file.choose(),dec=",",header=TRUE,strip.white=TRUE)
attach(ANOVA)

## 1. Avalie se os locais apresentam efeito sobre o peso (peso, DIC-simples)
dic(local, peso, quali =TRUE, sigF = 0.05, mcomp="tukey", sigT=0.05) 
bartlett.test(peso~local)
boxplot(peso~local)

# Resposta 1:
# O peso médio difere eem pelo menos um dos locais (Fc=24,34, gl=2, 
# P= < 0,001) com variação homogênea dos resíduos (CV=9,34%). O tratamento
# "A" aprentou uma média de peso significatimanente inferior aos tratamen-
# tos "B" e "C".

## 2. Avalie se os locais e a temperatura promovem efeito sobre o peso DIC
## fator-duplo.
fat2.dic(local, temperatura, peso, quali = c(TRUE, TRUE), 
         mcomp = "tukey", fac.names = c("Local", "Temperatura"), sigT = 0.05, sigF = 0.05)

interaction.plot(local, temperatura, peso)

## Resposta:
## O quadro da ANOVA mostra que "local" e "temperatura" atuam de maneira
## independente sobre a variável resposta "peso", uma vez que a interação
## representada por Local*Temperatura não foi significativa (P-valor = 0,77).
## Quando os fatores foram considerados individualmente, ambos apresentaram
## efeito sobre a variável resposta peso. A variação dos resíduos em rela-
## ção aos pedritos foi homogênea (CV%=6.63). 
## O local "C" e "B" apresentaram médias de peso significativamente maiores.
## A média de peso foi maiores na temperatura de 27ºC. 

## 3. Sabendo que curva de nível promovem um efeito sobre o peso das plantas,
## avalie se locais e temperatura elevam o efeito sobre o peso (DBC - duplo).

fat2.dbc(local, temperatura, curva, peso, quali = c(TRUE, TRUE), mcomp = "tukey", fac.names = c("Local", "Temperatura"), sigT = 0.05, sigF = 0.05)

## Resposta:
## Assim como no DIC, não houve efeito dos fatores quando considerados em 
## em conjunto. A separação dos tratamentos em blocos também não apresentou
## diferenças significativas.




