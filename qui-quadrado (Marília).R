
## ~~ Qui-quadrado ~~ ## 

## var. quanti ~ var. quanti

# A estatística chi-quadrado foi criada por K. Pearson para medir o grau de
# discrepância entre um conjunto de frequências observadas (O) e o conjunto
# de frequências esperada segundo determinada hipótese (E). 

# Em suma, a fórmula para calcular o chi-quadrado é:

# x² = ∑ (O - E)² / E

## Objetivo do teste chi-quadrado:

# 1 - Verificar se uma distribuição observada de dados ajusta-se a uma distri-
# buição esperada (teórica): O teste se chama teste chi-quadrado de aderência
# ou de ajustamento.

# 2 - Comparar duas ou mais populações com relação a uma variávei categórica:
# O teste denomina-se teste chi-quadrado de proporções (ou teste chi-quadrado
# de heterogeneidade entre populações).

# 3 - Verificar se existe associação entre duas variáveis qualitativas: O
# teste é chamado de teste chi-quadrado de associação. 

## -- TESTE DE ADERÊNCIA OU DE AJUSTAMENTO -- ## 

# Verificar se uma distribuição obs de frequências (O) ajusta-se a uma dis-
# tribuição de valores esperados segundo determinada teoria (E).

# ~ Hipóteses:
# H0: A distribuição de frequências observadas (O) é igual à distribuição 
# de frequências esperadas segundo a hipótese que se está testando (E).
# Abreviadamente H0: O = E. 

# ~ Restrições:

# 1. Tabela de entrada única com n > 25. 
# 2. Tabelas com apenas duas categorias (K = 2): Em tabelas com apenas duas
# categorias a frequência esperada deve ser > 5 em cada categoria e deve-se
# aplicar a correção de Yates para cálculo do X2. Se a FE for < 5, deve-se
# utilizar o teste de hipótese com distribuição binomial.
# 3. Tabelas com K > 2 e todas as frequências esperadas iguais: Para testes
# com nível de significância (alfa) = 0,05, os valores esperados devem ser
# maiores ou iguais a 1. Para um alfa = 0,01, os valores esperados devem ser
# maiores ou iguais a 2. Aplicar correção de Yates em FE > 5.
# 4. Tabelas com K > 2 e as frequências esperadas diferentres: Aplica-se o 
# teste chi-quadrado após conferir as seguintes condições
# 4.1 alfa = 0,05 --> n > ou = 10, n/k > ou = 2
# 4.2 alfa = 0,01 --> n > ou = 10, n/k > ou = 4

## !! SEMPRE APLICAR A CORREÇÃO DE YATES QUANDO n > OU = 25
##    FE > 5 !! -- INDEPENDENTE DO NÚMERO DE CATEGORIAS. 

# Exemplo 1: 
# Existe diferença em relação aos gêneros da turma de conservação e manejo?
sexo=c("feminino"=20, "masculino"= 12)

# - 1º É esperado que as proporções sejam iguais.
# - 2º São 20 mulheres e 12 homen --> n < 25*
# - 3º A tabela possui k = 2 e a FE é > 5. 
## !! Aplicar correção para continuidade de Yates.

# ~ Teste chi-quadrado:
chisq.test(sexo, correct=TRUE) # chi-quadrado = 2, df = 1, P = 0,15
                               
# ~ Resposta: A distribuição observada não difere da distribuição esperada;
# as diferenças observadas foram casuais. Dessa forma, a proporção de mu-
# lheres e homens no mestrado de conservação e manejo pode ser considerada
# como igual. 

# Exemplo 2:
## Exercício: Imagine a situação na qual um investigador está estudando a 
# presença dos antígenos R e S (fictícios) em tecido humano. O assunto é de 
# grande importância, pois seriam antígenos relacionados com a histocompati-
# bilidade e qualquer informação relativa à herança desses antígenos de gran-
# de valia nos casos de transfusões e transplante de tecidos e orgão. 
# Imagine que existam 3 tipos de heranças:

# TIPO R: pessoas que só possuem o antígeno R
# TIPO S: pessoas que só possuem o antígeno S
# TIPO RS: pessoas que possuem os dois antígenos

## O pesquisador faz a hipótese de que estes tipos são determinados genetica-
## mente por um par de genes R e S autossômicos co-dominantes, isto é, que 
## estão no mesmo loco cromossômico e se expressam ambos, independentemente, 
## no heterozigoto. No caso, os genótipos para cada tipo seriam: genótipo RR
## (determinando o fenótipo R), genótipo SS (fenótipo SS) e genótipo RS (fe-
## nótipo RS). Se esta hipótese estiver correta, então filhos resultantes de 
## cruzamentos de mulheres RS com homens RS devem apresentar os três genóti-
## pos possíveis, nas proporções 1/4 para R, 1/2 para RS e 1/4 para S, con-
## forme determina a Primeira Lei de Mendel.
## Desejando testar a hipótese de que os alelos R e S são co-dominantes, o 
## pesquisador estudou 24 filhos de casamentos do tipo RS x RS, escolhidos 
## aleatoriamente, e obteve 6 crianças do tipo R, 15 do tipo RS e 3 do tipo
## S. Até que ponto a distrição das frequências é feita ao acaso?

# Base de Dados
gen_obs=c("R"=6, "RS"=15,"S"=3)
gen_obs

# ~ Calculando na "raça" 

# Plotar os dados em um gráfico de colunas
barplot(gen_obs,xlab="genótipos",ylab="Frequência",cex.names=0.7)

# Calcular as frequ?ncias esperadas
gen_exp=c("R"=24*0.25, "RS"=24*0.5,"S"=24*0.25)
gen_exp

# Etapas do cálculo de Qui Quadrado
desvio=gen_obs-gen_exp
desvio

d.quad=desvio^2/gen_exp
d.quad

qui2=sum(d.quad)
qui2

## Cálculo do p-valor do teste de Qui Quadrado
pchisq(q=qui2, df=2,lower.tail=FALSE)

## Construção da curva de distribuição de qui quadrado
curve(dchisq(x, df=2),-0.5,10, xlab="Qui-quadrado, 2 g.l.", 
      ylab="Densidade probabil?stica")

## Sobrepõe uma linha vermelha a partir do valor do Qui-quadrado cr?tico
curve(dchisq(x, df=2), 5.99, 10, add=T, col="red", lwd=2)

## ~ Calculando com o R (mel na chupeta)
chisq.test(gen_obs, p = c(0.25, 0.5, 0.25)) # X² = 2,25, df = 2, P = 0,32

## ~ Resposta:
## As frequências genótipicas observadas seguem proporções esperadas de 
## 1:1:2 (x² = 2,25, gl = 2, p = 0,32).

## !! Caso o P valor tivesse sido menor que o 0.05, então teriamos que fa-
## zer um teste de acompanhamento após o teste de X². Este teste é denomina-
## do Teste de Marascuillo !!. 

# Exemplo 3:
# Existe diferença quanto a frequência de coleta seletiva entre 5
# municipios no oeste paranaense?

# ~ Dados:
data=matrix(c(96,123,82,112,100,204,177,218,188,200),nrow=2,byrow=5)
data
rownames(data)=c("Sim","Não")
colnames(data)=c("Cascavel","Corbélia","Toledo","Palotina","Medianeira")
data

# ~ Frequência relativa (%) de recolhimento do lixo reciclado
prop.table(data,2)*100
barplot(prop.table(data,2)*100,beside=TRUE,legend.text = rownames(data))

# ~ Aplicando o teste qui-quadrado: 
chisq.test(data)
# * GL = (nº colunas - 1) * (nº linhas - 1) = (5-1)*(2-1) = 4

# ~ Resposta: 
# A distribuição da frequência observada difere do que seria esperado, ou 
# seja, a mesma frequência de coleta de reciclados entre os municipios
# (x² = 14, 504, df = 4, P = 0,01). 

# ~ Pergunta: Mas quem difere de quem? 

## ~~ Teste Marascuilo - Post-Hoc de aderência ~~ ##

# Teste de marascuilo
# Teste de resíduos ajustados

# Preparação dos dados para a análise marascuilo:
n=c("Cascavel", "Corbélia", "Toledo", "Palotina", "Medianeira") # Categorias
p=c("Cascavel"=0.32, "Corbélia"=0.41,"Toledo"=0.27,"Palotina"=0.37,
    "Medianeir"=0.33) # Frequência observada nas categorias
N=length(p) # Comprimento da minha amostra - número de FR observadas
value=critical.range=c() # Valor critico - calculo critico do qui-quadrado
value
sig=c() # Significância do teste - ele gera depois
sig

1/5*513

# Teste do marascuilo e teste de resíduos ajustados: 
for(i in 1:(N-1))
{for (j in (i+1):N)
{
  value=c(value,(abs(p[i]-p[j])))# i = coluna J = linhas
  critical.range=c(critical.range,
                   sqrt(qchisq(0.95,4))*sqrt(p[i]*(1-p[i])/300 + p[j]*(1-p[j])/300))
}
} # 0.95 = intervalo de confiança, 4 = grau de liberdade
sig=ifelse(value>critical.range,"Sim","Não")
marascuilo=data.frame(prop.diff=round(value,3),critical.value=round(critical.range,3),sig)
row.names.temp=c()
for(i in 1:(N-1))
{for (j in (i+1):N)
{
  row.names.temp=c(row.names.temp,paste("p",i,"-","p",j,sep = ""))
}
}
rownames(marascuilo)=row.names.temp
marascuilo

# ~ INTERPRETAÇÃO:
# Houve diferença significativa entre pelo menos uma das proporções de
# separação de lixo nas cidades estudadas (X² = 14,504; GL = 4; p = 0,005).
# Na avaliação com comparações múltiplas das proporções, foi possível verifi-
# car que apenas Corbélia apresentou proporções significativamente mais eleva-
# das (41%) do que Toledo (27%) ( P < 0,05). As demais proporções consideradas
# estatisticamente equivalentes (P>0,05).


## -- TESTE DE ASSOCIAÇÃO OU DE INDEPENDÊNCIA -- ## 

# O teste chi-quadrado (ou teste de independência) é utilizado para testar
# a correlação entre variáveis categóricas, assim como o coeficiente r é
# calculado e testado com o mesmo fim para variáveis quantitativas. Para
# realizar um teste chi-quadrado de associação, os indivíduos de uma amos-
# tra são estudados quanto a duas variáveis qualitivativas e os dados são 
# organizados em uma tabela de contingência, na qual as linhas e colunas
# apresentam as categorias das duas variáveis em análise. Neste teste, o 
# único total fixo (controlado pelo pesquisador) é o total de indivíduos 
# estudados. 

# Hipóteses: 
# H0: Não existe associação entre as variáveis em estudo (ou H0: O = E su-
# pondo independência).
# H1: Existe associação entre as variáveis em estudo. 

# ~ Restrições:
# 1. As categorias das variáveis devem ser independentes, caso contrário
# deve ser utilizado o teste de McNemar. 
# 2. Para as tabelas de entrada dupla (2x2), o n > 25 e nenhuma frequência 
# esperada pode ser menor do que 5. Além disso, com este n deve-se utilizar 
# a correção de continuidade de Yates. Se a frequência esperada minima não
# for atingida (5) deve-se utilizar o teste Exato de Fisher. 
# 3. Tabelas de entrada dupla, mas com 2xc (ou seja, duas linhas e mais de
# 2 colunas), o X2 pode ser calculado se todas as frequências esperadas fo-
# rem > ou = 1. 
# 4. Tabelas LxC (ou seja, mais de duas linhas e mais de duas colunas): O 
# teste chi-quadrado é um procedimento seguro se o número esperado médio 
# for 6 ou mais com alfa = 0,05, e 10 ou mais para testes com alfa = 0,01. 
# O esperado médio pode ser obtido dividindo-se o total de individuos estu-
# dados pelo número de canselas. 

# Exemplo 1:
# O número de bactérias em um determinado ponto de um rio está relacionado
# a este ponto?

# ~ Dados
ponto1=c("e-coli"=23, "salmonella"=2324, "klebsiela"=1)
ponto2=c("e-coli"=144, "salmonella"=125, "klebsiela"=7)
ponto3=c("e-coli"=10, "salmonella"=4, "klebsiela"=120)

# ~ Base de dados
base=data.frame(ponto1,ponto2,ponto3)
base
base1.1=as.matrix(base)
base1=t(base)
base1

# ~ Gráfico de barra com a frequência das bactérias por ponto
barplot(prop.table(base1.1,2),beside=T,legend.text = rownames(base1.1))

# ~ Teste Chi-quadrado
chisq.test(base1)

# Resposta:
# Existe associação entre as variáveis em estudo (chi-quadrado = 3397,2, 
# df = 4, P < 0,001). 

## !!
## Uma conclusão simples de existência de associação é em geral insatisfa-
## tória para os pesquisadores que gostariam de entender melhor o tipo de 
## associação observada. Para responder a esta indagação, existem várias
## técnicas estatísticas; a análise de residuso é uma das mais interessan-
## tes. 

## ~~ Análise de resíduos -  Post Hoc L x C ~~ ##

# A análise de resíduos é usada como auxiliar na interpretação de dados or-
# ganizados em tabelas L x C. Por seu intermédio, é possível avaliar como 
# as diferentes caselas contribuem para o valor do qui-quadrado. 

# ~Calcula-se inicialmente o resíduo padronizado (Rp) para cada cansela:
x=chisq.test(base1) # Calcula-se o qui-quadrado em um vetor
x$observed # Mostrar as frequências observadas
x$expected # Mostrar os valores esperados
x$residuals # Mostrar os resíduos (O-E)
x$stdres # *Calcular os resíudos standartizados

# *estarndartizar = ajustar todos os 
# dados para uma mesma distribuição.
# Diferença entre os valores / desvio 

## !! Valor critico z-score= entre -1,96 e +1,96
##    Quem é maior que o valor critico?
##    Os que forem maiores que o valor critico apresentam seus freq. obser-
#     vadas diferentes do que seria esperado. 


## Resultado do post-hoc:
#         e-coli     
# ponto1 -27.8880413 < z critíco (+1,96): Freq. obs < esperada - b
# ponto2  32.6974994 > z critíco (+1,96): Freq. obs > esperada - a
# ponto3   0.5060512 obs = esperado - ab

#         Salmonela
# ponto1  40.21945 > z critíco (+1,96): Freq. obs > esperada - a
# ponto2  -24.37500 < z critíco (-1,96): Freq. obs < esperada - b
# ponto3  -32.52672 < z critíco (-1,96): Freq. obs < esperada - b

#         Klebsiela
# ponto1  -27.471196 < z critíco (-1,96): Freq. obs < esperada - b 
# ponto2  -1.752167 obs = esperado - ab
# ponto3  47.900933  > z critíco (+1,96): Freq. obs > esperada - a

## INTERPRETAÇÃO:
## Houve diferença estatística significativa entre a distribuição das 
## proporções das bactérias nos pontos (X² = 3397, gl = 4, p-valor < 0,001)
## Houve maior proporção de Salmonella no P1, E. coli no P2, e Klebesiela
## no P3 (P < 0,05) (Tabela 1). 

##########################################################################
##
## Lista de exercícios Qui-quadra

# 59. Três pesquisadores avaliaram o número de ovos depositados pela bor-
# boleta "maria-boba" (Heliconius erato phyllis) em plantas de maracujá 
# (Passiflora misera) durante o ano de 1981 e concluíram que houve uma va-
# riação mensal na ovoposição (Romanowsky e colaboradores, 1985). Comprove
# essa conclusão com os dados a seguir, usando o método estatístico apropri-
# ado. 

# - Entrada de dados
data60=matrix(c(17,4,28,39,3,1,0,1,32),nrow=9,byrow=1)
data60 
colnames(data60)=c("número de ovos")
rownames(data60)=c("mar","abr","mai","Jun","Jul","ago","set","out","nov")
data60

# - Frequência com duas casas
prop.table(data60,2)

# - Gráfico de barras
barplot(prop.table(data60,2)*100,beside=TRUE,names=c("mar","abr","mai","Jun","Jul","ago","set","out","nov"), ylab="Frequência %")

# - teste chi-quadrado
chisq.test(data60) # As frequências observadas são diferentes do esperado.

# - Teste Post-Hoc (Marascuilo)

sum(data60) # Total de observações
GL=9-1 # Graus de liberdade
GL
# -- 

p=c(prop.table(data60,2))
p
N=length(p)
value=critical.range=c()
sig=c()

for(i in 1:(N-1))
{for (j in (i+1):N)
{
  value=c(value,(abs(p[i]-p[j])))
  critical.range=c(critical.range,
                   sqrt(qchisq(0.95,8))*sqrt(p[i]*(1-p[i])/125 + p[j]*(1-p[j])/125))
}
}
sig=ifelse(value>critical.range,"Sim","Não")
marascuilo=data.frame(prop.diff=round(value,3),critical.value=round(critical.range,3),sig)
row.names.temp=c()
for(i in 1:(N-1))
{for (j in (i+1):N)
{
  row.names.temp=c(row.names.temp,paste("p",i,"-","p",j,sep = ""))
}
}
rownames(marascuilo)=row.names.temp

marascuilo # Resultado marascuilo. 

## Resposta:
## O chi-quadrado apontou diferenças na frequência de ovoposição observadas
## (chi-quadrado = 137,44, df = 8, P < 0,001). 

## 60. Em determinada empresa, foram registrados os seguintes números de 
## acidentes no mês de agosto de 1998: 42 em segundas-feiras, 23 em terças-
## feitas, 25 em quarta-feiras, 19 em quintas-feiras, 23 em sextas-feirais, 
## 48 em sábados (manhã e tarde). Verifique se os acidentes de trabalho
## ocorrem com a mesma frequência nos seis dias úteis da semana (alfa =
## 0,01). 

# - Entrada de dados
acidentes=matrix(c(42, 23, 25, 19, 23, 48), nrow=1, byrow=6) 
rownames(acidentes)=c("acidentes")
colnames(acidentes)=c("segunda","terça","quarta","quinta","sexta", "sabado")
acidentes

# - Frequência com duas casas
prop.table(acidentes)

# - Gráfico de barras
barplot(prop.table(acidentes)*100,beside=TRUE,names=c("segunda","terça","quarta","quinta","sexta", "sabado"), ylab="Frequência %")

# - Teste Qui-quadrado
chisq.test(acidentes) # P < 0,01

# - Teste Post-Hoc (Marascuilo)

sum(acidentes) # n = 180
6-1 # GL = 5

p=c(prop.table(acidentes))
N=length(p)
value=critical.range=c()
sig=c()

for(i in 1:(N-1))
{for (j in (i+1):N)
{
  value=c(value,(abs(p[i]-p[j])))
  critical.range=c(critical.range,
                   sqrt(qchisq(0.95,5))*sqrt(p[i]*(1-p[i])/180 + p[j]*(1-p[j])/180))
}
}
sig=ifelse(value>critical.range,"Sim","Não")
marascuilo=data.frame(prop.diff=round(value,3),critical.value=round(critical.range,3),sig)
row.names.temp=c()
for(i in 1:(N-1))
{for (j in (i+1):N)
{
  row.names.temp=c(row.names.temp,paste("p",i,"-","p",j,sep = ""))
}
}
rownames(marascuilo)=row.names.temp
marascuilo

## Resposta: A frequência de acidentes observados em cada dia da semana 
## difere do que seria esperado (igual proporção entre os dias) 
## (X² = 23,73, df = 5, P<0,001). Na avaliação com comparações múltiplas
## das proporções, foi possível verificar que o sabádo apresentou maior 
## frequência de acidentes, e a terça, a quinta e a sexta com menores freq.
## A segunda-feira e a quarta-feira apresentaram níveis intermediarios de
## acidentes (tabela Marascuilo). 

## 61. Em um experimento genético clássico realizado no início do século XX,
## H. Nilsson-Ehle cruzou plantas de trigo que produziam sementes vermelho-
## escuras e plantas com sementes brancas e obteve descendentes que produ-
## ziam sementes de cores diferentes, nas seguintes proporções: 1/16 eram
## plantas que produziam grão vermelho escuro, 4/16 produziam sementes ver-
## melhas, 6/16 vermelho médio, 4/16 vermelho claro e 1/16 eram plantas com
## sementes brancas. Em um cruzamento de igual natureza, certo agrônomo
## obteve 6, 12, 29, 17 e 0 indivíduos de cada tipo, respectivamente, e pro-
## -pôs a hipótese de que estaria havendo, nas condições de seu experimento, 
## uma seleção contra o trigo de grãos brancos. Os dados apoiam a hipótese
## formulada (alfa=0,05)?

## - Dados observados 
genesobs=c("gve"=6, "v"=12, "vm"=29, "vc"=17, "b"=0)
genesobs

# - Valores observados em um gráfico de colunas
barplot(genesobs,xlab="genótipos",ylab="Frequência",cex.names=1)

# - Dados esperados segundo Nolsson-Ehle
sum(genesobs) # 64
genesesp=c("gve"=1/16*64, "v"=4/16*64, "vm"=6/16*64, "vc"=4/16*64, "b"=1/16*64)
genesesp

# - Teste Qui-quadrado
chisq.test(genesobs, p = c(0.0625, 0.25, 0.375, 0.25, 0.0625)) 

## Resposta: A distribuição de frequências observadas (O) é igual à distri-
## buição de frequências esperadas segundo a hipótese que se está testando
## (E) (X² = 7,1042, Gl = 4, P = 0,1305), desta forma a frequência do 
## trigo branco está de acordo com o que é esperado. 

# 62. Na população caucasóide, 70% das pessoas sentem o gosto amargo da PTC
# enquanto 30% são insensíveis a essa substância. Suponha que em uma amostra
# de 240 caucasóides com problemas de tireóide, foram encontrados 144 sensí-
# veis. Existe uma relaçaõ entre problemas na tireóide e sensibilidade 
# ao PTC (alfa = 0,05)?

# - Entrada de dados
exe62=matrix(c(144, 96), nrow=2, byrow=1) 
rownames(exe62)=c("sim","não")
colnames(exe62)=c("tireóide")
exe62

# - Frequência observada
prop.table(exe62)

# - Frequência esperada
esp62=c("sim"=240*0.7, "não"=240*0.3)
esp62
esp62/240

# - Qui-quadrado
chisq.test(exe62, p = c(0.7, 0.3), correct=T) 

## Resposta:
## A frequência observada oara sentir ou não o gosto do PTC em caucasianos
## com problema na tireóide foi menor do que a frequência esperada para
## essa condição na população, portanto não há associação entre essas
## condições (x² = 11,43, gl=1, P < 0,001).

# 63. Os dados seguintes referem-se à cor da pelagem em uma amostra de roe-
# dores de determinada espécie. Verifique se tal característica está associ-
# ada com o sexo (alfa = 0,05). 

# - Entrada de dados
exe63=matrix(c(22, 13, 15, 16, 17, 17), nrow=2, byrow=3) 
rownames(exe63)=c("machos", "femeas")
colnames(exe63)=c("preto","marrom","manchada")
exe63

# ~ Gráfico de barra com a FR% das cores por sexo
barplot(prop.table(exe63,2),beside=T,legend.text = rownames(exe63))

# ~ Teste Chi-quadrado
chisq.test(exe63)

## Resposta: Não existe associação entre a cor da pelagem e o sexo dos 
## roedores (X² = 1,60, gl = 2, P = 0,45).

## 64. Com o objetivo de avaliar fatores de risco para o câncer intra-epi-
## telial da cérvice uterina, Soares (1998) estudou 43 casos com essa doen-
## ça e 63 mulheres controles da população de Porto Alegre. Dessa forma, 
## ele obteve relativos à idade de início das relações sexuais e à presen-
## ça do alelo DBQ1*03, do sistem HLA, nessas mulheres. Teste separadamente
## a associação entre cada uma destas variáveis e o desenvolviento da neo-
## plasia em estudo (alfa = 0,05).

# - Entrada de dados

# Tabela 1:
tab1=matrix(c(19, 22, 2, 16, 33, 14), nrow=2, byrow=3) 
rownames(tab1)=c("casos","controle")
colnames(tab1)=c("< 16", "17-20", ">20")
tab1

#Tabela 2:
tab2=matrix(c(33, 10, 24, 39), nrow=2, byrow=2) 
rownames(tab2)=c("casos","controle")
colnames(tab2)=c("DBQB1", "Outro")
tab2

# - Gráfico de barra com a FR% das cores por sexo
barplot(prop.table(tab1),beside=T,legend.text = rownames(tab1))
barplot(prop.table(tab2),beside=T,legend.text = rownames(tab2))

# - Chi-quadrado

# Para Tabela 1:
chisq.test(tab1)
x=chisq.test(tab1) # Calcula-se o qui-quadrado em um vetor
x$observed # Mostrar as frequências observadas
x$expected # Mostrar os valores esperados
x$residuals # Mostrar os resíduos (O-E)
x$stdres # *Calcular os resíudos standartizados

# Para Tabela 2:
chisq.test(tab2, correct = T)
x2=chisq.test(tab2) # Calcula-se o qui-quadrado em um vetor
x2$observed # Mostrar as frequências observadas
x2$expected # Mostrar os valores esperados
x2$residuals # Mostrar os resíduos (O-E)
x2$stdres # *Calcular os resíudos standartizados

## Resposta: 
## Conclui-se que o câncer intra-epitelial está associado com a idade com 
## que as mulheres começaram a ter relações sexuais (x² = 7,97, df = 2, 
## P = 0,02) e com o alelo DQB1*03 (x² = 13,84, df = 1, P < 0,001). 
## O risco é significativamente maior nas mulheres que iniciam atividade
## sexual antes dos 16 anos (tabela marascuilo), bem como em mulheres que
## apresentam o alelo DBQ1*03 (tabela marascuilo). 

# 65. Doll e Bradford-Hill (1952) realizaram um extenso estudo sobre a etio-
# logia do câncer de pulmão em doentes ingleses. Uma das partes do estudo
# referiu-se à associação entre esse carcionoma e o número de cigarros
# fumados por dia. Cada pacente com câncer havia sido pareado com outra 
# pessoa do mesmo sexo e idade, que estava hospitalizado por outro problema
# que não câncer de tórax, vias aéreas superiores, lábios ou outro órgão
# que pudesse estar relacionado ao habito do fumanete. A tabela a seguir
# apresenta os dados obtidos em homens. Conclua sobre a associação mensiona-
# da (alfa = 0,001).

# - Entrando com os dados:
exe65=matrix(c(55, 489, 475, 293, 38, 129, 570, 431, 154, 12), nrow=2, byrow=5) 
rownames(exe65)=c("câncer","outro")
colnames(exe65)=c("<5 cigarros", "5-14", "15-24", "25-49", "= ou > 50")
exe65

# - Gráfico frequência
barplot(prop.table(exe65)*100,beside=T,legend.text = rownames(exe65), ylab="Frequência %", xlab="Número de cigarros/dia")

# - Qui-quadrado
chisq.test(exe65)
x3=chisq.test(exe65) # Calcula-se o qui-quadrado em um vetor
x3$observed # Mostrar as frequências observadas
x3$expected # Mostrar os valores esperados
x3$residuals # Mostrar os resíduos (O-E)
x3$stdres # *Calcular os resíudos standartizados

# Etapas do cálulo de Qui Quadrado

# Esperado e observado 
exe65
obs=prop.table(exe65,2)
obs
esp=c(55+129/2, 489+570/2, 475+431/2, 293+154/2, 38+12/2)
esp      
esp=matrix(c(esp, esp), nrow=2, byrow=5)
esp
rownames(esp)=c("câncer","outro")
colnames(esp)=c("<5 cigarros", "5-14", "15-24", "25-49", "= ou > 50")
esp

# Desvio
desvio=obs-esp
desvio

d.quad=desvio^2/esp
d.quad

qui2=sum(d.quad)
qui2

# Grau de liberdade
(5-1)*(2-1) # 4

## Cálculo do p-valor do teste de Qui Quadrado
pchisq(q=qui2, df=4,conf.level=0.999, lower.tail=FALSE)

## Construçãoo da curva de distribuiçãoo de qui quadrado
curve(dchisq(x, df=4),-0,10, xlab="Qui-quadrado, 4 g.l.", 
      ylab="Densidade probabilística")

## Sobrepõe uma linha vermelha a partir do valor do Qui-quadrado crítico
curve(dchisq(x, df=4), 5.99, 10, add=T, col="red", lwd=2)

## 66. O "pé-duro" é uma variedade de gado bovino, derivada dos animais 
## trazidos pelos portugueses, que se adaptou às condições difíceis do nor-
## deste brasileiro. Hoje esse grupo está quase extinto, devido ao abate 
## indiscriminado e à substituição e/ou ao cruzamento com outras variedades,
## especialmente o gado zebuíno. Britto & Mello (1999) estudaram a morfolo-
## gia do cromossomo y em uma amostra de touros, mantidos para preservação
## no Piauí. Cromossomos Y submetacêntricos são próprios de gatos europeus
## e y acrocêntricos são tipicos de zebus. Na tabelas a seguir os dados 
## referem-se ao número de inidivíduos em cada categoria. Use-os para:

mat1=matrix(c(21, 17, 30, 7), nrow=2, byrow=2) 
rownames(mat1)=c("1 a 2", "3 ou +")
colnames(mat1)=c("Acrocêntrico", "Sumetacêntrico")
mat1

mat2=matrix(c(13, 8, 24, 12, 14, 4), nrow=3, byrow=2) 
rownames(mat2)=c("nenhuma", "baixa", "alta")
colnames(mat2)=c("Acrocêntrico", "Sumetacêntrico")
mat2

## 66.1 Comparar a distribuição de cromossomos acrocêntricos em touros
## "pé-duro" jovens com a de trouos mais velhos (alfa = 0,05).
barplot(prop.table(mat1)*100,beside=T,legend.text = rownames(mat1), ylab="Frequência %")
chisq.test(mat1, correct=T)
x4=chisq.test(mat1, correct=T) # Calcula-se o qui-quadrado em um vetor
x4$observed # Mostrar as frequências observadas
x4$expected # Mostrar os valores esperados
x4$residuals # Mostrar os resíduos (O-E)
x4$stdres # *Calcular os resíudos standartizados

## Resposta: A distribuição de cromossomos apresenta associação com a idade
## do touro (x² = 4,62, df = 1, P = 0,03). O cromossomo Acrocêntrico tende
## a se tornar mais frequênte conforme o animais fica mais velho, ao contrá
## rio do sumetacêntrico. 

## 66.2 Verificar se a frequência dos dois tipos de cromossomos Y está 
## relacionado com o grau de mistura racial aparente (alfa = 0,05). 
barplot(prop.table(mat2)*100,beside=T,legend.text = rownames(mat2), ylab="Frequência %")
x5=chisq.test(mat2) # Calcula-se o qui-quadrado em um vetor
x5

## Resposta: Não há evidência de associação entre a frequência de 
## cromossomos y e o grau de mistura racial (x²= 1,18, df=2, P=0,55).

## 67. Com o objetivo de detectar uma possível diferença genética entre 
## pessoas que apresentam formas distintas de esquistossomose, foi estu-
## dada uma amostra de 117 pacientes obtidas em catolância, Bahia, onde 
## essa doença é endêmica. Os dados a seguir referem-se aos fenótipos
## de haptoglobina (Hp) encontrados nessas pessoas. Compare as duas formas
## da enfermidade quanto à freuência dos tipo Hp.

exe67=matrix(c(17, 31, 8, 14, 37, 19), nrow=2, byrow=3) 
rownames(exe67)=c("hepa","intest")
colnames(exe67)=c("1-1", "2-1", "2-2")
exe67
barplot(prop.table(exe67)*100,beside=T,legend.text = rownames(exe67), ylab="Frequência %")
x5=chisq.test(exe67) # Calcula-se o qui-quadrado em um vetor
x5

## Resposta: Não há evidência de associação entre o fenótipo de 
## Hp e as distintas formas de esquistossomose (x²= 1,18, df=2, P=0,55).

## Revisão prof. Ana

## Qui-quadrado - Aderência (Post-Hoc: Marascuilo)
##              - Independência (Post-Hoc: Resíduos ajustados)

## Aderência 
dados=c(17,14,13,12)

## - Proporções iguais? SIM!
## - Restrições - Se o n < 25 NÃO FAZ O QUI-QUADRADO!! --> Faz o Teste exato de fisher
sum(dados) # n =56
## - FE > ou = a 5?




library(sos)
???"chi squared"
