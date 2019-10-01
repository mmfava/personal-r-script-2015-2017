

## Aula prática no R - One-way ANOVA e Tukey

# INTRODUÇÃO SOBRE A ANOVA 

# Existem situações nas quais um pesquisador deseja comparar mais do que dois grupos experimentais
# com relação a uma variável quantitativa. Suponha que quatro grupos estão sendo comparados em
# um mesmo experimento. A hipótese nula a ser testada é:

# H0: S²A = S²B = S²C = S²D

# Á primeira vista, pode parecer correto realizar vários testes t entre os grupos, comparando-os
# dois a dois. Neste caso, haveriam seis testes t de comparação entre médias. Tal procedimento, 
# no entanto, é estatisticamente inadequado. O teste t foi delineado para, em um mesmo experimen-
# to, comparar-se uma média A com apenas uma outra, B, com probabilidade α de se conculir, incor-
# retamente, por uma diferença que não existe. Se forem feitas mais de uma comparação envolvendo
# a média A, a probabilidade de um erro deste tipo passa a ser maior do que α. Assim, se H0 é ver-
# dadeira e o nível α de significância é fixado em 0,05, a probabilidade de se concluir por uma
# diferença entre as duas médias mais extremas não é mais 0,05, mas quatro vezes maior. 
# O procedimento correto para se evitar esse aumento no nível global de signíficância do experi-
# mento consiste em utilizar a técnica da Análise de Variância. Este método compara todas as 
# médias em um único teste e visa a identificar a existência de ao menos uma diferença entre 
# os grupos, se alguma existir. Caso o resultado seja estatísticamente significativo, aplica-se
# posteriormente uma das várias técnicas existentes de comparações multiplas entre as médias.
# Estes procedimentos permitem identificar quais grupos diferem entre si, mantendo controlado
# o nível de significância do teste.

# ANÁLISE DA VARIÂNCIA

# A análise da variância (ou ANOVA de 'Analyses of Variance') é uma poderosa técnica estatística
# desenvolvida por R.A Fisher. Ela consiste em um procedimento que decompõe, em vários compone-
# ntes identificáveis, a variação total entre os valores obtidos no experimento. Cada componente
# atribui a variação a uma causa ou fonte de variação diferente; o número de causas de variação 
# ou "fatores" depende do delinemanto da investigação. 

# HIPÓTESES DA ANOVA:

# Pr(>F) < 0,05 = H0: Os grupos apresentam variação/médias de variável X iguais entre sí 
# Pr(>F) > 0,05 = H1: Pelo menos um dos grupos difere quando a variação/médias de variável 

# CONDIÇÕES PARA USO DA ANOVA 

# Para que os resultados da ANOVA sejam válidos, é necessário que as variâncias amostrais 
# sejam semelhantes nas diferentes amostras. Além disso, os resíduos do modelo gerado pela ANOVA
# devem apresentar distribuição normal dos dados. Caso esses pressupostos não sejam aceitos,
# deve-se aplicar o post-teste de kruskall-wallis.

# TESTE ANOVA NO R

# No R são encontrados diversos procedimentos para realizar a ANOVA; entretanto, o usuário deve
# estar atento ao escolher e fazer a análise, pois alguns erros são frequentes, por exemplo, 
# não especificar algum fator ou esquecer sinal no modelo. 
# Hoje faremos uma análise one-way anova pois iremos comparar os grupos em relação a um único 
# fator.

# COMPARAÇÕES MÚLTIPLAS ENTRE MÉDIAS

# Um valor de F significativo na ANOVA não indica quais são os grupos significativamente
# diferentes entre si quando comparados dois a dois; ele apenas mostra que existe ao menos uma
# diferença entre os grupos estudados. A identificação de diferenças particulares entre médias,
# tomando-se duas a duas, deve ser feita usando um dos vários testes de Comparações Múltiplas
# entre Médias existentes hoje na literatura. Esses testes são semelhantes ao t, com a diferença
# de que controlam o nível de significância ao levar em conta o número de compraçãoes feitas 
# no experimento. Além disso, nesta técnica estatística, a variância dentro dos grupos é estima-
# da usando o QM resíduos, que é baseado em todas as amostras, enquanto em um teste t a variân-
# cia dentro dos grupos é estimada com base em duas amostras apenas. 
# Vários procedimentos têm sido propostos para proseguir na análise de dados, controlando a 
# taxa de erro de cada comparação. Na aula de hoje vamos ver o mais utilizado, que é o 
# teste de Tukey. LEMBRANDO que tal procedimento é utilizado quando somente o teste F é estatís-
# ticamente significado. 

# TESTE DE TUKEY

# O procedimento de Tukey é um complemento à ANOVA e visa identificar quais as médias que, toma-
# das duas a duas, diferem significativamente entre sí. O método de Tukey protege os testes de 
# um aumento no nível de significância devido ao grande número de comparações efetuadas. 

## ------------------------------------------------------------------------------------------

# APLICANDO A ANOVA 

# EXEMPLO 1 

# Criando o banco de dados
exercicio = data.frame(
  tratamento = as.factor(rep(1:3,each=6)),
  resposta = c(14,14,13,12,15,13,24,25,19,22,18,22,
               20,22,22,16,15,16))
exercicio

# Separando as variáveis-coluna
attach(exercicio)

# A ANOVA
exercicio1=aov(resposta~tratamento)

summary(exercicio1)
# SOBRE A TAB
# O "residuals" se refere aos resíduos, ou a causa da variação dentro dos tratamentos.
# "Sum Sq" = Soma dos quadrados
# "Mean Sq" = Média dos quadrados (Para achar o F QM entre/QM dentro)

boxplot(resposta~tratamento, ylab = "Resposta",
        xlab="Tratamento")

# validação do modelo

# a - homocedasticidade
bartlett.test(resposta~tratamento) 

# b - normalidade dos resíduos
residuos1=resid(exercicio1)
shapiro.test(residuos1)

# TESTE DE TUKEY
tukey1=TukeyHSD(exercicio1)
tukey1 # resultado do Tukey

plot(tukey1) # resultado gráfico da análise

## 

# EXEMPLO 2

# Banco de dados
melão = data.frame(
  variedade = rep(c("A","B","C","D"),each=6),
  produção = c(25.12,17.25,26.42,16.08,22.15,15.92,40.25,35.25,31.98,36.52,43.32,37.10,
               18.30,22.60,25.90,15.05,11.42,23.68,28.55,28.05,33.20,31.68,30.32,27.58))
melão

# Separando as var-coluna
attach(melão)

# Aplicando a ANOVA
exercicio2=aov(produção~variedade)
summary(exercicio2)

boxplot(produção ~ variedade, ylab = "produção (toneladas)",
        xlab="Variedades de melão")

# validação do modelo

# a - homocedasticidade
bartlett.test(produção ~ variedade) 

# b - normalidade dos resíduos
residuos2=resid(exercicio2)
shapiro.test(residuos2)

# TESTE DE TUKEY
tukey2=TukeyHSD(exercicio2)
tukey2 # resultado do Tukey

plot(tukey2) # resultado gráfico da análise

### --------------------------------------------------------------------------------------

# Exercicio em sala

# Banco de dados
predador = data.frame(lago = rep(c("A","B","C","D"),each=5), densidade = c(15,19,23,25,28,30,27,25,24,23,30,33,34,38,38,67,72,75,76,80))
attach(predador)
predador

# Fazer a anova

# Testar os dois pressupostos

# Interpretar os resultados 

# Caso necessário, realizar o post-test
# (fazer o boxplot para auxíliar na interpretação e resultado do Tukey)
