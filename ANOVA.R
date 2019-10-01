##
##  ANOVA (An?lise de Vari?ncia)
##  
## --------------------------------------

## A an?lise de vari?ncia ? um teste de hipoteses para modelos lineares

## Em um modelo, constru?mos uma express?o na qual elementos (vari?veis
## e constantes) determinam as causas de varia??o de uma vari?vel.

## PA <-- sexo + idade + etnia + sedentarismo + elast?ncia dos vasos + ignorados

## A hip?tese nula em um modelo de an?lise de vari?ncia e dada por:
## H0: "A m?dia de cada vari?vel tratamento ? igual as demais"

## Hipotese alternativa ser? a seguinte:
## Ha: "Pelo menos uma das medias eh diferente das demais"

## Assim, a ANOVA compara a varia??o devida aos tratamentos com
## a varia??o devido ao res?duo, por isso resulta em um teste
## F-Snedecor, semelhante a compara??o de vari?ncias. O teste
## ainda requer que as vari?veis dos tratamentos tenham distri-
## bui??o normal e que as vari?ncias sejam homog?neas.

## ANOVA
## -----------------------------------------------------
## F.V.          G.L.   S.Q.      Q.M          F.
## -----------------------------------------------------
## Tratamentos    k-1   SQt   QMt=SQt/(k-1)   QMt/QMd
## Desconhecido   n-k   SQd   QMd=SQd/(n-k)
## -----------------------------------------------------
## Total          n-1
# ------------------------------------------------------

##########################################################################################################

## EXEMPLO

## Entrada de dados
## --------------------------------------------------------
exercicio = data.frame(
  treatment = rep(1:3,each=6),
  response = c(14,14,13,12,15,13,24,25,19,22,18,22,
               20,22,22,16,15,16))


## Escrevendo o modelo
exercicio.model = lm(formula = response ~ treatment, 
                     data = exercicio)

exercicio.model

## Visualizacao grafica dos tratamentos
boxplot(formula=response~treatment,data=exercicio)

## Valida??o do modelo
##
## A especifica??o do modelo estabelece que os res?duos
## (erro ou termo desconhecido) tenham vari?ncias 
## constantes segundo os tratamentos e distribui??o 
## normal. A valida??o do modelo ? essencial para se 
## proceder a an?lise.
##
## Com o modelo ajustado (coeficientes) pode ser utilizado
## para realizar predi??es, assim, os valores preditos pelo\
## modelo sao dados por

preditos = fitted(object = exercicio.model)
preditos

## Os res?duos (erros) s?o obtidos pela difenca entre
## o valor obtido e o valor predito (estimado) pelo modelo

residuos = resid(object = exercicio.model)
residuos

## ? importanmte fazer um  gr?fico que ilustre a dispers?o 
## entre os valores reais (medidos) e os valores preditos 
## (estimados). Numa situa??o ideal, a quantidade de pontes 
## deve ser uniformemente distribuidos nos quadrantes formados 
## pela m?dias, ou seja, a dispers?o n?o pode sugerir algum 
## tipo de padrao ou de associa??o.

plot(x = preditos, y = residuos,
     pch = 20,
     col = "black")

abline(h=mean(residuos), col = "azure4")
abline(v = mean(preditos))

## A aus?ncia de padr?es sugere a homogeneidade das vari?ncias

## Teste de homogeneidade de vari?ncias segundo grupos
## (Teste de Levene)

library(car)

## O teste de Levene testa se todas as vari?ncias s?o
## homog?neas

## Ho: Todas as vari?ncias s?o iguais
## Ha: Pelo menos uma das vari?ncias diverge das demais

## Se p-valor for menor do que 0.05 (5%) o teste
## sera significativo (Rejeita-se Ho)

leveneTest(residuos,
           group=as.factor(exercicio$treatment))

## Como p-valor = 0.02938 foi menor do que 0.05, o teste
## foi significativo para a heterogeneidade (se rejeita
## Ho).
##

## Teste para a normalidade dos residuos
shapiro.test(residuos)

## Como p-valor foi maior do que 0.05 entao os residuos
##  tem distribuicao normal, logo a ANOVA poderia
## ser empregada, caso o primeiro pressuporto de 
## homogeneidade das vari?ncias fosse aceito!!!


## Ent?o, quais s?o os crit?rios a serem avaliados?
## - Homogeneidade da vari?ncia dos res?duos (Teste de Levene)
## - Distribui??o normal dos res?duos (shapiro-wilk)

## No nosso caso, o que podemos concluir?

## Mas caso fosse poss?vel, como far?amos a ANOVA?

anova(exercicio.model)


