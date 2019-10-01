## ---------------------------------------
## AMOSTRAS DEPENDENTES (Teste de McNemar)
##  Exemplo: Cefaleia
## ---------------------------------------

sonda = matrix(c(9, 10, 8, 8,9,10),
                  ncol=2,
                  byrow=TRUE)
lines.names =c('1A', '2A','3A')
col.names = c('SF', 'CF')
dimnames(sonda) = list(lines.names,col.names)
sonda

## Como os grupos nao sao independentes, o teste adequado
## sera o de McNemar. 

mcnemar.test(sonda, correct=TRUE)

## Como p-valor resultou significativo pode-se atribuir
## o resultado ao efeito do tratamento.

## ----------------------------------
## TABELAS rxs
## ----------------------------------

## Exemplo: Derrame
casos = matrix(c(16,179,12,70,21,78,12,54),
               ncol=2,
               byrow=TRUE)
lines.name=c("A","B","C","D")
col.names = c('Sim', 'Nao')
dimnames(casos) = list(lines.name,col.names)
casos

##       Com derrame recorrente | Sem derrame recorrente
##Centro   Observado Esperado       Observado Esperado
##  A        16        26,9            179     168,1
##  B        12        11,3             70      70,7
##  C        21        13,7             78      85,3
##  D        12         9,1             54      56,9

## Como nao existe nenhuma amostra com menos de 20 
## elementos nem ha nenhum valor esperado menor do que 5
## (tabela acima), entao o teste quiquadrado, com correcao 
## de Yates sera o adequado.

chisq.test(casos,correct=TRUE)
## Como o teste resultou significativo podemos afirmar
## que a proporcao de casos de derrame depende do
## centro
