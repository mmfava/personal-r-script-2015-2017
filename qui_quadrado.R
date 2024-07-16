## ~~ Qui-Quadrado ~~ ##

## var. quanti ~ var. quanti

# A estatística chi-quadrado foi criada por K. Pearson para medir o grau de
# discrepância entre um conjunto de frequências observadas (O) e o conjunto
# de frequências esperadas segundo determinada hipótese (E). 

# Em suma, a fórmula para calcular o chi-quadrado é:
# x² = ∑ (O - E)² / E

## Objetivo do teste chi-quadrado:
# 1 - Verificar se uma distribuição observada de dados ajusta-se a uma distri-
# buição esperada (teórica): O teste se chama teste chi-quadrado de aderência
# ou de ajustamento.
# 2 - Comparar duas ou mais populações com relação a uma variável categórica:
# O teste denomina-se teste chi-quadrado de proporções (ou teste chi-quadrado
# de heterogeneidade entre populações).
# 3 - Verificar se existe associação entre duas variáveis qualitativas: O
# teste é chamado de teste chi-quadrado de associação. 

## -- TESTE DE ADERÊNCIA OU DE AJUSTAMENTO -- ## 

# Exemplo 1: 
# Existe diferença em relação aos gêneros da turma de conservação e manejo?
sexo <- c("feminino" = 20, "masculino" = 12)

# Teste chi-quadrado com correção de Yates
chisq.test(sexo, correct = TRUE)

# Resposta: A distribuição observada não difere da distribuição esperada;
# as diferenças observadas foram casuais.

# Exemplo 2:
# Frequências de antígenos R e S em tecido humano

# Base de Dados
gen_obs <- c("R" = 6, "RS" = 15, "S" = 3)

# Plotar os dados em um gráfico de colunas
barplot(gen_obs, xlab = "Genótipos", ylab = "Frequência", cex.names = 0.7)

# Calcular as frequências esperadas
gen_exp <- c("R" = 24 * 0.25, "RS" = 24 * 0.5, "S" = 24 * 0.25)

# Teste chi-quadrado
chisq.test(gen_obs, p = c(0.25, 0.5, 0.25))

# Resposta: As frequências genotípicas observadas seguem proporções esperadas.

# Exemplo 3:
# Frequência de coleta seletiva entre 5 municípios no oeste paranaense

# Dados:
data <- matrix(c(96, 123, 82, 112, 100, 204, 177, 218, 188, 200), nrow = 2, byrow = TRUE)
rownames(data) <- c("Sim", "Não")
colnames(data) <- c("Cascavel", "Corbélia", "Toledo", "Palotina", "Medianeira")

# Gráfico de barra com a frequência de coleta seletiva
barplot(prop.table(data, 2) * 100, beside = TRUE, legend.text = rownames(data))

# Teste chi-quadrado
chisq.test(data)

# Resposta: A distribuição da frequência observada difere do que seria esperado.

# Teste de Marascuilo (Post-Hoc)
# Preparação dos dados para a análise Marascuilo:
p <- c("Cascavel" = 0.32, "Corbélia" = 0.41, "Toledo" = 0.27, "Palotina" = 0.37, "Medianeira" = 0.33)
N <- length(p)
value <- critical.range <- c()

for (i in 1:(N - 1)) {
  for (j in (i + 1):N) {
    value <- c(value, abs(p[i] - p[j]))
    critical.range <- c(critical.range,
                        sqrt(qchisq(0.95, 4)) * sqrt(p[i] * (1 - p[i]) / 300 + p[j] * (1 - p[j]) / 300))
  }
}

sig <- ifelse(value > critical.range, "Sim", "Não")
marascuilo <- data.frame(prop.diff = round(value, 3), critical.value = round(critical.range, 3), sig)
row.names.temp <- c()
for (i in 1:(N - 1)) {
  for (j in (i + 1):N) {
    row.names.temp <- c(row.names.temp, paste("p", i, "-", "p", j, sep = ""))
  }
}
rownames(marascuilo) <- row.names.temp
marascuilo

# Resposta: Corbélia apresentou proporções significativamente mais elevadas do que Toledo.

## -- TESTE DE ASSOCIAÇÃO OU DE INDEPENDÊNCIA -- ## 

# Exemplo 1:
# Associação entre tipos de bactérias em diferentes pontos de um rio

# Dados:
ponto1 <- c("e-coli" = 23, "salmonella" = 2324, "klebsiela" = 1)
ponto2 <- c("e-coli" = 144, "salmonella" = 125, "klebsiela" = 7)
ponto3 <- c("e-coli" = 10, "salmonella" = 4, "klebsiela" = 120)

# Base de dados
base <- data.frame(ponto1, ponto2, ponto3)
base1.1 <- as.matrix(base)
base1 <- t(base)

# Gráfico de barra com a frequência das bactérias por ponto
barplot(prop.table(base1.1, 2), beside = TRUE, legend.text = rownames(base1.1))

# Teste Chi-quadrado
chisq.test(base1)

# Resposta: Existe associação entre as variáveis em estudo.

## Análise de resíduos - Post Hoc L x C ##
x <- chisq.test(base1)
x$observed
x$expected
x$residuals
x$stdres

# Resposta: Houve diferença estatística significativa entre a distribuição das bactérias nos pontos.

## Lista de Exercícios Qui-Quadrado ##

# Exemplo 60:
# Testar diferenças na frequência de ovoposição de borboletas

# Dados:
data60 <- matrix(c(17, 4, 28, 39, 3, 1, 0, 1, 32), nrow = 9, byrow = TRUE)
colnames(data60) <- c("número de ovos")
rownames(data60) <- c("mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov")

# Gráfico de barras
barplot(prop.table(data60, 2) * 100, beside = TRUE, names = colnames(data60), ylab = "Frequência %")

# Teste Chi-quadrado
chisq.test(data60)

# Resposta: As frequências observadas são diferentes do esperado.

# Exemplo 61:
# Frequência de acidentes de trabalho durante a semana

# Dados:
acidentes <- matrix(c(42, 23, 25, 19, 23, 48), nrow = 1, byrow = TRUE)
colnames(acidentes) <- c("segunda", "terça", "quarta", "quinta", "sexta", "sábado")

# Gráfico de barras
barplot(prop.table(acidentes) * 100, beside = TRUE, names = colnames(acidentes), ylab = "Frequência %")

# Teste Chi-quadrado
chisq.test(acidentes)

# Resposta: A frequência de acidentes observados em cada dia da semana difere do esperado.

# Exemplo 62:
# Associação entre problemas na tireoide e sensibilidade ao PTC

# Dados:
exe62 <- matrix(c(144, 96), nrow = 2, byrow = TRUE)
rownames(exe62) <- c("sim", "não")
colnames(exe62) <- c("tireoide")

# Frequência esperada
esp62 <- c("sim" = 240 * 0.7, "não" = 240 * 0.3)

# Teste Chi-quadrado
chisq.test(exe62, p = c(0.7, 0.3), correct = TRUE)

# Resposta: Não há associação entre problemas na tireoide e sensibilidade ao PTC.

# Exemplo 63:
# Associação entre cor da pelagem e sexo em roedores

# Dados:
exe63 <- matrix(c(22, 13, 15, 16, 17, 17), nrow = 2, byrow = TRUE)
rownames(exe63) <- c("machos", "fêmeas")
colnames(exe63) <- c("preto", "marrom", "manchada")

# Gráfico de barra com a FR% das cores por sexo
barplot(prop.table(exe63, 2), beside = TRUE, legend.text = rownames(exe63))

# Teste Chi-quadrado
chisq.test(exe63)

# Resposta: Não existe associação entre a cor da pelagem e o sexo dos roedores.

# Exemplo 64:
# Associação entre câncer intra-epitelial e início das relações sexuais e alelo DBQ1*03

# Dados:
tab1 <- matrix(c(19, 22, 2, 16, 33, 14), nrow = 2, byrow = TRUE)
rownames(tab1) <- c("casos", "controle")
colnames(tab1) <- c("< 16", "17-20", ">20")

tab2 <- matrix(c(33, 10, 24, 39), nrow = 2, byrow = TRUE)
rownames(tab2) <- c("casos", "controle")
colnames(tab2) <- c("DBQB1", "Outro")

# Gráficos de barra
barplot(prop.table(tab1), beside = TRUE, legend.text = rownames(tab1))
barplot(prop.table(tab2), beside = TRUE, legend.text = rownames(tab2))

# Teste Chi-quadrado
chisq.test(tab1)
chisq.test(tab2, correct = TRUE)

# Resposta: O câncer intra-epitelial está associado com a idade de início das relações sexuais e o alelo DBQ1*03.

# Exemplo 65:
# Associação entre câncer de pulmão e número de cigarros fumados por dia

# Dados:
exe65 <- matrix(c(55, 489, 475, 293, 38, 129, 570, 431, 154, 12), nrow = 2, byrow = TRUE)
rownames(exe65) <- c("câncer", "outro")
colnames(exe65) <- c("<5 cigarros", "5-14", "15-24", "25-49", ">= 50")

# Gráfico de frequência
barplot(prop.table(exe65) * 100, beside = TRUE, legend.text = rownames(exe65), ylab = "Frequência %", xlab = "Número de cigarros/dia")

# Teste Chi-quadrado
chisq.test(exe65)

# Resposta: Existe associação entre o câncer de pulmão e o número de cigarros fumados por dia.

# Exemplo 66:
# Comparar distribuição de cromossomos acrocêntricos em touros "pé-duro"

# Dados:
mat1 <- matrix(c(21, 17, 30, 7), nrow = 2, byrow = TRUE)
rownames(mat1) <- c("1 a 2", "3 ou +")
colnames(mat1) <- c("Acrocêntrico", "Submetacêntrico")

mat2 <- matrix(c(13, 8, 24, 12, 14, 4), nrow = 3, byrow = TRUE)
rownames(mat2) <- c("nenhuma", "baixa", "alta")
colnames(mat2) <- c("Acrocêntrico", "Submetacêntrico")

# Gráficos de barra
barplot(prop.table(mat1) * 100, beside = TRUE, legend.text = rownames(mat1), ylab = "Frequência %")
barplot(prop.table(mat2) * 100, beside = TRUE, legend.text = rownames(mat2), ylab = "Frequência %")

# Teste Chi-quadrado
chisq.test(mat1, correct = TRUE)
chisq.test(mat2)

# Resposta: A distribuição de cromossomos acrocêntricos está associada com a idade do touro.
# Não há associação entre a frequência de cromossomos Y e o grau de mistura racial.

# Exemplo 67:
# Associação entre formas de esquistossomose e fenótipos de haptoglobina

# Dados:
exe67 <- matrix(c(17, 31, 8, 14, 37, 19), nrow = 2, byrow = TRUE)
rownames(exe67) <- c("hepa", "intest")
colnames(exe67) <- c("1-1", "2-1", "2-2")

# Gráfico de frequência
barplot(prop.table(exe67) * 100, beside = TRUE, legend.text = rownames(exe67), ylab = "Frequência %")

# Teste Chi-quadrado
chisq.test(exe67)

# Resposta: Não há evidência de associação entre o fenótipo de Hp e as distintas formas de esquistossomose.
