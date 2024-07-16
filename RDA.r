## Universidade Estadual do Oeste do Paraná (UNIOESTE), Campus Cascavel
## Disciplina: Análise Multivariada
## Profa. Luciana Pagliosa Carvalho Guedes
## Alunas: Gabriela Medeiros e Marília Melo Favalesso

## -- ANÁLISE DE REDUNDÂNCIA (RDA) -- ##

# Conceitualmente, a RDA é uma técnica multivariada que realiza uma regressão linear
# entre duas matrizes (resposta Y ~ preditora X), seguida de uma análise de componentes
# principais com a tabela de valores ajustados. 
# Ela é uma importante ferramenta dentro da ecologia, ajudando em análises de composição
# de comunidades.

# Pacotes necessários para realização da análise
library(vegan) # O comando utilizado para a análise de redundância está contido no pacote vegan.

# Carregando os dados
# Dados das espécies
sp <- read.table('clipboard', header = TRUE, row.names = 1, dec = ",")
head(sp)

# Dados abióticos
abio <- read.table('clipboard', header = TRUE, row.names = 1, dec = ",")
head(abio)

# Transformação nos dados
# Usamos o comando decostand, também do pacote vegan.
# Fazemos a transformação de Hellinger - transformação padrão da RDA.
spe.hel <- decostand(sp, "hellinger")

# Aplicação da RDA
# Para realização da RDA usamos o comando rda() do pacote vegan
spe.rda <- rda(spe.hel ~ ., abio) # Usando a matriz transformada

# Resultado da RDA
summary(spe.rda)

# Coeficiente de Determinação
# Existem dois meios de se calcular o R² e o R² ajustado

# Primeiro:
R2 <- RsquareAdj(spe.rda)$r.squared
R2adj <- RsquareAdj(spe.rda)$adj.r.squared

# Segundo:
RsquareAdj(spe.rda)

# Também podemos fazer a RDA incluindo as variáveis abióticas escritas como em
# um modelo de regressão. Esse modo de descrição de modelo é idealizado para
# aplicação de post-testes de seleção de variáveis.
attach(abio)
names(abio)
spe.rda <- rda(spe.hel ~ vaz + no3 + no2 + nh4 + nid + pt + st + prec + temp + zmax + ph + od + cond)

# Índice VIF: Fator de Inflação da Variância
# Ele faz a seleção das variáveis abióticas - tem que ser menor que 10 para saber se não tem colinearidade entre as variáveis
vif.cca(spe.rda)

# Plotando a RDA em formato gráfico
# A maneira mais simples de representar uma RDA com o pacote vegan
# é com o comando plot seguido do nome da RDA.
plot(spe.rda)

# Customizando o gráfico:
colvec <- c("orange", "green4") # Escolhe as cores que vai usar
plot(spe.rda) # Plota o gráfico
with(abio, points(spe.rda, display = "sites", cex = 1, col = colvec, pch = 21, bg = colvec)) # Adiciona os pontos no estilo e cor escolhidos

# Testando a significância da RDA
# Também é possível testar a significância da RDA, dos eixos da RDA e 
# das variáveis utilizadas.
anova.cca(spe.rda) # Testa a significância da RDA

anova.cca(spe.rda, by = 'axis') # Testa a significância dos eixos

anova.cca(spe.rda, by = 'terms') # Testa a significância das variáveis

# Salvando os escores em .txt para abrir e editar no Excel ou no Statistica posteriormente.
scores <- spe.rda$CCA
write.table(scores$wa, 'scoreslocais.txt', sep = '\t')
write.table(scores$biplot, 'biplot.txt', sep = '\t')
write.table(scores$v, 'especies.txt', sep = '\t')
