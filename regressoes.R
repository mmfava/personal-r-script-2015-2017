# Estatística Avançada: Regressões

# Marília Melo Favalesso
# mariliabioufpr@gmail.com

# Dados para a regressão
CE <- rep(c("20", "50", "80", "110", "140", "170", "200", "230", "260", "290"), each=3)
CE <- as.numeric(CE)
EPT <- c(84, 81, 79, 66, 62, 68, 41, 45, 47, 35, 38, 36, 27, 25, 24, 18, 15, 22, 21, 20, 15, 17, 19, 20, 14, 13, 17, 14, 16, 15)

# Plotando as variáveis para ver as relações entre elas
plot(EPT ~ CE, main = "Relação entre EPT e CE")
abline(h = mean(EPT), col = "blue")
abline(v = mean(CE), col = "blue")

# Normalidade dos dados
shapiro.test(EPT) # !!
shapiro.test(CE)

# Existe correlação entre as variáveis?
cor.test(EPT, CE) # p<0.001, associação negativa (-0,87) significativa entre EPT e CE

# Regressão linear
linear <- lm(EPT ~ CE)
summary(linear) # ambos os coeficientes são significativos

# Gráficos dos preditores e resíduos
preditos <- predict(linear)
residuos <- resid(linear)

# Normalidade dos resíduos
shapiro.test(residuos)

# Plot do modelo
plot(EPT ~ CE, main = "Modelo de Regressão Linear")
abline(linear, col = "red")
segments(CE, EPT, CE, preditos, col = "red")

# Observando os resíduos em relação à reta do modelo
plot(residuos ~ preditos, ylab = "Resíduos", xlab = "Preditos", main = "Resíduos vs. Preditos")
abline(0, 0, col = "red")

# Transformação logarítmica para linearizar a relação entre as variáveis
EPT_log <- log10(EPT)

# Gráfico entre as variáveis transformadas
plot(EPT_log ~ CE, ylab = "EPT", xlab = "CE", pch = 20, main = "Relação Log-transformada entre EPT e CE")

# Modelo linear com transformação logarítmica
linear2 <- lm(EPT_log ~ CE)
summary(linear2) # resultados do modelo

residuos_log <- resid(linear2) # resíduos
preditos_log <- predict(linear2) # preditos
shapiro.test(residuos_log)  # teste de normalidade para os resíduos

# Gráfico da regressão log-transformada
plot(EPT_log ~ CE, ylab = "EPT", xlab = "CE", pch = 20, main = "Modelo de Regressão Log-transformado")
abline(linear2, col = "red")
segments(CE, EPT_log, CE, preditos_log, col = "red")

# Gráfico para avaliação do modelo
plot(residuos_log ~ preditos_log, ylab = "Resíduos", xlab = "Preditos", main = "Resíduos vs. Preditos (Log-transformado)")
abline(0, 0, col = "red")

# Comparação dos resíduos dos dois modelos
par(mfrow = c(1, 2))
plot(residuos ~ preditos, ylab = "Resíduos", xlab = "Preditos", pch = 20, main = "Resíduos vs. Preditos (Linear)")
abline(0, 0, col = "red")
plot(residuos_log ~ preditos_log, ylab = "Resíduos", xlab = "Preditos", pch = 20, main = "Resíduos vs. Preditos (Log-transformado)")
abline(0, 0, col = "red")
dev.off()

# Modelo polinomial
poli <- lm(EPT ~ poly(CE))
summary(poli)

poli2 <- lm(EPT ~ CE + poly(CE, 2))
summary(poli2)
coefficients(poli2) 

residuos_poli <- resid(poli2)
predict_poli <- predict(poli2)

plot(residuos_poli ~ predict_poli, ylab = "Resíduos", xlab = "Preditos", pch = 20, main = "Resíduos vs. Preditos (Polinomial)")
abline(0, 0, col = "red")

# Melhorar a curva
xpred <- data.frame(CE = seq(min(CE), max(CE), len = 30))
plot(EPT ~ CE, xlab = "CE", ylab = "EPT", main = "Ajuste Polinomial")
lines(xpred$CE, predict(poli2, newdata = xpred), col = "red")

# Análise de Covariância
tempo <- ifelse(CE <= 140, 'antes', 'depois')

# Modelos ANCOVA
dados.Cov <- aov(EPT ~ CE * tempo)
summary(dados.Cov)

dados.trat <- aov(EPT ~ tempo * CE)
summary(dados.trat)

library(car)
Anova(dados.Cov, type = "III")
Anova(dados.trat, type = "III")

lm_antes <- lm(EPT[tempo == "antes"] ~ CE[tempo == "antes"])
lm_depois <- lm(EPT[tempo == "depois"] ~ CE[tempo == "depois"])
res_lm_antes <- resid(lm_antes)
res_lm_depois <- resid(lm_depois)
pred_lm_antes <- predict(lm_antes)
pred_lm_depois <- predict(lm_depois)

plot(EPT ~ CE, pch = 20, main = "Análise de Covariância")
abline(lm_antes, col = "red")
segments(CE[tempo == "antes"], EPT[tempo == "antes"], CE[tempo == "antes"], pred_lm_antes, col = "red")
abline(lm_depois, col = "blue")
segments(CE[tempo == "depois"], EPT[tempo == "depois"], CE[tempo == "depois"], pred_lm_depois, col = "blue")
