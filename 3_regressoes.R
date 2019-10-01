

## Estatística avançada
## Regressões

# Dados para a regressão
CE=rep(c("20", "50", "80", "110", "140", "170", "200", "230", "260", "290"), each=3)
CE
CE=as.numeric(CE)

EPT=c(84,81,79,66,62,68,41,45,47,35,38,36,27,25,24,18,15,22,21,20,15,17,19,20,14,13,17,14,16,15)

# Plotanto as variáveis para ver as relações entre elas
plot(EPT~CE)
abline(h=mean(EPT),col="blue")
abline(v=mean(CE),col="blue")

# Normalidade dos dados
shapiro.test(EPT) # !!
shapiro.test(CE)

# Existe correlação entre as variáveis?
cor.test(EPT, CE) # p<0.001, logo existe associação negativa (-0,87) e significativa entre EPT e CE

# Regressão linear
linear=lm(EPT~CE)
summary(linear) # ambos os coeficientes são significativos
linear

# Gráficos dos preditores e resíduos
preditos=predict(linear)
residuos=resid(linear)

# Normalidade dos resíduos!
shapiro.test(residuos)

# plot do modelo
plot(EPT~CE)
abline(linear, col="red")
segments(CE,EPT,CE,preditos, col="red")

# Observando os resíduos em relação a reta do modelo
plot(residuos~preditos, ylab="Resíduos", xlab="Preditos")
abline(0,0, col="red")
## ! Vemos como que é o comportamento real dos pontos
## ! Como o modelo mostra que o crescimento de x é logaritimico, vamos logaritimizar tmb o y
## ! para conseguir linearizar a relação entre as variáveis.

# Passando para log
EPT_log=log10(EPT)

# Gráfico entre as variáveis
plot(EPT_log~CE, ylab="EPT", xlab="CE", pch=20)

# Modelo
linear2=lm(EPT_log~CE) # modelo
summary(linear2) # resultados do modelo
residuos_log=residuals(linear2) # retirando os resíduos
preditos_log=predict(linear2) # retirando os preditos
shapiro.test(residuos_log)  # teste de normalidade para os resíduos

# Gráfico da regressão
plot(EPT_log~CE, ylab="EPT", xlab="CE", pch=20)
abline(linear2, col="red")
segments(CE,EPT_log,CE,preditos_log, col="red")

# Gráfico para avaliação do modelo
plot(residuos_log~preditos_log, ylab="Resíduos", xlab="Preditos")
abline(0,0, col="red")

## Colocar os gráficos um do lado do outro para comparação
par(mfrow = c(1, 2))
plot(residuos~preditos, ylab="Resíduos", xlab="Preditos", pch=20)
abline(0,0, col="red")
plot(residuos_log~preditos_log, ylab="Resíduos", xlab="Preditos", pch=20)
abline(0,0, col="red")
dev.off()

## Fazer o modelo polinomial
## Pq os resíduos não estavam bem ajustados!
poli=lm(EPT~poly(CE))
poli
summary(poli)

poli2=lm(EPT~CE+poly(CE,2))
summary(poli2)
coefficients(poli2) 

resi_poli=resid(poli2)
predict_poli=predict(poli2)

plot(resi_poli~predict_poli, ylab="Resíduos", xlab="Preditos", pch=20)
abline(0,0, col="red")

## Melhorar a curva!!
xpred <- data.frame(x = seq(min(CE), max(CE),len=30)) 
xpred

plot(EPT~CE, xlab="CE", ylab="EPT")
lines(xpred$x, predict(poli2, newdata=xpred), col="red") ## ajuste/predicao com erro numérico 

## Análise de covariância
tempo=ifelse(CE<=140,'antes','depois')
tempo

dados.Cov<???aov(EPT ~ CE * tempo)
summary(dados.Cov)
coef(dados.Cov)

dados.trat<???aov(EPT ~ tempo * CE)
summary(dados.trat)

require(car)
Anova(dados.Cov, type = "III")
# ou
Anova(dados.trat, type = "III")

lm_antes=lm(EPT[tempo=="antes"]~CE[tempo=="antes"])
lm_depois=lm(EPT[tempo=="depois"]~CE[tempo=="depois"])
res_lm_antes=resid(lm_antes)
res_lm_depois=resid(lm_depois)
pred_lm_antes=predict(lm_antes)
pred_lm_depois=predict(lm_depois)

plot(EPT~CE, pch=20)
abline(lm_antes, col="red")
segments(CE[tempo=="antes"],EPT[tempo=="antes"],CE[tempo=="antes"],pred_lm_antes, col="red")
abline(lm_depois, col="blue")
segments(CE[tempo=="depois"],EPT[tempo=="depois"],CE[tempo=="depois"],pred_lm_depois, col="blue")

