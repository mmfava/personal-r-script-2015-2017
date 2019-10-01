atividade=read.csv2(file.choose(),h=T,dec=",",sep=";")

require(MASS)         ## for oats data set
require(nlme)         ## for lme()
require(multcomp)  ## for multiple comparison stuff

AVD.aov <- aov(AVD ~ GRUPO + TEMPO + Error(ID/GRUPO), data=atividade)
Lme.AVD <- lme(AVD ~ GRUPO + TEMPO, random = ~1 | ID/GRUPO, data = atividade)

# testar os pressupostos estatísticos de normalidade e homocedasticidade
# se os dados estiverem em normalidade e homocedasticidade, continuar a fazer a anova para medidas repetidas e o post-hoc test
# se os dados estiverem apenas em normalidade, continuar a fazer a anova para medidas repetidas e o post-hoc test
# se os dados não estiverem em normalidade e homocedasticidade, fazer o teste de Friedman

attach(atividade)
residuos=resid(Lme.AVD)
shapiro.test(residuos)
bartlett.test(AVD[TEMPO=="T0"],GRUPO[TEMPO=="T0"])
bartlett.test(AVD[TEMPO=="T1"],GRUPO[TEMPO=="T1"])
bartlett.test(AVD[TEMPO=="T2"],GRUPO[TEMPO=="T2"])

summary(AVD.aov)
anova(Lme.AVD)

summary(Lme.AVD)
summary(glht(Lme.AVD, linfct=mcp(TEMPO="Tukey")))

attach(atividade)

interaction.plot(TEMPO,GRUPO,AVD)