
## ~ Pacotes ~ ##
library(readr)
library(lme4)
library(ggplot2)
library(bbmle)
library(DHARMa)
library(AICcmodavg)
library(RVAideMemoire)
library(MASS)
library(INLA)
library(sjPlot)
library(visreg)

## ~ Overdispersion ~ ##
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)}

## ~ Dados ~ ##
Nro_de_plantas <- read_delim("Nro_de_plantas.csv", 
                             ";", escape_double = FALSE, col_types = cols(`Nro de plantas nuevas` = col_number()), 
                             trim_ws = TRUE)
View(Nro_de_plantas)
dados<-data.frame(Nro_de_plantas)
names(dados)

## ~ Pre-analises visuais ~ ##
c=ggplot(dados,aes(y=Nro.de.plantas.nuevas, x=factor(Marco), fill=factor(Trat)))+geom_boxplot(color="black", fill="white")+xlab("Marco")+ylab("NP")
c
d=ggplot(dados,aes(y=Nro.de.plantas.nuevas, x=factor(Sitio), fill=factor(Trat)))+geom_boxplot(color="black", fill="white")+xlab("Tratamiento")+ylab("NP")
d
e=ggplot(dados,aes(y=Nro.de.plantas.nuevas, x=(Trat)))+geom_boxplot()+xlab("Tratamiento")+ylab("NP")
e
a=ggplot(dados,aes(y=Nro.de.plantas.nuevas, x=factor(Trat), colour=Marco))+geom_point()+xlab("Tratamiento")+ylab("Número de plantas (NP)")
a+facet_grid(dados$Sitio~.)

## ------- Modelos ------- ##

### ====== Modelo de poisson ====== ### 
PM0<-glm(Nro.de.plantas.nuevas~Trat, data=dados, family=poisson(link=log))
PM1<-glmer(Nro.de.plantas.nuevas~Trat+(1|Sitio), family=poisson(link=log), data=dados)
PM2<-glmer(Nro.de.plantas.nuevas~Trat+(1|Sitio/Trat), family=poisson(link=log), data=dados)

tab_model(PM0, PM1, PM2, show.aic=T)

# ~ AIC ~ #
AIC(PM0, PM1,PM2)
BIC(PM0, PM1,PM2)

## ~ dispersão ~ ##
overdisp_fun(PM0)
overdisp_fun(PM1)
overdisp_fun(PM2)
deviance(PM0)/df.residual(PM0)
deviance(PM1)/df.residual(PM1)
deviance(PM2)/df.residual(PM2)

## ~ gráfico ~ ##
plotresid(PM0, shapiro=T)
plotresid(PM1, shapiro=T)
plotresid(PM2, shapiro=T)

visreg(PM0)
visreg(PM1)
visreg(PM2)

### ====== Modelo negative binomial ====== ### 
NBM0<-glm.nb(Nro.de.plantas.nuevas~Trat, data=dados)
NBM1<-glmer.nb(Nro.de.plantas.nuevas~Trat+(1|Sitio), data=dados)
NBM2<-glmer.nb(Nro.de.plantas.nuevas~Trat+(1|Sitio/Trat), data=dados)

tab_model(NBM0, NBM1, NBM2, show.aic=T)

# ~ AIC ~ #
AIC(NBM0, NBM1, NBM2)
BIC(NBM0, NBM1, NBM2)

## ~ dispersão ~ ##
overdisp_fun(NBM0)
overdisp_fun(NBM1)
overdisp_fun(NBM2)
deviance(NBM0)/df.residual(NBM0)
deviance(NBM1)/df.residual(NBM1)
deviance(NBM2)/df.residual(NBM2)

## ~ gráfico ~ ##
plotresid(NBM1, shapiro=T)
plotresid(NBM2, shapiro=T)
plotresid(NBM0, shapiro=T)

visreg(NBM0)
visreg(NBM1)
visreg(NBM2)

## === Análise do modelo == ##

summary(NBM1) # Coeficientes do modelo
confint.merMod(NBM1) #intervalo de conf 95%
plot(NBM1, ylab="Resíduos de Perason", xlab="Preditos") # Graf.resíduos NBM1

# Algumas análises de resíduos
residuo1<-simulateResiduals(fittedModel=NBM1, n=1000)
plotSimulatedResiduals(residuo1)
testUniformity(residuo1)
testDispersion(residuo1)
plotresid(NBM1, shapiro=T)

# Exponencial dos modelos
exp(fixef(NBM1))  

## ==== Gráficos e tabelas extras ==== ##
plot_model(NBM1) # simple plot
plot_model(NBM1, type="re") # random effects
plot_model(NBM1, type="resid")

visreg(NBM1, "Trat", scale="response", partial=T)
visreg(NBM1, "Sitio", scale="response", partial=T)
visreg(NBM1, "Trat")
visreg(NBM1, "Sitio")
visreg(NBM1, "Trat", by="Sitio")
visreg(NBM1, type="contrast")
visreg(NBM1, type="contrast", scale="response")

## == R² == ##

r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

r2.corr.mer(NBM1)

## === Experimentando exercicio 4 === ##

tam<-rep(1:4,5)
tam

a<-data.frame(dados,tam)
a     

tamnb1<-glmer.nb(Nro.de.plantas.nuevas~Trat+(1|Sitio), offset=log(tam), data=a)
tamnb1
summary(tamnb1)
exp(fixef(tamnb1))
overdisp_fun(tamnb1)


## === Exercicio 5 === ##

Mod.bay<-inla(Nro.de.plantas.nuevas~Trat+(1|Sitio), offset = log(tam), family="nbinomial", data=a)
summary(Mod.bay)
               


## ~~ Extras ~~ ##

as.function(NBM1) # Mostrar as funções do comando
coef(NBM1) # Mostra o intercepto
confint(NBM1) # Mostra o conficende intervals
deviance(NBM1) #  Menos duas vezes amaxima prob de log
df.residual(NBM1) # GL dos resíduos
fitted(NBM1) # valores preditos?
fixef(NBM1)# Estimates of the fixed-effects coefficients, β
formula(NBM1) # formula
logLik(NBM1) # logLik
model.frame(NBM1) # Dados
model.matrix(NBM1)# Matriz de dados modelos
ngrps(NBM1) # número de fatores
nobs(NBM1) # Número de observações
plot(NBM1) # gráfico de análise do modelo
predict(NBM1) # preditos
print(NBM1) # Basic printout of mixed-model objects
profile(NBM1) #  Profiled likelihood over various model parameters.
ranef(NBM1) # Conditional modes of the random effects.
refit(NBM1) # A model (re)fitted to a new set of observations of the response variable.
refitML(NBM1) # A model (re)fitted by maximum likelihood.
residuals(NBM1) # Various types of residual values.
sigma(NBM1) # Residual standard deviation.
simulate(NBM1) # Simulated data from a fitted mixed model.
summary(NBM1) # Summary of a mixed model. 
terms(NBM1) # Terms representation of a mixed model.
update(NBM1) # An updated model using a revised formula or other arguments.
VarCorr(NBM1) # Estimated random-effects variances, standard deviations, and correlations.
vcov(NBM1) # Covariance matrix of the fixed-effect estimates.
weights(NBM1) # Prior weights used in model fitting.

########################################################################