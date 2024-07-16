# Estatística Avançada: Anova Fatorial

# Carregando e preparando os dados
dados <- data.frame(exercicio2)
colnames(dados) <- c("réplica", "período", "mata_ciliar", "Diversidade")
attach(dados)

# Visualização e análise inicial
fat2.dic(período, mata_ciliar, Diversidade)
interaction.plot(mata_ciliar, período, Diversidade)
boxplot(Diversidade ~ período)
boxplot(Diversidade ~ mata_ciliar)

# ANOVA Fatorial com teste de comparações múltiplas de Tukey
fat2.dic(período, mata_ciliar, Diversidade, quali = c(TRUE, TRUE), mcomp = "tukey",
         fac.names = c("Grupos", "Aplicação"), sigT = 0.05, sigF = 0.05)

interaction.plot(período, mata_ciliar, Diversidade)

# Resumo dos dados por período e mata ciliar
library(plyr)
aplic1 <- ddply(dados, ~período * mata_ciliar, summarise,
                mean = mean(Diversidade), sd = sd(Diversidade))

# Gráfico com erro padrão
library(ggplot2)
pd <- position_dodge(0.3)
ggplot(aplic1, aes(x = período, y = mean, colour = mata_ciliar, group = mata_ciliar)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, size = 0.25,
                colour = "black", position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 1) +
  ylab("Ganho de peso por gramas/dia")

# Segundo exercício
dados2 <- data.frame(exerc2_arranjo_fatorial)
logbiomassa <- log10(dados2$Biomassa)
dados3 <- data.frame(dados2, logbiomassa = logbiomassa)
attach(dados3)

# Resumo dos dados log-transformados por período e mata ciliar
aplic2 <- ddply(dados3, ~período * mata_ciliar, summarise,
                mean2 = mean(logbiomassa), sd2 = sd(logbiomassa))

# Gráfico com erro padrão
ggplot(aplic2, aes(x = mata_ciliar, y = mean2, colour = período, group = período)) +
  geom_errorbar(aes(ymin = mean2 - sd2, ymax = mean2 + sd2), width = 0.2, size = 0.25,
                colour = "black", position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 1) +
  ylab("Ganho de peso por gramas/dia")

# Teste de Bartlett para homocedasticidade
bartlett.test(logbiomassa ~ mata_ciliar) # Não homocedástico
bartlett.test(logbiomassa ~ período)

# ANOVA Fatorial com transformação logarítmica
fat2.dic(período, mata_ciliar, logbiomassa, quali = c(TRUE, TRUE), mcomp = "tukey",
         fac.names = c("Período", "Mata ciliar"), sigT = 0.05, sigF = 0.05)

# Adicionando blocos e ANOVA com blocos
bloco <- rep(LETTERS[1:5], length.out = nrow(dados3))
dados3 <- data.frame(dados3, bloco = bloco)
colnames(dados3) <- c("réplica", "período", "mata_ciliar", "H", "biomassa", "logbiom", "bloco")
attach(dados3)

fat2.dbc(período, mata_ciliar, bloco, logbiom, quali = c(TRUE, TRUE), mcomp = "tukey",
         fac.names = c("Período", "Mata ciliar"))
bartlett.test(logbiom ~ período)
bartlett.test(logbiom ~ mata_ciliar)

# Resumo dos dados por período, mata ciliar e bloco
aplic3 <- ddply(dados3, ~período * mata_ciliar + bloco, summarise,
                mean3 = mean(logbiomassa), sd3 = sd(logbiomassa))

# ANOVA para medidas repetidas
anovarepetidas <- aov(logbiom ~ mata_ciliar * período + Error(réplica / logbiom))
summary(anovarepetidas)

# Modelo linear misto para medidas repetidas
library(nlme)
l <- lme(logbiom ~ mata_ciliar * período, random = ~1 | réplica / logbiom)
summary(l)
