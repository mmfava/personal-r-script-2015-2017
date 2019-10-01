

## Estatística avançada
## Anova Fatorial

names(exercicio2)
dados=data.frame(exercicio2)
attach(dados)
colnames(dados)=c("réplica", "período", "mata_ciliar", "Diversidade")
names(dados)

# Exemplo em aula para fatorial
fat2.dic(período, mata_ciliar, Diversidade)
interaction.plot(mata_ciliar,período, Diversidade)
boxplot(Diversidade~período)
boxplot(Diversidade~mata_ciliar)


fat2.dic(período, mata_ciliar, Diversidade,quali=c(T,T),mcomp="tukey",fac.names=c("Grupos","Aplicação"),
         sigT=0.05, sigF=0.05)

interaction.plot(período, mata_ciliar, Diversidade)

aplic1=data.frame(ddply(dados,~período*mata_ciliar,summarise,mean=mean(Diversidade),sd=(sd(Diversidade))))
aplic1

pd <- position_dodge(.3)
período=factor(dados$período)
mata=factor(dados$mata_ciliar)

ggplot(aplic1,aes(x=período,y=mean, colour=mata_ciliar, group=mata_ciliar))+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width =.2, size=0.25,
                colour="black", position= pd)+geom_line(position=pd)+
  geom_point(position=pd, size=1)+ylab("ganho de peso por gramas/dia")

## Outro exercicio
dados2=data.frame(exerc2_arranjo_fatorial)
names(dados2)

logbiomassa=log10(dados2$Biomassa)
logbiomassa
dados3=data.frame(dados2, logbiomassa)
names(dados3)
attach(dados3)

aplic2=data.frame(ddply(dados3,~período*mata_ciliar,summarise,mean2=mean(logbiomassa),sd2=(sd(logbiomassa))))
aplic2

ggplot(aplic2,aes(x=mata_ciliar,y=mean2, colour=período, group=período))+
  geom_errorbar(aes(ymin=mean2-sd2, ymax=mean2+sd2), width =.2, size=0.25,
                colour="black", position= pd)+geom_line(position=pd)+
  geom_point(position=pd, size=1)+ylab("ganho de peso por gramas/dia")


logbiomassa=log10(dados2$Biomassa)
logbiomassa

bartlett.test(dados3$logbiomassa~mata_ciliar) ## ! Não homocedastico
bartlett.test(dados3$logbiomassa~período)

fat2.dic(período, mata_ciliar, logbiomassa,quali=c(T,T),mcomp="tukey",fac.names=c("Período","Mata ciliar"),
         sigT=0.05, sigF=0.05)

dim(dados3)

bloco=c("A","B","C","D","E","A","B","C","D","E","A","B","C","D","E","A","B","C","D","E","A","B","C","D","E","A","B","C","D","E")
length(bloco)

dados3=data.frame(dados3, bloco)
dados3
colnames(dados3)=c("réplica", "período", "mata_cilicar", "H", "biomassa", "logbiom","bloco")
names(dados3)
attach(dados3)

fat2.dbc(período, mata_ciliar, bloco, logbiom, quali=c(T,T), mcomp="tukey", fac.names=c("Período","Mata ciliar"))
bartlett.test(logbiom~período)
bartlett.test(logbiom~mata_ciliar)

aplic3=data.frame(ddply(dados3,~período*mata_ciliar+bloco,summarise,mean3=mean(logbiomassa),sd3=(sd(logbiomassa))))
aplic3

## -- ANOVA PARA MEDIDAS REPETIDAS -- ##
## Como medida repetita!
## O período seria uma medida repetida - mesma unidade em duas estações
## o fator sera a mata ciliar

names(dados3)

anovarepetidas<- aov(logbiom ~ mata_ciliar * período * Error(réplica/logbiom)) 
summary(anovarepetidas)

l=lme(logbiom ~ mata_ciliar*período,random=~1|réplica/logbiom) 
summary(l)
