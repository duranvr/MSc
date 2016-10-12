#### SOB - Lista 1 ####

#### Q11 ####
tempos <- c(12, 16, 16, 18, 19, 20, 28,
            10, 14, 15, 18, 18, 20, 21)
censura <- c(1, 1, 1, 0, 0, 1, 1,
             1, 1, 1, 1, 1, 0, 1)

grupos <- rep(c("A", "B"), each = 7)

require(survival)

ekm<-survfit(Surv(tempos,censura)~grupos,conf.type="plain")
summary(ekm)

plot(ekm, col = c("red", "blue"), conf.int = "both")

#Embora o grupo B esteja sempre abaixo do grupo A, com a construção dos intervalos
# de confiança fica claro que não há separação significativa entre eles.

#### Q12 ####

df <- read.table("2016.2/Survival/oc.txt", sep=" ")

names(df) <- c("idade", "grau", "estagio", "resid", "trata", "tempo", "cens")

df <- df[!df$grau == 9,]

# install.packages("survival")

#a)

ekmAll <- survfit(Surv(df$tempo, df$cens)~1)
summary(ekmAll)
time <- ekmAll$time #Extraindo os tempos
surv <- ekmAll$surv #Extraindo o valor da sobrevivência

table(surv)

table(df$tempo, df$cens)
table(time)

exponen <- survreg(Surv(df$tempo, df$cens)~1, dist = "exponential")
alphaExp <- exp(exponen$coefficients)
survExp <- exp(-time/alphaExp)


weibull <- survreg(Surv(df$tempo, df$cens)~df$idade, dist = "weibull")
summary(weibull)
alphaWei<-exp(weibull$coefficients[1])
gamaWei<-1/weibull$scale
survWei <- exp((-1)*(time/alphaWei)^gamaWei)

par(mfrow = c(1, 2))
plot(surv,survExp,pch=16,ylim=range(c(0.0,1)), xlim=range(c(0,1)), xlab = "S(t): Kaplan-Meier",
     ylab="S(t): exponencial")
lines(c(0,1), c(0,1), type="l", lty=1)

plot(surv,survWei,pch=16,ylim=range(c(0.0,1)), xlim=range(c(0,1)), xlab = "S(t): Kaplan-Meier",
     ylab="S(t): weibull")
lines(c(0,1), c(0,1), type="l", lty=1)


plot(ekmAll)
lines(time, survExp, col = "red")

plot(ekmAll)
lines(time, survWei, col = "purple")

par(mfrow = c(1, 1))
plot(ekmAll, conf.int=F, xlab="Tempos", ylab="S(t)")
lines(c(0,time),c(1,survWei), lty=2,col="red")
lines(c(0,time),c(1,survExp), col="purple", lty=3)

#b

ekmGrau <- survfit(Surv(df$tempo, df$cens)~grau, data = df, conf.type = "plain")
summary(ekmGrau)
plot(ekmGrau, col = 1:3, conf.int = "both")


survdiff(Surv(df$tempo, df$cens)~df$grau, rho = 0)


#c)
#Separando os bancos
table(df$grau)
df1 <- df[df$grau == 1,]
df2 <- df[df$grau == 2,]
df3 <- df[df$grau == 3,]

exp1 <- survreg(Surv(df1$tempo, df1$cens)~1, dist = "exponential")
summary(exp1)
alpha1 <- exp(coef(exp1))

exp2 <- survreg(Surv(df2$tempo, df2$cens)~1, dist = "exponential")
summary(exp2)
alpha2 <- exp(coef(exp2))

exp3 <- survreg(Surv(df3$tempo, df3$cens)~1, dist = "exponential")
alpha3 <- exp(coef(exp3))

r1 <- 1/alpha1
r2 <- 1/alpha2
r3 <- 1/alpha3

