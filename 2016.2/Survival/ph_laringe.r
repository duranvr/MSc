dados<-read.table("./2016.2/Survival/laringe.txt", header=TRUE)
require(survival)
fit3<-coxph(Surv(dados$tempos,dados$cens)~factor(dados$estagio)+dados$idade,
method="breslow")
summary(fit3)
hist(resid(fit3, type = "lkdsaj"))


fit4 <- survreg(Surv(dados$tempos,dados$cens)~factor(dados$estagio)+dados$idade, dist = "weibull")
summary(fit4)
par(mfrow = c(1, 1))
hist(resid(fit4, type = "dfbetas"))



fit4 <- survreg(Surv(dados$tempos,dados$cens)~factor(dados$estagio)+dados$idade, dist = "weibull")
summary(fit4)
par(mfrow = c(1, 1))
hist(resid(fit4, type = "dfbetas"))


require(flexsurv)

fit5 <- flexsurvreg(Surv(dados$tempos,dados$cens)~factor(dados$estagio)+dados$idade, dist = "gengamma")
summary(fit5)
plot(fit5)
plot(fit5, type = "hazard")
