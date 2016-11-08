#Atividade 2: dataset III

options(scipen = 999)

times <- c(0.20, 1.00, 1.00, 1.00, 1.00, 1.00, 2.00, 3.00, 6.00, 7.00, 11.00, 12.00,
           18.00, 18.00, 18.00, 18.00, 18.00, 21.00, 32.00, 36.00, 40.00, 45.00, 46.00,
           47.00, 50.00, 55.00, 60.00, 63.00, 63.00, 67.00, 67.00, 67.00, 67.00, 72.00,
           75.00, 79.00, 82.00, 82.00, 83.00, 84.00, 84.00, 84.00, 85.00, 85.00, 85.00,
           85.00, 85.00, 86.00, 86.00)

cens <- rep(1, length(times))
library(survival)
library(flexsurv)

hist(times)
# pdf("./2016.2/Survival/differentDists.pdf")
par(mfrow = c(2, 3))
#Exponential
plot(flexsurvreg(Surv(times, cens)~1, dist = "exponential"), type = "survival",
     main = "Survival", ylab = "Exponential")
plot(flexsurvreg(Surv(times, cens)~1, dist = "exponential"), type = "hazard",
     main = "Hazard function")
plot(flexsurvreg(Surv(times, cens)~1, dist = "exponential"), type = "cumhaz",
     main = "Cummulative Hazard")

#Weibull
plot(flexsurvreg(Surv(times, cens)~1, dist = "weibull"), type = "survival",
     main = "Survival", ylab = "Weibull")
plot(flexsurvreg(Surv(times, cens)~1, dist = "weibull"), type = "hazard",
     main = "Hazard function")
plot(flexsurvreg(Surv(times, cens)~1, dist = "weibull"), type = "cumhaz",
     main = "Cummulative Hazard")

#Log-Normal
plot(flexsurvreg(Surv(times, cens)~1, dist = "lnorm"), type = "survival",
     main = "Survival", ylab = "Log-Normal")
plot(flexsurvreg(Surv(times, cens)~1, dist = "lnorm"), type = "hazard",
     main = "Hazard function")
plot(flexsurvreg(Surv(times, cens)~1, dist = "lnorm"), type = "cumhaz",
     main = "Cummulative Hazard")

#Log Logística
plot(flexsurvreg(Surv(times, cens)~1, dist = "llogis"), type = "survival",
     main = "Survival", ylab = "Log-Logística")
plot(flexsurvreg(Surv(times, cens)~1, dist = "llogis"), type = "hazard",
     main = "Hazard function")
plot(flexsurvreg(Surv(times, cens)~1, dist = "llogis"), type = "cumhaz",
     main = "Cummulative Hazard")

#Gompertz
plot(flexsurvreg(Surv(times, cens)~1, dist = "gompertz"), type = "survival",
     main = "Survival", ylab = "Gompertz")
plot(flexsurvreg(Surv(times, cens)~1, dist = "gompertz"), type = "hazard",
     main = "Hazard function")
plot(flexsurvreg(Surv(times, cens)~1, dist = "gompertz"), type = "cumhaz",
     main = "Cummulative Hazard")


#Gamma
plot(flexsurvreg(Surv(times, cens)~1, dist = "gamma"), type = "survival",
     main = "Survival", ylab = "Gamma")
plot(flexsurvreg(Surv(times, cens)~1, dist = "gamma"), type = "hazard",
     main = "Hazard function")
plot(flexsurvreg(Surv(times, cens)~1, dist = "gamma"), type = "cumhaz",
     main = "Cummulative Hazard")

#Generalized Gamma
plot(flexsurvreg(Surv(times, cens)~1, dist = "gengamma"), type = "survival",
     main = "Survival", ylab = "Generalized Gamma")
plot(flexsurvreg(Surv(times, cens)~1, dist = "gengamma"), type = "hazard",
     main = "Hazard function")
plot(flexsurvreg(Surv(times, cens)~1, dist = "gengamma"), type = "cumhaz",
     main = "Cummulative Hazard")


dev.off()

#Testing a custom function

LAddWeibull <- function(theta){
    
  # a <- abs(theta[1])
  # b <- abs(theta[2])
  # c <- abs(theta[3])
  # d <- abs(theta[4])
  
  a <- theta[1]
  b <- theta[2]
  c <- theta[3]
  d <- theta[4]
  
  
  f <- exp(-(t/a)^b -(t/c)^d)*(b*(a^-b)*(t/a)^(b-1) + d*(c^-d)*(t/c)^(d-1))
  
  out <- -sum(log(f))
  return(out)
}

t <- times


LAddWeibull(runif(4, 0, 10))

addWeiPars <- optim(par = runif(4), fn = LAddWeibull, method = "SANN",
                    # lower = c(0, 0, 0, 0),
                    # upper = c(10, 10, 10, 10),
                    control = list(maxit = 50000))
addWeiPars$par

survAddWei <- function(t, theta){
  
  a <- abs(theta[1])
  b <- abs(theta[2])
  c <- abs(theta[3])
  d <- abs(theta[4])
  
  out <- exp(-(t/a)^b-(t/c)^d)
  return(out)
}

ekm <- survfit(Surv(times, cens)~1)
plot(ekm)
lines(t, survAddWei(t, theta = addWeiPars$par), col = "red")
