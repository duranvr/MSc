#### Exemplo 2 ####

gibsEx2 <- function(x0, NSim = 5000, n, a, b){
  #Inicialização
  t <- 1
  x <- x0
  teta <- rbeta(1, a, b)
  
  while(t < NSim){
    t <- t+1
    x[t] <- rbinom(1, n, teta[t-1])
    teta[t] <- rbeta(1, a + x[t], b + n - x[t])
  }
  return(data.frame(x = x, teta = teta))
}


output <- gibsEx2(x0 = 12, n = 15, a = 3, b = 7)

hist(output$x)
hist(output$teta)

mean(output$teta)

3/(3+7)


acf(output$x)
acf(output$teta)


plot(output$teta, type = "l")


acf(output$teta[seq(1, 5000, by = 5)])

newTeta <- output$teta[seq(1, 5000, by = 5)]

#### Exemplo 1 ####

gibbsEx1 <- function(rho, y0, x0, nSims = 5000){
  #Inicializar
  t <- 1
  x <- x0
  y <- y0
  
  while(t < nSims){
    x[t+1] <- rnorm(1, mean = rho*y[t], sd = sqrt(1-rho^2))
    y[t+1] <- rnorm(1, mean = rho*x[t+1], sd = sqrt(1-rho^2))
    t <- t + 1
  }
  
  return(data.frame(x = x, y = y))
}

output2 <- gibbsEx1(.5, 0, 0, nSims = 20000)

output2 <- output2[seq(1, 20000, by = 3), ]

acf(output2$x)
acf(output2$y)

par(mfrow = c(1, 2))
hist(output2$x)
hist(output2$y)

mean(output2$x); sd(output2$x)
mean(output2$y); sd(output2$y)

cor(output2$x, output2$y)



mean(output2$x[output2$y > 1.5 & output2$y < 1.7])

x <- 3:5
x[3]

A <- matrix(c(1, 2, 3, 4), 2, 2)
A[2, 1]


library(MASS)
output3 <- mvrnorm(6667, c(0, 0), matrix(c(1, 0.5 , 0.5, 1), 2, 2))

par(mfrow = c(2, 2))
hist(output2$x)
hist(output2$y)
hist(output3[,1])
hist(output3[,2])

par(mfrow = c(1, 2))
acf(output3[,1])
acf(output3[,2])

