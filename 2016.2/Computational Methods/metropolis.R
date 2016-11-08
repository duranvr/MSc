options(scipen = 999)
pi <- function(x){
  exp((-x^2)/2)
}

q <- function(x, y, delta){
  x + runif(1, -delta, delta)
}

metropolis1 <- function(delta, x0, pi, q, burn, n){
  #1) Etapa de inicialização
  t <- 1
  x <- x0
  if(pi(x) <= 0){
    return("Inicie com outro valor")
  }
  count <- 0
  while(t < burn + n){
    t <- t+1
    #2)
    y <- q(x[t-1], y, delta)
    #3)
    
    if(runif(1) <= min(c(1, pi(y)/pi(x[t-1])))){
      x[t] <- y
      count <- count+1
    } else {
      x[t] <- x[t-1]
    }
    
  }
  return(list(x= x, acceptRate = count/(burn + n)))
}


X.1 <- metropolis1(delta = .1, x0 = 0, pi = pi, 
                   q = q, burn = 5000, n = 500)
X.5 <- metropolis1(delta = .5, x0 = 0, pi = pi, 
                   q = q, burn = 5000, n = 500)
X1 <- metropolis1(delta = 1, x0 = 0, pi = pi, 
                   q = q, burn = 5000, n = 500)
X3 <- metropolis1(delta = 3, x0 = 0, pi = pi, 
                  q = q, burn = 5000, n = 500)


X10 <- metropolis1(delta = 10, x0 = 0, pi = pi, 
                   q = q, burn = 5000, n = 500)

X.1$acceptRate
X.5$acceptRate
X1$acceptRate
X3$acceptRate
X10$acceptRate

par(mfrow = c(1, 3))
hist(X.1$x[5000:5500], xlim = c(-4, 4), freq = FALSE)
lines(density(rnorm(10000)))
hist(X1$x[5000:5500], xlim = c(-4, 4), freq = FALSE)
lines(density(rnorm(10000)))
hist(X3$x[5000:5500], xlim = c(-4, 4), freq = FALSE)
lines(density(rnorm(10000)))

par(mfrow = c(1, 3))
plot(X.1, type = "l")
plot(X1, type = "l")
plot(X10, type = "l")

par(mfrow = c(2, 3))
acf(X.1$x[5000:5500])
acf(X.5$x[5000:5500])
acf(X1$x[5000:5500])
acf(X3$x[5000:5500])
acf(X10$x[5000:5500])

ks.test(X.1$x[5000:5500], "pnorm")
ks.test(X.5$x[5000:5500], "pnorm")
ks.test(X1$x[5000:5500], "pnorm")
ks.test(X3$x[5000:5500], "pnorm")
ks.test(X10$x[5000:5500], "pnorm")


par(mfrow = c(2, 3))
hist(X.1$x[5000:5500][seq(0, 500, by = 5)], xlim = c(-4, 4), freq = FALSE)
lines(density(rnorm(10000)))
hist(X.5$x[5000:5500][seq(0, 500, by = 5)], xlim = c(-4, 4), freq = FALSE)
lines(density(rnorm(10000)))
hist(X1$x[5000:5500][seq(0, 500, by = 5)], xlim = c(-4, 4), freq = FALSE)
lines(density(rnorm(10000)))
hist(X3$x[5000:5500][seq(0, 500, by = 5)], xlim = c(-4, 4), freq = FALSE)
lines(density(rnorm(10000)))
hist(X10$x[5000:5500][seq(0, 500, by = 5)], xlim = c(-4, 4), freq = FALSE)
lines(density(rnorm(10000)))

ks.test(X.1$x[5000:5500][seq(0, 500, by = 5)], "pnorm")
ks.test(X.5$x[5000:5500][seq(0, 500, by = 5)], "pnorm")
ks.test(X1$x[5000:5500][seq(0, 500, by = 5)], "pnorm")
ks.test(X3$x[5000:5500][seq(0, 500, by = 5)], "pnorm")
ks.test(X10$x[5000:5500][seq(0, 500, by = 5)], "pnorm")

acf(X.1$x[5000:5500][seq(0, 500, by = 5)])
acf(X.5$x[5000:5500][seq(0, 500, by = 5)])
acf(X1$x[5000:5500][seq(0, 500, by = 5)])
acf(X3$x[5000:5500][seq(0, 500, by = 5)])
acf(X10$x[5000:5500][seq(0, 500, by = 5)])
