#### Jackknife ####

canivete <- function(x, d = 1, func = "mean"){
  # x <- rnorm(10, 4, 5)
  # d <- 2; func <- "mean"
  
  n <- length(x)
  est <- do.call(func, list(x))
  
  indexes <- combn(n, n-d)
  
  nIters <- ncol(indexes)
  
  estJ <- numeric(nIters)
  for(i in 1:nIters){
    estJ[i] <- do.call(func, list(x[indexes[,i]]))
  }
  
  out.estJ <- mean(estJ)
  out.SEestJ <- sqrt(((n-d)/(d*nIters))*sum((estJ-out.estJ)^2))
  out.vies <- (n-1)*(out.estJ - est)
  
  
  if(func == "mean"){
    return(c(Estimativa = out.estJ, ErroPadrao = out.SEestJ, vies = out.vies,
             teorico = sd(x)/sqrt(n)))
  } else {
    return(c(Estimativa = out.estJ, ErroPadrao = out.SEestJ, vies = out.vies))
  }
  
}

set.seed(1)
N <- 15
X <- rnorm(N, 10, 2)
canivete(X, d = 1, func = "mean")


myVar <- function(x){
  sum((x-mean(x))^2)/length(x)
}

canivete(X, d = 1, func = "myVar")
#Teórico
-(1/N)*var(X)

X <- rpois(N, 1)
canivete(X, d = 1, func = "median")
