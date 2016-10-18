areaCirc <- function(r=1, n = 100000, alpha = .05){
  u1 <- runif(n, -r, r)
  u2 <- runif(n, -r, r)
  
  p <- mean((u1^2 + u2^2) < sqrt(r))
  A <- (2*r)^2
  out <- round(p*A, 3)
  
  IC <- c(L = out+(qnorm(alpha/2))*A*sqrt((p*(1-p))/(n)),
                  U = out-(qnorm(alpha/2))*A*sqrt((p*(1-p))/(n)))
  IC <- round(IC, digits = 3)
  
  return(cat("A área de um círculo de raio", r,
               "é aproximadamente igual a:", out, ". O limite inferior do intervalo de confiança de nível", 
               100-(alpha*100),"% é dado por:", IC[1], "e o limite superior por:", IC[2]))
}

areaCirc(r = 2, n=1000, alpha = .05)
