#### Lista 1 ####

#### Questão 01 ####

#a) 

varGen <- function(n){
  x <- runif(n, 0, 1)
  xCat <- ifelse(x < 1/3, 1, 2)
  out <- mean(xCat == 1)
  return(out)
}

varGen(100)

#b)
varGen(1000)

#c)
varGen(10000)

#### Questão 02 ####


