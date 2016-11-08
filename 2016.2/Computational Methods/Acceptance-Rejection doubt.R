Q3Correto <- function(n){
  va <- 0
  count <- 0
  while(count < n){
    u <- runif(1)
    x <- runif(1)
    
    if(u <= (x + 1/2)/(3/2)){
      count <- count+1
      va[count] = x
    }
  }
  return(va)
}

hist(Q3Correto(10000))


Q3Estranho <- function(n){
  va <- 0
  count <- 0
  while(count < n){
    u <- runif(1)
    x <- runif(1)
    
    if(u <= 2/3){
      count <- count+1
      va[count] = x
    }
  }
  return(va)
}

hist(Q3Estranho(10000))

