#1st Question
Q1 <- function(n){
  #Gerando n observações de uma uniforme
  u <- runif(n)
  
  #Dando valores a x, condicionado aos valores em u
  x <- ifelse(u < 1/3, 1, 2)
  
  #Calculando as proporções de 1s
  props <- mean(x == 1)
  return(list(valores = x, propDe1 = props))
}

Q1(100)$propDe1

Q1(1000)$propDe1

Q1(10000)$propDe1

#Modificação
Q1.1 <- function(n, k = 1, seed = 1){
  #Gerando n observações de uma uniforme
  set.seed(seed)
  u <- runif(n)
  
  #Dando valores a x, condicionado aos valores em u
  x <- ifelse(u < 1/2, 1, ifelse(u < (3/4), 2, 3))
  
  #Calculando as proporções de um determinado valor
  props <- mean(x == k)
  
  return(list(valores = x, propDeK = props))
}

Q1.1(100, k = 1)$propDeK
Q1.1(100, k = 2)$propDeK
Q1.1(100, k = 3)$propDeK

Q1.1(1000, k = 1)$propDeK
Q1.1(1000, k = 2)$propDeK
Q1.1(1000, k = 3)$propDeK


Q1.1(10000, k = 1)$propDeK
Q1.1(10000, k = 2)$propDeK
Q1.1(10000, k = 3)$propDeK

#Question 2
Q2 <- function(n = 100){
  #Gerando variável em branco pra colocar os valores aceitos
  X <- numeric(n)
  #gerando count para saber quando parar e quantos foram aceitos
  count <- 0
  #Gerando variável para calcular rejeitados
  reject <- 0
  #while loop que roda até que número de aceitos seja igual a n
  while(count < n){
      
      #Primeira condição que é pra saber se o x será aceito
      if(runif(1) < 1/2){
        #Caso seja aceito, incrementa o count em 1
        count <- count + 1
        #Adiciona um valor da g(x) para o vetor X
        X[count] <- runif(1, 2, 6)
      
      } else {
        #Caso seja rejeitado, regristramos incrementando o reject
        reject <- reject + 1
      }
  }
  #Retorno da função
  return(list(valores = X,
              accepted = count, 
              reject = reject,
              total = reject + count,
              acceptRate = count/(reject + count)))
}

result <- Q2(n = 132981)
result$acceptRate

#Modificação
Q3 <- function(n = 100){
  
  x1 <- function(t){
    return(2 + 2*sqrt(t))
  }
  
  x2 <- function(t){
    return(-6*(-1 + sqrt((1 - t)/3)))
  }
  
  u <- runif(n)
  
  x <- ifelse(u<1/4, x1(u), x2(u))
  
  return(x)
}


summary(Q3(100000))

hist(Q3(100000))




f <- function(x){
  return(30*(x^2 - 2*x^3 + x^4))
}
x <- seq(0, 1, by = .00001)

plot(x, f(x), type = "l")
max(f(x))

abline(h = 1.875)
abline(h = 1, col = "red")

1/1.875
