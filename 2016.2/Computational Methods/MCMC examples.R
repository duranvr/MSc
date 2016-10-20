w0 <- c(0, 1, 0)
P <- matrix(c(.4, .4, .2, .3, .2, .4, .3, .4, .4), 3, 3)


w1 <- w0%*%P
w1

install.packages("expm")
library(expm)

w2 <- w0%*%(P %*% P) 

w2 <- w1%*%P
w2

w3 <- w0%*%(P %*% P %*% P) 
w3

w4 <- w0%*%(P %^% 4)
w4

w5 <- w0%*%(P %^% 5)
w5

w6 <- w0%*%(P %^% 6)
w6

w30 <- w0%*%(P %^% 30)
w30

par(mfrow = c(1, 3))

We <- matrix(NA, 10, 3)

for(i in 1:10){
  We[i,] <- w0 %*% (P %^% i)
}

plot(We[,1], type = "l", ylim = c(0, 1), main = "Partindo de Empate")
lines(We[,2], col = "red")
lines(We[,3], col = "blue")


Wd <- matrix(NA, 10, 3)

w0 <- c(1, 0, 0)

for(i in 1:10){
  Wd[i,] <- w0 %*% (P %^% i)
}

plot(Wd[,1], type = "l", ylim = c(0, 1), main = "Partindo de Derrota")
lines(Wd[,2], col = "red")
lines(Wd[,3], col = "blue")


Wv <- matrix(NA, 10, 3)

w0 <- c(1, 0, 0)

for(i in 1:10){
  Wv[i,] <- w0 %*% (P %^% i)
}

plot(Wv[,1], type = "l", ylim = c(0, 1), main = "Partindo de Vitória")
lines(Wv[,2], col = "red")
lines(Wv[,3], col = "blue")

PArray <- array(NA, dim = c(3, 3, 10))

for(i in 1:dim(PArray)[3]){
  PArray[,,i] <- P %^% i
}

PArray[,,10]


#### Exemplo de matriz sem equilíbrio ####

P2 <- matrix(c(0, 1, 1, 0), 2, 2)

w0 <- c(1, 0)

W <- matrix(NA, 200, 2)

for(i in 1:nrow(W)){
  W[i,] <- w0 %*% (P2 %^% i)
}

plot(W[,1], type = "l")

#### Exemplo de matriz com convergência lenta ####

P3 <- matrix(c(0.01, .99, .99, .01), 2, 2)

w0 <- c(1, 0)

W <- matrix(NA, 200, 2)

for(i in 1:nrow(W)){
  W[i,] <- w0 %*% (P3 %^% i)
}

plot(W[,1], type = "l")
