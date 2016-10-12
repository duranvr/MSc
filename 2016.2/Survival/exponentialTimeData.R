#Modeling survival data with the exponential distribution
#Sample times (no censoring)
t <- c(4, 20, 22, 25, 27, 38, 40, 44, 56, 83, 89, 98, 110, 138, 145)

#Maximum likelihood estimation for exponential parameter
mean(t)

#Sufficient statistic used for constructing confidence interval
T <- sum(t)

#Exact confidence interval function (Chi-squared)
IC <- function(x, alpha = .05){
  estML <- mean(x)
  U <- (2*T)/(qchisq(alpha/2, df = 2*length(x)))
  L <- (2*T)/(qchisq(1-alpha/2, df = 2*length(x)))
  return(cat("Your maximum likelihood estimation is: ", estML, 
             ", with ", 1-alpha, "% confidence interval equal to: ",
             "[", L, ", ", U, "]", sep = ""))
}

IC(t)


