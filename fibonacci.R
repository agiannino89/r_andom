# Fibonacci series of length n
# by Antonio Giannino


fibonacci <- function(n) {
  fibonacci <- numeric(n)
  fibonacci[1] <- 0
  fibonacci[2] <- 1
  for (i in 3:n) { 
    fibonacci[i] <- fibonacci[i-1]+fibonacci[i-2]
  } 
return(fibonacci)
} 


