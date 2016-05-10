# Fibonacci series of length n
# by Antonio Giannino


fibonacci <- function(n) {
  x <- n
  fibonacci <- numeric(x)
  fibonacci[1] <- 0
  fibonacci[2] <- 1
  for (i in 3:x) { 
    fibonacci[i] <- fibonacci[i-1]+fibonacci[i-2]
  } 
  fibonacci
} 



