## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
NR <- function(f,df,x0){
  x< - x0
  iter <- 0
  
  
  while (abs(f(x)) > 1e-4 && iter < 1e4) {
    x <- x-f(x)/df(x)
    iter <- iter + 1 
  }
  
  if (iter == 1e4) {  
    stop("Did not converge")  
    
  } else {  
    return(x) 
  }  
  
}

## -----------------------------------------------------------------------------
gibbsR <- function(N, thin) {
  mat <- matrix(nrow = N, ncol = 2)
  x <- y <- 0
  for (i in 1:N) {
    for (j in 1:thin) {
      x <- rgamma(1, 3, y * y + 4)
      y <- rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))
    }
    mat[i, ] <- c(x, y)
  }
  mat
}

