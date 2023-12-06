#' @import microbenchmark
#' @importFrom Rcpp evalCpp
#' @importFrom stats rnorm rgamma
#' @useDynLib SA23204190
NULL

#' @title Newton-Raphson method using R
#' @description Newton-Raphson method using R
#' @param f function of which we want to find a root
#' @param df derivative of f
#' @param x0 initial guess
#' @return Numerical solution of the equation
#' @examples
#' \dontrun{
#' f<-function(x){
#' sin(x)+0.56
#' }
#' df <- function(x){
#' cos(x)
#' }
#' 
#' number<-NR(f,df,0)
#' number
#' }
#' @export
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


#' @title A Gibbs sampler using R
#' @description A Gibbs sampler using R
#' @param N the number of samples
#' @param thin the number of between-sample random numbers
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#'     rnR <- gibbsR(100,10)
#'     par(mfrow=c(2,1));
#'     plot(rnR[,1],type='l')
#'     plot(rnR[,2],type='l')
#' }
#' @export
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

