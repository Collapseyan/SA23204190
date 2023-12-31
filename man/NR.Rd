% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StatCompR.R
\name{NR}
\alias{NR}
\title{Newton-Raphson method using R}
\usage{
NR(f, df, x0)
}
\arguments{
\item{f}{function of which we want to find a root}

\item{df}{derivative of f}

\item{x0}{initial guess}
}
\value{
Numerical solution of the equation
}
\description{
Newton-Raphson method using R
}
\examples{
\dontrun{
f<-function(x){
sin(x)+0.56
}
df <- function(x){
cos(x)
}

number<-NR(f,df,0)
number
}
}
