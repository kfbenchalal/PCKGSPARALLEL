
library(microbenchmark)
library(ptw)
library(compiler)

#' @export
isEqualVecotrs<-function(){
  X <- runif(1,0, pi)
  Y <- runif(1,0,d/2)
  if (inequality(X,Y))
    return(TRUE)
}
