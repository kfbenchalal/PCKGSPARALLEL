
library(microbenchmark)
library(ptw)
library(compiler)


#' @export
test <- function(){
  n= 10000
  for (i in 1:n) {
    X <- runif(1,0, pi)
    Y <- runif(1,0,d/2)

    if(inequality(X,Y)){
      points(X, Y, col="red")
    } else {
      points(X, Y, col="blue")
    }
  }
}
