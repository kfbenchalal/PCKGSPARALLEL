
library(microbenchmark)
library(ptw)
library(compiler)


#' @export
inequality <- function(X,Y){
  if (Y <= (l/2)*sin(X))
    return (TRUE)
  else
    return (FALSE)
}
