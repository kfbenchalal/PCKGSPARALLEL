
library(microbenchmark)
library(ptw)
library(compiler)

# Ici on cherche ? ploter les individus infect?s et supprim?s
#' @export
forest.fire.plot = function(M) {
  for (i in 1:nrow(M)) {
    for (j in 1:ncol(M)) {
      if (M[i, j] == 1)
        points(i, j, col = "red", pch = 17)
      else if (M[i, j] == 0)
        points(i, j, col = "green", pch = 17)
    }
  }
}

