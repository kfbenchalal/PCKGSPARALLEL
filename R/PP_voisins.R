
library(microbenchmark)
library(ptw)
library(compiler)


# 1) calculer le nombre de voisins de Mat [i, j] (qui sont infect?s)
# 2) Ensuite on Additionne la sous-matrice autour du point sp?cifi? pour calculer la somme des voisins
# et le sous-produit 2 puisque Mat [i, j] == 2

#' @export
PP_voisins = function(Mat_pad, i, j) {
  return(sum(Mat_pad[i - 1:i + 1, j - 1:j + 1]) - 2)
}

