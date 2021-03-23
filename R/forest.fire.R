
library(microbenchmark)
library(ptw)
library(compiler)


#' @export
forest.fire = function(M, a, b, pausing = FALSE) {
  # Dans cette partie on simule le mod?le d'?pid?mie de feu de for?t
  # M [i, j] =     2  pour sensible
  #                1  pour infect?
  #                0  pour supprim?

  # Apr?s on fait du (Padding) remplissage de la matrice avec des z?ros de tous les c?t?s

  # Voici notre matrice par exemple


  #   Matrice "Mat"       Matrice "Mat" apr?s remlissage(Padding)

  #                     0  0  0  0  0
  #     1  2  2         0  1  2  2  0
  #     2  1  1         0  2  1  1  0
  #     2  1  2         0  2  1  2  0
  #                     0  0  0  0  0



  M_pad = rbind(0, padzeros(M, 1, side = 'both'), 0)

  # On va configurer le plot
  plot(
    c(1, nrow(M)),
    c(1, ncol(M)),
    type = "l",
    xlab = "",
    ylab = ""
  )
  forest.fire.plot(M)

  # g?n?ration d'une matrice al?atoire
  randmat =
    matrix(c(runif(
      nrow(M) * ncol(M), min = 0, max = .5
    )), nrow = nrow(M), ncol = ncol(M))


  # On va initialiser l'?tat de gravure avec TRUE
  burning = TRUE
  # boucle principale
  while (burning) {
    burning = FALSE

    Mat_pad = M_pad
    for (i in 2:(nrow(M_pad) - 1)) {
      for (j in 2:(ncol(M_pad) - 1)) {
        if (M_pad[i, j] == 2) {
          if (randmat[i - 1, j - 1] > (1 - a) ^ PP_voisins(M_pad, i, j)) {
            Mat_pad[i, j] = 1
          }
        } else if (M_pad[i, j] == 1) {
          burning = TRUE
          if (randmat[i - 1, j - 1] < b) {
            Mat_pad[i, j] = 0
          }
        }
      }
    }
    M_pad = Mat_pad

    # Ici on v?rifie si il y a une pause entre les mises ? jour comme suit:
    if (pausing) {
      forest.fire.plot(M_pad)
      input = readline("hit any key to continue")
    }
  }

  # Affichage le graphe de "forest fire final"
  forest.fire.plot(M_pad)

  # ici on renvoie la matrice sans Padding
  return(M_pad[2:(nrow(M_pad) - 1),2:(ncol(M_pad) - 1)])
}

