\name{checkAreaC}
\alias{checkAreaC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Function for package
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
    The funct is construted

}
\usage{
forest.fire.plot(M)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
%%     ~~Describe \code{M} here~~
 Dataframe, M which we apply our function
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use

%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
    Nour-Dass HAMMADI & Farida BENCHENLAL students M2 Data Mining at Lyon 2 Lumiere university

}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.



library(microbenchmark)
library(ptw)
library(compiler)

# Ici on cherche ? ploter les individus infect?s et supprim?s







## The function is currently defined as

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




M = matrix(2, 32, 32)
M[12, 12] = 1 # on remplie des "1"
forest.fire.plot(M)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
