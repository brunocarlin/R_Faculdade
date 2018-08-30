Manhatam <- function(x,y) {
  sum(abs(x-y))
}

Chebi <- function(x,y) {
  max(abs(x-y))
}

x <- c(1,3,1)
y <- c(2,5,1)

Manhatam(x,y)
Chebi(x,y)


