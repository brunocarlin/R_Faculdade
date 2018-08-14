cord<- c(First= 1, Second = 2, Thrid =3)

cord["First"]

names(cord) <- c("Primeiro","Segundo","Terceiro")

?month.abb

vec<- month.abb
vec
length(vec)

vec[-c(3,6,9)]
vec[-c(1:3)]




A <- matrix(1:12, nrow = 4)
A

class(A)

str(A)

A <- 
  matrix(1:25, nrow  = 5, byrow = TRUE)

A[3,2]
A[,]
A[,2]
A[1:3,2:4]


A[A>5]

A[A%%2 == 0]

colnames(A) <- c("Primeira Coluna",2,3,4,5)
rownames

dimnames

B <- matrix(5, nrow = 5, ncol = 5)

A+B

trA <- t(A)

I <- diag(1, nrow = 5)

A <- matrix(1:4, nrow = 2, byrow = TRUE)

X <- matrix(1:2, ncol = 1)

A %o% X


70 %in% c(2,7,10)

?solve

A <- matrix(c(1,2,4,5), nrow = 2, byrow = TRUE)
B

library(matlib)