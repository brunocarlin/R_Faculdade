runs <- 100000
#runif samples from a uniform distribution
x <- runif(runs,min=-0,max=1)
y <- runif(runs,min=-0,max=1)

(Sweet <- mean((x > y) & (x ^ 2 < y)))


# Ex 2

BetaP <- 10/2.25
AlphaP <- Beta*10

Profit <- rgamma(runs, shape= AlphaP, rate = BetaP)

BetaL <- 7/4
AlphaL <- Beta*7

Loses <- rgamma(runs, shape= AlphaL, rate = BetaL)

mean(Profit - Loses)


# Ex 3
Distances <- 8
x <- replicate(Distances,runif(runs,min=-0,max=1))
runs <- 1000
mean(dist(x, method = "euclidean"))


# Ex 4



