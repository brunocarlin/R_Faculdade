Soma <- 2 + 2
Subtract <- 2 - 2
Mult <- 2 * 2
Div <- 2 / 2
2 ^ 2
2 ** 2
5 %% 2
5 %/% 2

Radius <- 5
CircleArea <- function (rad) {Radius ^ 2* pi}

Area <- CircleArea(Radius)
signif(Area,digits = 3)

??"Regression Model"

install.packages("plot3D")
demo("graphics")


Level2 <- 1 
Level1 <- -3
Level0 <- -10

baskara <- function(a,b,c) {
  
  ResultsP<- (-b + sqrt(b^2 - 4*a*c))/2*a
  ResultsN<-(-b - sqrt(b^2 - 4*a*c))/2*a
  list(first_root = ResultsP,second_root = ResultsN)
  }

baskara(Level2,Level1,Level0)

PV <- function(FV, r, n) {
  FV /(1 + r) ^ n
}

round(PV(1000,0.01,7),2)



# Vetores

x <- sqrt(2)
class(x)

school <- "Insper"
class(school)

logic <- 2 == 2
class(logic)
str(logic)
logic > 0
as.numeric(logic)

(2 != 3) & (5==5) # And
(2 != 3) | (5==5) #Or
(2 != 3) | (5==5) #Or
(2 != 3) | (5==5) #Or


# Concatenando com c()

Num <- c(10,20,30,40,50)
Stringg <- c("Insper","FGV","FEA")
Logicla <- c(T,F,F)


Num[c(1:3,5)]

seq(7.5,3.5,by = -0.15)

sex <- factor( c("M","F","F","F","M"))

str(sex)

Num * Logicla


schools <- c("Insper","FGV","FEA")
best<- c(T,F,F)

Num
Num[-c(Num >25)]

nchar