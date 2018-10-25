library(MASS)

data("Boston")

resultado <-
  data.frame(Metodo = c(
    "OLS",
    "Ridge",
    "LASSO",
    "KNN",
    "CART",
    "Bagging",
    "Floresta",
    "Boosting"
  ))

set.seed(1234)

idx_tr <- sample(1:nrow(Boston), round(0.8*nrow(Boston)), replace = FALSE)

#Separa treinamento e validacao

treinamento <- Boston[idx_tr,]
teste <- Boston[-idx_tr,]

ols <- lm(medv~., data = treinamento)
summary(ols)

Predicao <- predict(ols,newdata = teste)

ListaResultado <- list(resultado = resultado,Resposta = teste$medv)

ListaResultado$resultado$Erros[1] <- 0
ListaResultado$resultado$Erros[1]<- mean((ListaResultado$Resposta - Predicao)^2)

#Ridge pg#215
library(glmnet)

X <- model.matrix(medv~., data = treinamento)
Y <- treinamento$medv

ridge <- glmnet(X, Y, alpha = 0)
plot(ridge)
library(plotmo)
plot_glmnet(ridge)

cv_ridge <- cv.glmnet(X, Y, alpha = 0)
plot(cv_ridge)
cv_ridge$lambda.min

X_teste <- model.matrix(medv~., teste)
Predicao <- predict(ridge, s = cv_ridge$lambda.min,newx = X_teste)
Predicaot <-  mean((
  ListaResultado$Resposta - Predicao)^2)
ListaResultado$resultado$Erros[2] <- Predicaot

#Lasso p219
cv_lasso <- cv.glmnet(X, Y, alpha = 1)
cv_lasso$lambda.min

Predicao <- predict(ridge, s = cv_lasso$lambda.min,newx = X_teste)
Predicaot <-  mean((
  ListaResultado$Resposta - Predicao)^2)
ListaResultado$resultado$Erros[3] <- Predicaot

library(MASS)
library(FNN)

set.seed(1234)

x <- scale(subset(Boston, select = -medv))
y <- Boston$medv

max_k <- 15
replications <- 50
estimated_test_error <- matrix(nrow = replications, ncol = max_k)
number_of_folds <- 10

pb <- txtProgressBar(min = 1, max = replications * max_k, style = 3, width = 50)
for (t in 1:replications) {
  fold <- sample(1:number_of_folds, size = nrow(Boston), replace = TRUE)
  for (k in 1:max_k) {
    mse <- numeric(number_of_folds)     
    for (i in 1:number_of_folds) {
      out_of_sample <- (fold == i)
      in_sample <- (fold != i)
      y_pred <- knn.reg(x[in_sample,], x[out_of_sample,], y[in_sample], k = k)
      mse[i] <- mean((y[out_of_sample] - y_pred$pred)^2)
    }
    estimated_test_error[t, k] <- mean(mse)
    setTxtProgressBar(pb, (t-1)*max_k + k)
  }
  
}
close(pb)

error <- colMeans(estimated_test_error)

plot(1:max_k, error, type = "b", lwd = 2, col = "dark green", las = 1,
     xlab = "k", ylab = "", main = sprintf("Validação cruzada em %i lotes", number_of_folds))
title(ylab = "Erro de classificação", mgp = c(3.5, 1, 0), cex.lab = 1.2)

(best_k <- which.min(error))

ListaResultado$resultado$Erros[4] <- min(error)


## ARVORE 303
library(rpart)
library(rpart.plot)

arvore <- rpart(medv~., data = treinamento)
rpart.plot(arvore)

erro_medio <- function(resultado,predicao){
  mean((resultado- predicao)^2)
}
Predicao <-  predict(arvore, teste)
Predicaot <- erro_medio(ListaResultado$Resposta,Predicao)
ListaResultado$resultado$Erros[5] <- Predicaot

#Bagging 316
library(randomForest)
set.seed(1234)
bagging <-
  randomForest(medv ~ .,
               mtry = 13,
               data = treinamento,
               importance = TRUE)
Predicao <-  predict(bagging, teste)
Predicaot <- erro_medio(ListaResultado$Resposta,Predicao)
ListaResultado$resultado$Erros[6] <- Predicaot


#Bagging 319
library(randomForest)
set.seed(1234)
floresta <- randomForest(medv~.,mtry = floor(13/3), data = treinamento,importance = TRUE)
Predicao <-  predict(floresta, teste)
Predicaot <- erro_medio(ListaResultado$Resposta,Predicao)
ListaResultado$resultado$Erros[7] <- Predicaot
ListaResultado$resultado$Erros


## Boosting 321
library(gbm)
set.seed(1234)
boosting <-
  gbm(
    medv ~ .,
    distribution = "gaussian",
    n.trees = 5000,
    interaction.depth = 3,
    shrinkage = 0.01,
    data = treinamento
  )
Predicao <-  predict(boosting, teste,n.trees = 5000)
Predicaot <- erro_medio(ListaResultado$Resposta,Predicao)
ListaResultado$resultado$Erros[8] <- Predicaot

summary(boosting)


ListaResultado$resultado