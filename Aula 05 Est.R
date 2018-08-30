# Aula 5

# Regressão logística

rm(list = ls())

# install.packages("ISLR")
library(ISLR)

?Default

# 10.000 individuals
# annual income
# monthly credit card balance
# is a student (factor "Yes" or "No")

str(Default)

par(mfrow = c(1, 3))

# Para entender os valores de pch leia ?points

plot(Default$balance, Default$income, 
     col = ifelse(Default$default == "Yes", "red", "blue"),
     pch = ifelse(Default$default == "Yes", 3, 1),
     cex = 0.75,
     xlab = "Balance", ylab = "Income")

plot(Default$default, Default$balance, col = c("blue", "red"),
     xlab = "Default", ylab = "Balance")

plot(Default$default, Default$income, col = c("blue", "red"),
     xlab = "Default", ylab = "Income")

options(scipen = 999) # desliga a notação científica nos sumários

# Modelos com apenas uma preditora

model <- glm(default ~ balance, data = Default, family = binomial)
summary(model)

model <- glm(default ~ student, data = Default, family = binomial)
summary(model)

# Modelo completo

model <- glm(default ~ balance + income + student, data = Default, family = binomial)
summary(model)

# ?predict.glm
# predict() é uma função genérica que chama predict.glm() internamente

# This function estimates the ROC curve by k-fold CV
roc_by_cv <- function(df, number_of_folds, cutoff_interval) {
    cutoff <- seq(from = 0, to = 1, by = cutoff_interval)
    fpr <- numeric(length(cutoff))
    tpr <- numeric(length(cutoff))
    fold <- sample(1:number_of_folds, size = nrow(df), replace = TRUE)
    pb <- txtProgressBar(min = 1, max = length(cutoff), style = 3, width = 50)
    for (i in 1:length(cutoff)) {
        fpr_fold <- numeric(number_of_folds)
        tpr_fold <- numeric(number_of_folds)
        for (j in 1:number_of_folds) {
            out_of_sample <- (fold == j)
            in_sample <- (fold != j)
            # Regressão logística via Generalized Linear Models glm()
            model <- glm(default ~ balance + income + student,
                         data = df[in_sample,], family = binomial)
            prob <- predict(model,
                            newdata = df[out_of_sample, c("balance", "income", "student")],
                            type = "response")
            y_pred <- factor(ifelse(prob > cutoff[i], "Yes", "No"), levels =  c("No", "Yes"))
            fpr_fold[j] <- sum(y_pred == "Yes" & df$default[out_of_sample] == "No") /
                               sum(df$default[out_of_sample] == "No")
            tpr_fold[j] <- sum(y_pred == "Yes" & df$default[out_of_sample] == "Yes") /
                               sum(df$default[out_of_sample] == "Yes")
        }
        fpr[i] = mean(fpr_fold)
        tpr[i] = mean(tpr_fold)
        setTxtProgressBar(pb, i)
    }
    close(pb)
    data.frame(cutoff = cutoff, FPR = fpr, TPR = tpr)
}

roc <- roc_by_cv(Default, number_of_folds = 10, cutoff_interval = 0.01)

View(roc)

par(mfrow = c(1, 1))

# plot(roc$FPR, roc$TPR, xlim = c(0, 1), ylim = c(0, 1), col = "blue", 
#      type = "l", lwd = 2, xlab = "FPR", ylab = "TPR", main = "ROC")
# identify(roc$FPR, roc$TPR, labels = roc$cutoff)

library(ggplot2)

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5)) # center titles

plt <- ggplot(roc, aes(x = FPR, y = TPR, color = cutoff)) +
              geom_line(size = 1, alpha = 0.35) +       
              geom_point(size = 1.5, alpha = 0.9) +       
              scale_color_gradient(low = "blue", high = "red") +
              ggtitle("ROC curve")

plt

library(plotly)

ggplotly(plt)
