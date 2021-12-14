library("glmnet")
library("ggplot2")
library("caret")
library("dplyr")

load("hiv.rda")

control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        search = "random",
                        savePredictions = "all")

data = cbind(hiv.train$x, y = hiv.train$y)

elastic_model <- train(y ~ .,
                       data = data,
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       tuneLength = 25,
                       trControl = control)

elastic_model

# Training Best Result
get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

get_best_result(elastic_model)

# Model Prediction on Test Data
x_hat_pre <- predict(elastic_model, hiv.test$x)
x_hat_pre

# Multiple R-squared
rsq <- cor(hiv.test$y, x_hat_pre)^2
rsq

# Mean Squared Error
# mse <- sum((x_hat_pre - hiv.test$y)^2) / length(hiv.test$y)
mse <- mean((x_hat_pre - hiv.test$y)^2)
mse

# Variable Selection
elnet.coef = coef(elastic_model$finalModel, elastic_model$bestTune$lambda)
elnet.coef.arr = elnet.coef[-1]
elnet.ind <- ((elnet.coef.arr!=0))

cat("No of variables selected", sum(elnet.ind), "\n")
cat("Estimates for selected coefficients \n", elnet_cv_est[elnet.ind], "\n")
cat("Which columns were selected?", which(elnet.ind == 1))

plot(elnet.coef.arr, pch = 4, col = 4, xlab = "Indices", ylab = "Estimates")


