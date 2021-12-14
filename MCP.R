library("glmnet")
library("ggplot2")
library("caret")
library("dplyr")
library("oem")

load("hiv.rda")

x <- hiv.train$x
y <- hiv.train$y

fit <- cv.oem(x = x, y = y,
              penalty = "mcp", groups = rep(1:30, each = 5))

plot(fit, which.model = 1)

preds.mcp <- predict(fit, newx = hiv.test$x, type='response', which.model = 1)
apply(preds.mcp, 2, function(x) mean((hiv.test$y - x) ^ 2))

mcp.coef <- predict(fit, newx = hiv.test$x, type='coefficient', which.model = 1)
mcp.coef.arr = mcp.coef[-1]
mcp.nz <- ((mcp.coef.arr!=0))

cat("No of variables selected", sum(mcp.nz), "\n")
cat("Estimates for selected coefficients \n", mcp.coef.arr[mcp.nz], "\n")
cat("Which columns were selected?", which(mcp.nz == 1))

plot(mcp.coef.arr, pch = 4, col = 4, xlab = "Indices", ylab = "Estimates")

