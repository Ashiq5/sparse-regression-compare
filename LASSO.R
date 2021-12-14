library(glmnet)

set.seed(1)

load("hiv.rda")
dim(hiv.train$x)
dim(hiv.test$x)

cvfit <- cv.glmnet(hiv.train$x, hiv.train$y)
fit <- cvfit$glmnet.fit
plot(fit, xvar = "lambda")

xlim <- log(c(fit$lambda[1], cvfit$lambda.min))
plot(fit, xlim=xlim, xvar="lambda")

plot(cvfit)


lasso.mod = glmnet(hiv.train$x, hiv.train$y,alpha = 1)
bestlam = cvfit$lambda.min
lasso.coef = predict(lasso.mod,type ="coefficients",s=bestlam)
lasso_cv_est = lasso.coef[-1]
lasso.ind <- ((lasso_cv_est!=0))

cat("No of variables selected", sum(lasso.ind), "\n")
cat("Estimates for selected coefficients \n", lasso_cv_est[lasso.ind], "\n")
cat("Which columns were selected?", which(lasso.ind == 1))


lasso.pred=predict(fit, s = bestlam , newx = hiv.test$x)
mean((lasso.pred - hiv.test$y)^2)
