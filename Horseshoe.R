library(horseshoe)

set.seed(1)

hs.fit <- horseshoe(y = hiv.train$y, X = hiv.train$x, 
                    method.tau = "truncatedCauchy", method.sigma = "Jeffreys",
                    burn = 5000, nmc = 10000, thin = 2)

par(mfrow=c(1,2))
plot(hs.fit$BetaHat, main = "Horseshoe", ylab = "Estimates")
plot(lasso_cv_est, main = "Lasso", ylab = "Estimates")

hs.ind <- HS.var.select(hs.fit, hiv.train$y, method = "interval") 
cat("No of variables selected", sum(hs.ind), "\n")
cat("Estimates for selected coefficients \n", hs.fit$BetaHat[hs.ind], "\n")
cat("Which columns were selected?", which(hs.ind == 1))

hs.pred <- hiv.test$x%*%hs.fit$BetaHat
(MSE = mean((hiv.test$y - hs.pred)^2))
