
###'######################################################################
###'
###' Predicted values based on linear model object.
###' 
###' 20180828 JoonHo Lee
###' 
###' 

###'######################################################################
###'
###' Basic settings
###'
###'

### Start with a clean slate
gc()            # force R to release memory it is no longer using
rm(list=ls())   # delete all the objects in the workspace


### Set working directory 
work_dir <- c("~/data-manipulation-and-visualization")
setwd(work_dir)


### Call libraries
library(graphics)



###'######################################################################
###'
###' Predictions
###'
###'

### Model fitting
x <- rnorm(15)
y <- x + rnorm(15)
predict(lm(y ~ x))


### Generate new data
new <- data.frame(x = seq(-3, 3, 0.5))


### Predict y based on new x
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction")
pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")



###'######################################################################
###'  
###' Prediction intervals, special cases
###' 
###' (1) Weighted regression 
###' (2) ANOVA
###' 
###' 

### Fit weighted regression
w <- 1 + x^2
fit <- lm(y ~ x)
wfit <- lm(y ~ x, weights = w)


###' Predict intervals
###' The first three of these throw warnings
predict(fit, interval = "prediction")
predict(wfit, interval = "prediction")
predict(wfit, new, interval = "prediction")
predict(wfit, new, interval = "prediction", weights = (new$x)^2)
predict(wfit, new, interval = "prediction", weights = ~x^2)


### From  aov(.) example: predict(.. terms)
npk.aov <- aov(yield ~ block + N*P*K, npk)
(termL <- attr(terms(npk.aov), "term.labels"))
(pt <- predict(npk.aov, type = "terms"))
pt. <- predict(npk.aov, type = "terms", terms = termL[1:4])
stopifnot(all.equal(pt[,1:4], pt.,
                    tolerance = 1e-12, check.attributes = FALSE))






