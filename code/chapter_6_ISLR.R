library(ISLR2)
library(leaps)

Hitters <- na.omit(Hitters)

# Subset  Selection Methods

# Using default nvmax = 8
regfit.full <- regsubsets(Salary ~ ., Hitters)

# Using nvmax = 19
regfit.full <- regsubsets(Salary ~ ., Hitters, nvmax = 19)

# Get summary
reg.summary <- summary(regfit.full)

# Get names from regfit.full
names(reg.summary)

# Plot RSS adjusted R2, Cp, and BIC
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R squared", type = "l")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)

# Using plot from leaps package
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

# Check model with only 6 variables
coef(regfit.full, 6)

# Forward and Backward Stepwise Selection
regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)