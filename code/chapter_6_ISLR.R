library(ISLR2)
library(leaps)
library(glmnet)

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

# Choosing Among Models Using Validation-Set and Cross-Validation
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), replace = TRUE)
test <- (!train)
regfit.best <- regsubsets(Salary ~ ., data = Hitters[train, ], nvmax = 19)

test.mat <- model.matrix(Salary ~ ., data = Hitters[test,])
val.errors <- rep(NA, 19)
for (i in 1:19) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}

which.min(val.errors)

coef(regfit.best, 7)

# Create function for subsets
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

regfit.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(regfit.best, 7)

# Cross validation 10 folds
k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ ., data = Hitters[folds != j, ], nvmax = 19)
  for (i in 1:19) {
    pred <- predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] <- mean((Hitters$Salary[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")

reg.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(reg.best, 10)


# Ridge regression and the lasso
x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary

# Ridge regression
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

dim(coef(ridge.mod))



