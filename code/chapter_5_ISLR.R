

library(ISLR2)
library(boot)

# Validation set approach #####################################


set.seed(1)
train <- sample(392, 196) # 50% for training 50% for validation

# run the linear regression model using the training set
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

# calculate the mse for the validation set
mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)


# add squared term
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)

mean((Auto$mpg - predict(lm.fit2, Auto))[-train]^2)

# add a cubic term
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)

mean((Auto$mpg - predict(lm.fit3, Auto))[-train]^2)

# It seems that the model with cubic term perfoms worse than the cuadratic model
# This might be due to the train set selection, we need to test with another
# sampling for the training and validation set.

set.seed(2)
train <- sample(392, 196)

# run the linear regression model using the training set
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

# calculate the mse for the validation set
mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)


# add squared term
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)

mean((Auto$mpg - predict(lm.fit2, Auto))[-train]^2)

# add a cubic term
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)

mean((Auto$mpg - predict(lm.fit3, Auto))[-train]^2)


# We find evidence that adding a cuadractic term improves the performance out of sample
# when compared to a linear term model. On the other hand, there is no strong evidence that
# adding a cubic term can actually improve the model better.


# Leave one out ####################################################

glm.fit <- glm(mpg ~ horsepower, data = Auto) # just a linear regression
cv.err <- cv.glm(Auto, glm.fit) # computing the error rate for the model
cv.err$delta 

# starting the vectors for the for loop
cv.error <- rep(0, 10)
cv.multiple <- rep(0, 392)

# nested for loop
# first for loop, loops over the degree of the polynomial
# second for loop, loops over the rows producing the leave-one-out 
# cv.error contains the mse

for(i in 1:10) {
  
  for(j in 1:392) {
    
    training <- Auto[-j,]
    validation <- Auto[j,]
    glm.fit <- glm(mpg ~ poly(horsepower, i), data = training)
    cv.multiple[j] <- predict(glm.fit, newdata = validation)
  }
  cv.error[i] <- mean((Auto$mpg - cv.multiple)^2)
  
}

cv.error

# k-Fold Cross-Validation ############################################
set.seed(17)

# Randomly order the data set
Auto2 <- Auto[sample(nrow(Auto)),]

# Create 10 folds
folds <- cut(seq(1, nrow(Auto2)), breaks = 10, labels = FALSE)

k_error <- rep(0, 10)
k_multiple <- rep(0, 10)
for(j in 1:10) {
  
  for (i in 1:10) {
    
    test_index <- which(folds == i, arr.ind = TRUE)
    test <- Auto2[test_index, ]
    training <- Auto2[-test_index, ]
    glm.fit <- glm(mpg ~ poly(horsepower, j), data = training)
    k_multiple[i] <- mean((test$mpg- predict(glm.fit, newdata = test))^2)
    
  }
  k_error[j] <- mean(k_multiple)
}

k_error


# Bootstrap ###########################################################

# create the same function as in example one
alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  
  (var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2 * cov(X,Y))
}

# set seed
set.seed(7)
boot_multiple <- rep(0, 1000)
boot_original <- rep(0.576, 1000)
for (i in 1:1000) {
  
  boot_multiple[i] <- alpha.fn(Portfolio, sample(100, 100, replace = T))
  
}

boot_total <- data.frame(original = 0.576)
boot_total["bias"] <- mean((boot_original - boot_multiple))
boot_total['std.dev'] <- sd(boot_multiple)

# create the same function as in example two
boot.fn <- function(data, index) {
  coef(lm(mpg ~ horsepower, data = data, subset = index))
}

# create the coefficients from the original data set
boot_total <- data.frame(original_inter = boot.fn(Auto, 1:392)[1], original_coef = boot.fn(Auto, 1:392)[2])

# set seed
set.seed(1)
boot_multiple <- data.frame(intercept = rep(0, 1000), horsepower = rep(0, 1000))
boot_original <- data.frame(original_inter = rep(boot.fn(Auto, 1:392)[1], 1000), original_coef = rep(boot.fn(Auto, 1:392)[2], 1000))
for (i in 1:1000) {
  
  boot_multiple[i, 1] <- boot.fn(Auto, sample(392, 392, replace = T))[1]
  boot_multiple[i, 2] <- boot.fn(Auto, sample(392, 392, replace = T))[2]
}

boot_total["bias_inter"] <- mean((boot_original[['original_inter']] - boot_multiple[,1]))
boot_total["bias_slope"] <- mean((boot_original[['original_coef']] - boot_multiple[,2]))
boot_total['sd_inter'] <- sd(boot_multiple[,1])
boot_total['sd_slope'] <- sd(boot_multiple[,2])
