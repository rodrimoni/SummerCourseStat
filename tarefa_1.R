library(ggplot2)

# Loading data
df <- read.table("C:\\Users\\Rodrigo\\Documents\\verao_1\\191934.txt", header = TRUE)

MQOLinearRegression <- function (train, test) {
  # b) Simple linear regression
  linearRegressionL2 <- lm(Y ~ X, train)
  summary(linearRegressionL2)
  
  # Linear regression plot
  #ggplot() + geom_point(data = train, mapping =  aes(x=X, y=Y)) + theme_bw() + 
  # geom_smooth(data = train, mapping =  aes(x=X, y=Y), method = "lm") + 
  #geom_point(data = test, mapping = aes(x=X, y=Y), color = "red")
  
  # Prediction and mean absolute error out-of-sample
  predictionsL2 <- predict(linearRegressionL2, test)
  errorOutOfSampleL2 <- mean(abs(test$Y - predictionsL2))
  
  # Mean absolute error in sample
  errorInSampleL2 <- mean(abs(train$Y - linearRegressionL2$fitted.values)) # (train$Y - linearRegression$fitted.values) = linearRegression$residuals
  
  return(list(linearRegressionL2$coefficients, errorOutOfSampleL2, errorInSampleL2))
}

customLinearRegression <- function (train, test) {
  # c) Adjust for Loss L1
  funcaoObjetivo <- function (beta) {
    # Function to be minimized
    sum(abs(train$Y - (beta[1] +  beta[2] * train$X)))
  }
  
  # Setting the seed
  #set.seed(191934)
  
  # using optim with default method to find the minimal argument
  betas <- optim(c(0, 0), funcaoObjetivo)[[1]]
  
  # d) Using the function created in c) to adjust the model and predict values in and out of sample.
  linearRegressionL1 <- function(x) {
    betas[1] + betas[2] * x
  }
  
  # Prediction and mean absolute error out-of-sample
  predictionsL1 <- linearRegressionL1(test$X)
  errorOutOfSampleL1 <- mean(abs(test$Y - predictionsL1))
  
  # Mean absolute error in sample
  errorInSampleL1 <- mean(abs(train$Y - linearRegressionL1(train$X)))
  
  return(list(betas, errorOutOfSampleL1, errorInSampleL1))
}

# Setting the seed
set.seed(191934)

# Basic scatter plot
ggplot(df, aes(x=X, y=Y)) + geom_point() + theme_bw()

resultMQO <- list()
resultCustom <- list()
n <- nrow(df)
beta0MQO <- rep(0, n)
beta1MQO <- rep(0, n)
errorOutMQO <- rep(0, n)
errorInMQO <- rep(0, n)
beta0Custom <- rep(0, n)
beta1Custom <- rep(0, n)
errorOutCustom <- rep(0, n)
errorInCustom <- rep(0, n)

# e) repeat 100 times the regressions
for (i in 1:n)
{
  # a) Splitting data in two groups: train 85% and test 15%
  index <- sample(1:n, size = round(0.85*n), replace=FALSE)
  train <- df[index ,]
  test <- df[-index ,]
  
  resultMQO[[i]] <- MQOLinearRegression(train, test)
  beta0MQO[i] <- resultMQO[[i]][[1]][1]
  beta1MQO[i] <- resultMQO[[i]][[1]][2]
  errorOutMQO[i] <- resultMQO[[i]][[2]]
  errorInMQO[i] <- resultMQO[[i]][[3]]
  
  resultCustom[[i]] <- customLinearRegression(train, test)
  beta0Custom[i] <- resultCustom[[i]][[1]][1]
  beta1Custom[i] <- resultCustom[[i]][[1]][2]
  errorOutCustom[i] <- resultCustom[[i]][[2]]
  errorInCustom[i] <- resultCustom[[i]][[3]]
}

# f) plots

ggplot() + geom_boxplot(data = data.frame(beta = c(beta0MQO, beta0Custom), 
                                          method = c(rep("beta0MQO", 100), 
                                                     rep("beta0Custom", 100))),
                        mapping = aes(y = beta, fill = method)) + theme_bw()

ggplot() + geom_boxplot(data = data.frame(beta = c(beta1MQO, beta1Custom), 
                                          method = c(rep("beta1MQO", 100), 
                                                     rep("beta1Custom", 100))),
                        mapping = aes(y = beta, fill = method)) + theme_bw()

ggplot() + geom_boxplot(data = data.frame(error = c(errorOutMQO, errorOutCustom), 
                                          method = c(rep("errorOutMQO", 100), 
                                                     rep("errorOutCustom", 100))),
                        mapping = aes(y = error, fill = method)) + theme_bw()

ggplot() + geom_boxplot(data = data.frame(error = c(errorInMQO, errorInCustom), 
                                          method = c(rep("errorInMQO", 100), 
                                                     rep("errorInCustom", 100))),
                        mapping = aes(y = error, fill = method)) + theme_bw()

#g) mean calculation
mean(errorInMQO)
mean(errorOutMQO)
mean(errorInCustom)
mean(errorOutCustom)





