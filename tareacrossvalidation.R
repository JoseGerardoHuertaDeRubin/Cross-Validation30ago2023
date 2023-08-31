install.packages("readxl")

library(readxl)
kidneydata <- read_excel("3er semestre/Tópicos selectos/kidneydata.xlsx")
View(kidneydata)

m1 = lm(kidneydata$tot ~ kidneydata$age , data = kidneydata)
summary(m1)

hist(kidneydata$tot)

n = dim(kidneydata)[1]

# Set the sizes of the test and training samples.
# We use 20 % of the data for testing:
ntest <- round(0.2*n)
ntrain <- n - ntest

# Split the data into two sets:
train_rows <- sample(1:n, ntrain)
kd_train <- kidneydata[train_rows,]
kd_test <- kidneydata[-train_rows,]

# Fit model to training set:
m2 <- lm(kd_train$tot ~  kd_train$age, data = kd_train)
# Evaluate on test set:
rmse2 <- sqrt(mean((predict(m2, kd_test) - kd_test$tot)^2))
rmse2


#compare with actual model, how much the result is improved
rmse1 <- sqrt(mean((predict(m1, kd_test) - kd_test$tot)^2))
rmse1

########################################################

install.packages("caret")
library(caret)

#first define the method of cross-validation
tc1 <- trainControl(method = "LOOCV") #it can be replace by LGOCV

tc2 <- trainControl(method = "cv" , number = 5) #k-fold CV
tc3 <- trainControl(method = "repeatedcv",
                   number = 10, repeats = 20) # repeated k-fold

tc4 <- trainControl(method = "stratifiedKFold", number = 5) # stratified Cv

library(readxl)
kidneydata <- read_excel("3er semestre/Tópicos selectos/kidneydata.xlsx")

# then we define our prediction method using training data
m5 <- train(tot ~ .,
           data = kidneydata,
           method = "lm",
           trControl = tc3)

summary(m5)
#plot(m)

qqnorm(m5$finalModel$residual)
qqline(m5$finalModel$residual)

hist(m5$finalModel$residuals)

plot(m5$finalModel$fitted.values, m5$finalModel$residual)
abline(h = 0, col="red")


balancedFolds <- function(data, k) {
  # Calculate the number of samples in each fold
  samples_per_fold <- ceiling(nrow(data) / k)
  
  # Create a vector to store the fold assignment
  fold_assignment <- rep(1:k, each = samples_per_fold)
  fold_assignment <- fold_assignment[1:nrow(data)]  # Truncate to the number of samples
  
  return(fold_assignment)
}

tc <- trainControl(
  method = "cv", 
  number = 5, 
  index = list(Folds = balancedFolds(DATA, 5))
)
