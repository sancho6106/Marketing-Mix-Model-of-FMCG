install.packages("caret")
install.packages("MASS")
install.packages("forecast", dependencies = TRUE)
install.packages("MLmetrics")
install.packages("leaps")
library(leaps)
library(MLmetrics)
library(forecast)
library(caret)
library(readxl)
library(readxl)
Marketing_Mix <- read_excel("Marketing_Mix.xlsx")
View(Marketing_Mix)
summary(Marketing_Mix)

# create detect outlier function
detect_outlier <- function(x) {
  Quantile1 <- quantile(x, probs=.25)
  Quantile3 <- quantile(x, probs=.75)
  IQR = Quantile3-Quantile1
  x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)
}

remove_outlier <- function(dataframe,
                            columns=names(dataframe)) {
  for (col in columns) {
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  
  # return dataframe
  print("Remove outliers")
  print(dataframe)
}
zero_outlier <- remove_outlier(Marketing_Mix, c(8))
nrow(zero_outlier)

##Data Normalization
pre_proc_val <- preProcess(zero_outlier[-1], method = c("center", "scale"))

total_scale = predict(pre_proc_val, zero_outlier[-1])

##dividing data into training and validation set
set.seed(1)
sample_data <- sample(c(1:647), 457)
train_data <- total_scale[sample_data, ]
test_data <- total_scale[-sample_data, ]

##Building the regression Model proper
mmm_lm <- lm(Sales~., data = train_data)
summary(mmm_lm)
## using the built model to predict sales.
MMM_pred <- predict(mmm_lm, test_data)
summary(MMM_pred)

## residual
Residuals <- test_data$Sales[1:20] - MMM_pred[1:20]
outcome_table<-data.frame("Predicted" = MMM_pred[1:20], "Actual"=test_data$Sales[1:20], "Residual"= Residuals[1:20])

##Evaluation of the model
accuracy(MMM_pred, test_data$Sales)
R2_Score(MMM_pred, test_data$Sales)
######>>>>>>>Feature Engineering<<<<<<<<<<<<<
###Best subset selection methods

##Exhaustive Search Method
search <- regsubsets(Sales ~ ., data = train_data, nbest = 1, nvmax = 8,
                     method = "exhaustive")
sum <- summary(search)
class(sum)

names(sum)
 
sum$which

sum$rsq
sum$adjr2
sum$cp
plot(search)  
plot(search, scale="adjr2", main = "Best Subset using Exhaustive Search Method") # "Cp")

##Backward method
search_backward <- regsubsets(Sales ~ ., data = train_data, nbest = 1, nvmax = 8,
                     method = "backward")
sum_back <- summary(search_backward)
class(sum_back)

names(sum_back)

sum_back$which

sum_back$rsq
sum_back$adjr2
sum_back$cp
plot(search_backward)  
plot(search_backward, scale="adjr2", main = "Best Subset using Backward Selection Method") # "Cp")

## forward Method
search_forward <- regsubsets(Sales ~ ., data = train_data, nbest = 1, nvmax = 8,
                              method = "forward")
sum_forward <- summary(search_forward)
class(sum_forward)

names(sum_forward)

sum_forward$which

sum_forward$rsq
sum_forward$adjr2
sum_forward$cp
plot(search_forward)  
plot(search_forward, scale="adjr2", main = "Best Subset using forward Selection Method") # "Cp")


### selecting the best features and fit the model again
best_train_feat <- train_data[c(-5,-9)]
best_test_feat <- test_data[c(-5,-9)]

mmm_lm_best <- lm(Sales~., data = best_train_feat)
summary(mmm_lm_best)
## using the built model to predict sales.
MMM_pred_feat <- predict(mmm_lm_best, best_test_feat)
summary(MMM_pred_feat)
accuracy(MMM_pred_feat, best_test_feat$Sales)
R2_Score(MMM_pred_feat, best_test_feat$Sales)

Table<- data.frame("actual" = best_test_feat$Sales, "predicted" = MMM_pred_feat,"error"=
             best_test_feat$Sales - MMM_pred_feat)

Table$index = 1:nrow(Table)

## A line Chart of Actual and Predicted value of Sales
Table
plot(x = Table$index, y = Table$actual, type = "l", col = "red")
lines(Table$index, Table$predicted, col = "blue", type = "l", pch = "*")
legend(170,5, legend = c("Actual", "Predicted"), col = c("Blue", "Red"), pch = c("*", "+"), cex= 0.5)
title(main = "A line Chart of Actual and Predicted value of Sales" )
