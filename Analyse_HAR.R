#Clear All data before starting
rm(list=ls())

#Import required libraries
library("klaR")
library("data.table")
library("glmnet")
library("neuralnet")

#Set working directory. NEEDS TO BE MODIFIED
setwd("G:\\R\\AlgLearningTheory\\Project\\UCI HAR Dataset\\UCI HAR Dataset")


features <- read.table("features.txt")
act_labels <- read.table("activity_labels.txt")
train_features <- read.table("train\\X_train.txt")
train_labels <- read.table("train\\y_train.txt")
test_features <- read.table("test\\X_test.txt")
test_labels <- read.table("test\\y_test.txt")

#Remove bad characters from column names, such as "(",")" and "-"
colnames(nn_train_data) = make.names(colnames(nn_train_data), unique = TRUE, allow_ = TRUE)
colnames(nn_test_data) = make.names(colnames(nn_test_data), unique = TRUE, allow_ = TRUE)


#train_features <- rbind(t(features)[2,], train_features)
#test_features <- rbind(t(features)[2,], test_features)

neatify <- function(x) {
  gsub(",", "", x)
  gsub("(", "", x,fixed=TRUE)
  gsub(")", "", x)
  gsub(">", "", x)
  gsub("-", "", x)
}

features <- data.frame(lapply(features, neatify))


colnames(train_features) <- c(t(features)[2,])
colnames(test_features) <- c(t(features)[2,])


colnames(train_labels) <- c("act")
colnames(test_labels) <- c("act")
colnames(act_labels) <- c("act", "Activity")



train_dataset <- cbind(train_features,train_labels )
test_dataset <- cbind(test_features,test_labels )




train_dataset <- merge(train_dataset, act_labels )
test_dataset <- merge(test_dataset,act_labels )

train_dataset <- subset(train_dataset, select = -c(act))
test_dataset <- subset(test_dataset, select = -c(act))


# #BayesModel1 <- NaiveBayes(formula(form), data=train_dataset)
# BayesModel1 <- NaiveBayes(formula =Activity ~ ., data=train_dataset)
# print(summary(BayesModel1))
# 
# pred1 <- predict(BayesModel1, test_dataset)

#
x <- model.matrix(Activity~., train_dataset)[,-1]
y = train_dataset$Activity

x_test <- model.matrix(Activity~., test_dataset)[,-1]

#Try out glmnet
#https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html#log
fit = glmnet(x, y, family = "multinomial")
summary(fit)
pred_class <- predict(fit, newx = x_test, s = 0,        type="class" )
pred_score <- predict(fit, newx = x_test, s = NULL,        type="response" )


table(pred_class, test_dataset$Activity)
mean(as.character(pred_class) == as.character(test_dataset$Activity))


cv.out <- cv.glmnet(x,y,alpha=1,family="multinomial",type.measure = "mse" )
plot(cv.out)
lambda_1se <- cv.out$lambda.1se
coef(cv.out,s=lambda_1se)
lasso_prob <- predict(cv.out,newx = x_test,s=lambda_1se,type="response")
max_Levels <- colnames(lasso_prob)[apply(lasso_prob,1,which.max)]
table(max_Levels, test_dataset$Activity)
mean(as.character(max_Levels) == as.character(test_dataset$Activity))


layers=c(30,22)
nn_train_data <- train_dataset
nn_test_data <- test_dataset


form <- paste0("Activity ~ ", colnames(nn_train_data)[1])
for (i in 2:561)
   form<-paste0(form," + ", colnames(nn_train_data)[i])



nnModel1<-neuralnet(formula = formula(form),data=nn_train_data, hidden=layers,linear.output=T, err.fct="sse")

# 
# #Attempt logistic with nnnet package
# library(nnet)
# multinomModel <- multinom(Activity ~ ., data=train_dataset, MaxNWts = 6000) # multinom Model
# predicted_scores <- predict (multinomModel, test_dataset, "probs")
# predicted_class <- predict (multinomModel, test_dataset)
# table(predicted_class, test_dataset$Activity)
# mean(as.character(predicted_class) != as.character(test_dataset$Activity))
# mean(as.character(predicted_class) == as.character(test_dataset$Activity))
# 
# glm_model <- glm(formula = Activity ~ ., family = "multinomial", data = train_dataset,control = list(maxit = 100))
# 
# #pred
# #predictions <- predict.glm(glm_model,subset(test_dataset, select = -c(Activity)),type= "response")
# predictions <- predict.glm(glm_model,test_dataset,type= "response")
# summary(predictions)
# typeof(predictions)
# print(table(test_dataset$Activity, predict > 0.5))
# 
# 
# cv.out <- cv.glmnet(x,y,alpha=1,family="multinomial",type.measure = "mse" )
# 
# #The one that ran
# cv.out <- cv.glmnet(x,y,alpha=1,family="multinomial",type.measure = "mse" )
# plot(cv.out)
# lambda_min <- cv.out$lambda.min
# lambda_1se <- cv.out$lambda.1se
# coef(cv.out,s=lambda_1se)
# #Done, test now
# 
# x_test <- model.matrix(Activity~.,test_dataset)
# #Lasso probabilities
# new.x<-model.matrix(~.,data=test_dataset)
# lasso_prob <- predict(cv.out,newx = x_test,s=lambda_1se,type="response")
# dim(x_test)
# dim(x)
# summary(cv.out)
# 
# 
# library(caret)
# model <- train(Activity~., train_dataset, method='glmnet', tuneGrid=expand.grid(.alpha=0:1,    .lambda=0:30/10))
# 
# plot(model)
# coef(model$finalModel, s=model$bestTune$.lambda)

#### Naive Bayes code: ########################################################
rm(list=ls())
#library("klaR")
library("data.table")
library(glmnet)
library(naivebayes)

act_labels <- read.table("activity_labels.txt")
train_features <- read.table("X_train.txt")
train_labels <- read.table("y_train.txt")
test_features <- read.table("X_test.txt")
test_labels <- read.table("y_test.txt")

colnames(train_labels) <- c("act")
colnames(test_labels) <- c("act")
colnames(act_labels) <- c("act", "Activity")
train_dataset <- cbind(train_features,train_labels )
test_dataset <- cbind(test_features,test_labels )

train_dataset <- merge(train_dataset, act_labels )
test_dataset <- merge(test_dataset,act_labels )
train_dataset$act <- NULL
test_dataset$act <- NULL

NBModel <- naive_bayes(Activity ~ ., data=train_dataset)
test <- test_dataset
test$Activity <- NULL
pred <- predict(NBModel, test)
mean(as.character(pred) == as.character(test_dataset$Activity))
##############################################################################
