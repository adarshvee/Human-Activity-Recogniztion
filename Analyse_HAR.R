#Clear All
rm(list=ls())
library("klaR")
library("data.table")
library(glmnet)

setwd("G:\\R\\AlgLearningTheory\\Project\\UCI HAR Dataset\\UCI HAR Dataset")
features <- read.table("features.txt")
act_labels <- read.table("activity_labels.txt")
train_features <- read.table("train\\X_train.txt")
train_labels <- read.table("train\\y_train.txt")
test_features <- read.table("test\\X_test.txt")
test_labels <- read.table("test\\y_test.txt")

subject_train <- read.table("train//subject_train.txt")
subject_test <- read.table("test//subject_test.txt")


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


#BayesModel1 <- NaiveBayes(formula(form), data=train_dataset)
BayesModel1 <- NaiveBayes(formula =Activity ~ ., data=train_dataset)
print(summary(BayesModel1))

pred1 <- predict(BayesModel1, test_dataset)

#
x <- model.matrix(Activity~., train_dataset)[,-1]
y = train_dataset$Activity


glm_model <- glm(formula = Activity ~ ., family = binomial, data = train_dataset,control = list(maxit = 100))

#pred
#predictions <- predict.glm(glm_model,subset(test_dataset, select = -c(Activity)),type= "response")
predictions <- predict.glm(glm_model,test_dataset,type= "response")

cv.out <- cv.glmnet(x,y,alpha=1,family="multinomial",type.measure = "mse" )

#The one that ran
cv.out <- cv.glmnet(x,y,alpha=1,family="multinomial",type.measure = "mse" )
plot(cv.out)
lambda_min <- cv.out$lambda.min
lambda_1se <- cv.out$lambda.1se
coef(cv.out,s=lambda_1se)
#Done, test now

x_test <- model.matrix(Activity~.,test_dataset)
#Lasso probabilities
new.x<-model.matrix(~.,data=test_dataset)
lasso_prob <- predict(cv.out,newx = x_test,s=lambda_1se,type="response")
dim(x_test)
dim(x)
summary(cv.out)
