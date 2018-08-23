setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\LDA")
########################## LDA #############################################
library(MASS)
library(caTools)
library(ISLR)
library(glmnet)
library(caret)
cusCatData<-read.csv("custCategory.csv", header = TRUE, stringsAsFactors = FALSE)
head(cusCatData)
dim(cusCatData)

cusCatData$gender<-as.factor(cusCatData$gender)
cusCatData$custcat<-as.factor(cusCatData$custcat)
cusCatData$retire<-as.factor(cusCatData$retire)
cusCatData$marital<-as.factor(cusCatData$marital)
cusCatData$reside<-as.factor(cusCatData$reside)

#Detect NAs
detectNAs<-function(x){
  return(sum(is.na(x)))
}
lapply(cusCatData, detectNAs)




set.seed(100) # set seed to replicate results
split<-sample.split(cusCatData$custcat, SplitRatio=0.8)
train_set<-subset(cusCatData, split==TRUE)
test_set<-subset(cusCatData, split==FALSE)

dim(train_set)
dim(test_set)


################### Prediction on whole dataset #################
cat("\014")
lda.fit<-lda(custcat ~ . ,data=cusCatData)
lda.fit
summary(lda.fit)

predTrain<-predict(lda.fit, newdata=cusCatData)

conf<-table(predicted=predTrain$class, actual=cusCatData$custcat)
accuracy<-sum(diag(conf))/sum(conf)
accuracy


##### Prediction by using K fold ###########################
#train_control<- trainControl(method="cv", number=10)
lda.fit = train(custcat ~ ., data=cusCatData, method="lda",
                trControl = trainControl(method = "cv", number=10))
summary(lda.fit)

predTrain = predict(lda.fit, newdata=test_set) 
conf <- table(test_set$custcat, predTrain)
accuracy<-sum(diag(conf))/sum(conf)
paste0('Accuracy of Churn Prediction is ', accuracy)
print("End cust category prediction-------------")


######################## Churn Prediction ############################
print("Starting Churn Prediction-------------")
churnData<-read.csv("telComm.csv", header = TRUE, stringsAsFactors = FALSE)
dim(churnData)
set.seed(100) # set seed to replicate results
split<-sample.split(churnData$churn, SplitRatio=0.8)
train_set<-subset(churnData, split==TRUE)
test_set<-subset(churnData, split==FALSE)


checkClass<-function(x){
  return(class(x))
}
sapply(churnData,checkClass)

churnData$region<-as.factor(churnData$region)
churnData$gender<-as.factor(churnData$gender)
churnData$custcat<-as.factor(churnData$custcat)
churnData$retire<-as.factor(churnData$retire)
churnData$marital<-as.factor(churnData$marital)
churnData$multine<-as.factor(churnData$multine)
churnData$pager<-as.factor(churnData$pager)
churnData$internet<-as.factor(churnData$internet)
churnData$callid<-as.factor(churnData$callid)
churnData$callwait<-as.factor(churnData$callwait)
churnData$forward<-as.factor(churnData$forward)
churnData$confer<-as.factor(churnData$confer)
churnData$voice<-as.factor(churnData$voice)
churnData$ebill<-as.factor(churnData$ebill)
churnData$churn<-as.factor(churnData$churn)
churnData$retire<-as.factor(churnData$retire)
churnData$reside<-as.factor(churnData$reside)
churnData$tollfree<-as.factor(churnData$tollfree)
churnData$callcard<-as.factor(churnData$callcard)
churnData$equip<-as.factor(churnData$equip)
churnData$wireless<-as.factor(churnData$wireless)



x=model.matrix(churn~., churnData)[,-1]
y=churnData$churn
train<-sample(1:nrow(x),nrow(x)/2)
test<--train
grid<-10^seq(10, -2, length=100)
##Fit the Lasso model
lasso.fit=glmnet(x[train,], y[train], family="binomial", alpha=1, lambda=grid)
plot(lasso.fit)
##Choose the best value of lambda by cross validation for predicting
set.seed(1)
cv.out<-cv.glmnet(x[train,], y[train],alpha=1, family="binomial")
plot(cv.out)
bestLambda=cv.out$lambda.min
bestLambda

lasso.pred=predict(lasso.fit, s=bestLambda, newx = x[test,])
mean((lasso.pred-y[test])^2)

out=glmnet(x,y,alpha=1, lambda=grid, family="binomial")
lasso.coeff=predict(out, type="coefficients", s=bestLambda)[1:36,]
lasso.coeff[lasso.coeff!=0]



##### Model ###########################
lda.fit<-lda(churn ~ (tenure+emply+equip+callcard+voice) ,data=train_set)
lda.fit
summary(lda.fit)

predTrain<-predict(lda.fit, newdata=test_set)
conf<-table(test_set$churn,predTrain$class)
accuracy=sum(diag(conf))/sum(conf)
paste0('Accuracy of Churn Prediction is ', accuracy)