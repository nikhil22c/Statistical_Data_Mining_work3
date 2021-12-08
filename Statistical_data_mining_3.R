#Assignment 3
#decision tree
install.packages("ISLR")
library(ISLR)
data(package="ISLR")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
original_df2 <- read.csv("data2.csv", header = TRUE)
hist(original_df2$Property)
Property_cat = ifelse(original_df2$Property<=7.9, "No", "Yes")
original_d = data.frame(original_df2, Property_cat)
original_d=original_d[,-1]
fit<-rpart(Property_cat~.,original_d)
rpart.plot(fit)
summary(fit)
plot(fit)
text(fit,pretty=1)
#Training
set.seed(1001)
train=sample(1:nrow(original_d),70)
fit.original_d<-rpart(Property_cat~.,original_d,subset=train)
#features used
printcp(fit)
#Let's name the data differently to keep it easier to remember
data_test=original_d[-train,]
data_train=original_d[train,]
#Get a table for the test accuracy
tree.pred=predict(fit.original_d,data_test,type="class")
with(data_test,table(tree.pred,Property_cat))

accuracy_tune<-function(fit){
  predict_unseen<-predict(fit,data_test,type='class')
  table_mat<-table(data_test$Property_cat,predict_unseen)
  accuracy_Test<-sum(diag(table_mat))/sum(table_mat)
  accuracy_Test
}
#Allow us to change the parameters (minsplit,minbucket,maxdepth,cp) and see how the model changes
control<-rpart.control(minsplit=4,minbucket=2,maxdepth=3,cp=0)
tune_fit<-rpart(Property_cat~.,data=data_train,method='class')
accuracy_tune(tune_fit)
#Plotting the accuracy
plot(fit, uniform=TRUE,main="Classification Tree for Property")
text(fit, use.n=TRUE, all=TRUE, cex=0.7)
conf_mtx_dtree = confusionMatrix(as.factor(tree.pred),as.factor(data_test$Property_cat))
conf_mtx_dtree

#######################  Bagging  #################################################
cvcontrol<-trainControl(method="repeatedcv",number=10,allowParallel=TRUE)
train.bagg=train(as.factor(Property_cat)~.,data=data_train,method="treebag",trControl=cvcontrol,importance=TRUE)
train.bagg
plot(varImp(train.bagg))
#changing the value of B
train.bagg = train(as.factor(Property_cat)~.,data=data_train,method="treebag",trControl=cvcontrol,importance=TRUE,nbagg=10)
train.bagg
conf_mtx_bag = confusionMatrix(as.factor(predict.train(train.bagg,data_test)),as.factor(data_test$Property_cat))
conf_mtx_bag
########################  RandomForest #########################################
library(randomForest)
train.rf = train(as.factor(Property_cat)~.,data=data_train,method="rf",trControl=cvcontrol,importance=TRUE)
train.rf
conf_mtx_rf = confusionMatrix(as.factor(predict.train(train.rf,data_test)),as.factor(data_test$Property_cat))
conf_mtx_rf
######################## Random Forest Boosting #########################################
library(gbm)
train.gbm= train(as.factor(Property_cat)~.,data=data_train,method="gbm",verbose = F,trControl=cvcontrol)
train.gbm
conf_mtx_gbm = confusionMatrix(as.factor(predict.train(train.gbm,data_test)),as.factor(data_test$Property_cat))
conf_mtx_gbm$byClass
summary(train.gbm)
#Random Forest Regression, using sales values, instead of just category
#Would want to split into training and test data first
set.seed(1001)
property=original_df2$Property
descriptors=original_df2[,2:9]
model = randomForest(property~.,data=descriptors,mtry=8,importance=TRUE,na.action=na.omit)
pred<-predict(model,descriptors)


test_pred  = predict(model,test_x)
plot(property,property_pred)
print(model)
plot(model)

conf_mtx_dtree = confusionMatrix(as.factor(tree.pred),as.factor(data_test$Property_cat))