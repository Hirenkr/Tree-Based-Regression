##Assignment 9

library(ISLR)
library(tree)
library(randomForest)
library(gbm)
attach(Carseats)

set.seed(5)
##(a) Splitting the data
Train = sample(nrow(Carseats),0.7*nrow(Carseats))
Train.data = Carseats[Train,]
Test.data = Carseats[-Train,]

##(b) Fit a regression tree to the training set. Plot the tree, and interpret the results.
## Then compute the test MSE.
tree.train = tree(Sales~.,Train.data)
summary(tree.train)
plot(tree.train)
text(tree.train,pretty = 0.25)
summary(tree.train)

tree.test = predict(tree.train,Test.data)
mean((tree.test-Test.data$Sales)^2)


##(c) Pruning
set.seed(5)
cv.train.sales = cv.tree(tree.train, FUN=prune.tree)
names(cv.train.sales)
cv.train.sales

plot(cv.train.sales$size,cv.train.sales$dev,'b')

###Note: Lowest Misclassification rate is for size =15
prune.train = prune.tree(tree.train,best = 8)
plot(prune.train)
text(prune.train)

tree.test.cv = predict(prune.train,Test.data)
mean((tree.test.cv-Test.data$Sales)^2)

##(d) Bagging
n<-c(200,300,400,500,600,700)
m<-c(0,0,0,0)
par(mfrow = c(3,2))
for (i in 1:6)
{
  bagging.train = randomForest(Sales~.,data =Train.data, mtry=10, ntree =n[i], importance = TRUE)
  bagging.predict = predict(bagging.train,Test.data)
  m[i] <- mean((bagging.predict-Test.data$Sales)^2)
  varImpPlot(bagging.train)  
  importance(bagging.train)
}
m
##Lowest MSE is for ntree =600; i.e. 2.309579
##Computing for minimum MSE
bagging.train = randomForest(Sales~.,data =Train.data, mtry=10, ntree =600, importance = TRUE)
bagging.predict = predict(bagging.train,Test.data)
varImpPlot(bagging.train)  
importance(bagging.train)


##(e) Random Forrest to analyze data
n<-c(200,300,400,500,600,700)
m.rf<-c(0,0,0)
for (i in 1:6)
{
  randomforest.train = randomForest(Sales~.,data =Train.data, mtry=4, ntree =n[i], importance = TRUE)
  randomforest.predict = predict(randomforest.train,Test.data)
  m.rf[i] <- mean((randomforest.predict-Test.data$Sales)^2)
  varImpPlot(randomforest.train)  
  importance(randomforest.train)
}
m.rf
##Lowest MSE is for ntree =600 i.e. 2.690857; The MSE values are greater for Random Forest; 
##Computing for minimum MSE
randomforest.train = randomForest(Sales~.,data =Train.data, mtry=4, ntree =600, importance = TRUE)
randomforest.predict = predict(randomforest.train,Test.data)
varImpPlot(randomforest.train)  
importance(randomforest.train)


##(f) Boosting to analyze data
m.boost<-c(0,0,0)
par(mfrow = c(1,3))
n<-c(200,300,400,500,600,700)
for (i in 1:6)
{
  boost.carseats = gbm(Sales~.,Train.data,distribution = "gaussian",n.trees = n[i],interaction.depth = 4, cv.folds = 10)
  summary(boost.carseats)  
  boost.predict = predict(boost.carseats,Test.data,n.trees = n[i])
  m.boost[i]=mean((boost.predict-Test.data$Sales)^2)
  plot(boost.carseats,i="Price")            ##partial dependence plot
}
m.boost
##Lowest MSE is for ntree =300 i.e. 1.776859; 
##Computing for minimum MSE
boost.carseats = gbm(Sales~.,Train.data,distribution = "gaussian",n.trees = 300,interaction.depth = 4, cv.folds = 10)
summary(boost.carseats)  
boost.predict = predict(boost.carseats,Test.data,n.trees = 300)
