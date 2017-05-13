#SVM Regression

# Divided the data with train data and test data keeping last 10000 records for test data.
train<-
test<-

#Tuning SVM for finding best parameters for all possible 4 kernels.
obj<-tune(svm, Response~.,kernel ="radial", data= train, ranges=list(gamma=10^(-2:2), cost=10^(-2:2)))
summary(obj)

obj1<-tune(svm, Response~.,kernel ="linear", data= train, ranges=list(cost=10^(-2:2)))
summary(obj1)

obj2<-tune(svm, Response~.,kernel ="sigmoid", data= train, ranges=list(gamma=10^(-2:2), cost=10^(-2:2)))
summary(obj2)

obj3<-tune(svm, Response~.,kernel ="polynomial", data= train, ranges=list(gamma=10^(-2:2), cost=10^(-2:2)))
summary(obj3)

#Applying SVM with the best parameters we found after tuning. 
model1 = svm(Response ~ ., kernel = "radial", cost =1 ,gamma=0.01, data = train, scale = F)

# Predicting the Values using model created using SVM.
predictions <-  predict(model1, test)

#Validating accuracy of predicted results.
accuracy(predictions,test$Response)