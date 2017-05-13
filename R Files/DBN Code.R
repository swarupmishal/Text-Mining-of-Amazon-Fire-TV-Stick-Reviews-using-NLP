library(darch)

#Read Final CSV
getwd()
setwd("D:/Spring 2017/ADS/Final Project")
final <-read.csv(file="final_temp.csv", header=TRUE, sep=",")
View(final)

??dataSet

# Divided the data with train data and test data keeping last 10000 records for test data.
train<-final[1:4000,]
test<-final[4001:5000,]
ncol(train)
train$RESULT <- as.factor(train$RESULT)

model <- darch(train$RESULT ~ ., final, generateWeightsFunction = "generateWeightsNormal",
               weights.mean = .1, weights.sd = .05)
darch  <- darch(train[,1:1377], train$RESULT, train, rbm.numEpochs = 10, rbm.batchSize = 100, rbm.trainOutputLayer = F, layers = c(75,100,10), darch.batchSize = 100, darch.learnRate = 2, darch.retainData = F, darch.numEpochs = 20 )
predictions <- predict(darch, test[,1:1377], type="class")
View(predictions)
