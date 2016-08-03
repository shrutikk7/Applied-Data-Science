install.packages("kernlab")   
library("kernlab")

install.packages("ggplot2")
library("ggplot2")

install.packages("e1071")
library("e1071")

install.packages("gridExtra")
library(gridExtra)


airquality


aq = airquality # Reading the data
str(aq)

# Removing all the NAs and Replacing by mean: 
for(i in 1:ncol(aq)){
  aq[is.na(aq[,i]), i] <- mean(aq[,i], na.rm = TRUE)
}



randIndex <- sample(1:dim(aq)[1])
cutPoint2_3 <-floor(2*dim(aq)[1]/3)

trainData <- aq[randIndex[1:cutPoint2_3],]
testData <- aq[randIndex[(cutPoint2_3+1):dim(aq)[1]],]

head(trainData)
head(testData)

str(trainData)
str(testData)
#require(e1071)
#model.svm <- svm(Ozone ~ Solar.R , data=aq)
#predictY(model.svm, df)

# KSVM

model.ksvm <- ksvm(Ozone ~., data=trainData)
predict1 <- predict(model.ksvm, testData)
predict(model.ksvm,testData)
summary(predict1)

error <- testData$Ozone - predict1
sqrt(mean(error^2))

# Scatter Plot:

g1 = ggplot(data = testData, aes(x=Temp,y=Wind)) + geom_point(aes(size=error, col = error))

# SVM

model.svm = svm(Ozone ~ ., data = trainData)
predict2 <- predict(model.svm, testData)
summary(predict2)
error2 <- testData$Ozone - predict2
sqrt(mean(error2^2))
# Scatter Plot:
g2 = ggplot(data = testData, aes(x=Temp,y=Wind)) + geom_point(aes(size=error2, col = error2))

# Linear Model:

model.lm = lm(Ozone ~ ., data = trainData)
predict3 <- predict(model.lm, testData)
summary(predict3)
error3 = testData$Ozone - predict3
sqrt(mean(error3^2))
# Scatter Plot:
g3 = ggplot(data = testData, aes(x=Temp,y=Wind)) + geom_point(aes(size=error3, col = error3))

grid.arrange(g1,g2,g3)

meanOzone = mean(aq$Ozone)
meanOzone

aq[2,1]

for ( i in 1:length(aq$Ozone))
{
  if( aq[i,1] < meanOzone )
  {
    aq[i,7] = '0'
  }
  else
  {
    aq[i,7] = '1'
  }
}
  
str(aq)

newaq = aq

randIndex <- sample(1:dim(newaq)[1])
cutPoint2_3 <-floor(2*dim(newaq)[1]/3)

trainData2 <- newaq[randIndex[1:cutPoint2_3],]
testData2 <- newaq[randIndex[(cutPoint2_3+1):dim(newaq)[1]],]

head(trainData2)
head(testData2)

str(trainData2)
str(testData2)

# KSVM
str(trainData2)

model.ksvm2 <- ksvm(V7 ~ .,trainData2)
predict1_2 <- predict(model.ksvm2, testData2)
predict(model.ksvm2,testData2)
summary(predict1_2)
str(predict1_2)
error_1 <- as.numeric(testData2$V7) - as.numeric(predict1_2)
sqrt(mean(error_1^2))

#predict1_2 = as.character(predict1_2)

#Accuracy


results <- table(predict1_2, testData2$V7)
print(results)
percentCorrect <- (results[1,1]+results[2,2])/(results[1,1]+results[1,2]+results[2,1]+results[2,2])*100
percentCorrect


str(testData2)


# Scatter Plot:

g_1 = ggplot(data = testData2, aes(x=Temp,y=Wind)) + geom_point(aes(size=error_1,shape = predict1_2, col = testData2$V7))



# SVM


model.svm2 <- svm(V7 ~ ., trainData2, type = "C-classification")
predict1_3 <- predict(model.svm2, testData2)

summary(predict1_3)

error_2 <- as.numeric(testData2$V7) - as.numeric(predict1_3)
sqrt(mean(error_2^2))


#Accuracy

str(predict1_3)
results2 <- table(predict1_3, testData2$V7)
print(results2)
percentCorrect2 <- (results[1,1]+results[2,2])/(results[1,1]+results[1,2]+results[2,1]+results[2,2])*100
percentCorrect2


# Scatter Plot:

g_2 = ggplot(data = testData2, aes(x=Temp,y=Wind)) + geom_point(aes(size=error_2,shape = predict1_3, col = testData2$V7))





# Naive Bayes


model.nb <- naiveBayes(as.factor(V7) ~ ., trainData2)

predict1_4 <- predict(model.nb, testData2)
str(predict1_4)
summary(predict1_4)

error_3 <- as.numeric(testData2$V7) - as.numeric(predict1_4)
sqrt(mean(error_3^2))


#Accuracy

results3 <- table(predict1_4, testData2$V7)
print(results3)
percentCorrect3 <- (results[1,1]+results[2,2])/(results[1,1]+results[1,2]+results[2,1]+results[2,2])*100
percentCorrect3


# Scatter Plot:

g_3 = ggplot(data = testData2, aes(x=Temp,y=Wind)) + geom_point(aes(size=error_3,shape = predict1_4, col = testData2$V7))



grid.arrange(g_1,g_2,g_3)


# Naive Bayes is the best model according to me followed by KSVM and then SVM.
# Because Naive Bayes gives the least error rate among the three algorithms in this case
# Different Models may suite different needs
