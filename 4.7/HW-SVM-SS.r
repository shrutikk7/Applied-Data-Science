#Shrutik Katchhi


install.packages("kernlab")

library("kernlab")



df <- read.csv(file = "C:/Users/Shrutik/Desktop/Spring 2016/IST 687/4.7/Food_Service_Establishment__Last_Inspection.csv", sep = ",")




df <- na.omit(df)



df$VIOLATIONS <- ifelse(df$VIOLATIONS != "No violations found.",yes = "Yes",no = "No")



df$VIOLATIONS <- factor(df$VIOLATIONS)



RndmIndex <- sample(1:nrow(df))



TrainTestCut <- floor(2*nrow(df)/3)
TrainTestCut



trainData <- df[RndmIndex[1:TrainTestCut],]
testData <- df[RndmIndex[(TrainTestCut+1):nrow(df)],]



#Step 3: Building a Model using KSVM

model.ksvm <- ksvm(VIOLATIONS~TOTAL...CRITICAL.VIOLATIONS+TOTAL...NONCRITICAL.VIOLATIONS,data=trainData,
                kernel="rbfdot",kpar="automatic",C=50,cross=10,prob.model=TRUE)


model.ksvm


#Having a look over the range of support vectors
hist(alpha(model.ksvm)[[1]])


#Tried creating SVM model with different value of regularization paramter i.e from 5 to 50
#and k-fold Cross validation parameter from 2 to 10
#But Training error and cross-validation error remained almost same in all the cases
#Not able to lower the error value any more
#So testing the test data for prediction using the model


#Predicting the variable using model
model.ksvmPred <- predict(model.ksvm,testData,type="votes")


#Creating a new data from to check the ground truth versus prediction
compTable <- data.frame(testData[,27],factor(model.ksvmPred[2,]))


#Renaming columns as GroundTruth and Prediction
colnames(compTable) <- c("GroundTruth","Prediction")


#Printing out the confusion matrix
ConfusionMatrix <- table(compTable)

ConfusionMatrix



#Explanation for confustion matrix
#Prediction has 0 as No and 1 as Yes
#So main diagonal below shows the correctly classified instance
#Prediction identified 2965 non-Violations correctly and 6 incorrectly
#Prediction identified 5899 Violation correctly and 4 incorrectly
#             Prediction
#GroundTruth    0     1
#         No  2965    4
#         Yes   6   5899


#Checking the accuracy of prediction by calculating error rate
#Formula is to sum incorreclty classified instances and divide by total instances
paste("Prediction Error rate = ",round(((ConfustionMatrix[1,2]+ConfustionMatrix[2,1])/nrow(compTable))*100,2),"%")



#Step 4: Creating second model with additional predictors
model.ksvm2 <- ksvm(VIOLATIONS~TOTAL...CRITICAL.VIOLATIONS+TOTAL...NONCRITICAL.VIOLATIONS+INSPECTION.TYPE+LAST.INSPECTED,
                data=trainData,kernel="rbfdot",kpar="automatic",C=50,cross=10,prob.model=TRUE)

#Checking output of OzoneSVM
model.ksvm2


#Having a look over the range of support vectors
hist(alpha(model.ksvm2)[[1]])


#Tried creating SVM model with different value of regularization paramter i.e from 5 to 50
#and k-fold Cross validation parameter from 2 to 10
#But Training error and cross-validation error remained almost same in all the cases
#Not able to lower the error value any more
#So testing the test data for prediction using the model


#Predicting the variable using model
model.ksvmPred2 <- predict(model.ksvm2,testData,type="votes")


#Creating a new data from to check the ground truth versus prediction
compTable2 <- data.frame(testData[,27],factor(model.ksvmPred2[2,]))


#Renaming columns as GroundTruth and Prediction
colnames(compTable2) <- c("GroundTruth","Prediction")


#Printing out the confusion matrix
ConfusionMatrix1 <- table(compTable2)

ConfusionMatrix1

#             Prediction
#GroundTruth    0     1
#         No  2965    4
#         Yes   6   5899
#Explanation for confustion matrix
#Prediction has 0 as No and 1 as Yes
#So main diagonal below shows the correctly classified instance
#Prediction identified 2965 non-Violations correctly and 6 incorrectly
#Prediction identified 5899 Violation correctly and 4 incorrectly




#Difference between 1st and 2nd model
#In the second model added more predictors compared to 1st model like INSPECTION.TYPE, LAST.INSPECTED etc.
#Although, was able to reduce the cross validation error
#But, training error remained same in all cases
#Also, the confustion matrix remained same
#No. of instances identified correctly and incorrectly remained same in both models
#Model 1, itself, is performing very well. So, cannot drive down the error anymore
#Had I chosen some predictors which were not helpful in model1
#I would have then chosen good predictors for model2 and would have drived down the error rate



#Checking the accuracy of prediction by calculating error rate
#Formula is to sum incorreclty classified instances and divide by total instances
paste("Prediction Error rate = ",round(((ConfusionMatrix1[1,2]+ConfusionMatrix1[2,1])/nrow(compTable2))*100,2),"%")


#End of program