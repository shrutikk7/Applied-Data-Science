install.packages("kernlab")   
library("kernlab")

install.packages("ggplot2")
library("ggplot2")

install.packages("e1071")
library("e1071")

install.packages("gridExtra")
library(gridExtra)

install.packages("caret")
library("caret")

df <- read.csv(file = "C:/Users/Shrutik/Desktop/Spring 2016/IST 687/4.7/Food_Service_Establishment__Last_Inspection.csv", sep = ",")

str(df)
fix(df)

df$ViolationsBool <- 1

head(df[,4])
  for (i in 1:length(df))
{
  if(df[i,4] == 'No violations found.')
     {
     df$ViolationsBool[i] = 0
     }
  else
  {
    df$ViolationsBool[i] = 1
  }
}

fix(df)

# Removing all the NAs and Replacing by 0: 
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- 0
}


# Dividing the data into test and train data:

randIndex <- sample(1:dim(df)[1])
cutPoint2_3 <-floor(2*dim(df)[1]/3)

trainData <- df[randIndex[1:cutPoint2_3],]
testData <- df[randIndex[(cutPoint2_3+1):dim(df)[1]],]



# SVM
model.svm = svm(ViolationsBool ~ TOTAL...CRITICAL.VIOLATIONS + TOTAL...NONCRITICAL.VIOLATIONS, data = trainData)
predict1 <- predict(model.svm, testData)
summary(predict1)
error1 <- testData$ViolationsBool - predict1
sqrt(mean(error1^2))
# Scatter Plot:
g1 = ggplot(data = testData, aes(x=TOTAL...CRITICAL.VIOLATIONS,y=TOTAL...NONCRITICAL.VIOLATIONS)) + geom_point(aes(size=error1, col = error1))


# KSVM

model.ksvm <- ksvm(ViolationsBool ~ TOTAL...CRITICAL.VIOLATIONS + TOTAL...NONCRITICAL.VIOLATIONS, data = trainData)
predict2 <- predict(model.ksvm, testData)
summary(predict2)
error2 <- testData$ViolationsBool - predict2
sqrt(mean(error2^2))

# Scatter Plot:

g2 = ggplot(data = testData, aes(x=TOTAL...CRITICAL.VIOLATIONS,y=TOTAL...NONCRITICAL.VIOLATIONS)) + geom_point(aes(size=error2, col = error2))



# Linear Model:

model.lm = lm(ViolationsBool ~ TOTAL...CRITICAL.VIOLATIONS + TOTAL...NONCRITICAL.VIOLATIONS, data = trainData)
predict3 <- predict(model.lm, testData)
summary(predict3)
error3 = testData$ViolationsBool - predict3
sqrt(mean(error3^2))

# Scatter Plot:
g3 = ggplot(data = testData, aes(x=TOTAL...CRITICAL.VIOLATIONS,y=TOTAL...NONCRITICAL.VIOLATIONS)) + geom_point(aes(size=error3, col = error3))

grid.arrange(g1,g2,g3)


# All the prediction models gives us a similar kind of output
# maybe because we have just two levels, a scatter plot isn't the best way to
# visualiza he data
# 
# Lets try a different KSVM Model:
# 
# 
# KSVM
fix(df)

model.ksvm1 <- ksvm(ViolationsBool ~ ZIP.CODE + DESCRIPTION, data = trainData,kernel="rbfdot",
                    kpar="automatic",C=50,cross=3,prob.model=TRUE)


predict21 <- predict(model.ksvm1, testData,type="votes")
summary(predict21)
error21 <- testData$ViolationsBool - predict21
sqrt(mean(error21^2))

# Scatter Plot:

g21 = ggplot(data = testData, aes(x=ZIP.CODE,y=DESCRIPTION)) + geom_point(aes(col = error21))

# If we look closely at the plot, we can have a ,uch better view about what kind of 
#  Food services does most violations occur.
#  


testpredict = format(round(predict(model.svm,testData)))
confusionMatrix(testpredict,testData$ViolationsBool)




testpredict = format(round(predict(model.ksvm1,testData)))
confusionMatrix(testpredict,testData$ViolationsBool)


# I can't get the confusion matrix to work, and if this is correct, I do not know how it works.
