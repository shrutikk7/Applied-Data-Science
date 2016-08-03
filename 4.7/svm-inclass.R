install.packages("e1071")
install.packages("kernlab")

#http://www.svm-tutorial.com/2014/10/support-vector-regression-r/

#read in a simple datafile (of X and Y)
file <- "/Users/jsaltz/Google Drive/Courses/IST 687/Spring - 15/Week 11 - svm/simpleData.csv"
df <- read.csv(file)
colnames(df) <- c("X", "Y")

# Plot the data
plot(df)

# Create a linear regression model
model.lm <- lm(Y ~ X, data=df)

# Add the fitted line
abline(model.lm)

#see how good the model is
predictY(model.lm, df)



######################
# now try svm
##############
require(e1071)
model.svm <- svm(Y ~ X , data=df)
predictY(model.svm, df)

######################
# now try ksvm
##############
require(kernlab)
model.ksvm <- svm(Y ~ X , data=df)
predictY(model.ksvm, df)

#############################################
#use the model & df to predict Y
#return the Root mean squared error and plot the results
#############################################
predictY <- function(model, df) {
  predictedY <- predict(model, data=df$X)

  plot(df)
  points(df$X, predictedY, col = "red", pch=4)

  error <- df$Y - predictedY
  predictionRMSE <- rmse(error)
  return(predictionRMSE)
}

#root mean squared error
rmse <- function(error)
{
  sqrt(mean(error^2))
}


######################
# now try nb (from e1071)
#doesn't work - predicts probabilities of a categorical class variable
##############
model.nb <- naiveBayes(Y ~ X, data = df)
tmp <- data.frame(df$X)
predictedY.nb <- predict(model.nb, tmp)










## Example of using a contingency table:
load("/Users/jsaltz/Downloads/titanic.raw.rdata")
titanic <- titanic.raw

#try the different models
model.glm <- glm(Survived ~ ., family = binomial, data = titanic)
survived.gml <- predict(model.glm, list(Class = titanic$Class, Sex = titanic$Sex, Age = titanic$Age),type="response")
predictedSurvived <- round(survived.gml)
results <- table(predictedSurvived, titanic$Survived)
print(results)
percentCorrect <- (results[1,1]+results[2,2])/(results[1,1]+results[1,2]+results[2,1]+results[2,2])*100
percentCorrect


model.ksvm <- ksvm(Survived ~ ., data = titanic)
predictSurvived(model.ksvm, titanic)


model.svm <- svm(Survived ~ ., data = titanic)
predictSurvived(model.svm, titanic)

model.nb <- naiveBayes(Survived ~ ., data = titanic)
predictSurvived(model.nb, titanic)



nrows <- nrow(titanic)
random.indexes <- sample(1:nrows)
cutPoint <- floor(nrows/3*2)
titanic.train <- titanic[random.indexes[1:cutPoint],]
titanic.test <- titanic[random.indexes[(cutPoint+1):nrows],]

model.ksvm.train <- ksvm(Survived ~ ., data=titanic.train)
predictSurvived(model.ksvm.train, titanic.test)

model.svm.train <- svm(Survived ~ ., data = titanic.train)
predictSurvived(model.svm.train, titanic.test)

model.nb.train <- naiveBayes(Survived ~ ., data = titanic.train)
predictSurvived(model.nb.train, titanic.test)



#function to determine accuracy of model
predictSurvived <- function(m, titanic){
  predictedSurvived <- predict(m, titanic)
  results <- table(predictedSurvived, titanic$Survived)
  print(results)
  percentCorrect <- (results[1,1]+results[2,2])/(results[1,1]+results[1,2]+results[2,1]+results[2,2])*100
  round(percentCorrect)  
  return(percentCorrect)
}


