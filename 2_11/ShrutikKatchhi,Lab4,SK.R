########
# Name: Shrutik Katchhi
# Lab 2: Sampling
#

install.packages("moments")
library(moments)

# A new function to create a new function which calculates mean, median etc...

printVecInfo = function ( myVector )
{
 
 print(paste("Mean: ", mean(myVector)))
 print(paste("Median: ", median(myVector)))
 print(paste("Minimum: ", min(myVector)))
 print(paste("Maximum: ", max(myVector)))
 print(paste("Standatd Deviation: ", sd(myVector)))
 print(paste("Quantile at 0.05: ", quantile(myVector, prob = c(0.05,0.95))))
 print(paste("Skewness: ", skewness(myVector)))
}

printVecInfo (c(1,2,3,4,5,6,7,8,9,10,50))



#################################

# Step 2:

################################




jartempred <- c(replicate(50,c("red"),simplify = TRUE)) # Generate 50 red Samples
jartempblue <- c(replicate(50,c("blue"),simplify = TRUE)) # Similarly for blue

jartemp <- c(jartempred,jartempblue) #Combining the samples
jartemp

jar <- jartemp


# Checking the number of reds

jreds <- jartemp[jartemp == "red"]  
length(jreds)


#Sampling 10 marbles
jars1 <- sample(jartemp, size = 10, replace = TRUE)

#Number of Red marbles
len1 = length(jars1[jars1 == "red"])
len1
print(paste0("Percentage of Reds in 10 samples: ",(len1/length(jars1))*100))

samplejars = sample(jartemp, size = 100, replace = TRUE)



# Sampling 20 times and sample size: 10
jarsmean <- replicate(20
                      ,mean(length(sample(jartemp, size = 10, replace = TRUE)[sample(jartemp, size = 100, replace = TRUE) == "red"]))
                      ,simplify = TRUE
                      )


jarsmean

printVecInfo(jarsmean)
hist(jarsmean)


# Sampling 20 times and sample size: 100
jarsmean1 <- replicate(20
                      ,mean(length(sample(jartemp, size = 100, replace = TRUE)[sample(jartemp, size = 100, replace = TRUE) == "red"]))
                      ,simplify = TRUE
)

printVecInfo(jarsmean1)
hist(jarsmean1)



# Sampling 100 times and sample size: 100
jarsmean2 <- replicate(100
                       ,mean(length(sample(jartemp, size = 100, replace = TRUE)[sample(jartemp, size = 100, replace = TRUE) == "red"]))
                       ,simplify = TRUE
                       )

printVecInfo(jarsmean2)
hist(jarsmean2)

####################

# Step 3

###################


temp1 <- airquality # Copying the Dataset
temp1

str(temp1)
fix(temp1)

temp1 <- na.omit(temp1) # Removing all the NAs

str(temp1)

#Ozone Details and Histogram
printVecInfo(temp1$Ozone)
hist(temp1$Ozone)

#Wind
printVecInfo(temp1$Wind)
hist(temp1$Wind)

#Temperature
printVecInfo(temp1$Temp)
hist(temp1$Temp)

abc = 5
cat("AAA: ", abc)



##################################################

a = c("a","b")
a
a[a=="b"]
