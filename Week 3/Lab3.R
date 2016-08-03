###
###
###

# States Data:

sdata <- c("http://www.census.gov/popest/data/state/totals/2011/tables/NST-EST2011-01.csv")
sdata

readStates <- function(myVector)
{
  read.csv(url(myVector))
}

dfStates <- readStates(sdata)
dfStates <- data.frame(dfStates)

str(dfStates)

dfStates <- dfStates[-1,-8]
dfStates <- dfStates[,-8]
dfStates <- dfStates[,-7]
dfStates <- dfStates[,-6]

dfStates

str(dfStates)

tail(dfStates)

dfStates <- dfStates[-65,]
tail(dfStates)

dfStates <- dfStates[-61:-65,]
tail(dfStates)
dfStates <- dfStates[-59,]
dfStates <- dfStates[,-6]
dfStates$States <- dfStates[,1]
dfStates
dfStates <- dfStates[,-1]
dfStates

dfStates$States <- gsub("\\.","",dfStates$States)
dfStates <- dfStates[-1:-7,]
 
###
#aaa
aaa <- dfStates
aaa
aaa <- aaa[,-6]

aaa$State <- aaa[,1]
aaa
aaa <- aaa[,-1]
aaa$State <- gsub("\\.","",aaa$State)
str(aaa)
aaa <- aaa[-1:-2,]
aaa <- aaa[-1,]
###


dfStates$stateName <- dfStates[,5]
dfStates$Jul2010 <- dfStates[,1]
dfStates$Jul2011 <- dfStates[,2]
dfStates$base2010 <- dfStates[,3]
dfStates$base2011 <- dfStates[,4]
str(dfStates)
dfStates <- dfStates[,-1:-5]
str(dfStates)


dfStates$Jul2010 <- gsub(",","",dfStates$Jul2010)
dfStates$Jul2011 <- gsub(",","",dfStates$Jul2011)
dfStates$base2010 <- gsub(",","",dfStates$base2010)
dfStates$base2011 <- gsub(",","",dfStates$base2011)
str(dfStates)


dfStates$Jul2010 <- as.numeric(gsub(",","",dfStates$Jul2010))
dfStates$Jul2011 <- as.numeric(gsub(",","",dfStates$Jul2011))
dfStates$base2010 <- as.numeric(gsub(",","",dfStates$base2010))
dfStates$base2011 <- as.numeric(gsub(",","",dfStates$base2011))
str(dfStates)

rownames(dfStates) <- NULL

str(dfStates)

mean(dfStates$Jul2011)

unique(dfStates$stateName)

dfStates <- dfStates[-52,]
dfStates


mean(dfStates$Jul2011)

#Highest Population:
highP <- max(dfStates$Jul2011)
highP
#State with Highest Population:

dfStates[dfStates[,2] == highP,1:2]

#Order in Ascending order:

orderedDf <- order(dfStates$Jul2011)
orderedDf <- dfStates[orderedDf,]
orderedDf

########
# Step 5
########

myNewFunction<-function(myVector,a)
{
  b<-length(myVector)
  myVector<-which(myVector<a)
  length(myVector)*100/b
  return(length(myVector)*100/b)
}

z = c(1,2,3,4,5)
myNewFunction(z,2)
myNewFunction(dfStates$Jul2011,mean(dfStates$Jul2011))






##### Doubt #####
#So I tried using edfc function aswell. But I am getting the answet for all numbers
# less than or equal to n(or in this case 2).

#myFun <- function(myVector, int)
#{
#  fn = ecdf(myVector)
#  return (fn((int))
#}

#z = c(1,2,3,4,5)
#myFun(z,2)

