# Name: Shrutik Katchhi
# Home Work
#

# Normal Dist of 1000 samples, mean: 80

n <- rnorm(1000
           , mean = 80
           )
n

# Function to find number of values between two numbers

myNewFunction<-function(myVector,a,b)
{
  #clen <- length(myVector)

  a1 <- which(myVector < b)
  b1 <- which(myVector > a)
  
  ab <- intersect(a1,b1)
  ans <- length(ab+2) # +2 to include the comparing values as well, could've been dobe by <=b or >=a also
  return(ans)
}

x <- myNewFunction(n,79,80)
x


x1 <- myNewFunction(n,79,81)
x1

# Result 1 between 79 & 81: 686
# Result 2 between 79 & 81: 648
# Result 3 between 79 & 81: 670


##############################################
#Task 2
##############################################

#installed the VGAM library using R, then reloaded the Rconsole in RStudio after calling library()

library(VGAM)

# Location, scale, shape.

FSApops <- rparetoII(51,563626,36690330,8)

FSApops

hist(FSApops, breaks = 20)

meanP <- mean(FSApops)
meanP

sdP <- sd(FSApops)
sdP

maxP <- max(FSApops)
maxP

minP <- min(FSApops)
minP

