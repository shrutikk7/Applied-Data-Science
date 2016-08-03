# Name: Shrutik Katchhi
# Home work 4


# To keep the random numbers same, we eun the set.seed function.
# It makes sure that the random numbers which we generate using rnorm remain the same.
set.seed(2)

# Sample size is 100, the longer the sample size, more accurate is the mean
sampleSize <- 30

# Here we create a vector studentpop which stores random ages. Specified mean and sd is 20 and 3
studentPop <- rnorm(20000,mean=20,sd=3)

# Now we create another variable and store 100 means by sampling studentPop  
studentPopmeans <- replicate(100,mean(sample(studentPop, size = 30, replace = TRUE)), simplify = TRUE)

undergrads <- sample(studentPop,size=sampleSize,replace=TRUE)

# sd is 3 because we have set sd for undergrad as 2, the max undergrad vaue can be 22,
# so the max grad can be of age 23, hence sd is 3\
grads <- rnorm(sampleSize,mean=25,sd=3)

#runif will give a random value between 0 and 1,
if (runif(1)>0.5) { testSample <- grads } else { testSample <- undergrads }
mean(testSample)

#Now we use the quantile function to check if our sample value is between 2.5 and 97.5%
quantile(studentPopmeans, probs = c(0.025,0.975))

# Our value does not lie between the two:
print("Sample mean is Extreme")



############################
### OUTPUT###
############################

#> mean(testSample)
#[1] 19.97058
#  > quantile(studentPopmeans, probs = c(0.025,0.975))
#2.5%    97.5% 
#  19.19519 21.07595 
#[1] "Sample mean is Extreme"
