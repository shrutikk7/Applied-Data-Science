library(xlsx)
hwdata = read.xlsx(file = "C:/Users/Shrutik/Desktop/Spring 2016/IST 687/3.3/mlr01.xls",1)

hwdatadf = data.frame(hwdata)
colnames(hwdatadf) = c("babyf", "adultf", "annualprep", "winter") # Changing Col names for better understanding
str(hwdatadf)
hwdatadf

# bivariate plots:

plot(hwdatadf$adultf, hwdatadf$babyf
     , xlab = "Number of Baby Fawns"
     , ylab = "Number of Adult Fawns"
     , main = "Number of Baby Fawns versus Adult Antelope Population"
     , pch = 10)

plot(hwdatadf$annualprep, hwdatadf$babyf
     , xlab = "Number of Baby Fawns"
     , ylab = "Annual Precipitation"
     , main = "Number of Baby Fawns versus Precipitation"
     , pch = 10)

plot(hwdatadf$winter, hwdatadf$babyf
     , xlab = "Number of Baby Fawns"
     , ylab = "Severity of the Winter"
     , main = "Number of Baby Fawns versus Severity of the Winter"
     , pch = 10)

# Regression Models:

x = lm(formula = babyf ~ winter, data = hwdatadf)
summary(x)
# Here, there is just a '*' after winter, which means that it had negligible effect.
# It had just a 5% effect on the Fawns

x1 = lm(formula = babyf ~ winter + annualprep, data = hwdatadf)
summary(x1)
# We can see that annual precipitation was eventful. The R squared value also increased significantly.

x2 = lm(formula = babyf ~ winter + annualprep + adultf, data = hwdatadf)
summary(x2)
# When we take all the factirs into account, the R Squared value is maximum and we can see that it is the best model


# To calculate the most parsimonous model:

x3 = lm(formula = babyf ~ annualprep + adultf, data = hwdatadf)
summary(x3)

x4 = lm(formula = babyf ~ adultf, data = hwdatadf)
summary(x4)

x5 = lm(formula = babyf ~ annualprep, data = hwdatadf)
summary(x5)

# Between x4 and x5, we can discard x5 since it has a lower R Squared, fewer '*' and same deree of freedom 

anova(x,x1,x2,x3,x4)

# As we can see, model x3 gives a slightly better R Squared value than x3 
# But if we truly want to be parsimonous, we need to select model x4.
# Hence, according to me, the population of adult antelope gives the best individual estimate for Fawns


