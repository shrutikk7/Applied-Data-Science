install.packages("arules")
library("arules")

install.packages("arulesViz")
library("arulesViz")

install.packages("apriori")
library("apriori")


t <- load("C:/Users/Shrutik/Desktop/Spring 2016/IST 687/3.31/titanic.raw.RData", ex <- new.env())
t <- ls.str(ex) 

head(t)
str(t)

##########
## Step 1
##########

# 1:

titanic.raw$Survived

length(titanic.raw$Survived)

percentSurvived <- (length(titanic.raw$Survived[titanic.raw$Survived == 'Yes']) / length(titanic.raw$Survived))*100
percentSurvived

# 2:

str(titanic.raw)
titanic.raw$Age

percentChildren <- (length(titanic.raw$Age[titanic.raw$Age == 'Child']) / length(titanic.raw$Age))*100
percentChildren


# 3:

percentFemale <- (length(titanic.raw$Age[titanic.raw$Sex == 'Female']) / length(titanic.raw$Sex))*100
percentFemale


# 4:

percentFirst <- (length(titanic.raw$Class[titanic.raw$Class == '1st']) / length(titanic.raw$Class))*100
percentFirst


##########
## Step 2
##########

# 1:

percentChildrenSurvived <- (length(titanic.raw$Age[(titanic.raw$Age == 'Child') & (titanic.raw$Survived == 'Yes')] ) / length(titanic.raw$Age[titanic.raw$Age == 'Child']))*100
percentChildrenSurvived

# 2:

percentFemaleSurvived <- (length(titanic.raw$Sex[(titanic.raw$Sex == 'Female') & (titanic.raw$Survived == 'Yes')] ) / length(titanic.raw$Sex[titanic.raw$Sex == 'Female']))*100
percentFemaleSurvived

# 3:

percentFirstSurvived <- (length(titanic.raw$Class[(titanic.raw$Class == '1st') & (titanic.raw$Survived == 'Yes')] ) / length(titanic.raw$Class[titanic.raw$Class == '1st']))*100
percentFirstSurvived

# 4:

percentThirdSurvived <- (length(titanic.raw$Class[(titanic.raw$Class == '3rd') & (titanic.raw$Survived == 'Yes')] ) / length(titanic.raw$Class[titanic.raw$Class == '3rd']))*100
percentThirdSurvived



##########
## Step 3
##########


# 1:

str(titanic.raw)

fun1 <- function(sex, age, class, survived)
{
  newdf = titanic.raw[(titanic.raw$Sex == sex) & (titanic.raw$Age == age) & (titanic.raw$Class == class) & (titanic.raw$Survived == survived) ]
  
  return(newdf)
}

fun1('Female','Adult','1st','Yes')
fun1('Male','Child','3rd','No')
fun1('Male','Child','3rd','Yes')
fun1('Male','Child','1st','No')


# 2:

fun2 <- function(age, class, sex)
{
  percentLive = (
    (length(titanic.raw$Age[(titanic.raw$Age == age) & (titanic.raw$Class == class) & (titanic.raw$Sex == sex) & (titanic.raw$Survived == 'Yes') ]))
    /
      (length(titanic.raw$Age[(titanic.raw$Age == age) & (titanic.raw$Class == class) & (titanic.raw$Sex == sex) ]))) * 100
  print(paste(" Percentage Lived: ",percentLive))
  print(paste(" Percentage Died: ", (100 - percentLive)))
  return(NULL)
}

fun2('Adult','3rd','Male')
fun2('Adult','3rd','Female')

# 3:

fun2('Adult','3rd','Male')
fun2('Child','3rd','Male')

# 3:

fun2('Adult','3rd','Female')
fun2('Child','3rd','Female')


#############
## Step 4:
#############


str(titanic.raw)



# find rules 

rules <- apriori(titanic.raw)
summary(rules)
inspect(rules)
plot(rules)

# rules with rhs containing "Survived" only
rules <- apriori(titanic.raw,parameter = list(minlen=2, supp=0.005, conf=0.9),appearance = list(rhs=c("Survived=No", "Survived=Yes"),default="lhs"),control = list(verbose=F))
inspect(rules.sorted)

#Visualization:
plot(rules)

#######
# Comparison
#######

inspect(rules.sorted)

fun2('Child','3rd','Female') # " Percentage Lived:  45.1612903225806"
fun2('Child','2nd','Female') # " Percentage Lived:  100"
fun2('Child','1st','Female') # " Percentage Lived:  100"


fun2('Adult','1st','Female') # " Percentage Lived:  97.2222222222222"
fun2('Child','3rd','Female') # " Percentage Lived:  45.1612903225806"

percentFirstSurvived1 <- (length(titanic.raw$Class[(titanic.raw$Class == '1st') & (titanic.raw$Survived == 'Yes')] ) / length(titanic.raw$Class[titanic.raw$Class == '1st']))*100
percentFirstSurvived1 # 62.46154 

percentSecondSurvived1 <- (length(titanic.raw$Class[(titanic.raw$Class == '2nd') & (titanic.raw$Survived == 'Yes')] ) / length(titanic.raw$Class[titanic.raw$Class == '1st']))*100
percentSecondSurvived1 # 36.30769 

percentThirdSurvived1 <- (length(titanic.raw$Class[(titanic.raw$Class == '3rd') & (titanic.raw$Survived == 'Yes')] ) / length(titanic.raw$Class[titanic.raw$Class == '3rd']))*100
percentThirdSurvived1 # 25.21246

# Conclusion: Yes, money does buy you a new life (or so it did in 1912)
