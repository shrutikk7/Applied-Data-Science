#Shrutik Kaychhi
#IST687 - HW: Association Rules Mining


install.packages("arules")
library("arules")

install.packages("arulesViz")
library("arulesViz")

load("C:/Users/Shrutik/Desktop/Spring 2016/IST 687/3.31/termDocMatrix.RData")
head(termDocMatrix)

t <- t(termDocMatrix) # Transposed dataFrame
str(t)
head(t)
fix(t)


rules <- apriori(t,
                 parameter = list(supp=0.02, conf=0.9),
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

#Visualization:
plot(rules)

plot(rules, method="graph", control=list(type="items"))

plot(rules, method="paracoord",control=list(reorder = T))

fix(t)

# Here, the highest Lift value shows us the highest chance of that particular
# ruleset to occur again.

# For example, we have the same lift values for series and time. This can be changed
# by mentioning RHS = time or RHS = series. But when we think about it, its obvious
# that the term time series go together.

# Another such interesting ruke is 
# when parallel and r are used, chances are that computing will also be used.

# Lastly the Paracoord graph gives us a great insight into this as well.
# the Interpretation on paracoord. the width of the arrow is the support, 
# the intensity of the color is confidence for each rule.  
# X axis should be the lift. higher the lift, the better the rule. 