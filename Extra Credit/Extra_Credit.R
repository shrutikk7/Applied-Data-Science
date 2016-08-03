# Shrutik Katchhi
# Extra Credit Assignment


install.packages("arulesViz")
install.packages("arules")

library(arulesViz)
library(arules)

# Loading data

data(Epub)
str(data)
Epub

# Since Epub is a set of transactions, we have to do a 
# little more computations with the data before we can 
# do any kind of rule mining
Epub@data

Edata <- Epub@data
str(Edata)

Emean <- rowMeans(Edata) # Getting the mean of transactions

Elabel <- Epub@itemInfo$labels # Getting documents of the means

EdataFrame <- data.frame(Emean, Elabel)
str(EdataFrame)
head(EdataFrame)



rules <- apriori(Epub,
                 parameter = list(supp=0.0005, conf=0.5),
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
inspect(head(rules.sorted))

top50rules <- head(rules.sorted,50)

# Here, the highest Lift value shows us the highest chance of that particular
# ruleset to occur again.

#Visualization:
#
plot(top50rules)

plot(top50rules, method="graph", control=list(type="items"))

plot(top50rules, method="paracoord",control=list(reorder = T))

# # Lastly the Paracoord graph gives us a great insight into this as well.
# the Interpretation on paracoord. the width of the arrow is the support, 
# the intensity of the color is confidence for each rule.  
# X axis should be the lift. higher the lift, the better the rule. 
