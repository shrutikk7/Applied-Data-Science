


install.packages("arulesViz")
install.packages("arules")

library(arulesViz)
library(arules)

#Creating DataFrame
data(Epub)
ED <- Epub
ED@data[1,1]
EpubData <- ED@data
EDMean <- rowMeans(EpubData)
EDLabel <- ED@itemInfo$labels
EDDataFrame <- data.frame(EDMean, EDLabel)

#Analyzing using arules package
RulesTermData <- apriori(Epub,parameter=list(support=0.0007,confidence=0.5))
inspect(RulesTermData)
summary(RulesTermData)
plot(RulesTermData)
plot(RulesTermData, method="graph")

RulesTermData1 <- apriori(Epub,parameter=list(support=0.0007,confidence=0.8))
summary(RulesTermData1)
plot(RulesTermData1)
plot(RulesTermData1, method="graph")

RulesTermData2 <- apriori(Epub,parameter=list(support=0.001,confidence=0.5))
summary(RulesTermData2)
plot(RulesTermData2)
plot(RulesTermData2, method="graph")

#Rules with lift > 2.0
library(graphics)
RulesTermData3 <- RulesTermData2[quality(RulesTermData2)$lift > 2.0]
plot(RulesTermData3)
plot(RulesTermData3, method="graph")

#Using RulesTermData3 to improve results
RulesTermData4 <- RulesTermData3[quality(RulesTermData3)$support > 0.001]
plot(RulesTermData4)
plot(RulesTermData4, method="graph")

#Analyzing using arules package
RulesTermData5 <- apriori(Epub,parameter=list(support=0.001,confidence=0.03), appearance = list(default="lhs", rhs=c("doc_87c")))
plot(RulesTermData5)
plot(RulesTermData5, method="graph")

