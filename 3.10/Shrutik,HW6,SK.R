install.packages("ggplot2")
library(ggplot2)
install.packages("reshape")
library(reshape)

cars = mtcars # Reading the data
str(cars)

# Histogram
ggplot(cars, aes(x = mpg)) + geom_histogram(col = "darkblue" , fill = "cornflowerblue" , alpha = 0.7)+ ggtitle("Histogram of MPG")


#BoxPlot

cars$cyl

ggplot(cars, aes(factor(cyl),mpg)) + geom_boxplot()


# Multi-line plot

cars$wt # X
cars$mpg # Y
cars$am # 0 1

g_line = ggplot(cars, aes(x = wt, y = mpg, col = as.factor(am))) + geom_line()

# Bar Chart

str(cars)
cars = data.frame(cars)
head(cars)
cars$carnames = rownames(cars)

g = ggplot(cars,aes(x=carnames,wt)) + geom_bar(stat="identity", col = "darkblue" , fill = "cornflowerblue" , alpha = 0.8) 
g1 = g  + theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 1))
       

# Scatter Chart

cars$qsec
s = ggplot(cars, aes(x=mpg, y=wt)) + geom_point(aes(size = qsec, color = qsec))


# Heat map

library(reshape2)

carsmatrix <- data.matrix(cars)
meltedcars = melt(carsmatrix)
meltedcars

meltedcars <- melt(cars,id=c("wt","cyl","mpg"))

h <- ggplot(meltedcars, aes(x=factor(wt), y=factor(cyl)))
h1 <- h + geom_tile(aes(fill=mpg)) + scale_fill_gradient(low = "cornflowerblue", high = "black") + theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 1))
h1




