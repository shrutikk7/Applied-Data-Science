install.packages("ggplot2")
library(ggplot2)
install.packages("reshape")
library(reshape)

aq = airquality # Reading the data
str(aq)

# Removing all the NAs
for(i in 1:ncol(aq)){
  aq[is.na(aq[,i]), i] <- mean(aq[,i], na.rm = TRUE)
}


aq = data.frame(aq)

###################################################################


# Plotting different histograms using ggplot

ggplot(aq, aes(x = Ozone)) + geom_histogram() + ggtitle("Ozone")
ggplot(aq, aes(x = Solar.R)) + geom_histogram()
ggplot(aq, aes(x = Wind)) + geom_histogram()
ggplot(aq, aes(x = Temp)) + geom_histogram()
ggplot(aq, aes(x = Month)) + geom_histogram()
ggplot(aq, aes(x = Day)) + geom_histogram(binwidth = 7) 


#################################################################

# Plotting the boxplot
ggplot(aq, aes(x = Ozone, y = Ozone)) + geom_boxplot()


aq$Wind
roundedwind = round(aq$Wind) # Rounding the wind values

# Plotting the Boxplot with rounded wind values
ggplot(aq, aes(x = roundedwind, y = roundedwind)) + geom_boxplot()


#################################################################

# Converting day and month into a date format and storing it in a variable named dates
dates <- as.Date(paste(1973,aq$Month, aq$Day, sep = "."), format = "%Y.%m.%d")
dates



#################################################################

# Plotting the individual plots

g1 = ggplot(aq, aes(x = dates, y = Ozone)) + geom_line(col = "Red")


g2 = ggplot(aq, aes(x = dates, y = Temp)) + geom_line(col = "Blue")


g3 = ggplot(aq, aes(x = dates, y = Wind)) + geom_line(col = "Green")


g4 = ggplot(aq, aes(x = dates, y = Solar.R)) + geom_line(col = "Grey")


#################################################################

# Plotting all the plots together
# We plot the Solar first since it has the largest scale and sets up the canvas

g_line = ggplot(aq, aes(x = dates, y = Solar.R)) + geom_line(col = "Grey")

# Adding more lines
g_line1 = g_line + geom_line(aes(x = dates, y = Ozone),col = "Red")
g_line2 = g_line1 + geom_line(aes(x = dates, y = Temp),col = "Blue")
g_line3 = g_line2 + geom_line(aes(x = dates, y = Wind),col = "Green")  


#################################################################

# Heat Map:


gp = ggplot(meltedaq, aes(x=Day, y=Ozone)) + geom_tile(aes(fill = value))
gp1 = gp + geom_tile(aes(x=Day,y=Temp), color = "Blue")
gp2 = gp1 + geom_tile(aes(x=Day,y=Wind),color = "Green")
gp3 = gp2 + geom_tile(aes(x=Day,y=Solar.R),color = "Black")

gp3


###############
# Doubt: I cannot get the output using melt()

str(aq)
meltedaq = melt(aq,id=c("Day","Ozone"))
str(meltedaq)

p <- ggplot(meltedaq, aes(x=Day, y=Ozone)) 
p1 <- p + geom_tile(aes(fill=value)) + scale_fill_gradient(low="blue", high="white") + xlab("") + ylab("")
p1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))

###############



#################################################################

# Scatter Chart:


ggplot(data = aq, aes(x=Wind,y=Temp)) + geom_point(aes(size=Ozone, col = Solar.R))


#####################################################

# There are many patetrns which we can see when we visualize the data.
# E.g we can see in the scatter plot thatwhen the Temperature increases, the Ozone increases.
# But the Solar values are fairly distributed.

# Scatter plot is the most important visualization according to me.
# Because it shows data in many dimensions and we can see a clear pattern.

####################################################



