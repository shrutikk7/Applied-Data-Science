library(ggplot2)

aq = airquality # Reading the data
str(aq)
aq = na.omit(aq) # Removing all the NAs
aq = data.frame(aq)

# Plotting different histograms using ggplot

ggplot(aq, aes(x = Ozone)) + geom_histogram() + ggtitle("Ozone")
ggplot(aq, aes(x = Solar.R)) + geom_histogram()
ggplot(aq, aes(x = Wind)) + geom_histogram()
ggplot(aq, aes(x = Temp)) + geom_histogram()
ggplot(aq, aes(x = Month)) + geom_histogram()
ggplot(aq, aes(x = Day)) + geom_histogram() 



# Plotting the boxplot
ggplot(aq, aes(x = Ozone, y = Ozone)) + geom_boxplot()


aq$Wind
roundedwind = round(aq$Wind) # Rounding the wind values

# Plotting the Boxplot with rounded wind values
ggplot(aq, aes(x = roundedwind, y = roundedwind)) + geom_boxplot()


# Converting day and month into a date format and storing it in a variable named dates
dates <- as.Date(paste(1973,aq$Month, aq$Day, sep = "."), format = "%Y.%m.%d")
dates


# Plotting the individual plots

g1 = ggplot(aq, aes(x = dates, y = Ozone)) + geom_line(col = "Red")


g2 = ggplot(aq, aes(x = dates, y = Temp)) + geom_line(col = "Blue")


g3 = ggplot(aq, aes(x = dates, y = Wind)) + geom_line(col = "Green")


g4 = ggplot(aq, aes(x = dates, y = Solar.R)) + geom_line(col = "Grey")


# Plotting all the plots together
# We plot the Solar first since it has the largest scale and sets up the canvas

g_line = ggplot(aq, aes(x = dates, y = Solar.R)) + geom_line(col = "Grey")

# Adding more lines
g_line1 = g_line + geom_line(aes(x = dates, y = Ozone),col = "Red")
g_line2 = g_line1 + geom_line(aes(x = dates, y = Temp),col = "Blue")
g_line3 = g_line2 + geom_line(aes(x = dates, y = Wind),col = "Green")  

gg = g_line3  + scale_colour_manual(values=c("grey", "red","blue","green"))

legend("topright"
       , legend = c("Solar","Ozone","Temperature","Wind")
       , col =  c("Grey","Red","Blue","Green")
       , cex = 0.5
       )




##################

str(aq)
gp = ggplot(aq, aes(x=Day, y=Ozone)) + geom_tile(color = "Red")
gp1 = gp + geom_tile(aes(x=Day,y=Temp), color = "Blue")
gp2 = gp1 + geom_tile(aes(x=Day,y=Wind),color = "Green")
gp3 = gp2 + geom_tile(aes(x=Day,y=Solar.R),color = "Grey")

##################

# Scatter Chart:


ggplot(data = aq, aes(x=Wind,y=Temp)) + geom_point(aes(size=Ozone, col = Solar.R))

