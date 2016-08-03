#Shrutik Katchhi
# HW 4

install.packages("sqldf")
library(sqldf)


airquality
head(airquality)

temp = airquality

# Average Ozone Calculation
avg_ozone <- sqldf("SELECT AVG(temp.Ozone) FROM temp")

print(paste("Average Ozone Value is: ", avg_ozone))


# Ozone Higher than Average
sqldf("SELECT * FROM temp WHERE temp.Ozone > (SELECT AVG(temp.Ozone) FROM temp)")


newAQ <- sqldf("SELECT * FROM temp WHERE temp.Ozone > (SELECT AVG(temp.Ozone) FROM temp)")
str(newAQ)
typeof(newAQ)
head(newAQ)

temp = na.omit(temp) # To remove all NAs
tapply(temp$Ozone > mean(temp$Ozone), temp$Ozone , print)

