install.packages("maps")
library(maps)

install.packages("mapdata")
library(mapdata)

install.packages("maptools")
library(maptools)

install.packages("gdata")
library(gdata)

install.packages("xlsx")
library(xlsx)

install.packages("zipcode")
library(zipcode)

install.packages('ggplot2', dependencies = TRUE)
library(ggplot2)

install.packages("ggmap")
library(ggmap)

syrcrime = read.csv(file= "C://Users/Shrutik/Desktop/Spring 2016/IST 687/3.10/crimeInSYR.csv",1)
str(syrcrime)

# Changing row names

names = c("TypeOfCrime","Address","City","Date") 
names(syrcrime) = names
syrcrime$City <- tolower(syrcrime$City) # Converting all City names to Lowercase so that we can easily use geocode
head(syrcrime)

geos = geocode(syrcrime$City) 
str(geos)

syrcrime$state <- "?" # Empty state col


us <- map_data("state") # Plotting US map

dummyDF <- data.frame(state.name, stringsAsFactors=FALSE)
dummyDF$state <- tolower(dummyDF$state.name)
map.simple <- ggplot(dummyDF, aes(map_id = state))  

map.simple <- map.simple+  geom_map(map = us, fill="white", color="black") 
map.simple <- map.simple + expand_limits(x = us$long, y = us$lat)
map.simple  
map.simple <- map.simple + coord_map() + ggtitle("basic map of USA")
map.simple

zoomGeo <- geocode("Syracuse, New York") # Zooming around Syracuse
zoomAmount <- 1

centerx <- zoomGeo$lon
centery <- zoomGeo$lat
ylimit <- c(centery-zoomAmount, centery+zoomAmount)
xlimit <- c(centerx-zoomAmount, centerx+zoomAmount)


syrcrime$geocode <- geocode(syrcrime$City) # Geocodes of every City

map.zoom = map.simple + coord_cartesian(x = xlimit, y = ylimit)

unique(syrcrime$City)

# Plotting the first Map
firstmap = map.zoom + geom_point(data = syrcrime, aes(x=geocode$lon, y=geocode$lat, color= TypeOfCrime, size = City))


# Plotting the density Map

density_map = firstmap + stat_density2d(aes(x=geocode$lon, y=geocode$lat), data = syrcrime, geom = "polygon")

# Map is almost similar to the previous map because we just have around 16 unique cities, unilike thousands of Zipcodes in the Lab Assignment. So we have just 16 Lat and Lon values to work with here.

