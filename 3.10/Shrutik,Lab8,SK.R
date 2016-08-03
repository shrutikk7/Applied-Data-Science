install.packages("maps")
library(maps)

install.packages(mapdata)
library(mapdata)

install.packages("maptools")
library(maptools)

install.packages("gdata")
library(gdata)

install.packages("xlsx")
library(xlsx)

install.packages("zipcode")
library(zipcode)

medianzip = read.xlsx(file= "C://Users/Shrutik/Desktop/Spring 2016/IST 687/3.10/MedianZIP.xlsx",1)
str(medianzip)


# Cleaning the data

head(medianzip)
medianzip[1,]
medianzip = medianzip[-1,]
head(medianzip)

# Changing row names

names = c("zip","median","mean","population")
names(medianzip) = names 
head(medianzip)

data(zipcode)
head(zipcode)

mergedzip = merge(medianzip,zipcode,match="zip")
head(mergedzip)

# Removing Hawaii and Alaska

mergedzip <- mergedzip[-which(mergedzip$state=='AK'),]
mergedzip <- mergedzip[-which(mergedzip$state=='HI'),]


####Step 2:

mergedzip$median = as.numeric(mergedzip$median)
mergedzip$population = as.numeric(mergedzip$population)
str(mergedzip)
head(mergedzip)

# Getting mean salary and population of 43 states

simplemedian <- aggregate(mergedzip$median ~ mergedzip$state, FUN = mean, data = mergedzip)
simplepop <- aggregate(mergedzip$population ~ mergedzip$state, FUN = mean, data = mergedzip)

simpledf <- merge(simplemedian,simplepop, match = "state")
head(simpledf)
str(simpledf)

simpledf
names(simpledf) = c("state","median","population")
head(simpledf)

install.packages("openintro")
library(openintro)

# Converting abbreviations to state names
simpledf$statename = abbr2state(simpledf$state)
head(simpledf)

# Uppercase to Lower Case
simpledf$statename = tolower(simpledf$statename)
head(simpledf)

install.packages('ggplot2', dependencies = TRUE)
library(ggplot2)

usmap = map_data("state")

#Plotting  and unbelievable amount of maps
map1 = ggplot(simpledf, aes(map_id = statename)) + geom_map(map = usmap, aes(fill=median))
map2 = map1 + expand_limits(x= usmap$long,y=usmap$lat)
map3 = map2 + coord_map()
map3

map21 = ggplot(simpledf, aes(map_id = statename)) + geom_map(map = usmap, aes(fill=population))
map22 = map21 + expand_limits(x= usmap$long,y=usmap$lat)
map23 = map22 + coord_map()
map23



#####Step 3:

install.packages("ggmap")
library(ggmap)

str(mergedzip)


usmap = map_data("state", region = "New York")

map1 = ggplot(simpledf, aes(map_id = statename)) + geom_map(map = usmap, aes(fill=population))
map2 = map1 + expand_limits(x= usmap$long,y=usmap$lat)
map3 = map2 + coord_map()
map3



geos = geocode(simpledf$statename)
str(geos)
map_3 = map3 + geom_point(aes(x=geos$lon,y=geos$lat), color="darkred")
map_3




########STep 4:

str(mergedzip)
head(simpledf)
simpledf$state = simpledf$statename
map_infinity = ggplot(data = simpledf, aes(map_id = state)) + geom_map(map = usmap, fill="white", color="black") 
map_infinity1 = map_infinity + expand_limits(x= usmap$long,y=usmap$lat)
map_infinity2 = map_infinity1 + coord_map()

map_zip = map_infinity2 + geom_point(aes(x=longitude,y=latitude, color = median), data = mergedzip) 
  
map_zip1 = map_zip + stat_density2d(aes(x=longitude, y=latitude), data = mergedzip, geom = "polygon")
map_zip1

#####Step 5:

# Code from inclass.R

zoomGeo <- geocode("New York, ny")
zoomAmount <- 3

centerx <- zoomGeo$lon
centery <- zoomGeo$lat
ylimit <- c(centery-zoomAmount, centery+zoomAmount)
xlimit <- c(centerx-zoomAmount, centerx+zoomAmount)

# Zooming in using coord_cartesian:

map_3 + coord_cartesian(x = xlimit, y = ylimit)

map_zip1 + coord_cartesian(x = xlimit, y = ylimit)
