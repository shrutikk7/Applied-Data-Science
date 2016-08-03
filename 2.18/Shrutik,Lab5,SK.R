# Name: Shrutik Katchhi
# Lab: 4

install.packages("sqldf")
library(sqldf)

install.packages("rjson")
library(rjson)

# JSON packages in R
require(RJSONIO)
require(RCurl)


json_file <- "https://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD"
url = getURL(json_file)
json_data <- fromJSON(url) # To get JSON in a variable

# str(json_data)
json_length = length(json_data[[2]]) # The second dimension of JSON files contains the actual lists
json_length



# TO convert JSON data into R dataframe
# we use unlist because the data in JSON is list type
df_json = data.frame(matrix(unlist(json_data),nrow = json_length, byrow = T),stringsAsFactors = FALSE) 


# For loop to make noise into NA

for(i in 1:json_length)
{
  tt <- json_data[[1]][[i]]
  tt[sapply(json_data, is.null)] <- NA
  json_data[[1]][[i]] <- tt
}



names(df_json)
str(df_json)

temp = df_json
# df_json = temp
df_json = df_json[,-1:-8] # Delete the first 8 columns from the dataframe
str(df_json)

names(df_json) = (c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2"))

str(df_json)

fix(df_json)

head(df_json)

# Question 1: Get accidents from sunday
sql1 <- sqldf("SELECT COUNT(*) FROM df_json WHERE DAY_OF_WEEK = 'SUNDAY   '")
(sql1)

# Question 2: Number of Injuries
sql2 <- sqldf("SELECT COUNT(*) FROM df_json WHERE INJURY = 'YES' ")
head(sql2)

# Question 3: Injuries by Day
sql3 <- sqldf("SELECT INJURY,DAY_OF_WEEK FROM df_json ORDER BY DAY_OF_WEEK ")
head(sql3)


# tapply functions to do the same thing in R style
tapply(df_json$DAY_OF_WEEK == "SUNDAY   " , df_json$DAY_OF_WEEK, sum)

tapply(df_json$INJURY== "YES", df_json$INJURY, length)

tapply(df_json$INJURY == "YES", df_json$DAY_OF_WEEK, length)
