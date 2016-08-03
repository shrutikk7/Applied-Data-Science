#Shrutik Katchhi
#Preffered Name: Shrutik
#Lab 2: Data Frames

# To get the summary 
summary(mtcars)

# We copy the dataset in case we mess up mtcars
myCars <- mtcars

myCars

# We create one  more copy and convert it into a Data Frame
df <- data.frame(myCars)
df

# Highest HP:

# This will gve us the location of the max hp not the hp value itself
max_HP <- which.max(df$hp)
max_HP

# We convert int to numeric
str(max_HP)
max_HP <- as.numeric(max_HP)

# We use the grep function to get the valut at a particular index
# colnames gives us the indexes of each column
print("The Maximum HP is: ")
df[max_HP,grep("hp",colnames(df))]

print("The car with max HP is: ")


#To get the name, we first select the entire row and then select the subset just to get the HP and the car name
df_name_temp <- df[max_HP,]
df_name <- subset(df_name_temp,select=("hp"))
df_name







# Highest MPG:

# Same method is used for MPG as well

max_MPG <- which.max(df$mpg)
max_MPG

str(max_MPG)
max_MPG <- as.numeric(max_MPG)


print("The Maximum MPG is: ")
df[max_MPG,grep("mpg",colnames(df))]

print("The car with max MPG is: ")

df_name_temp1 <- df[max_MPG,]
df_name1 <- subset(df_name_temp1,select=("mpg"))
df_name1


# To order the car with respect to mpg:

mtcarsdataframe <- order(df$mpg)
x <- df[mtcarsdataframe,]
x

# To find the best car:
# Note: we have not given mpg and hp the same weightage 

x$df1 <- (x$mpg / x$hp)
x

best_car <- order(-x$df1)
x1 <- x[best_car,]
head(x1)

print("The best Car is Honda Civic")


# Now we want to select a car cosidering both mpg and hp equally.
# So first we need to define a constant range, say 0 to 1, in which we can define both mpg and hp

#We first define a new attribute:
percentMPG <- 0.5

# now we will scale mpg and hp:

mpgScaled <- (x1$mpg - min(x1$mpg)) / (max(x1$mpg) - min(x1$mpg))
hpScaled <- (x1$hp - min(x1$hp)) / (max(x1$hp) - min(x1$hp))


# now we combine to get the rating

x1$rating <- mpgScaled*percentMPG + hpScaled*(1.0 - percentMPG)

best_rating <-which.max(x1$rating)
best_rating <- x1[best_rating,]
head(best_rating)

#####
#End of Code
####