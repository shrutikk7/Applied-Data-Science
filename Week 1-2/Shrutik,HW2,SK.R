#Shrutik Katchhi 
#IST687 - HW 2: RowCols

# We will use the r dataset mtcars:


#######
# task 1:
#######

mtcars
myCars <- mtcars
myCars

summary(myCars)


# to caculate displacement per cyclinders

myCars$dpc <- (myCars$disp) / (myCars$cyl)
myCars

summary(myCars$dpc)



#######
# task 2:
#######

# Information about 5 friends:

# question 1: 	Create three vectors of integers as described above, 
#               using the c( ) (concatenate) command to store data reported by group members, 
#               with these variable names: Pets, Order, and Siblings

pets <- c(1,0,1,0,1)
order <- c(1,1,6,7,1)
siblings <- c(0,0,6,6,0)


# question 2: Also create a vector of user IDs for the friends and family members

userid <- c("HarryPotter","HermioneGranger","RonWeasley","GinnyWeasley","NevilleLongbottom")

# question 3: Bind those four vectors together into a data frame called myFriends

myFriends <- data.frame(userid,pets,order,siblings)
myFriends

# question 4:	Use the appropriate R command to report the structure of your data frame 
#             as well as a summary of the data (with minimums, means, maximums, etc. as shown on page 32. 
#             The result should show, "X obs. Of 4 variables," where X is the number of friends and family
#             members who reported their data

summary(myFriends)
str(myFriends)


# question 5: Use the $ notation explained on page 33 to list all of the values for each of the variables 
#             in the myFriends data frame (example myGroup$Pets)

myFriends$userid
myFriends$pets
myFriends$order
myFriends$siblings


