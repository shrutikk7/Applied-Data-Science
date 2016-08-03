install.packages("dpylr")
library(dplyr)

install.packages("arules")
library("arules")

install.packages("arulesViz")
library("arulesViz")

library(e1071)

x = read.csv(file = "C:/Users/Shrutik/Desktop/Spring 2016/IST 687/Project/Book1.csv", sep = ",")
str(x)

#fix(x)

for(i in 1:length(x$Likelihood_Recommend_H))
{
  
  if(x$Likelihood_Recommend_H[i]<9)
    x$NPS[i] <- c("Detractor")
  else
    x$NPS[i] <- c("Promotor")
}

table(x$NPS)

temp = data.frame( x$All.Suites_PL
                  ,x$Bell.Staff_PL
                  ,x$Boutique_PL
                  ,x$Business.Center_PL
                  ,x$Casino_PL
                  ,x$Conference_PL
                  ,x$Convention_PL
                  ,x$Dry.Cleaning_PL
                  ,x$Elevators_PL
                  ,x$Fitness.Center_PL
                  ,x$Fitness.Trainer_PL
                  ,x$Golf_PL
                  ,x$Indoor.Corridors_PL
                  ,x$Laundry_PL
                  ,x$Limo.Service_PL
                  ,x$Mini.Bar_PL
                  ,x$Pool.Indoor_PL
                  ,x$Pool.Outdoor_PL
                  ,x$Regency.Grand.Club_PL
                  ,x$Resort_PL
                  ,x$Restaurant_PL
                  ,x$Self.Parking_PL
                  ,x$Shuttle.Service_PL
                  ,x$Ski_PL
                  ,x$Spa_PL
                  ,x$Spa.services.in.fitness.center_PL
                  ,x$Spa.online.booking_PL
                  ,x$Spa.F.B.offering_PL
                  ,x$Valet.Parking_PL
                  ,x$NPS
                  ,x$POV_CODE_C)

str(temp)


#apriori(temp)
#rules
#
#


rules <- apriori(temp,
                 parameter = list(minlen=2, supp=0.3, conf=0.3),
                 appearance = list(rhs=c("x.NPS=Detractor"),default="lhs"),
                 control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

rules <- apriori(temp,
                 parameter = list(minlen=2, supp=0.3, conf=0.5),
                 appearance = list(rhs=c("x.NPS=Promotor"),default="lhs"),
                 control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
#inspect(rules.sorted)

inspect(head(rules.sorted))

length(x$Likelihood_Recommend_H[ (x$NPS=="Promotor")
                         &(x$Restaurant_PL=="Y")
                         &(x$Self.Parking_PL=="Y")
                         &(x$Elevators_PL=="Y")])

length(x$Likelihood_Recommend_H[ (x$NPS=="Detractor")
                                 &(x$Restaurant_PL=="Y")
                                 &(x$Self.Parking_PL=="Y")
                                 &(x$Elevators_PL=="Y")])



# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA

redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)


plot(rules.pruned, method="graph", control=list(type="items"))


plot(rules, method="graph", control=list(type="items"))

#########################################
#########################################
#########################################

# Modelling


temp1 = data.frame( x$LENGTH_OF_STAY_C
                   ,x$ADULT_NUM_C
                   ,x$QUOTED_RATE_C
                   ,x$PMS_ROOM_REV_USD_C
                   ,x$PMS_TOTAL_REV_USD_C
                   ,x$PMS_FOOD_BEVERAGE_REV_USD_C
                   ,x$PMS_OTHER_REV_USD_C
                   ,x$NT_RATE_R
                   ,x$Likelihood_Recommend_H
                   ,x$City_PL)
str(temp1)

temp1 = temp1[(temp1$x.City_PL=="Atlanta"),]
temp1 = temp1[(temp1$x.Likelihood_Recommend_H<9),]
str(temp1)
temp1 = temp1[,-10]

svmfit = svm( x.Likelihood_Recommend_H ~ x.ADULT_NUM_C
             ,data = temp1
             ,kernel = "linear"
             ,cost = 0.1
             ,scale = FALSE)

plot(svmfit,temp1)
summary(svmfit)
fix(temp1)
p = predict(svmfit,temp1)
table(predict = p, truth = temp1$x.Likelihood_Recommend_H)


#################
#################
#################
#################
# Next thing


# NYC Data:
# 
# 
head(x$City_PL)
nyData = x[(x$City_PL=="New York"),]

str(nyData)

table(nyData$NPS)

table(nyData$LENGTH_OF_STAY_C,nyData$NPS)

table(nyData$Postal.Code_PL,nyData$NPS)


newdf = data.frame(nyData[(nyData$Postal.Code_PL==10036),]
                   ,nyData[(nyData$Postal.Code_PL==10017),])


table(nyData$Postal.Code_PL,nyData$NPS)

nyData$Pool.Outdoor_PL[(nyData$Postal.Code_PL==10036) & (nyData$Brand_PL=="Grand Hyatt")]

fix(nyData)

nyData$Spa_PL[(nyData$Postal.Code_PL==10036) & (nyData$Brand_PL=="Hyatt")]
nyData$Spa_PL[(nyData$Postal.Code_PL==10017) & (nyData$Brand_PL=="Hyatt")]

nyData$Restaurant_PL[(nyData$Postal.Code_PL==10036) & (nyData$Brand_PL=="Hyatt")]
nyData$Restaurant_PL[(nyData$Postal.Code_PL==10017) & (nyData$Brand_PL=="Hyatt")]

nyData$Indoor.Corridors_PL[(nyData$Postal.Code_PL==10036) & (nyData$Brand_PL=="Hyatt")]
nyData$Indoor.Corridors_PL[(nyData$Postal.Code_PL==10017) & (nyData$Brand_PL=="Hyatt")]

mean(nyData$Staff_Cared_H[(nyData$Postal.Code_PL==10036) & (nyData$Brand_PL=="Hyatt")], na.rm=T)
mean(nyData$Staff_Cared_H[(nyData$Postal.Code_PL==10017) & (nyData$Brand_PL=="Hyatt")], na.rm=T)

mean(nyData$Customer_SVC_H[(nyData$Postal.Code_PL==10036) & (nyData$Brand_PL=="Hyatt")], na.rm=T)
mean(nyData$Customer_SVC_H[(nyData$Postal.Code_PL==10017) & (nyData$Brand_PL=="Hyatt")], na.rm=T)

mean(nyData$Condition_Hotel_H[(nyData$Postal.Code_PL==10036) & (nyData$Brand_PL=="Hyatt")], na.rm=T)
mean(nyData$Condition_Hotel_H[(nyData$Postal.Code_PL==10017) & (nyData$Brand_PL=="Hyatt")], na.rm=T)

mean(nyData$Tranquility_H[(nyData$Postal.Code_PL==10036) & (nyData$Brand_PL=="Hyatt")], na.rm=T)
mean(nyData$Tranquility_H[(nyData$Postal.Code_PL==10017) & (nyData$Brand_PL=="Hyatt")], na.rm=T)

mean(nyData$Overall_Sat_H[(nyData$Postal.Code_PL==10036) & (nyData$Brand_PL=="Hyatt")], na.rm=T)
mean(nyData$Overall_Sat_H[(nyData$Postal.Code_PL==10017) & (nyData$Brand_PL=="Hyatt")], na.rm=T)



fun(nyData)

fun <- function(df,postalCode)
{
# Customer Service:
  cs1 = mean(df$Customer_SVC_H[(df$Postal.Code_PL==10036) & (df$Brand_PL=="Hyatt")], na.rm=T)
  print(paste("Desired Customer Service: ",cs1))
  
  cs2 = mean(df$Customer_SVC_H[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")], na.rm=T)
  print(paste("Current Customer Service: ",cs2))
  
# Spa Service: 
  
a1=  (df$Spa_PL[(df$Postal.Code_PL==10036) & (df$Brand_PL=="Hyatt")])
if( length(a1[a1=="Y"])>length(a1[a1=="N"]) )
   {a1<-c("Y")}
if(length(a1[a1=="Y"])<length(a1[a1=="N"]))
   {a1<-c("N")}
print(paste("Desired Spa Service: ",a1))

a2=  (df$Spa_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
if( length(a2[a2=="Y"])>length(a2[a2=="N"]) )
{a2<-c("Y")}
if(length(a2[a2=="Y"])<length(a2[a2=="N"]))
{a2<-c("N")}
print(paste("Current Spa Service: ",a2))
  

#Casino

b1=  (df$Casino_PL[(df$Postal.Code_PL==10036) & (df$Brand_PL=="Hyatt")])
if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
{b1<-c("Y")}
if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
{b1<-c("N")}
print(paste("Desired Casino: ",b1))

b2=  (df$Casino_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
{b2<-c("Y")}
if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
{b2<-c("N")}
print(paste("Current Casino: ",b2))
  
  
# Boutique_PL

b1=  (df$Boutique_PL[(df$Postal.Code_PL==10036) & (df$Brand_PL=="Hyatt")])
if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
{b1<-c("Y")}
if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
{b1<-c("N")}
print(paste("Desired Boutique: ",b1))

b2=  (df$Boutique_PLL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
{b2<-c("Y")}
if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
{b2<-c("N")}
print(paste("Current Boutique: ",b2))




#,x$Bell.Staff_PL

#,x$Business.Center_PL

#,x$Conference_PL
#,x$Convention_PL
#,x$Dry.Cleaning_PL
#,x$Elevators_PL
#,x$Fitness.Center_PL
#,x$Fitness.Trainer_PL
#,x$Golf_PL
#,x$Indoor.Corridors_PL
#,x$Laundry_PL
#,x$Limo.Service_PL
#,x$Mini.Bar_PL
#,x$Pool.Indoor_PL
#,x$Pool.Outdoor_PL
#,x$Regency.Grand.Club_PL
#,x$Resort_PL
#,x$Restaurant_PL
#,x$Self.Parking_PL
#,x$Shuttle.Service_PL
#,x$Ski_PL
#,x$Spa_PL
#,x$Spa.services.in.fitness.center_PL
#,x$Spa.online.booking_PL
#,x$Spa.F.B.offering_PL
#,x$Valet.Parking_PL




}

fun(x,10017)
