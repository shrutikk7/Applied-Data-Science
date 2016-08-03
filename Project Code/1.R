install.packages("dpylr")
library(dplyr)

install.packages("arules")
library("arules")

install.packages("arulesViz")
library("arulesViz")

x = read.csv(file = "C:/Users/Shrutik/Desktop/Spring 2016/IST 687/Project/Data/hyatt2-Jan15.csv", sep = ",")
str(x)

length(df$Likelihood_Recommend_H[df$Likelihood_Recommend_H>8])

df = x[!is.na(x$Likelihood_Recommend_H),]
str(df)
df$NPS = c("0")
for(i in 1:length(df))
{
  if(df$Likelihood_Recommend_H[i]>8)
    df$NPS[i] <- c("P")
  else
    df$NPS[i] <- c("D")
}



head(table(df$City_PL,df$NPS))

str(df$NPS)

mean(df$Likelihood_Recommend_H)

df$City_PL[(df$NPS=="D") & (df$City_PL == "New York")]

#glimpse(df)



table(df$Likelihood_Recommend_H)

temp = (table(df$City_PL,df$Likelihood_Recommend_H))



#fix(df)
#df.USA <- subset(df, COUNTRY_CODE_R == "UNITED STATES")

#str(df.USA)
#head(df.USA$COUNTRY_CODE_R, 10)
#A



mean(df$Likelihood_Recommend_H)

(table(df$City_PL,df$NPS))

# Nothing interesting in Length of Stay
#mean(df.USA$LENGTH_OF_STAY_C[(df.USA$Likelihood_Recommend_H<6)])
#mean(df.USA$LENGTH_OF_STAY_C[(df.USA$Likelihood_Recommend_H>6) & (df.USA$Likelihood_Recommend_H<8)])
#mean(df.USA$LENGTH_OF_STAY_C[(df.USA$Likelihood_Recommend_H>7)])

# Lets do the NPS first

# Promoters: 9-10
# Detractors: 1-6
# Passives: 7-8

#fix(df.USA)

#PercentPromoters <- ((length(df.USA$Likelihood_Recommend_H[df.USA$Likelihood_Recommend_H>8]))/(length(df.USA$Likelihood_Recommend_H)))*100

#PercentDetractors <- ((length(df.USA$Likelihood_Recommend_H[df.USA$Likelihood_Recommend_H<7]))/(length(df.USA$Likelihood_Recommend_H)))*100


for(i in 1:length(df))
{
  if(df$Likelihood_Recommend_H[i] > 8)
    df$NPS[i] <- c("P")
  if(df$Likelihood_Recommend_H[i] < 7)
    df$NPS[i] <- c("D")
  else
    df$NPS[i] <- c("Pas")
}


for(i in 1:length(df))
{
  if(df$Likelihood_Recommend_H[i] > 8)
    df$NPS[i] <- c("P")
  else
    df$NPS[i] <- c("D")
}




# Nothing Here
mean(df.USA$Length_Stay_H[(df.USA$NPS_Type)=="Promoter"])
mean(df.USA$Length_Stay_H[(df.USA$NPS_Type)=="Passive"])
mean(df.USA$Length_Stay_H[(df.USA$NPS_Type)=="Detractor"])


head(df.USA$POV_H)


table(df.USA$POV_CODE_C[(df.USA$NPS_Type)=="Promoter"])
table(df.USA$POV_CODE_C[(df.USA$NPS_Type)=="Passive"])
table(df.USA$POV_CODE_C[(df.USA$NPS_Type)=="Detractor"])

PercentLeisureSatisfied <-( (length(df.USA$POV_H[((df.USA$POV_H)=="Leisure")&(df.USA$NPS_Type=="Promoter")]))
                            /
                              (length(df.USA$POV_H[(df.USA$POV_H)=="Leisure"])) ) * 100
                        
PercentLeisureNotSatisfied <-( (length(df.USA$POV_H[((df.USA$POV_H)=="Leisure")&(df.USA$NPS_Type=="Detractor")]))
                            /
                              (length(df.USA$POV_H[(df.USA$POV_H)=="Leisure"])) ) * 100
# Nothing here


fix(df.USA)



temp = sort(table(df$City_PL))
tail(temp,10)
sort(table(df.USA$STATE_R[(df.USA$NPS_Type)=="Promoter"]))
sort(table(df.USA$STATE_R[(df.USA$NPS_Type)=="Passive"]))
sort(table(df.USA$STATE_R[(df.USA$NPS_Type)=="Detractor"]))


#getwd()
#write.csv(df.USA, file = "df.USA.csv")


library(plyr) 
res<-ddply(df.USA,.(STATE_R=STATE_R,NPS_Type=NPS_Type), summarize, Count/sum(Count)) 



length(df.USA$Conference_PL[((df.USA$NPS_Type)=="Detractor") & ((df.USA$POV_CODE_C)=="BUSINESS") & ((df.USA$Conference_PL)=="N")])
length(df.USA$Conference_PL[((df.USA$NPS_Type)=="Detractor") & ((df.USA$POV_CODE_C)=="BUSINESS") & ((df.USA$Conference_PL)=="Y")])

length(df.USA$Conference_PL[((df.USA$NPS_Type)=="Promoter") & ((df.USA$POV_CODE_C)=="BUSINESS") & ((df.USA$Conference_PL)=="Y")])

length(df.USA$Conference_PL[((df.USA$NPS_Type)=="Detractor") & ((df.USA$POV_CODE_C)=="LEISURE") & ((df.USA$Conference_PL)=="N")])
length(df.USA$Conference_PL[((df.USA$NPS_Type)=="Detractor") & ((df.USA$POV_CODE_C)=="LEISURE") & ((df.USA$Conference_PL)=="Y")])



str(df.USA$Age_Range_H)
head(df.USA$Internet_Sat_H)
length(df.USA[(df.USA$NPS_Type=="Promoter") & (df.USA$Internet_Sat_H<6),])

fix(df.USA)
table(df.USA$Internet_Sat_H, df.USA$NPS_Type)




length(df.USA$NPS_Type[( ((df.USA$POV_CODE_C)=="LEISURE") & ((df.USA$Mini.Bar_PL)=="N") & ((df.USA$Restaurant_PL)== "N"))])

length(df.USA$NPS_Type[( ((df.USA$POV_CODE_C)=="LEISURE") & ((df.USA$Mini.Bar_PL)=="N") & ((df.USA$Restaurant_PL)== "N")) & (df.USA$NPS_Type=="Detractor") ])





###############################################################Map
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################


install.packages("maps")
install.packages("mapproj")
install.packages("plotrix")
install.packages("rworldmap")



library(maps)
library(mapproj)
library(plotrix)
library(rworldmap)
library(openintro)

tf <- read.csv(file = "C:/Users/Shrutik/Desktop/Spring 2016/IST 687/Project/StatesPDN.csv", sep =",")
str(tf)
states <- map_data("state")

tf$region <- abbr2state(tf$StateName)


map.df <- merge(states,tf, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]
ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Neutral))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()

