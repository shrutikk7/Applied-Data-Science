
#fun(nyData)



       
table(sfData$Postal.Code_PL,sfData$NPS)


head(x$City_PL)
sfData = x[(x$City_PL=="San Francisco"),]




fun <- function(df,postalCode)
{
  # Overall Satisfaction:
  cs1 = mean(df$Overall_Sat_H[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")], na.rm=T)
  print(paste("Desired Overall Satisfaction: ",cs1))
  
  cs2 = mean(df$Customer_SVC_H[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")], na.rm=T)
  print(paste("Current Overall Satisfaction: ",cs2)) 
  
  
  
  # Tranquility:
  cs1 = mean(df$Tranquility_H[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")], na.rm=T)
  print(paste("Desired Tranquility: ",cs1))
  
  cs2 = mean(df$Tranquility_H[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")], na.rm=T)
  print(paste("Current Tranquility: ",cs2))
  
  # Hotel Condition:
  cs1 = mean(df$Condition_Hotel_H[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")], na.rm=T)
  print(paste("Desired Hotel Condition: ",cs1))
  
  cs2 = mean(df$Condition_Hotel_H[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")], na.rm=T)
  print(paste("Current Hotel Condition: ",cs2))
  
  
  # Customer Service:
  cs1 = mean(df$Customer_SVC_H[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")], na.rm=T)
  print(paste("Desired Customer Service: ",cs1))
  
  cs2 = mean(df$Customer_SVC_H[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")], na.rm=T)
  print(paste("Current Customer Service: ",cs2))
  
  # Staff Cared:
  cs1 = mean(df$Staff_Cared_H[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")], na.rm=T)
  print(paste("Desired Staff Cared: ",cs1))
  
  cs2 = mean(df$Staff_Cared_H[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")], na.rm=T)
  print(paste("Current Staff Cared: ",cs2))
  
  # Spa Service: 
  
  a1=  (df$Spa_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(a1[a1=="Y"])>length(a1[a1=="N"]) )
  {a1<-c("Y")}
  if(length(a1[a1=="Y"])<length(a1[a1=="N"]))
  {a1<-c("N")}
  if(length(a1[a1=="Y"])==length(a1[a1=="N"]))
  {a1<-c("Data Not Available")}
  
  a2=  (df$Spa_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(a2[a2=="Y"])>length(a2[a2=="N"]) )
  {a2<-c("Y")}
  if(length(a2[a2=="Y"])<length(a2[a2=="N"]))
  {a2<-c("N")}
  if(length(a2[a2=="Y"])==length(a2[a2=="N"]))
  {a2<-c("Data Not Available")}
  
  
  if(a1!=a2)
  {
    print(paste("Desired Spa Service: ",a1))
    print(paste("Current Spa Service: ",a2))
  }
  
  
  #Casino
  
  b1=  (df$Casino_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  b2=  (df$Casino_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Casino: ",b1))
    print(paste("Current Casino: ",b2))
  }
  
  # Boutique_PL
  
  b1=  (df$Boutique_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  b2=  (df$Boutique_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Boutique: ",b1))
    print(paste("Current Boutique: ",b2))
  }
  
  
  
  
  
  
  #,df$Business.Center_PL
  
  
  b1=  (df$Business.Center_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  b2=  (df$Business.Center_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Business.Center_PL: ",b1))
    print(paste("Current Business.Center_PL: ",b2))
  }
  
  
  
  
  
  #,df$Conference_PL
  #
  b1=  (df$Conference_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  b2=  (df$Conference_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Conference_PL: ",b1))
    print(paste("Current Conference_PL: ",b2))
  }
  
  #,df$Convention_PL
  #
  b1=  (df$Convention_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  b2=  (df$Convention_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Convention_PL: ",b1))
    print(paste("Current Convention_PL: ",b2))
  }
  
  #,df$Dry.Cleaning_PL
  #
  #
  b1=  (df$Dry.Cleaning_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  b2=  (df$Dry.Cleaning_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Dry.Cleaning_PL: ",b1))
    print(paste("Current Dry.Cleaning_PL: ",b2))
  }
  
  
  #,df$Elevators_PL
  
  b1=  (df$Elevators_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  b2=  (df$Elevators_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Elevators_PL: ",b1))
    print(paste("Current Elevators_PL: ",b2))
  }
  
  
  #,df$Fitness.Center_PL
  
  b1=  (df$Fitness.Center_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  b2=  (df$Fitness.Center_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  if(b1!=b2)
  {
    print(paste("Desired Fitness.Center_PL: ",b1))
    print(paste("Current Fitness.Center_PL: ",b2))
  }
  
  
  
  
  #,df$Fitness.Trainer_PL
  
  
  b1=  (df$Fitness.Trainer_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  b2=  (df$Fitness.Trainer_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Fitness.Trainer_PL: ",b1))
    print(paste("Current Fitness.Trainer_PL: ",b2))
  }
  
  
  
  #,df$Golf_PL
  
  b1=  (df$Golf_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  
  b2=  (df$Golf_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Golf_PL: ",b1))
    print(paste("Current Golf_PL: ",b2))
  }
  
  
  
  #,df$Indoor.Corridors_PL
  
  b1=  (df$Indoor.Corridors_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  b2=  (df$Indoor.Corridors_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Indoor.Corridors_PL: ",b1))
    print(paste("Current Indoor.Corridors_PL: ",b2))
  }
  
  
  
  #,df$Laundry_PL
  
  b1=  (df$Laundry_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  b2=  (df$Laundry_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Laundry_PL: ",b1))
    print(paste("Current Laundry_PL: ",b2))
  }
  
  
  #,df$Limo.Service_PL
  
  b1=  (df$Limo.Service_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  b2=  (df$Limo.Service_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  if(b1!=b2)
  {
    print(paste("Desired Limo.Service_PL: ",b1))
    print(paste("Current Limo.Service_PL: ",b2))
  }
  
  
  
  
  #,df$Mini.Bar_PL
  
  b1=  (df$Mini.Bar_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  
  b2=  (df$Mini.Bar_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Mini.Bar_PL: ",b1))
    print(paste("Current Mini.Bar_PL: ",b2))
  }
  
  
  
  #,df$Pool.Indoor_PL
  
  b1=  (df$Pool.Indoor_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  b2=  (df$Pool.Indoor_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Pool.Indoor_PL: ",b1))
    print(paste("Current Pool.Indoor_PL: ",b2))
  }
  
  
  
  #,df$Pool.Outdoor_PL
  
  b1=  (df$Pool.Outdoor_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  
  b2=  (df$Pool.Outdoor_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Pool.Outdoor_PL: ",b1))
    print(paste("Current Pool.Outdoor_PL: ",b2))
  }
  
  
  
  #,df$Regency.Grand.Club_PL
  
  b1=  (df$Regency.Grand.Club_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  
  b2=  (df$Regency.Grand.Club_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Regency.Grand.Club_PL: ",b1))
    print(paste("Current Regency.Grand.Club_PL: ",b2))
  }
  
  
  
  #,df$Resort_PL
  
  
  b1=  (df$Resort_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  
  b2=  (df$Resort_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Resort_PL: ",b1))
    print(paste("Current Resort_PL: ",b2))
  }
  
  
  #,df$Restaurant_PL
  
  b1=  (df$Restaurant_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  
  b2=  (df$Restaurant_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Restaurant_PL: ",b1))
    print(paste("Current Restaurant_PL: ",b2))
  }
  
  
  #,df$Self.Parking_PL
  
  
  b1=  (df$Self.Parking_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  
  b2=  (df$Self.Parking_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  
  if(b1!=b2)
  {
    print(paste("Desired Self.Parking_PL: ",b1))
    print(paste("Current Self.Parking_PL: ",b2))
  }
  
  
  #,df$Shuttle.Service_PL
  
  
  b1=  (df$Shuttle.Service_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  
  b2=  (df$Shuttle.Service_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Shuttle.Service_PL: ",b1))
    print(paste("Current Shuttle.Service_PL: ",b2))
  }
  
  #,df$Ski_PL
  
  b1=  (df$Ski_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  
  b2=  (df$Ski_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Ski_PL: ",b1))
    print(paste("Current Ski_PL: ",b2))
  }
  
  
  
  
  
  #,df$Valet.Parking_PL
  
  b1=  (df$Valet.Parking_PL[(df$Postal.Code_PL==94108) & (df$Brand_PL=="Grand Hyatt")])
  if( length(b1[b1=="Y"])>length(b1[b1=="N"]) )
  {b1<-c("Y")}
  if(length(b1[b1=="Y"])<length(b1[b1=="N"]))
  {b1<-c("N")}
  if(length(b1[b1=="Y"])==length(b1[b1=="N"]))
  {b1<-c("Data Not Available")}
  
  
  
  b2=  (df$Valet.Parking_PL[(df$Postal.Code_PL==postalCode) & (df$Brand_PL=="Hyatt")])
  if( length(b2[b2=="Y"])>length(b2[b2=="N"]) )
  {b2<-c("Y")}
  if(length(b2[b2=="Y"])<length(b2[b2=="N"]))
  {b2<-c("N")}
  if(length(b2[b2=="Y"])==length(b2[b2=="N"]))
  {b2<-c("Data Not Available")}
  
  
  if(b1!=b2)
  {
    print(paste("Desired Valet.Parking_PL: ",b1))
    print(paste("Current Valet.Parking_PL: ",b2))
  }
  
  
  
}


fun(sfData,94133)





sfdf <- data.frame(c("Overall Satisfaction","Tranquility","Hotel Condition","Customer Service","Staff Cared")
                   ,c(8.59,8.45,8.90,8.99,8.72)
                   ,c(7.80,6.80,6.52,7.80,7.80)
)

colnames(sfdf) <- c("Rating","Desired","Current")



melteddf = melt(sfdf, id = "Rating")

ggplot(melteddf, aes(Rating, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + 
  labs(title = "San Francisco")



