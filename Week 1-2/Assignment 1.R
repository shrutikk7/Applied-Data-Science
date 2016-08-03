#Name: Shrutik Katchhi
#Preferred Name: Shrutik
#Lab 1:

temp<-"Hi"
 temp

 #demo()
 height<-c(59,60,61,58,67,72,70)
 weight<-c(150,140,180,220,160,140,130)
 height
 weight
 a<-150
 a
 #Average Height:
   av_height<-mean(height)
   av_height
   #Average Weight:
     av_weight<-mean(weight)
     av_weight
     #Length of Vectir Height:
       len_height<-length(height)
       len_height
       #Length of Vector Weight:
         len_weight<-length(weight)
         len_weight
       #Sum of Heights:
           sum_height<-sum(height)
           sum_height
           #Calculating Mean without the Function: 
             #Sum:
             sum_height
           #Length:
             len_height
           #So the mean is sum divided by length:
             average_height<-sum_height/len_height
             average_height
             #Similarly, mean for Weight:
               average_weight<-sum_weight/len_weight
              
               #First we need to define Sum of Weights:
               sum_weight<-sum(weight)
               sum_weight
               average_weight<-sum_weight/len_weight
               average_weight
               # So, we can see that the values for the mean(Average) remains the same
                 #To calculate maximum value in the vector height:
                 maxH<-max(height)
               maxH
               #To calculate minimum value in the vector weight:
                 minW<-min(weight)
               minW
               #To increase the weight of each person by 5:
                 new_weight<-weight+5
               new_weight
               #To compute pounds/inch individually:
                 person1<-new_weight[1]/height[1]
               person1
               person2<-new_weight[2]/height[2]
               person2
               person3<-new_weight[3]/height[3]
               person3
               person4<-new_weight[4]/height[4]
               person4
               person5<-new_weight[5]/height[5]
               person5
          
              #To test if max height is greater that 60:
               if(maxH>60) "Yes" else "No"
              #To test if minimum weight is greater than 'a'
               if(minW>a) "Yes" else "No"
               
               
              