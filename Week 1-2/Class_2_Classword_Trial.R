Names<- c("A","B","C")
GPA <- c(33,2,3)

df <- data.frame(Names,GPA)

df

o <- order(df$GPA)

str(o)
o
sortedo <- df[o,]
sortedo
sortedo1 <- df[o,2]
sortedo1





df