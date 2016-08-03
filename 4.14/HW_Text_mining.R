# Name: Shrutik
# Text Mining HW


install.packages("tm")
library(tm)

install.packages("wordcloud")
library(wordcloud)

install.packages("reshape")
library(reshape)

# Reading positive and negative data

pos <- "C:/Users/Shrutik/Desktop/Spring 2016/IST 687/4.14/afinn111.txt"
p <- scan(pos, character(0),sep = "\n") 

head(p)
str(p)

p <- data.frame(p, stringsAsFactors = F)
p <- data.frame(do.call(rbind, strsplit(as.vector(p$p), split = "\t")))
names(p) <- c("word","level")

head(p)
str(p)




mlkSource <- "C:/Users/Shrutik/Desktop/Spring 2016/IST 687/4.14/MLK_speech.txt"

mlk <- readLines(mlkSource) # Reading the speech. Notice how we use readLines for speech and scan for words.
str(mlk)
mlk[1]

words.vec <- VectorSource(mlk) # To treate each component of a vectoe as a document
words.corpus <- Corpus(words.vec) # Creating the corpus(heart) of the document

# Removing stop words 
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))


tdm <- TermDocumentMatrix(words.corpus) # Does what it says. Its also called Document Term Matrix
tdm

words <- as.matrix(tdm) # Matrix: It's easy to compute with matrix in this case
str(words)

wordCounts <- rowSums(words) # Number of Rows: Duh
wordCounts <- sort(wordCounts, decreasing = T) # this will give you the major stock words, but we dont need this cause we already removed stock words

head(wordCounts) # We do not have any stock words

#overall_score <- 0 # Set the score to 0 initially. Set as 0 everytime you run the loop
overall_score

str(p)
length(p$word)

str(df)



    
for(i in 1:length(df$word))
{
  for(j in 1:length(p$word))
  {
    if(df$word[i] == p$word[j])
    {
      overall_score = overall_score + (as.numeric(df$freq[i]) * as.numeric(p$level[j]))
    }
  }
}

print(paste("Overall Score: ",overall_score))




########################################
########################################
# A Dummy DF since we need to return 2 values
# So we will create a dummy df and store the two variables in that
# and then return the dummyDF
# Masterstroke, I know!
dummyDF <- data.frame(c("a"))
# Function to calculate Positive and Negative Ratio
# We can use this to calculate the Ratios which we found above,
# its the same code. 
# 
# 


posFunction <- function(MLK)
{
  
  words.vec <- VectorSource(MLK) # To treate each component of a vectoe as a document
  words.corpus <- Corpus(words.vec) # Creating the corpus(heart) of the document
  
  # Removing stop words 
  words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
  words.corpus <- tm_map(words.corpus, removePunctuation)
  words.corpus <- tm_map(words.corpus, removeNumbers)
  words.corpus <- tm_map(words.corpus, content_transformer(tolower))
  
  
  tdm <- TermDocumentMatrix(words.corpus) # Does what it says. Its also called Document Term Matrix
  
  words <- as.matrix(tdm) # Matrix: It's easy to compute with matrix in this case
  
  
  wordCounts <- rowSums(words) # Number of Rows: Duh
  wordCounts <- sort(wordCounts, decreasing = T) # this will give you the major stock words, but we dont need this cause we already removed stock words
  
  
  
  df <- data.frame(word = names(wordCounts), freq = wordCounts, stringsAsFactors=FALSE) # Creating a df which has all the words and their frequency
  # Creating a word Cloud
  
  overall_score <- 0
  
  
  
  
  for(i in 1:length(df$word))
  {
    for(j in 1:length(p$word))
    {
      if(df$word[i] == p$word[j])
      {
        overall_score = overall_score + (as.numeric(df$freq[i]) * as.numeric(p$level[j]))
      }
    }
  }
  
  print(paste("Overall Score: ",overall_score))
  
  
  dummyDF <- data.frame(overall_score)
  return(dummyDF)
  
}

posFunction(mlk)


len <- length(mlk)
len <- round(len/4) # Dividing the Speech in 4 parts
l =1 # Important, this will be the lower limit
dummyDF1 = data.frame(c("a"))

# Please Note: this for loop will run accurately just once. 
# After that you will have to re initialize len and l values.

# Remember the dummyDF, same thing we do here
#x = data.frame(c("1")) 
x = data.frame(c("1"),c("2"),c("3"),c("4"))

# Loop where magic happens
for(i in 1:4)
{
  lower = l
  upper = l + len
  x[i] <- posFunction(mlk[lower:upper])
  #dummyDF1[i] <- x[i]
  l = upper
}

x$Para1
str(x)
colnames(x) <- c("Para1","Para2","Para3","Para4") # Renaming the columns

finaldf = data.frame(  c("1st","2nd","3rd","4th")
                      ,c(x$Para1,x$Para2,x$Para3,x$Para4))

colnames(finaldf) = c("Para","value")



barplot(finaldf$value
        ,names = finaldf$Para
        ,xlab = "Quarter"
        ,ylab = "Score")

