# Name: Shrutik
# Text Mining HW


install.packages("tm")
library(tm)

install.packages("wordcloud")
library(wordcloud)

install.packages("reshape")
library(reshape)

# Reading positive and negative data

pos <- "C:/Users/Shrutik/Desktop/Spring 2016/IST 687/4.14/positive_words.txt"
p <- scan(pos, character(0),sep = "\n") 

head(p)
str(p)

p<- p[-1:-34] # Deleting all the rows which we don't need. Could've done it in the txt file as well.

# Same for negative

neg <- "C:/Users/Shrutik/Desktop/Spring 2016/IST 687/4.14/negative_words.txt"
n <- scan(neg, character(0),sep = "\n")

head(p)
str(p)

n<- n[-1:-34]

mlkSource <- "C:/Users/Shrutik/Desktop/Spring 2016/IST 687/4.14/MLK_speech.txt"

mlk <- readLines(mlkSource) # Reading the speech. Notice how we use readLines for speech and scan for words.
str(mlk)
mlk[1]

words.vec <- VectorSource(mlk) # To treate each component of a vectoe as a document
words.corpus <- Corpus(words.vec) # Creating the corpus(heart) of the document

# Removing stop words and s**t
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

df <- data.frame(word = names(wordCounts), freq = wordCounts) # Creating a df which has all the words and their frequency
# Creating a word Cloud
wordcloud(names(wordCounts), wordCounts, min.freq = 3, max.words=50, rot.per=0.40, colors=brewer.pal(8,"Dark2"))
# Function  Unique Names      Frequncy    At least 3 times  upper limit   40% will be verticle  colors                

# Calculate Positive and Neative word ratio

totalWords <- sum(wordCounts)
words1 <- names(wordCounts)

matched <- match(words1, p, nomatch = 0)
wordCounts[which(matched != 0)]
head(matched)

matchedCounts <- wordCounts[which(matched!=0)]
length(matchedCounts)

matchedWords <- names(matchedCounts)
matchedPositive <- sum(matchedCounts)

############
############

matched1 <- match(words1, n, nomatch = 0)
wordCounts[which(matched1 != 0)]
head(matched1)

matchedCounts1 <- wordCounts[which(matched1!=0)]
length(matchedCounts1)

matchedWords1 <- names(matchedCounts1)
matchedNegative <- sum(matchedCounts1)

matchedPositive # Number of Positive Words
matchedNegative # Number of Negative Words

totalWords <- length(words)

ratioPos <- matchedPositive/totalWords
ratioPos

ratioNeg <- matchedNegative/totalWords
ratioNeg


########################################
########################################
# A Dummy DF since we need to return 2 values
# So we will create a dummy df and store the two variables in that
# and then return the dummyDF
# Masterstroke, I know!
dummyDF <- data.frame(c("a","b"))
# Function to calculate Positive and Negative Ratio
# We can use this to calculate the Ratios which we found above,
# its the same code. 
posFunction <- function(MLK)
{

WORDS.VEC <- VectorSource(MLK)
WORDS.CORPUS <- Corpus(WORDS.VEC)
WORDS.CORPUS <- tm_map(WORDS.CORPUS, removeWords, stopwords("english"))
WORDS.CORPUS <- tm_map(WORDS.CORPUS, removePunctuation)
WORDS.CORPUS <- tm_map(WORDS.CORPUS, removeNumbers)
WORDS.CORPUS <- tm_map(WORDS.CORPUS, content_transformer(tolower))


TDM <- TermDocumentMatrix(WORDS.CORPUS)

WORDS <- as.matrix(TDM)

WORDCOUNTS <- rowSums(WORDS)

DF <- data.frame(word = names(WORDCOUNTS), freq = WORDCOUNTS)


TOTALWORDS <- sum(WORDCOUNTS)
WORDS1 <- names(WORDCOUNTS)

MATCHED <- match(WORDS1, p, nomatch = 0)
WORDCOUNTS[which(MATCHED != 0)]

MATCHEDCOUNTS <- wordCounts[which(MATCHED!=0)]

MATCHEDWORDS <- names(MATCHEDCOUNTS)
MATCHEDPOSITIVE <- sum(MATCHEDCOUNTS)

############
############

MATCHED1 <- match(WORDS1, n, nomatch = 0)
WORDCOUNTS[which(MATCHED1 != 0)]

MATCHEDCOUNTS1 <- WORDCOUNTS[which(MATCHED1!=0)]


MATCHEDWORDS1 <- names(MATCHEDCOUNTS1)
MATCHEDNEGATIVE <- sum(MATCHEDCOUNTS1)


TOTALWORDS <- length(WORDS)

RATIOPOS <- MATCHEDPOSITIVE/TOTALWORDS
RATIONEG <- MATCHEDNEGATIVE/TOTALWORDS

print(paste("Positive Ratio: ", RATIOPOS))
print(paste("Negative Ratio: ", RATIONEG))
print(paste("Difference: ", abs(RATIONEG - RATIOPOS) ))

dummyDF <- data.frame(RATIOPOS,RATIONEG)
return(dummyDF)

}


len <- length(mlk)
len <- round(len/4) # Dividing the Speech in 4 parts
l =1 # Important, this will be the lower limit
dummyDF1 = data.frame(c("a","b"))

# Please Note: this for loop will run accurately just once. 
# After that you will have to re initialize len and l values.

# Remember the dummyDF, same thing we do here
x = data.frame(c(1,1)) 
dummyDF1 = data.frame(c(1,1),c(2,2),c(3,3),c(4,4))

# Loop where magic happens
for(i in 1:4)
{
  lower = l
  upper = l + len
  x <- posFunction(mlk[lower:upper])
  dummyDF1[1,i] <- x[1,1]
  dummyDF1[2,i] <- x[1,2]
  l = upper
}

dummyDF1
str(dummyDF1)
colnames(dummyDF1) <- c("Para1","Para2","Para3","Para4") # Renaming the columns


Word = c("pos","neg")
OneMoreDF = data.frame(Word,dummyDF1) # This is the final Dataframe, I promise.


meltedOneMoreDF = melt(OneMoreDF, id = "Word") # Opps. We need it to plot a cool bar chart.

ggplot(meltedOneMoreDF, aes(variable, value)) +   
  geom_bar(aes(fill = Word),position = "dodge", stat="identity")





