

# BASIC REQUIRED PACKAGES
rm(list=ls(all.names=TRUE)) #remove the list
library(twitteR)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# STEP 1 : SET TWITTER API FOR USING twitteR PACKAGE"

consumerKey <- "O1PEoDol15NeMT9aMipuljzOz"
consumerSecret <- "WxMijfX7nptqq261LOf1NY7Iw7r38uE9iTFz9DnNIowJ7nlaou"
accessToken <- "1032167512186675200-V5eAAYm0fmT1jj8pJdyg7WyEgFF1SH"
accessSecret <- "xTP3a5CyGk01GKg639o8vkZXDA6jyyVcC7f4e3iNMdTzj"
options(httr_oauth_cache=T) # This will enable the use of a local file to cache OAuth access credentials between R sessions.
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)


# STEP 2: GET THE  TAGGED TWEETS DATA LIST
# GET THE '#___________" TAGGED TWEETS
fub <- searchTwitter('#futurebass', n=10000)
fub_txt <- sapply(fub, function(x) x$getText())
Encoding(fub_txt)<-'UTF-8'
fub_doc <- Corpus(VectorSource(fub_txt))

fub_doc <- tm_map(fub_doc, content_transformer(tolower))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
fub_doc <- tm_map(fub_doc, content_transformer(removeNumPunct))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
fub_doc <- tm_map(fub_doc, content_transformer(removeURL))
fub_doc <- tm_map(fub_doc, content_transformer(gsub), pattern="\\W", replace=" ")

fub_doc <- tm_map(fub_doc, removePunctuation)
fub_doc <- tm_map(fub_doc, removeNumbers)
fub_doc <- tm_map(fub_doc, stripWhitespace)
fub_doc <- tm_map(fub_doc, removeWords, stopwords("en"))

fuba <- TermDocumentMatrix(fub_doc)
b <- as.matrix(fuba)
u <- sort(rowSums(b), decreasing = T)
f <- data.frame(word = names(u), freq=u)
head(f,10)

wordcloud(words = f$word, freq = f$freq, min.freq = 1, max.words =100,
          random.order = F, colors = brewer.pal(8,"Dark2"))
