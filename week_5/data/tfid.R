library(httr)
library(RCurl)
library(XML)
library(dplyr)
library(readtext)
library(tm)
library(jiebaR)
library(jiebaRD)
library(tmcn)
library(tidytext)

data <- list()

url  <- "https://mojim.com/twh107409.htm"
html <- htmlParse( GET(url) )
url.list <- xpathSApply( html, "//span[@class='hc3' or @class='hc4']/a[@href]", xmlAttrs )
for( i in 1:104) {
 data <- rbind( data, paste('https://mojim.com', url.list[[i]][1], sep='') )
}
data <- unlist(data)
data<-gsub("https://mojim.com_blank", "", data)
data<-gsub("https://mojim.com/twy107409x4x10.htm", "", data)
data<-gsub("https://mojim.com/twy107409x4x11.htm", "", data)
data<-gsub("https://mojim.com/twy107409x3x7.htm", "", data)

data<-data[data!=""]



getdoc <- function(url)
{
  html <- htmlParse( getURL(url) )
  doc  <- xpathSApply( html, "//*[(@id = 'fsZx3')]", xmlValue )
  song <- xpathSApply( html, "//div[@id='Tb3']/a[5]", xmlValue )
  name <- paste('C:/Users/USER/Documents/GitHub/csx_rproject/week_5/data/', song, ".txt")
  write(doc, name, append = TRUE)
}

sapply(data, getdoc)


page <- readtext("C:/Users/USER/Documents/GitHub/csx_rproject/week_5/data/*.txt", encoding = "big5")
corpus <- Corpus(VectorSource(page$text))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})
myStopWords <- c("更多更詳盡歌詞","Mojim.com","魔鏡歌詞網", "作曲", "作詞", "編曲", "蛋", "堡", "杜", "振", "熙", "純音樂")
corpus <- tm_map(corpus, removeWords, myStopWords)
corpus$content

mixseg = worker()
jieba_tokenizer = function(c)
{
  unlist( segment(c[[1]], mixseg) )
}
seg = lapply(corpus, jieba_tokenizer)
seg<-unlist(seg)
corpuss <- Corpus(VectorSource(seg))

myStopWords <- c("感謝", "作曲", "作詞", "編曲", "蛋", "堡", "杜", "振", "熙", "純音樂")
corpuss <- tm_map(corpuss, removeWords, myStopWords)

dtm<-createDTM(corpuss, removeStopwords =F)
DTM<-as.matrix(dtm)
TDM<-t(DTM)
freq=rowSums(TDM)
head(freq,10)
tail(freq,10)
tail(sort(freq),n=10)

tidy<-tidy(dtm)
bind_tf_idf(tidy, term, document, n)
