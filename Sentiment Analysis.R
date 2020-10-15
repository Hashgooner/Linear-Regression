library(lubridate)
library(wordcloud)
library(tm)
library(tidyr)
library(wordcloud2)
library(syuzhet)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(stringr)

newsHeadlineData <- read.csv("C:/Users/harsh/Downloads/india-news-headlines.csv")
head(newsHeadlineData)
str(newsHeadlineData)

newsHeadlineData$publish_date <- ymd(newsHeadlineData$publish_date)

head(newsHeadlineData)
tail(newsHeadlineData)
summary(newsHeadlineData)

dplyr::mutate(newsHeadlineData$year <- lubridate::year(newsHeadlineData$publish_date))
dplyr::mutate(newsHeadlineData$month <- lubridate::month(newsHeadlineData$publish_date))
dplyr::mutate(newsHeadlineData$day <- lubridate::day(newsHeadlineData$publish_date))
par(mfrow=c(1,1))
newsHeadlineData$headline_category[newsHeadlineData$headline_category=='unknown'] <- ''
unique(newsHeadlineData$headline_category)
newsHeadlineData$mainCategory <- sub('\\..*','',newsHeadlineData$headline_category)
mainCategoryFreq <- table(newsHeadlineData$mainCategory)
mainCategoryFreq
str_trim(newsHeadlineData$headline_category,side = 'both')

newsHeadlineData_split <- split(newsHeadlineData, newsHeadlineData$year)
newsHeadlineData_2001 <- newsHeadlineData_split$'2001'
newsHeadlineData_2001split <- split(newsHeadlineData_2001, newsHeadlineData_2001$month)
newsHeadlineData_2001split_1 <- newsHeadlineData_2001split$'1'
text <- newsHeadlineData_2001split_1$headline_text
docs <- Corpus(VectorSource(text))
docs <- tm_map(docs,tolower)
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs,removeNumbers)
docs <- tm_map(docs,removeWords,stopwords('english'))
docs <- tm_map(docs, stripWhitespace)
inspect(docs[1:5])

tdm <- TermDocumentMatrix(docs)
tdm <- as.matrix(tdm)
w <- rowSums(tdm)
w <- subset(w,w>=40)
barplot(w,las = 2)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w), freq = w, max.words = 100, min.freq = 5)

w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,shape = 'circle')

s <- get_nrc_sentiment(newsHeadlineData_2001split_1$headline_text)
head(s)
barplot(colSums(s),las=2)

# newsHeadlineData_split <- split(newsHeadlineData, newsHeadlineData$year)
# newsHeadlineData_2001 <- newsHeadlineData_split$'2001'
# newsHeadlineData_2001split <- split(newsHeadlineData_2001, newsHeadlineData_2001$month)
# newsHeadlineData_2001split_1 <- newsHeadlineData_2001split$'2'
# 
# text <- newsHeadlineData_2001split_1$mainCategory
# docs <- Corpus(VectorSource(text))
# docs <- tm_map(docs,tolower)
# docs <- tm_map(docs,removePunctuation)
# docs <- tm_map(docs,removeNumbers)
# docs <- tm_map(docs,removeWords,stopwords('english'))
# docs <- tm_map(docs, stripWhitespace)
# inspect(docs[1:5])
# 
# tdm <- TermDocumentMatrix(docs)
# tdm <- as.matrix(tdm)
# w <- rowSums(tdm)
# w <- subset(w,w>=40)
# barplot(w,las = 2)
# w <- sort(rowSums(tdm), decreasing = TRUE)
# set.seed(222)
# wordcloud(words = names(w), freq = w, max.words = 100, min.freq = 5)
# 
# w <- data.frame(names(w),w)
# colnames(w) <- c('word','freq')
# wordcloud2(w,shape = 'circle')
# 
# s <- get_nrc_sentiment(newsHeadlineData_2001split_1$mainCategory)
# head(s)
# barplot(colSums(s),las=2)