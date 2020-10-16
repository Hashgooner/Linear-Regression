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
library(plotly)

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
newsHeadlineData$headline_category <- str_trim(newsHeadlineData$headline_category,side = 'both')
newsHeadlineData$headlineLen <- apply(newsHeadlineData,2,nchar)[,3]
h <- hist(newsHeadlineData$headlineLen, main = 'Histogram for Headline Length', col = '#add8e6', 
          xlab = 'Headline Length', breaks = 15)
by_year <- count(newsHeadlineData %>% group_by(year))
barplot(by_year$n, names.arg=by_year$year, las=2, xlab='Years',ylab = 'Number of Articles', col='black',
        main = 'Number of articles published in a year')
by_month <- count(newsHeadlineData %>% group_by(month))
barplot(by_month$n, names.arg =  by_month$month, las=2, xlab='Months',ylab = 'Number of Articles', col='black',
        main = 'Number of articles published in specific month')

newsHeadlineData$headline_category[newsHeadlineData$headline_category=='unknown'] <- ''
newsHeadlineData$mainCategory <- sub('\\..*','',newsHeadlineData$headline_category)
newsHeadlineData$SubmainCategory <- sub('\\w+\\.*','',newsHeadlineData$headline_category)
newsHeadlineData$SubmainCategory <- sub('\\..*','',newsHeadlineData$SubmainCategory)
mainCategoryFreq <- sort(table(newsHeadlineData$mainCategory), decreasing = T)
SubmainCategoryFreq <- sort(table(newsHeadlineData$SubmainCategory), decreasing = T)
barplot(mainCategoryFreq[0:10], las=2, xlab = 'Main Category', ylab = 'Frequency', col = 'black',
        main = 'Frequency chart of main categories (Top 10)')
barplot(SubmainCategoryFreq[0:10], las=2, xlab = 'Sub Main Category', ylab = 'Frequency', col = 'black',
        main = 'Frequency chart of sub main categories (Top 10)')


newsHeadlineData_split <- split(newsHeadlineData, newsHeadlineData$year)
newsHeadlineData_2001 <- newsHeadlineData_split$'2002'
newsHeadlineData_2001split <- split(newsHeadlineData_2001, newsHeadlineData_2001$month)
newsHeadlineData_2001split_1 <- newsHeadlineData_2001split$'1'

mainCategoryFreq <- table(newsHeadlineData_2001split_1$mainCategory)
barplot(mainCategoryFreq, las = 2)

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