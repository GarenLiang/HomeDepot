# HomeDepot
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(wordcloud)
library(tm)
library(SnowballC)

cat("Reading data\n")
train <- read_csv('../input/train.csv')
test <- read_csv('../input/test.csv')
desc <- read_csv('../input/product_descriptions.csv')


search_text <- paste(train$search_term, collapse=" ")
search_source <- VectorSource(search_text)
corpus <- Corpus(search_source)
corpus <- tm_map(corpus, content_transformer(tolower))
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency)
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])
