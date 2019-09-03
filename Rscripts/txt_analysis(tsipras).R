library(tidyverse)
library(tokenizers)
library(tm)
library(quanteda)

# defaultEncoding <- "UTF-8" (it s already readable)

text <- paste(readLines("tsipras.txt"), collapse = "\n")

text

words <- tokenize_words(text)

count_words(text)

count_characters(text)

# count bytes of file
bytes <- file.size("tsipras.txt")

# approximate average words
aprox_words <- bytes/10

# I used this one instead of the bytes
minutes <- 8119/200

minutes
#average adult human can read 200-220 words in one minute
read_time_minutes <- aprox_words/220

# print estimated reading time
read_time_minutes


length(words[[1]])

# count unique words
length(unique(words[[1]]))

# count sentences
nsentence(text)

#
nchar(text)

# number of characters
nchar(words)

# create oblect for counting each word length
word_lengths <- lapply(words, str_length)

# print
word_lengths

# average word lenth
lapply(word_lengths, mean)


tab <- table(words[[1]])

tab <- data_frame(word = names(tab), count = as.numeric(tab))

tab <- arrange(tab, desc(count))

tab

view(tab)

# find longest words and their frequency
filter(tab, nchar(word) >15)

filter_tab <- filter(tab, nchar(word) >15)

options(tibble.print_max = 40, tibble.print_min = 20)

filter_tab

view(filter_tab)

tab

library(wordcloud)

library(RColorBrewer)

myCorpus <- Corpus(VectorSource(filter_tab))# transform to Corpus in order to implement Word Cloud function


tdm <- TermDocumentMatrix(myCorpus) # transform to TermDocumentMatrix

wordcloud (myCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
set.seed(142)   
wordcloud(myCorpus, max.words=100, rot.per=0.2, colors=brewer.pal(8, "Dark2"))
