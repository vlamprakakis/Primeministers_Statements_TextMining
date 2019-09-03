library(tidyverse)
library(tokenizers)
library(tm)
library(ggplot2)
library(wordcloud)
library(udpipe)
library(lattice)
install.packages("wordcloud2")
library(wordcloud2)
install.packages("corpus")
library(corpus)

# Set UTF-8 as default encoding (in order to read greek characters)
defaultEncoding <- "UTF-8"

# read (print) the lines of the text
text2 <- readLines("mitsotakis.txt",encoding = defaultEncoding)

# create a Corpus variable
docs <- Corpus(VectorSource(text2))

# check the text format
inspect(docs)

# text statistics
text_stats(docs)

# **DATA CLEANING**
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# remove stopwords ((Error in stopwords(kind = "gr") : no stopwords available for 'gr')
docs <- tm_map(docs,removeWords, stopwords(kind = "gr"))

# remove (manually) stopwords.See also https://github.com/stopwords-iso/stopwords-el
docs <- tm_map(docs,removeWords, c("και","της", "την", "να", "από", "το", "η", 
                                   "που", "του", "με", "για", "σε", "τα", "στην", 
                                   "τη", "των", "τους", "οι", "στο", "τον", "τις", 
                                   "ο", "αυτό", "αυτή", "ότι", "στα", "στη", "ως", 
                                   "στις", "οποίο", "στον", "αυτά", "ή", "στους",
                                   "τι", "έναν", "ενός", "ό", "μία", "απ", "κ",
                                   "κι", "γι", "αλλά", "δεν", "είναι", "έχουν",
                                   "οποία", "μια", "ένα", "όπως", "δύο", "ενώ", "έτσι"))

# remove (extra) stopwords
docs <- tm_map(docs,removeWords, c("νέας", "δημοκρατίας", "χειροκροτήματα", 
                                   "παρατεταμένα", "ζωηρά", "πτέρυγα"))

# **END DATA CLEANING**

#create a document term matrix.
dtm <- DocumentTermMatrix(docs)

#transpose the matrix
tdm <- TermDocumentMatrix(docs)

# check-point...
tdm

# Each row represents a word in the text and each column represents an individual sentence. 
# So if you look at the word ελλάδα, you can see it appears once in sentence 176 and 2 and twice 
#in sentence 209.The other documents are not appearing at the console. 

inspect(tdm)

#Organize by frequency
freq <- colSums(as.matrix(dtm)) 

length(freq)  

ord <- order(freq)

# check-point
ord

freq

# in order to check most and least frequently terms
freq <- colSums(as.matrix(dtm))

# the ‘colSums()’ function generates a table reporting how often each word frequency occurs.
#  Using the ’head()" function we can see the distribution of the least-frequently used words.
head(table(freq), 20)

# Sort into dectreasing order
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)

head(freq, 20)  

# terms with lowest frequency = 10
findFreqTerms(dtm, lowfreq=10)


# transform to dataframe for charts 
wf <- data.frame(word=names(freq), freq=freq) 

head(wf)

# create chart from dataframe object -> terms that appear more than 8 times 
b <- ggplot(subset(wf, freq>8), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))

# check chart
b

# word cloud 1 -> terms with min. freq. =20
set.seed(142)
wordcloud(names(freq), freq, min.freq=10, scale = c(5, .4),
          colors = brewer.pal(6, "Dark2")) 

# word cloud 2 -> 50 most frequent words
wordcloud(names(freq), freq, max.words = 70, random.order = FALSE, scale = c(5, .4), rot.per = .1,
          colors = brewer.pal(6, "Dark2"))

# wordclous2 function
wordcloud2(data = wf)

# find correlation of term "θα" with other terms
findAssocs(tdm, c("θα"), corlimit = 0.45)

# find correlation of term "ελλάδα" with other terms
findAssocs(tdm, c("ελλάδα"), corlimit = 0.7)

# find correlation of term "κυβέρνηση" with other terms
findAssocs(tdm, c("κυβέρνηση"), corlimit = 0.5)

# find correlation of term "ανάπτυξη" with other terms
findAssocs(tdm, c("ανάπτυξη"), corlimit = 0.5)

# find correlation of term "πολιτική" with other terms
findAssocs(tdm, c("πολιτική"), corlimit = 0.4)

# find correlation of term "χώρα" with other terms
findAssocs(tdm, c("χώρα"), corlimit = 0.4)

# save dataFrame as txt file to perform udpipe function
write.table(wf, file = "wf.txt", sep="\t")

#**annotation analysis**

ud_model <- udpipe_download_model(language = "greek")

ud_model <- udpipe_load_model(ud_model$file_model)

txt <- readLines ("mitsotakis(without ..).txt", encoding = defaultEncoding)

x <- udpipe_annotate(ud_model, x = txt)

x <- as.data.frame (x)

# x structure
str(x)

table(x$upos)

stats <- txt_freq(x$upos)

stats$key <- factor(stats$key, levels = rev(stats$key))

barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

## Collocation (words following one another)
stats <- keywords_collocation(x = x, 
                              term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)
stats

head(stats)

## Co-occurrences: How frequent do words occur in the same sentence, 
## in this case only nouns or adjectives?

stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                      term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))

head(stats)

## most occuring Nouns
stats <- subset(x, upos %in% c("NOUN"))

stats <- txt_freq(stats$token)

stats$key <- factor(stats$key, levels = rev(stats$key))

barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")

## most occuring Adjectives
stats <- subset(x, upos %in% c("ADJ"))

stats <- txt_freq(stats$token)

stats$key <- factor(stats$key, levels = rev(stats$key))

barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")

## most occuring Verbs
stats <- subset(x, upos %in% c("VERB"))

stats <- txt_freq(stats$token)

stats$key <- factor(stats$key, levels = rev(stats$key))

barchart(key ~ freq, data = head(stats, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")

# Using RAKE -> short for Rapid Automatic Keyword Extraction algorithm. 
# It is a domain independent keyword extraction algorithm which tries to determine 
# key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))

stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

barchart(key ~ rake, data = head(subset(stats, freq > 2), 10), col = "red", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")
####
stats3 <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("VERB", "NOUN"))

stats3$key <- factor(stats3$keyword, levels = rev(stats3$keyword))

barchart(key ~ rake, data = head(subset(stats3, freq > 2), 10), col = "blue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")

## Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")

stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)

stats <- subset(stats, ngram > 1 & freq > 2)

stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

barchart(key ~ freq, data = head(stats, 20), col = "magenta", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")


