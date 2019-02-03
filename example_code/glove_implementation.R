remove(list = ls())
setwd("/Users/francisco06121988/Desktop/online_learning/text_mining_in_practice_with_r/")

##Set Options
options(stringsAsFactors = FALSE)
options(scipen = 999)
Sys.setlocale('LC_ALL','C')

##Load word2vec packages
library(text2vec)
library(data.table)
library(tm)

text <- fread('Airbnb-boston_only.csv',showProgress = TRUE)##Load text
airbnb <- data.table(review_id = text$review_id,
                     comments = text$comments,
                     review_scores_rating = text$review_scores_rating)


##Preprocess the text
airbnb$comments <- removeWords(airbnb$comments,
                               c(tidytext::stop_words$word,"Boston")) ##Remove stop words

airbnb$comments <- removePunctuation(airbnb$comments) ##No Punctuation
airbnb$comments <- stripWhitespace(airbnb$comments) ##Remove white space
airbnb$comments <- tolower(airbnb$comments) ##To lower
airbnb$comments <- rtweet::plain_tweets(airbnb$comments)

##Split the tokens
tokens <- str_split(airbnb$comments, pattern = " ")

##Create a vocabulary: basic occurrence data frame
##Word i appeared y times and in z documents
vocab <- create_vocabulary(itoken(tokens), ngram = c(1,1))

#Pruning the vocabulary: Removing words that appear infrequently
vocab <- prune_vocabulary(vocab, term_count_min = 5)

##Build the TCM
iter <- itoken(tokens)
vectorizer <- vocab_vectorizer(vocabulary = vocab)
tcm <- create_tcm(iter, vectorizer, skip_grams_window = 5) ##Create term co-occurrence matrix

##Fit Glove Model
fit.glove <- GloVe$new(word_vectors_size = 50,vocabulary = vocab,
                       x_max = 10, learning_rate = 0.2)

##Explore the output
word_vectors_main = fit.glove$fit_transform(tcm, n_iter = 10)
word_vectors_context = fit.glove$components
word_vectors  = word_vectors_main + t(word_vectors_context)

row.names(word_vectors) <- rownames(tcm)
word.vec.norm <- sqrt(rowSums(word_vectors**2))

##
good.walks <- word_vectors['walk', ,drop = FALSE] - 
    word_vectors['terrible',,drop = FALSE] - 
    word_vectors['disappointed',,drop = FALSE] + 
    word_vectors['good', , drop = FALSE]

cos.dist <- dist2(good.walks,word_vectors,'cosine',norm = 'l2')
head(sort(1 - cos.dist[1,], decreasing = T), 20)

##
dirty.sink <- word_vectors['sink', ,drop = FALSE] +
    word_vectors['dirty', , drop = FALSE]

cos.dist <- dist2(dirty.sink,word_vectors,'cosine',norm = 'l2')
head(sort(1 - cos.dist[1,], decreasing = T), 20)
