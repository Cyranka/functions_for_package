remove(list = ls())
setwd("/Users/francisco06121988/Desktop/online_learning/text_mining_in_practice_with_r/")

##Set Options
options(stringsAsFactors = FALSE)
options(scipen = 999)
Sys.setlocale('LC_ALL','C')

##add Libraries
library(skmeans)
library(tm)
library(clue)
library(cluster)
library(fpc)
library(clue)
library(wordcloud)

##Define Clean Corpus Function
clean.corpus <- function(corpus){
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, c(stopwords('en'),'customer',
                                            'service', 'cusotmers', 'calls'))
    return(corpus)
}

wk.exp <- readr::read_csv("1yr_plus4.csv")
wk.source <- VCorpus(VectorSource(wk.exp$text))
wk.corpus <- clean.corpus(wk.source) ##Clean Corpus
wk.dtm <- DocumentTermMatrix(wk.corpus,
                             control = list(weighting = weightTfIdf))

wk.dtm.s <- scale(wk.dtm, scale = T)

##
wk.clusters <- kmeans(wk.dtm.s, 3)
barplot(wk.clusters$size, main = 'K-means')
plotcluster(cmdscale(dist(wk.dtm)),wk.clusters$cluster)

dissisimilarity.m <- dist(wk.dtm.s)
plot(silhouette(wk.clusters$cluster, dissisimilarity.m))

work.clus.proto <- t(cl_prototypes(wk.clusters))
comparison.cloud(work.clus.proto, max.words = 100,scale = c(0.1,1), title.size = 1.5)

##Appying Spherical k-means clustering
remove(list = ls())

clean.corpus <- function(corpus){
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, c(stopwords('en'),'customer',
                                            'service', 'cusotmers', 'calls'))
    return(corpus)
}

wk.exp <- readr::read_csv("1yr_plus4.csv")
wk.source <- VCorpus(VectorSource(wk.exp$text))
wk.corpus <- clean.corpus(wk.source) ##Clean Corpus
wk.dtm <- DocumentTermMatrix(wk.corpus,
                             control = list(weighting = weightTfIdf))


soft.part <- skmeans(wk.dtm, 3, m = 1.2, control = 
                         list(nruns = 5, verbose = TRUE))

#Check Cluster Sizes
barplot(table(soft.part$cluster), main = 'Spherical K-means')
plotcluster(cmdscale(dist(wk.dtm)),soft.part$cluster)

plot(silhouette(soft.part))

s.clus.proto <- t(cl_prototypes(soft.part))
comparison.cloud(s.clus.proto, max.words = 100, scale = c(0.1,1), title.size = 1)

sort(s.clus.proto[,1], decreasing = T)[1:5]
sort(s.clus.proto[,2], decreasing = T)[1:5]
sort(s.clus.proto[,3], decreasing = T)[1:5]


##Applying K-medioid clustering
wk.mediods <- pamk(wk.dtm, krange = 2:4, critout = T)
dissisimilarity.m <- dist(wk.dtm)
plot(silhouette(wk.mediods$pamobject$clustering, dissisimilarity.m))

#Comparing clustering algorithms
wk.dtm.s <- scale(wk.dtm, scale = T)
results <- cluster.stats(dist(wk.dtm.s), wk.clusters$cluster,
                         wk.mediods$pamobject$clustering)


######String Distance Methods#########
library(stringdist)
stringdist('crabapple','apple', method = 'lcs') #4
stringdist('crabapples','apple', method = 'lcs') #5

stringdist('crabapples','apple', method = 'hamming') ##Inf, because different length
stringdist('crabapples','apple', method = 'osa') #5 deletions should do

#Fuzzy Matching
match('apple', c('crabapple', 'pear')) ##No match, because dist = 0

amatch('apple', c('crabapple','pear'), maxDist = 3, method = 'dl') 
#No string with a maximum of distance of 3

amatch('apple', c('crabapple', 'pear'), maxDist = 4, method = 'dl')
#Matches the 1st string in the vector, because max dist is 4

ain('raspberry', c('berry', 'pear'), maxDist = 4)

##Functions that return minimum number of operations to convert a string into another
stringdist('raspberry', c('berry', 'pear'), method = 'osa')

fruit <- c('crabapple','apple','raspberry') ##Vector of strings
fruit.dist <- stringdistmatrix(fruit) #Square Matrix of distances
fruit.dist

plot(hclust(fruit.dist), labels = fruit)

##Topic Modeling Example: Guardian Articles
remove(list = ls())
library(tm)
library(qdap)
library(lda)
library(GuardianR)
library(pbapply)
library(LDAvis)
library(treemap)
library(car)

text <- readr::read_csv("Guardian_articles_11_14_2015_12_1_2015.csv")

#create vector for analysis
articles <- iconv(text$body, "latin1", "ASCII", sub = "") ##Convert encoding
articles <- gsub('http\\S+\\s*', '', articles) ##Get rid of links
articles <- bracketX(articles, bracket = 'all') #Kill text between brackets
articles <- gsub("[[:punct:]]","", articles)
articles <- removeNumbers(articles)
articles <- tolower(articles)
articles <- removeWords(articles, c(stopwords('en'),'pakistan','gmt', 'england'))

#Removing blank spaces
blank.removal <- function(x){
    x <- unlist(strsplit(x,' '))
    x <- subset(x, nchar(x) > 3)
    x <- paste(x, collapse = ' ')
    return(x)
} ##basic string trimming

articles <- pblapply(articles, blank.removal) ##Cool stuff

ex.text <- c('this is a text document')
ex.text.lex <- lexicalize(ex.text)

ex.text <- c('this is a document', 'text mining a text document is great')
ex.text.lex <- lexicalize(ex.text)
ex.text.lex

##
documents <- lexicalize(articles)

wc <- word.counts(documents$documents,vocab = documents$vocab) ##word count
doc.length <- document.lengths(documents$documents)

##Parameters
k <- 4 
num.iter <- 25
alpha <- 0.02
eta <- 0.02
set.seed(1234)
fit <- lda.collapsed.gibbs.sampler(documents = documents$documents,
                                   K = k, vocab = documents$vocab,
                                   num.iterations = num.iter, alpha = alpha,
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

plot(fit$log.likelihoods[1,])

top.topic.words(fit$topics, 7, by.score = TRUE)
top.topic.documents(fit$document_sums, 1)

##Estimating theta and phi matrices
theta <- t(pbapply(fit$document_sums + alpha,2,function(x)x/sum(x)))
phi <- t(pbapply(t(fit$topics) + eta,2,function(x)x/sum(x)))

article.json <- createJSON(phi, theta,doc.length, documents$vocab,
                           term.frequency = as.vector(wc))
# serVis(article.json)


##
doc.assignment <- function(x){
    x <- table(x)
    x <- as.matrix(x)
    x <- t(x)
    x <- max.col(x)
}

assignments <- unlist(pblapply(fit$assignments, doc.assignment))
assignments <- recode(assignments, "1 = 'Paris Attacks';2='Cricket 1';
                      3 = 'Unknown';4 ='Cricked 2'")
article.ref <- seq(1:nrow(text))
article.pol <- polarity(articles)[[1]][3]

article.tree.df <- cbind(article.ref,
                         article.pol,doc.length, assignments)

treemap(article.tree.df, index = c('assignments','article.ref'),
        vSize = "doc.length", vColor = 'polarity', type = "value",
        title = "Guardian Articles Mentioning Pakistan",
        palette = c("red", "White", "Green"))


###Text to Vectors
library(text2vec)
library(data.table)
library(tm)

text <- fread('Airbnb-boston_only.csv',showProgress = TRUE)
airbnb <- data.table(review_id = text$review_id,
                     comments = text$comments,
                     review_scores_rating = text$review_scores_rating)

airbnb$comments <- removeWords(airbnb$comments, c(stopwords('en'),'Boston'))
airbnb$comments <- stripWhitespace(airbnb$comments)
airbnb$comments <- removePunctuation(airbnb$comments)
airbnb$comments <- removeNumbers(airbnb$comments)
airbnb$comments <- tolower(airbnb$comments)

tokens <- strsplit(airbnb$comments, split = " ", fixed = TRUE) ##Split comment into a list of strings

##Creating the vocabulary
vocab <- create_vocabulary(itoken(tokens), ngram = c(1,1))

#Prune vocabulary by removing sparse terms
vocab <- prune_vocabulary(vocab, term_count_min = 5)
vocab[[1]][221:230]

iter <- itoken(tokens)
vectorizer <- vocab_vectorizer(vocabulary = vocab)
tcm <- create_tcm(iter, vectorizer, skip_grams_window = 5) ##Create term co-occurrence matrix

str(tcm)

tcm[1:5,1:5] ##Subset the matrix

fit.glove <- GloVe$new(word_vectors_size = 50,vocabulary = vocab,
                       x_max = 10, learning_rate = 0.2)

word_vectors_main = fit.glove$fit_transform(tcm, n_iter = 10)
word_vectors_context = fit.glove$components
word_vectors  = word_vectors_main + t(word_vectors_context)

row.names(word_vectors) <- rownames(tcm)

good.walks <- word_vectors['walk', ,drop = FALSE] - 
    word_vectors['disappointed',,drop = FALSE] + 
    word_vectors['good', , drop = FALSE]


cos.dist <- dist2(good.walks,word_vectors,'cosine',norm = 'l2')
head(sort(1 - cos.dist[1,], decreasing = T), 20)

###
dirty.sink <- word_vectors['sink', ,drop = FALSE] +
    word_vectors['dirty', , drop = FALSE]

cos.dist <- dist2(dirty.sink,word_vectors,'cosine',norm = 'l2')
head(sort(1 - cos.dist[1,], decreasing = T), 20)
