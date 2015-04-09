#install.packages('twitteR')
#install.packages(c("tm", "wordcloud"))

library(twitteR)
library(wordcloud)
library(tm)
library(igraph)
library(stringr)
library(ggplot2)
library(SnowballC)
source("http://biostat.jhsph.edu/~jleek/code/twitterMap.R")

######### Twitter credentials #################################
# #connect to API
# download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
# reqURL <- "https://api.twitter.com/oauth/request_token"
# accessURL <- "https://api.twitter.com/oauth/access_token"
# authURL <- "https://api.twitter.com/oauth/authorize"
# consumer_key = 'kHvAEPPn37oyrQPaXc3zY8eAa'
# consumer_secret= 'kZvuYaMV95mIeSCR2X40t8UqEVmx6JAmTu92FyzGyppQPsi120'
# 
# setup_twitter_oauth(consumer_key, consumer_secret, access_token=NULL, access_secret=NULL)

#######TEXT MINING FUNCTIONS #######################
cleanCorpus = function(corpus){
  b <- tm_map(corpus,
              content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
              mc.cores=1)
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  b <- tm_map(b, toSpace, 'www\\..*?\\.com')
  b <- tm_map(b, toSpace, 'http.*')
  b <- tm_map(b, toSpace, 'http.*[[:alnum:]]')
  b <- tm_map(b, toSpace, '@[[:alnum:]]*')
  b<- tm_map(b, content_transformer(tolower), mc.cores=1) #Changes case to lower case 
  b <- tm_map(b, PlainTextDocument)
  myStopwords <- c(stopwords('english'), '2015', '20142015', 'rt')
  b <- tm_map(b, removeWords, myStopwords)
  b <- tm_map(b, removePunctuation, mc.cores=1) #Removes Punctuation, this removes hastag
  b <- tm_map(b, removeNumbers, mc.cores=1)
  return(b)
}

stemCorpus = function(corpus){
  mydict <- corpus
  corpus <- tm_map(corpus, stemDocument)
  stemCompletion_mod <- function(x,dict=corpuscopy) {
   paste(stemCompletion(unlist(strsplit(as.character(x)," ")),
                        dictionary=mydict, type="prevalent"),sep="", collapse=" ")
  }
  corpus <- lapply(corpus, stemCompletion_mod)
  return(corpus)
}
#####################################################

#Start text mining script

tweets=read.csv('@WhoWhatWear _stack_val.csv', header=TRUE)
tweets_df = data.frame(tweets) #change out searches here

corpus=Corpus(VectorSource(tweets_df$text), readerControl = list(language = "eng"))
corpus_clean = cleanCorpus(corpus) #calls cleanCorpus function
corpus_stem = stemCorpus(corpus_clean)#Call Function for stemming 
corpus_stem = Corpus(VectorSource(corpus_stem))

corpus.tdm <- TermDocumentMatrix(corpus_stem, control = list(minWordLength = 3)) 

# inspect most popular words
findFreqTerms(corpus.tdm, lowfreq=30)
findAssocs(corpus.tdm, 'WhoWhatWear', 0.20) 

### BUILD WORDCLOUD
m1 <- as.matrix(corpus.tdm) 
v1<- sort(rowSums(m1),decreasing=TRUE) 
names <- names(v1)
d1<- data.frame(word = names(v1),freq=v1) 
set.seed(1589)
png(paste(searchterm, '_wordcloud.png'), width=12, height=8, units='in', res=300)
wordcloud(d1$word,d1$freq, min.freq=5, max.words=100, rot.per=.15, scale=c(8,.2), colors=brewer.pal(8,"Dark2")) #Creates wordcloud
dev.off()
#ggsave(paste(searchterm, '_wordcloud.png'))
