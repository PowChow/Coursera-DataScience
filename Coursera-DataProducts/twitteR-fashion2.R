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
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey = 'kHvAEPPn37oyrQPaXc3zY8eAa'
consumerSecret= 'kZvuYaMV95mIeSCR2X40t8UqEVmx6JAmTu92FyzGyppQPsi120'

twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret ,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)
twitCred$handshake() 
registerTwitterOAuth(twitCred) 

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
searchterm = '@Fashionista_com'
tweets=searchTwitter(searchterm, n=300,lang='en', encoding='utf-8')
write.csv(stat, file=paste(searchterm, '_raw_tweets.csv'), row.names=TRUE)
tweets_df = twListToDF(tweets) #change out searches here

corpus=Corpus(VectorSource(tweets_df$text), readerControl = list(language = "eng"))
#exclude = c('style', 'winter', 'fall', 'winterfall')
corpus_clean = cleanCorpus(corpus, exclude) #calls cleanCorpus function
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
wordcloud(d1$word,d1$freq, min.freq=2, max.words=350, scale=c(5,.1), colors=brewer.pal(8, "Dark2")) #Creates wordcloud

############ ANALYZE TWITTER USERS #######################
www = getUser('WhoWhatWear')
friends.object<-lookupUsers(www$getFriendIDs())
followers.object<-lookupUsers(www$getFollowerIDs(n=2000))

friends <- sapply(friends.object,name)
followers <- sapply(followers.object,name)

# Create a data frame that relates friends and followers for expression in the graph
relations <- merge(data.frame(User='WhoWhatWear', Follower=friends), 
                   data.frame(User=followers, Follower='WhoWhatWear'), all=T)

# Create graph from relations.
g <- graph.data.frame(relations, directed = T)

# Assign labels to the graph (=people's names)
V(g)$label <- V(g)$name

# Plot the graph using plot() or tkplot().
tkplot(g)

####### MAP VISUALIZATIONS ##########
twitterMap('WhoWhatWear', fileName="Map_socialmedia2day.pdf", nMax=3000)
  
