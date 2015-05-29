#install.packages('twitteR')
#install.packages(c("tm", "wordcloud"))
library(twitteR)
library(wordcloud)
library(tm)
library(igraph)

#download key
#key_data = read.csv(file="twitter_fashion.key", header=T, sep=',')

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
#consumerKey = key_data[['consumerKey']]
#consumerSecret= key_data[['consumerSecret']]

twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret ,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)
twitCred$handshake() #Pause for the Handshake Pin Code
registerTwitterOAuth(twitCred) 

######### REVIEW TWITTER USERS #######################
www = getUser('WhoWhatWear')
friends.object<-lookupUsers(www$getFriendIDs())
followers.object<-lookupUsers(www$getFollowerIDs())

friends <- sapply(friends.object,name)
followers <- sapply(followers.object,name)

# Create a data frame that relates friends and followers to you for expression in the graph
relations <- merge(data.frame(User='WhoWhatWear', Follower=friends), 
                   data.frame(User=followers, Follower='WhoWhatWear'), all=T)

# Create graph from relations.
g <- graph.data.frame(friends, directed = T)

# Assign labels to the graph (=people's names)
V(g)$label <- V(g)$name

# Plot the graph using plot() or tkplot().
tkplot(g)


#######CREATE WORD CLOUD ########
a=searchTwitter("#lbd", n=2000) #Get the Tweets
tweets_df = twListToDF(a) #Convert to Data Frame

b=Corpus(VectorSource(tweets_df$text), readerControl = list(language = "eng"))
b <- tm_map(b,
            content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
            mc.cores=1)
b<- tm_map(b, content_transformer(tolower), mc.cores=1) #Changes case to lower case 
b<- tm_map(b, stripWhitespace, mc.cores=1) #Strips White Space 
b<-tm_map(b, removeNumbers, mc.cores=1)
b <- tm_map(b, removePunctuation, mc.cores=1) #Removes Punctuation, this removes hastag
# myStopwords <- c(stopwords('english'), "available", "via")
# idx <- which(myStopwords == "r")
# myStopwords <- myStopwords[-idx]
# myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

#inspect(b) 
tdm <- TermDocumentMatrix(b, control = list(minWordLength = 1)) 
m1 <- as.matrix(tdm) 
v1<- sort(rowSums(m1),decreasing=TRUE) 
names <- names(v1)
d1<- data.frame(word = names(v1),freq=v1) 
wordcloud(d1$word,d1$freq)
############################
