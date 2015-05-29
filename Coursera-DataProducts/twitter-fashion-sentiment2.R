#connect all libraries
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

#download key
key_data = read.csv(file="twitter_fashion.key", header=T, sep=',')

#connect to API
#download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumer_key = key_data[['consumerKey']]
consumer_secret= key_data[['consumerSecret']]

setup_twitter_oauth(consumer_key, consumer_secret, access_token=NULL, access_secret=NULL)
# twitCred <- OAuthFactory$new(consumerKey=consumerKey,
#                              consumerSecret=consumerSecret ,
#                              requestURL=reqURL,
#                              accessURL=accessURL,
#                              authURL=authURL)
# twitCred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')) #There is URL in Console. You need to go to it, get code and enter it on Console
# save(twitCred, file='twitter authentication.Rdata')
# load('twitter authentication.Rdata') #Once you launch the code first time, you can start from this line in the future (libraries should be connected)
# registerTwitterOAuth(twitCred)


########## FUNCTION: SEARCH FOR TWEETS AND OUTPUT #########################
search <- function(searchteam, limit)
{
  #access tweets and create cumulative file
  list <- searchTwitter(searchterm, n=limit, lang='en')
  df <- twListToDF(list)
  df <- df[, order(names(df))]
  df$created <- strftime(df$created, '%Y-%m-%d %H:%M:%S')
  if (file.exists(paste(searchterm, '_stack_val.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack_val.csv'), row.names=F)
  #merge last access with cumulative file and remove duplicates
  stack <- read.csv(file=paste(searchterm, '_stack_val.csv'))
  stack <- rbind(stack, df)
  stack <- subset(stack, !duplicated(stack$text))
  write.csv(stack, file=paste(searchterm, '_stack_val.csv'), row.names=F)
  return(stack)
}
  
#evaluation tweets function
score.sentiment <- function(sentences, ratingDict, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, ratingDict){
    sentence <- gsub('http.*[[:alnum:]]', '', sentence) #cleaning sweets
    sentence <- gsub('[[:punct:]]', '', sentence) #cleaning tweets
    sentence <- gsub('[[:cntrl:]]', '', sentence) #cleaning tweets
    sentence <- gsub('\\d+', '', sentence) #cleaning tweets
    sentence <- tolower(sentence) #cleaning tweets
    word.list <- str_split(sentence, '\\s+') #separating words
    words <- unlist(word.list)
    val.matches <- match(words, ratingDict$Word) #find words from tweet in "Word" column of dictionary
    val.match <- ratingDict$Rating[val.matches] #evaluating words which were found (suppose rating is in "Rating" column of dictionary).
    val.match <- na.omit(val.match)
    val.match <- as.numeric(val.match)
    score <- sum(val.match)/length(val.match) #rating of tweet (average value of evaluated words)
    return(score)
  }, ratingDict, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences) #save results to the data frame
  write.csv(scores.df, file=paste(searchterm, '_scores_val.csv'), row.names=TRUE) #save evaluation results into the files
  return(scores.df)
}

####### START IMPLEMENTATION CODE FOR SENTIMENT ######
searchterm = '@SarahPalinUSA'
limit = 2000
Dataset <- search(searchterm,limit) #enter keyword and limit, Call functions
Dataset$text <- as.factor(Dataset$text)
ratingDict <- read.csv('Ratings_Warriner_et_al.csv', sep=',' , header=TRUE) #load dictionary from .csv file

Dataset$text_nosym <- str_replace_all(Dataset$text, '[^[:alnum:][:blank:]+?&/\\-]', '')
scores <- score.sentiment(Dataset$text_nosym, ratingDict, .progress='text') #Call Function

#modify evaluation
stat <- scores
stat$created <- Dataset$created
stat$date<- as.Date(stat$created)
stat$time <- ymd_hms(stat$created)
stat <- na.omit(stat) #delete unvalued tweets
write.csv(stat, file=paste(searchterm, '_final_val.csv'), row.names=TRUE)

#create chart
ggplot(stat, aes(time, score)) + geom_point(size=1) +
  stat_summary(fun.data = 'mean_cl_normal', mult = 1, geom = 'smooth') +
  ggtitle(paste(searchterm, as.Date(stat$created), nrow(stat), 'tweets')) + 
  scale_y_continuous(limits=c(1, 9), breaks=c(1:9))
ggsave(file=paste(searchterm, '_plot_val.png'))