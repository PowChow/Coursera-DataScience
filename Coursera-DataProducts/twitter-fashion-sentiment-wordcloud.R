#connect all libraries
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

#connect to API

#######################################################################################
#################### FUNCTION: SEARCH FOR TWEETS AND OUTPUT #########################
search <- function(input)
{
  #access tweets and create cumulative file
  #list <- searchTwitter(searchterm, n=limit, lang='en', encoding='utf-8')
  #df <- twListToDF(list)
  df <- input[, order(names(input))]
  df$created <- strftime(df$created, '%Y-%m-%d %H:%M:%S')
  if (file.exists(paste(searchterm, '_stack_val.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack_val.csv'), row.names=F)
  #merge last access with cumulative file and remove duplicates
  stack <- read.csv(file=paste(searchterm, '_stack_val.csv'))
  stack <- rbind(stack, df)
  stack <- subset(stack, !duplicated(stack$text))
  write.csv(stack, file=paste(searchterm, '_raw.csv'), row.names=F)
  return(stack)
}

##################################################################################
################# FUNCTION: WORD SENTIMENT ANALYSIS ##############################
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

###############################################################
####### FUNCTION: TEXT MINING FUNCTIONS #######################
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
####################################################################################
################# FUNCTION: STEM CORPUS #################################
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

##------------------------------------------------
### A. START IMPLEMENTATION CODE FOR SENTIMENT ######
searchterm = '@refinery29'
n <- 500
raw <- searchTwitter(searchterm, limit=n, lang='en', encoding='utf-8')
df <- twListToDF(raw)

Dataset <- search(df) #enter keyword and limit, Call functions
Dataset$text <- as.factor(Dataset$text)
ratingDict <- read.csv('Ratings_Warriner_et_al.csv', sep=',' , header=TRUE) #load dictionary from .csv file

Dataset$text_nosym <- str_replace_all(Dataset$text, "[^[:punct:]]", "")
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
  ggtitle(paste(searchterm, stat$date))
ggsave(file=paste(searchterm, '_plot_val.png'))

##------------------------------------------------
### B. START CODE FOR WORD CLOUD

#Start text mining script
corpus=Corpus(VectorSource(df$text), readerControl = list(language = "eng"))
corpus_clean = cleanCorpus(corpus) #calls cleanCorpus function
corpus_stem = stemCorpus(corpus_clean)#Call Function for stemming 
corpus_stem = Corpus(VectorSource(corpus_stem))

corpus.tdm <- TermDocumentMatrix(corpus_stem, control = list(minWordLength = 3)) 

# inspect most popular words
findFreqTerms(corpus.tdm, lowfreq=30)

### BUILD WORDCLOUD
m1 <- as.matrix(corpus.tdm) 
v1<- sort(rowSums(m1),decreasing=TRUE) 
names <- names(v1)
d1<- data.frame(word = names(v1),freq=v1) 
set.seed(1589)
wordcloud(d1$word,d1$freq, min.freq=2, max.words=300, scale=c(5,.1), colors=brewer.pal(8, "Dark2")) #Creates wordcloud