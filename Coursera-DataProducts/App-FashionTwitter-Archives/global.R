library(tm)
library(wordcloud)
library(twitteR)
library(SnowballC)
library(memoise) # "memoise" automatically cache the results

# The list of valid twitter handles
TwH <<- list('@WhoWhatWear' = '@WhoWhatWear'
#                '@refinery29',
#                '@REVOLVEclothing',
#                '@SteveMadden',
#                '@Fashionista_com',
#                '@DailyFrontRow',
#                '@Singer22'
            )

getTermMatrix = memoise(function(TwH) {
  # Function: returns matrix for word cloud
  # Security to help filter malicious inputs
  if (!(TwH %in% TwH))
    stop("Unknown Twitter Handle")

  #######################  FUNCTION: ClEAN CORPUS ########################
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
  #######################  FUNCTION: STEM CORPUS ######################
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
  ########################################################################  

  #Start code here
  tweets <- read.csv(file= sprintf("./data/%s _stack_val.csv", TwH), #twitter text, create csv with twitter text 
                    encoding="UTF-8", header=TRUE)
  tweets_df = data.frame(tweets) #change out searches here
  corpus=Corpus(VectorSource(tweets_df$text), readerControl = list(language = "eng")) 
  corpus_clean = cleanCorpus(corpus) #calls cleanCorpus function
  corpus_stem = stemCorpus(corpus_clean)#Call Function for stemming 
  corpus_stem = Corpus(VectorSource(corpus_stem))
 
  corpus.tdm <- TermDocumentMatrix(corpus_stem, control = list(minWordLength = 3)) 
  
  m = as.matrix(corpus.tdm )
  sort(rowSums(m), decreasing = TRUE)
  return(m)
})