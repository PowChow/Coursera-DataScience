library(qdap)
library(data.table)

#get tweets
searchterm = '@refinery29'
tweets = read.csv(paste(searchterm, '_stack_val.csv'), header=TRUE)
tw_df = data.frame(tweets)
some_txt = tw_df$text

