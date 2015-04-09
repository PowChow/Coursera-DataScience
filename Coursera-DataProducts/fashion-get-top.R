
searchterm = '@WhoWhatWear'
tweets = read.csv(paste(searchterm, '_stack_val.csv'), header=TRUE)
tw_df = data.frame(tweets)

###################

extract.hashes = function(vec){
  
  hash.pattern = "#[[:alpha:]]+"
  have.hash = grep(x = vec, pattern = hash.pattern)
  
  hash.matches = gregexpr(pattern = hash.pattern,
                          text = vec[have.hash])
  extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
  
  df = data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) = c("tag","freq")
  df = df[order(df$freq,decreasing = TRUE),]
  return(df)
}

dat = head(extract.hashes(tw_df$text), 15)
dat2 = transform(dat,tag = reorder(tag,freq))

p = ggplot(dat2, aes(x = tag, y = freq)) + geom_bar(stat='identity', fill="#DD8888") + 
  coord_flip() + 
  ggtitle(paste(searchterm, ": Hashtag frequencies in the mentions"))
ggsave(file=paste(searchterm, '_hashtag_hist.png'))

####################
#searchterm ='@WhoWhatWear'
#searchterm='@Fashionista_com'
searchterm='@refinery29'
#searchterm='@SteveMadden'
s =  read.csv(paste(searchterm, '_final_val.csv'), header=TRUE)
s.df = data.frame(s, header=TRUE)
summary(s.df)
s.df = s.df[with(s.df, order(-score, time)),]
write.table(head(s.df, n=5), file=paste(searchterm, '_top10.csv'))
head(s.df, n=5)
tail(s.df, n=5)
s.df_head = head(subset(s.df, select=c('score', 'text')), n=5)
s.df_tail = tail(subset(s.df, select=c('score', 'text')), n=5)
dev.off()
print.data.frame(s.df_head)
print.data.frame(s.df_tail)





grid.table(s.df_head, gp=gpar(fontsize=8), padding.h=unit(2,'mm'))
dev.off()
grid.table(s.df_tail, gp=gpar(fontsize=6))
dev.off()

print.data.frame()


