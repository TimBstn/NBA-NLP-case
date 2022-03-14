#' Title: NLP case 1 - NBA tweets
#' NAME: Tim Bastian
#' Date: Feb 28 2022


################ Settings###########################

Sys.setlocale('LC_ALL','C')

# set to your file location
setwd('')

options(stringsAsFactors = FALSE) #text strings will not be factors of categories


################ Libraries #########################

library(ggplot2)
library(ggthemes)
library(ggdendro)
library(stringi)
library(tm)
library(readr)
library(stringr)
library(mgsub)
library(qdap)
library(dplyr)
library(textclean)
library(wordcloud)
library(RColorBrewer)
library(igraph)

# Create custom stop words
stops <- c(stopwords('english'), 'lol', 'smh', 'amp')

################ Functions #########################

# function to filter for the tweets with the most retweets
getRetweets <- function(x){
  x <- x[grepl('^RT', x,ignore.case=T)]
  x <- as.data.frame(x)%>% count(x) %>% 
    arrange(desc(n)) %>% 
    slice(1:20)
}

# function to get all the @'s (tweeter and linkings) out of column
getTweeter <- function(x){
  x <- unlist(strsplit(x, " "))
  
  regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
  regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @
  x <- gsub(regex2, "", x[grep(regex1, x, perl = T)])
  x <- as.data.frame(x)%>% count(x) %>% 
    arrange(desc(n)) %>% 
    slice(1:20)
  return(x)
}

# function to get all the @'s (tweeter and linkings) out of column
getHashtags <- function(x){
  x <- unlist(strsplit(x, " "))
  
  regex1 <- "(^|[^#\\w])#(\\w{1,15})\\b" # get strings with @
  regex2 <- "[^[:alnum:]#_]"             # remove all punctuation except _ and @
  x <- gsub(regex2, "", x[grep(regex1, x, perl = T)])
  x <- as.data.frame(x)%>% count(x) %>% 
    arrange(desc(n)) %>% 
    slice(1:20)
  return(x)
}

# function to replace RT (retweet) and links
basicSubs <- function(x){
  x <- gsub('http\\S+\\s*', '', x)
  x <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', x)
  return(x)
}

# function to translate the emojies into words using the textclean package
emojiSubs <- function(x){
  x <- replace_emoji(x)
  x <-  gsub('<..>', '', x) # delete patters like <e2><80><99>
  return(x)
}

# function to delete the emojies using the textclean package
deleteEmojis <- function(x){
  hash2 <- lexicon::hash_emojis
  hash2$y <- ""
  
  x <- replace_emoji(x, emoji_dt =hash2)
  x <-  gsub('<..>', '', x) # delete patters like <e2><80><99>
  return(x)
}

# 2-gram token maker
BigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# 3-gram token maker
TrigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 3), paste, collapse = " "), 
         use.names = FALSE)
}

# custom function to lower every letter in dataframe
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# custom function to clean the corpus
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  #corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}


# function to create DTM as matrix
create_DTM <- function(x, n, sparse_matrix = 1){
  # create Corpus Object
  x <- VCorpus(VectorSource(x))
  
  # Clean the Corpus
  x <- cleanCorpus(x, stops)
  
  if(n == 1){
    # Construct a TDM 
    x <- DocumentTermMatrix(x,control=list(weighting=weightTf))
  }else if(n==2){
    x <- DocumentTermMatrix(x,control=list(weighting=weightTf,
                                           tokenize=BigramTokens))
  }else if(n==3){
    x <- DocumentTermMatrix(x,control=list(weighting=weightTf,
                                           tokenize=TrigramTokens))
  }else{
    return(NULL)
  }
  
  # Reduce TDM with sparse terms
  if(sparse_matrix < 1){
    x<- removeSparseTerms(x, sparse=sparse_matrix)
  }
  
  # Switch this to a simple matrix
  x <-as.matrix(x)
  
  return(x)
}

# function to create TDM as matrix
create_TDM <- function(x, n, sparse_matrix = 1){
  # create Corpus Object
  x <- VCorpus(VectorSource(x))
  
  # Clean the Corpus
  x <- cleanCorpus(x, stops)
  
  if(n == 1){
    # Construct a TDM 
    x <- TermDocumentMatrix(x,control=list(weighting=weightTf))
  }else if(n==2){
    x <- TermDocumentMatrix(x,control=list(weighting=weightTf,
                                           tokenize=BigramTokens))
  }else if(n==3){
    x <- TermDocumentMatrix(x,control=list(weighting=weightTf,
                                           tokenize=TrigramTokens))
  }else{
    return(NULL)
  }
  
  # Reduce TDM with sparse terms
  if(sparse_matrix < 1){
    x<- removeSparseTerms(x, sparse=sparse_matrix)
  }
  
  # Switch this to a simple matrix
  x <-as.matrix(x)
  
  return(x)
}

################ Load the data #########################

# load data for finals month
sep <- read_csv("data/L_Sep2020.csv")
oct <- read_csv("data/M_Oct2020.csv")

# combine to one dataframe
nba_df <- rbind(sep, oct)
nba_df <- subset(nba_df, created <= "2020-10-05")

# filter dataframe for teams that took part in the finals
finals_teams <- c("LA Lakers", "Miami Heat")
nba_finals <- subset(nba_df, team %in% finals_teams)

# filter for first 3 games of finals series
nba_finals_first3 <- subset(nba_finals, created >= "2020-09-29")


################ Clean the data ########################

# apply the function and add a cleaned column to the dataframe
nba_finals_first3$text_cleaned <- basicSubs(nba_finals_first3$text)

# replace emojies --> important for sentiment
nba_finals_first3$text_cleaned_emojis <- emojiSubs(nba_finals_first3$text_cleaned)

# delete emojies
nba_finals_first3$text_cleaned_without_emojis <- deleteEmojis(nba_finals_first3$text_cleaned)


################ Add polarity ##########################

# get polarity of each tweet
nba_pol <- polarity(nba_finals_first3$text_cleaned_emojis)

# plot distribution of polarity
ggplot(data = nba_pol$all, 
       aes(x=polarity, y=..density..)) + 
  geom_histogram(binwidth=.25,
                 fill="white", color="black", size=.5) +
  geom_density(size=.5, color="grey60")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

# add polartity to dataframe
nba_finals_first3$polarity<-scale(nba_pol$all$polarity)

# get positive comments
pos.comments<-subset(nba_finals_first3$text_cleaned_without_emojis,
                     nba_finals_first3$polarity>0)

# get negative comments
neg.comments<-subset(nba_finals_first3$text_cleaned_without_emojis,
                     nba_finals_first3$polarity<0)

# create a corpus
pos.comments<-paste(pos.comments,collapse = " ")
neg.comments<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.comments,neg.comments)
all.tdm.m<- create_TDM(all.terms, 1)

colnames(all.tdm.m)<-c('positive','negative')

# create wordcloud
comparison.cloud(all.tdm.m, max.words=100, 
                 colors=c('darkgreen','darkred'))


################ Create DTM's ##########################

# create nba dtm without smileys for one word
nba_one_word_dtm <- create_DTM(nba_finals_first3$text_cleaned_without_emojis, 
                               n=1, 
                               sparse_matrix = 1)

# create nba dtm without smileys for two words
nba_two_word_dtm <- create_DTM(nba_finals_first3$text_cleaned_without_emojis, 
                               n=2, 
                               sparse_matrix = 0.99)

# create nba dtm without smileys for three words
nba_three_word_dtm <- create_DTM(nba_finals_first3$text_cleaned_without_emojis, 
                                 n=3, 
                                 sparse_matrix = 0.99)


################ Analysis #############################


############## Part 1: most tweeted teams

# see how often teams were tweeted about before game 3 in month Sep and Oct
count_team_tweet <- nba_df %>% count(team) %>% 
  arrange(desc(n)) %>% 
  slice(1:5)


# create chart with teams with most tweets
ggplot(data=count_team_tweet) +
  geom_bar(stat="identity", color="black",fill="white")+
  aes(x=reorder(team,-n,sum),y=n) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


############## Part 2: most retweeted tweets, most used hashtags, most popular twitter accounts

# most retweeted tweets
retweets <- getRetweets(nba_finals_first3$text)

# get most tweeted hashtags
hashtags <- getHashtags(nba_finals_first3$text)

pal <- brewer.pal(8, "Greys")
pal <- pal[-(1:2)]

wordcloud(hashtags$x,
          hashtags$n,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale=c(10,1.5))

# get most popular twitter accounts
tweeter <- getTweeter(nba_finals_first3$text)

ggplot(data = tweeter[1:10,])+
  geom_bar(stat="identity", fill='white', color="black")+
  coord_flip()+
  geom_text(aes(label=n), 
            colour="black",hjust=1.25, size=3.5)+
  aes(x=reorder(x,n,sum),y=n)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

############## Part 3: most frequent words, wordcombinations

####### 3.1 one word
top_words <- data.frame(terms = colnames(nba_one_word_dtm), freq = colSums(nba_one_word_dtm))
top_words <- top_words[order(-top_words$freq),]

### wordcloud

pal <- brewer.pal(8, "Greys")
pal <- pal[-(1:2)]

wordcloud(top_words$terms,
          top_words$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale=c(7,0.5))

####### 3.2 two words
top_words_two <- data.frame(terms = colnames(nba_two_word_dtm), freq = colSums(nba_two_word_dtm))
top_words_two <- top_words_two[order(-top_words_two$freq),]

# plot most frequent words (top 25)
ggplot(data = top_words_two[1:25,])+
  geom_bar(stat="identity", fill='white', color="black")+
  coord_flip()+
  geom_text(aes(label=freq), 
            colour="black",hjust=1.25, size=3.5)+
  aes(x=reorder(terms,freq,sum),y=freq)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

####### 3.3 three words
top_words_three <- data.frame(terms = colnames(nba_three_word_dtm), freq = colSums(nba_three_word_dtm))
top_words_three <- top_words_three[order(-top_words_three$freq),]

# plot most frequent words (top 25)
ggplot(data = top_words_three[1:25,])+
  geom_bar(stat="identity", fill='white', color="black")+
  coord_flip()+
  geom_text(aes(label=freq), 
            colour="black",hjust=1.25, size=3.5)+
  aes(x=reorder(terms,freq,sum),y=freq)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

############## Part 4: Inspect word associations with "nike"

nike<-nba_finals_first3[grep("nike", nba_finals_first3$text_cleaned_without_emojis, ignore.case=T), ]

####### 4.1 create dtm 
# create Corpus Object
word_asso_dtm <- VCorpus(VectorSource(nba_finals_first3$text_cleaned_without_emojis))

# Clean the Corpus
word_asso_dtm <- cleanCorpus(word_asso_dtm, stops)

# create dtm
word_asso_dtm <- TermDocumentMatrix(word_asso_dtm,control=list(weighting=weightTf))

####### 4.2 find associations
associations <- findAssocs(word_asso_dtm, 'nike', 0.10)

# Organize the word associations
associations <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
associations$terms <- factor(associations$terms, levels=associations$terms)
rownames(associations) <- NULL

####### 4.3 plots
ggplot(data = associations) +
  geom_point(fill='white', col='black') +
  aes(x=value,y=terms)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) + 
  geom_text(aes(x=value,label=value), 
            colour="black",
            hjust=1.2, 
            vjust ="outward", 
            size=3)


# adjacency graph

# create adjacency matrix
nike.m<- create_TDM(nike$text_cleaned_without_emojis, n=1)
nike.adj<-nike.m %*% t(nike.m)
nike.adj<-graph.adjacency(nike.adj, weighted=TRUE, 
                            mode="undirected", diag=T)
nike.adj<-simplify(nike.adj)

# plotting graph
plot.igraph(nike.adj, vertex.shape="none",
            vertex.label.font=2, vertex.label.color="darkred",
            vertex.label.cex=.7, edge.color="gray85")
title(main='Nike Word Network')




