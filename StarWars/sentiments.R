#SET UP

library(syuzhet)
library(plotly)
library(tm)
library(wordcloud)

load("tweets.rda")

#DATA CLEANING

# Function for data cleaning
f_clean_tweets <- function (tweets) {
  
  clean_tweets = sapply(tweets, function(x) x$getText())
  # remove retweet entities
  clean_tweets = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', clean_tweets)
  # remove at people
  clean_tweets = gsub('@\\w+', '', clean_tweets)
  # remove punctuation
  clean_tweets = gsub('[[:punct:]]', '', clean_tweets)
  # remove numbers
  clean_tweets = gsub('[[:digit:]]', '', clean_tweets)
  # remove html links
  clean_tweets = gsub('http\\w+', '', clean_tweets)
  # remove unnecessary spaces
  clean_tweets = gsub('[ \t]{2,}', '', clean_tweets)
  clean_tweets = gsub('^\\s+|\\s+$', '', clean_tweets)
  # remove emojis or special characters
  clean_tweets = gsub('<.*>', '', enc2native(clean_tweets))
  
  clean_tweets = tolower(clean_tweets)
  
  clean_tweets
}


#SENTIMENTS SCORE

# First call the function defined above to clean the data
clean_tweets <- f_clean_tweets(some_tweets)

# extract the timestamp
timestamp <- as.POSIXct(sapply(some_tweets, function(x)x$getCreated()), origin="1970-01-01", tz="GMT")
timestamp <- timestamp[!duplicated(clean_tweets)]
clean_tweets <- clean_tweets[!duplicated(clean_tweets)]


#COMPARE SENTIMENTS LEXICONS

# Get sentiments using the four different lexicons
syuzhet <- get_sentiment(clean_tweets, method="syuzhet")
bing <- get_sentiment(clean_tweets, method="bing")
afinn <- get_sentiment(clean_tweets, method="afinn")
nrc <- get_sentiment(clean_tweets, method="nrc")
sentiments <- data.frame(syuzhet, bing, afinn, nrc, timestamp)


#EMOTIONS ANALYSIS

#NRC EMOTION LEXICON

# get the emotions using the NRC dictionary
emotions <- get_nrc_sentiment(clean_tweets)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])


#VISUALIZATION

#COMPARING SENTIMENTS SCORE

# plot the different sentiments from different methods
plot_ly(sentiments, x=~timestamp, y=~syuzhet, type="scatter", mode="jitter", name="syuzhet") %>%
  add_trace(y=~bing, mode="lines", name="bing") %>%
  add_trace(y=~afinn, mode="lines", name="afinn") %>%
  add_trace(y=~nrc, mode="lines", name="nrc") %>%
  layout(title="Recent sentiments of HDB in Singapore",
         yaxis=list(title="score"), xaxis=list(title="date"))


#VISUALIZE THE SENTIMENTS

# Visualize the emotions from NRC sentiments
plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for HDB (1-10 June 2017)")


#WHICH WORDS CONTRIBUTE TO EMOTIONS

# Comparison word cloud
all = c(
  paste(clean_tweets[emotions$anger > 0], collapse=" "),
  paste(clean_tweets[emotions$anticipation > 0], collapse=" "),
  paste(clean_tweets[emotions$disgust > 0], collapse=" "),
  paste(clean_tweets[emotions$fear > 0], collapse=" "),
  paste(clean_tweets[emotions$joy > 0], collapse=" "),
  paste(clean_tweets[emotions$sadness > 0], collapse=" "),
  paste(clean_tweets[emotions$surprise > 0], collapse=" "),
  paste(clean_tweets[emotions$trust > 0], collapse=" ")
)
all <- removeWords(all, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(all))
#
# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdm1 <- tdm[nchar(rownames(tdm)) < 11,]

# add column names
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdm1) <- colnames(tdm)
comparison.cloud(tdm1, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)





