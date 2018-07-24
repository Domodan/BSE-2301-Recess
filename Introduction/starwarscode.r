#these are some of the packages we are using in this project
#loading the packages
library("tm")
library("NLP")
library("ggplot2")
library("SnowballC")
install.packages("corpus")
#defining the path of the text for analysis ie star wars movie scripts
cname<-file.path("c:","text")
cname#checking whether the path is correct
length (dir(as.matrix(cname)))
dir(cname)
docs <- VCorpus(DirSource(cname)) 
#summary of the texts for analysis
summary(docs)
inspect(docs[1])
writeLines(as.character(docs[1]))#trying to list the text as characters
docs<-tm_map(docs,removePunctuation)
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])  # This is an ascii character that did not translate, so it had to be removed.
}
docs<- tm_map(docs,tolower)
docs<- tm_map(docs, PlainTextDocument)
DocsCopy <- docs
docs <- tm_map(docs, PlainTextDocument)
#defining the stop words to be removed for better analytics
mystopwords<-c("uh","now","us","must","yes","oh","im","way","the","can","just","okay","seem","made","kid","your","even","much","one","theyr","youll","two","isnt")
docs <- tm_map(docs, removeWords, mystopwords)

docs <- tm_map(docs, removeWords, c("syllogism", "tautology"))
#Strip digits (std transformation, so no need for content_transformer)
docs <- tm_map(docs, removeNumbers) 
docs_st <- tm_map(docs, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1]))
# docs <- docs_stc
#Strip whitespace (from star wars movie scripts)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)
dtm <- DocumentTermMatrix(docs)   
dtm
tdm <- TermDocumentMatrix(docs)   
tdm 
freq <- colSums(as.matrix(dtm))   
length(freq)
ord <- order(freq) 
m <- as.matrix(dtm)   
dim(m)
write.csv(m, file="DocumentTermMatrix.csv") 
dtms <- removeSparseTerms(dtm, sparse = 0.95) # This makes a matrix that is 20% empty space, maximum.   
dtms
findAssocs(dtms,"luke",0.3)
freq <- colSums(as.matrix(dtm))
#inspect most frequently occuring terms in the texts
head(table(freq), 80)
#look for least frequently occuring terms in the texts
tail(table(freq), 80)
freq <- colSums(as.matrix(dtms))   
freq 

#arrange terms in order of decreasing their occurences in the texts
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 80)
#generate a graphical representation of most occuring words
p <- ggplot(subset(wf, min.freq=100), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p 
#load wordcloud package
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(20)
#limit words by specifying min frequency
wordcloud(names(freq),freq, min.freq=50)
#.add color
wordcloud(names(freq),freq,min.freq=50,colors=brewer.pal(6,"Dark2"))
#looking for keywords in the text

install.packages("RWeka")
library(RWeka)
