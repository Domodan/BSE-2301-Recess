##################################################################################
#$==============================================================================$#
#$++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                              $#
#$        TEXT ANALYTICS AS APPLIED TO STAR WARS MOVIE SCRIPTS RAW TEXT         $#
#$                   SCRIPT USED HERE IS FOR EPISODE IV                         $#
#$                                                                              $#
#$++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$==============================================================================$#
##################################################################################

# Set Working directory where the files are located

setwd("E:/ClassWork/Recess2/StarWars")

#Load the packages that will be used

library(tm)
library(wordcloud)
library(stringr)
library(sentimentr)

#Reading text data in text format

starwars4 <- read.table("SW_EpisodeIV.txt", stringsAsFactors = FALSE)

write.csv(starwars4, 
          file = "E:/ClassWork/Recess2/StarWars/StarWarsApp/SW_EpisodeIV.csv")


##################################################################################
#$==============================================================================$#
#$++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                              $#
#$         FIRST WE EXPLORE THE CHARACTERS THEN LATER WE SHALL TRY              $#
#$                    TO TAKE A LOOK ON THE DIALOGS                             $#
#$                                                                              $#
#$         OKAY, LET'S INSPECT THE CHARACTER COLUMN SO AS TO FIND               $# 
#$                OUT TOP CHARACTERS AND THEIR FREQUENCIES                      $#
#$                                                                              $#
#$++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$==============================================================================$#
##################################################################################

#Extracting the column for characters only in our dataset

starwars4_characters <- starwars4$character

#Now, taking a look at our character's object "starwars4_characters"

View(starwars4_characters[1:3])

#Let's visualise the characters on a Word Cloud so as to know 
#the top characters, hence the most talkative one

wordcloud(starwars4_characters, min.freq = 2, colors = brewer.pal(8, "Set2"), 
          random.order = FALSE, rot.per = 0.5)

#Let's try to get the most frequent charaters as seen on the Word Cloud
#Inorder to do that, we need to create a Corpus for characters document

#Creating the Characters corpus so as to make things a easier for us, meaning
#We can be able to create the term matrix without any error

characters_corpus <- Corpus(VectorSource(starwars4_characters))

#Creating a term document matrix for the characters

characters_tdm <- TermDocumentMatrix(characters_corpus)
inspect(characters_tdm[1:3,10:20])

#Trying to sort the document in a decreasing order of occurance such that
#the top character appears on top.
#In order to do that, we need to transform the term document a matrix
#that is, into a tabular format.

#Character's term document represented as a matrix

characters_matrix <- as.matrix(characters_tdm)

#Taking a look at what is on top of the matrix

head(characters_matrix[1:3,1:3])

#Now we can sort it in any order you want

characters_sorted <- sort(rowSums(characters_matrix), decreasing = TRUE)

#Working with data frame is a bit cool, let's transform the matrix into a df

characters_df <- data.frame(word = names(characters_sorted), 
                            freq = characters_sorted)

#What is topping your data frame? You might wanna check it out

head(characters_df)

#Creating the second Word Cloud for the Character's Data Frame

wordcloud(characters_df$word, characters_df$freq, min.freq = 1, 
          colors = brewer.pal(8, "Accent"), random.order = TRUE,
          rot.per = .5)

#Okay, we can now get the most frequent characters appearing more than 50 times

characters_frequency <- findFreqTerms(characters_tdm, lowfreq = 50)
characters_frequency

#Incase there exist a correlation, let's find it out practically

characters_correlation <- findAssocs(characters_tdm, 'threepio', 0)
characters_correlation


##################################################################################
#$==============================================================================$#
#$++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                              $#
#$        LET'S TAKE THE DIALOGS COLUMN ONLY IN OUR DATASET AND WE               $#
#$             TO CLEAN IT. AFTER CLEANING, WE CAN VISUALIZE                    $#
#$                          USING A WORDCLOUD                                   $#
#$                                                                              $#
#$       THE WORD CLOUD WILL TRY TO DEPICT WHAT YOU MIGHT WANT TO               $#
#$             ACHIEVE DEPENDING ON THE WORDS DISTRIBUTION                      $#
#$                                                                              $#
#$++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$==============================================================================$#
##################################################################################

#creating a corpus of the dialogs

corpus_dialogs4 <- Corpus(VectorSource(starwars4$dialogue))

#Changing first caps lock to lower

corpus_clean <- tm_map(corpus_dialogs4, content_transformer(tolower))

#Defining stop words to be removed

my_stopwords <- c(stopwords('english'), "they've", "we'll", "we're", "he'll",
                  "she'll", "you'll", "it's", "you're", "will", "don't",
                  "i'm", "i've", "what's", "didn't", "can", "there'll")

#Removing stop words

corpus_clean <- tm_map(corpus_clean, removeWords, my_stopwords)

#Removing Punctuations

corpus_clean <- tm_map(corpus_clean, removePunctuation)

#Removing numbers, in case they exist

corpus_clean <- tm_map(corpus_clean, removeNumbers)

#Stripping out extra whitespaces

corpus_clean <- tm_map(corpus_clean, stripWhitespace)

#Stem the Document, i mean combine the words in past, present, or any tense
#into its parent word.
#For instance "going", "go", "gone", will all be stemmed to "go"

corpus_clean <- tm_map(corpus_clean, stemDocument)


#Inspect and compare the original corpus with the cleaned one
#Inspecting the first two observations

inspect(corpus_dialogs4[1:3])
inspect(corpus_clean[1:3])

#Create a word cloud to visualise frequently used words

wordcloud(corpus_clean, colors = brewer.pal(8, "Accent"), random.order = TRUE,
          rot.per = .5, max.words = 300)

##################################################################################
#$==============================================================================$#
#$++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                              $#
#$        TOKENIZE THE DOCUMENT AND VISUALIZE IT.HERE WE CAN CREATE ANOTHER     $#
#$                   WORD CLOUDAND IT'S BACKGROUND TABLE TO HELP                $#
#$                                UNDERSTAND IT BETTER.                         $#
#$                                                                              $#
#$        NEXT WE SHALL CHECK FOR THE CORRELATION BETWEEN THE WORDS USED        $#
#$                                                                              $#
#$++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$==============================================================================$#
##################################################################################

#Creating a term document matrix for tokenization

starwars4_tdm <- TermDocumentMatrix(corpus_clean)

inspect(starwars4_tdm[100:110,100:115])

#Creating a matrix so that the terms can be sorted in a desired order

starwars4_matrix <- as.matrix(starwars4_tdm)

#Sorting such that the most frequent word are listed first

starwars4_sorted <- sort(rowSums(starwars4_matrix), decreasing = TRUE)

#Checking what is on top of the matrix

head(starwars4_sorted)

#Creating a data frame for the above matrix
#Give it two columns, first is the words and their corresponding frequency

starwars4_df <- data.frame(word = names(starwars4_sorted), 
                           freq = starwars4_sorted)

#Checking if the most frequent words are on top of the matrix

head(starwars4_df)

#Word cloud for the most frequent words in the matrix

wordcloud(starwars4_df$word, starwars4_df$freq, min.freq = 5, 
          colors = brewer.pal(8, "Accent"), random.order = FALSE,
          rot.per = .5, max.words = 300)


##################################################################################
#$==============================================================================$#
#$++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                              $#
#$        TRY TO GET SOME CORRELATION BETWEEN THE TERMS THAT ARE                $#
#$           FREQUENTLY BE USED AND THAT CAN HELP IN MAKING                     $#
#$                     SOME PREDICTIVE CONCLUSION                               $#
#$                                                                              $#
#$++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$==============================================================================$#
##################################################################################

#Derrive some correlation amongst the words.
#Check if the words are related and if they have some meaning.
#Okay, get the most frequent terms which is ocurring more than 10 times

starwars4_frequency <- findFreqTerms(starwars4_tdm, lowfreq = 10)
starwars4_frequency

#Find the Association between the frequent terms and other terms
#Let's take a look at "sir" since it appears to be frequent

starwars4_assocs <- findAssocs(starwars4_tdm, 'come',0.1)
starwars4_assocs


##################################################################################
#$==============================================================================$#
#$++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                              $#
#$          TRY TO PERFORM SENTIMENT ANALYSIS ON THE CHARACTERS THE             $#
#$                  MOVIE SCRIPT FOR EPISODE IV AT LARGE                        $#
#$                                                                              $#
#$          GET HOW POSITIVE OR NEGATIVE EACH CHARATER IS AND DRAW A            $#
#$                     CONLUSION FROM THEIR CONVERSATION                        $#
#$                                                                              $#
#$++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$==============================================================================$#
##################################################################################

#Get a sentiment out of these terms

starwars4_sentiment <- sentiment_by(get_sentences(starwars4$dialogue))
View(starwars4_sentiment)

#Not to change the original file, we make a copy of it here

starwars4_movie <- starwars4 
View(starwars4_movie)

#Now, add the sentiment column to the duplicate object created above

starwars4_movie$sentiments <- starwars4_sentiment$ave_sentiment

#Review the data in the starwars4_movie with the sentiments table inclusive
View(starwars4_movie)

#Try to analyze the data using a box plot
#The Box plot will clearly show they polarity and as well the positivity and
#Negativity of the characters

boxplot(starwars4_movie$sentiments ~ starwars4_movie$character, col = 'yellow',
        main = 'SENTIMENTS AS IN EPISODE IV',
        xlab = 'Charaters', ylab = 'Sentiments', 
        ylim = c(-1, 1), yaxs = 'i')

#Sentimental terms that are dominateing

starwars4_sentiment_words <- extract_sentiment_terms(starwars4_sentiment)

#Okay, That said we can plot the sentiment scores alone
#We can be able to tell the fluctuation in the emotions of the characters here

plot(starwars4_sentiment, type = "h", main = "Sentiments Score", col = "red",
     xlab = "Percentage Duration", ylab = "Character's Emotional Variance")






























