#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                        THEME: STARWARS                                      $#
#$                                                                             $#
#$                        GOAL: PROCESSING, VISUALIZING AND ANALYZING          $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################


# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#Loading all the required packages

library(shiny)
library(tm)
library(wordcloud)
library(sentimentr)
library(ggplot2)
library(plotly)
library(syuzhet)
library(caret)
library(rminer)
library(e1071)


#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                        THEME: THE SERVER SIDE                               $#
#$                                                                             $#
#$                        GOAL: ACTUAL PROCESSING TASK                         $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################

#Server Function
shinyServer(function(input, output) {
  
  
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                        THEME: UPLOADING FILE                                $#
#$                                                                             $#
#$                        GOAL: CREATING FUNCTION TO UPLOAD FILE               $# 
#$                                                                             $#
#$                        HINT: FILE WILL BE SHARED BY OTHER FUNCTIONS         $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################
  
  #Creating a reactive function for uploading files
  upload_function <- reactive({
    
    #Storing input file being uploaded
    upload_file <- input$file
    
    #Checking to see if the file is empty
    if(is.null(upload_file)) {
      return()
    }
    
    #Reading the file as a using a csv file
    read.csv(upload_file$datapath, header = input$header, sep = input$sep,
             stringsAsFactors = FALSE)
  })
  
  
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                        THEME: UPLOADED FILE                                 $#
#$                                                                             $#
#$                        GOAL: SHOWING THE FILE STRUCTURE                     $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
################################################################################# 
  
  #Displaying the uploaded file using renderTable()
  output$input_file <- renderTable(striped = TRUE, hover = TRUE, bordered = TRUE,
    {
      
      #Calling the upload function to supply the uploaded file
      upload_function()
   })
   
  
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$            THEME: CHARACTERS                                                $#
#$                                                                             $#
#$            GOAL: CREATING TERM DOCUMENT MATRIX                              $#
#$                                                                             $# 
#$            DETAILS: CREATE A REACTIVE FUNCTION FOR HANDLING                 $#
#$                               TDM                                           $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################  
   
  #Reactive Function for handling top characters
  starwarsCharacters_data <- reactive({
     
    #Setting Progress bar, so as to tell the User what's taking place
    #on the background
     withProgress({
       
       #Setting Progress message
       setProgress(message = "Processing...")
       
       #Extracting the uploaded file by calling the upload function
       starwars_file <- upload_function()
       
       #Extracting only the characters column from the uploaded file
       starwars_characters <- starwars_file[, 2]
       
       #Creating a Corpus for the characters
       characters_corpus <- Corpus(VectorSource(starwars_characters))
       
       #Creating a term document matrix for the characters
       characters_tdm <- TermDocumentMatrix(characters_corpus)
       
       #Transforming the tdm into a matrix
       characters_matrix <- as.matrix(characters_tdm)
       
       #Sorting the matrix so that Top characters are on top
       characters_sorted <- sort(rowSums(characters_matrix), decreasing = TRUE)
       
       #Creating Data Frame that will help us in outputing the Word Cloud
       characters_df <- data.frame(word = names(characters_sorted), 
                                   freq = characters_sorted)
       
       return(characters_df)
       
     })
   })
   
  
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$            THEME: CHARACTERS' DIALOGUES                                     $#
#$                                                                             $#
#$            GOAL: CLEANING THE CHARACTERS' DIALOGUES                         $#
#$                                                                             $# 
#$            DETAILS: CREATE A CORPUS BEFORE CLEANING THE DATA                $#
#$                     AFTER CLEANING, VISUALIZE USING WORD CLOUD              $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################
   
  #Creating a reactive function to hold the clean dialogs data
  starwarsDialogue_data <- reactive({
     
    #Getting the uploaded file
    starwars <- upload_function()
    
    #Extracting the dialogs from the Uploaded File
    starwars_dialogs <- starwars$dialogue
    
    #creating a corpus of the dialogs
    
    starwars_corpus <- Corpus(VectorSource(starwars_dialogs))
    
    #Changing first caps lock to lower
    
    starwars_clean <- tm_map(starwars_corpus, content_transformer(tolower))
    
    #Defining stop words to be removed
    
    my_stopwords <- c(stopwords('english'), "they've", "we'll", "we're", "he'll",
                      "she'll", "you'll", "it's", "you're", "will", "don't",
                      "i'm", "i've", "what's", "didn't", "can", "there'll")
    
    #Removing stop words
    
    starwars_clean <- tm_map(starwars_clean, removeWords, my_stopwords)
    
    #Removing Punctuations
    
    starwars_clean <- tm_map(starwars_clean, removePunctuation)
    
    #Removing numbers, in case they exist
    
    starwars_clean <- tm_map(starwars_clean, removeNumbers)
    
    #Stripping out extra whitespaces
    
    starwars_clean <- tm_map(starwars_clean, stripWhitespace)
    
    #Stem the Document, i mean combine the words in past, present, or any tense
    #into its parent word.
    #For instance "going", "go", "gone", will all be stemmed to "go"
    
    starwars_clean <- tm_map(starwars_clean, stemDocument)
    
    return(starwars_clean)
    
   })
   
  
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$            THEME: CHARACTERS' DIALOGUES                                     $#
#$                                                                             $#
#$            GOAL: CREATING TERM DOCUMENT MATRIX                              $#
#$                                                                             $# 
#$            DETAILS: CREATE A TERM DOCUMENT MATRIX FOR DIALOGUES             $#
#$                     TRANSFORM THE TDM INTO A MATRIX                         $#
#$                     THEN SORT THE MATRIX IN DESCENDING ORDER                $# 
#$                                                                             $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################
  
  #Creating Term Document Matrix for the Character's Dialogues
  
  starwarsDialogue_tdm <- reactive({
    
    #First, get the clean dialog corpus from starwarsDialogue_data function
    
    starwarsDialogs_corpus <- starwarsDialogue_data()
    
    #Creating a term document matrix for tokenization
    
    starwarsDialogs_tdm <- TermDocumentMatrix(starwarsDialogs_corpus)
    
    #Creating a matrix so that the terms can be sorted in a desired order
    
    starwarsDialogs_matrix <- as.matrix(starwarsDialogs_tdm)
    
    #Sorting such that the most frequent word are listed first
    
    starwarsDialogs_sorted <- sort(rowSums(starwarsDialogs_matrix),
                                   decreasing = TRUE)
    
    
    #Creating a data frame for the above matrix
    #Give it two columns, first is the words and their corresponding frequency
    
    starwarsDialogs_df <- data.frame(word = names(starwarsDialogs_sorted), 
                               freq = starwarsDialogs_sorted)
    
    
    return(starwarsDialogs_df)
    
  })
  
  
  
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$            THEME: SENTIMENT ANALYSIS                                        $#
#$                                                                             $#
#$            GOAL: OPINION & EMOTION MINING                                   $#
#$                                                                             $# 
#$            DETAILS: CALCULATE THE SENTIMENTS BASED ON THE                   $#
#$                          CHARACTERS' DIALOGUE                               $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################
  
  starwars_sentiments <- reactive({
    
    #Getting the uploaded data 
    starwars_data <- upload_function()
    
    #Get a sentiment out of these terms
    
    starwars_sentiment <- sentiment_by(starwars_data$dialogue)
    
    #Not to change the original file, we make a copy of it here
    
    starwars_movie <- starwars_data 
    
    #Now, add the sentiment column to the duplicate object created above
    
    starwars_movie$Sentiments <- starwars_sentiment$ave_sentiment
    
    
    return(starwars_movie)
    
  })
  
  
  
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                        THEME: CHARACTERS                                    $#
#$                                                                             $#
#$                        GOAL: CREATING CHARACTERS' WORD CLOUD                $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################
      
   #Making the word cloud repeatable
   wordcloud_rep <- repeatable(wordcloud)
   
   #Outputing the Word Cloud for Characters using renderPlot
   output$characters_wc <- renderPlot({
     
     input$update_characters
     
     #Setting Progress Bar
     withProgress({
       
       #Setting Progress message
       setProgress(message = "Creating Word Cloud")
       
       #Getting Characters data From starwarsCharacter_data reactive Function
       characters_wordcloud <- starwarsCharacters_data()
       
       #Setting the Word Cloud Color Themes
       wordcloud_color <- brewer.pal(8, "Accent")
       
       #Obtain the Color Theme from the user
       if(input$color == "Accent"){
         wordcloud_color <- brewer.pal(8, "Accent")
       }
       else if (input$color == "Dark2") {
         wordcloud_color <- brewer.pal(8, "Dark2")
       }
       else if (input$color == "Paired") {
         wordcloud_color <- brewer.pal(12, "Paired")
       }
       else if (input$color == "Pastel1") {
         wordcloud_color <- brewer.pal(9, "Pastel1")
       }
       else if (input$color == "Pastel2") {
         wordcloud_color <- brewer.pal(8, "Pastel2")
       }
       else if (input$color == "Set1") {
         wordcloud_color <- brewer.pal(9, "Set1")
       }
       else if (input$color == "Set2") {
         wordcloud_color <- brewer.pal(8, "Set2")
       }
       else {
         wordcloud_color <- brewer.pal(12, "Set3")
       }
       
       #Now, Create a Wordcloud for the Characters data
       wordcloud(characters_wordcloud$word, characters_wordcloud$freq,
                 min.freq = input$min.freq, colors = wordcloud_color, 
                 random.order = input$random, max.words = input$max.word,
                 rot.per = .5)
     })
   })
   
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                  THEME: CHARACTERS' DIALOGUES                               $#
#$                                                                             $#
#$                  GOAL:  WORD CLOUD                                          $#
#$                                                                             $#
#$                  DETAILS: AFTER CREATING THE CORPUS AND CLEANING THE        $#     
#$                           DIALOGS, CREATE A WORD CLOUD                      $#      
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################
   
   #Making the word cloud repeatable
   wordcloud_rep <- repeatable(wordcloud)
   
   #Outputing the Word Cloud for Clean Dialogues using renderPlot
   output$cleanDialogs_wc <- renderPlot({
     
     input$update_dialogs
     
     #Setting Progress Bar
     withProgress({
       
       #Setting Progress message
       setProgress(message = "Creating Word Cloud")
       
       #Getting CLEAN dialog data From starwarsDialogue_data reactive Function
       cleanDialogs_wordcloud <- starwarsDialogue_data()
       
       #Setting the Word Cloud Color Themes
       wordcloud_color <- brewer.pal(8, "Accent")
       
       #Obtain the Color Theme from the user
       if(input$colord == "Accent"){
         wordcloud_color <- brewer.pal(8, "Accent")
       }
       else if (input$colord == "Dark2") {
         wordcloud_color <- brewer.pal(8, "Dark2")
       }
       else if (input$colord == "Paired") {
         wordcloud_color <- brewer.pal(12, "Paired")
       }
       else if (input$colord == "Pastel1") {
         wordcloud_color <- brewer.pal(9, "Pastel1")
       }
       else if (input$colord == "Pastel2") {
         wordcloud_color <- brewer.pal(8, "Pastel2")
       }
       else if (input$colord == "Set1") {
         wordcloud_color <- brewer.pal(9, "Set1")
       }
       else if (input$colord == "Set2") {
         wordcloud_color <- brewer.pal(8, "Set2")
       }
       else {
         wordcloud_color <- brewer.pal(12, "Set3")
       }
       
       
       #Now, Create a Wordcloud for the Clean Dialogue data
       wordcloud(cleanDialogs_wordcloud,
                 min.freq = input$min, colors = wordcloud_color, 
                 random.order = input$random.order, max.words = input$max,
                 rot.per = .5)
     })
   })
   
   
   
   
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                  THEME: CHARACTERS' DIALOGUES                               $#
#$                                                                             $#
#$                  GOAL:  WORD CLOUD                                          $#
#$                                                                             $#
#$                  DETAILS: AFTER CREATING THE TDM AND THE MATRIX             $#     
#$                           CREATE A WORD CLOUD OF THE DATA FRAME             $#      
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################
   
   #Making the word cloud repeatable
   wordcloud_rep <- repeatable(wordcloud)
   
   #Outputing the Word Cloud for Dialogs Data.Frame using renderPlot
   output$wordcloud_tdm <- renderPlot({
     
     input$update_visualization
     
     #Setting Progress Bar
     withProgress({
       
       #Setting Progress message
       setProgress(message = "Creating Word Cloud")
       
       #Getting Characters data From starwarsDialogue_tdm reactive Function
       starwarsDialogue_wc <- starwarsDialogue_tdm()
       
       #Setting the Word Cloud Color Themes
       wordcloud_color <- brewer.pal(8, "Accent")
       
       #Obtain the Color Theme from the user
       if(input$colorv == "Accent"){
         wordcloud_color <- brewer.pal(8, "Accent")
       }
       else if (input$colorv == "Dark2") {
         wordcloud_color <- brewer.pal(8, "Dark2")
       }
       else if (input$colorv == "Paired") {
         wordcloud_color <- brewer.pal(12, "Paired")
       }
       else if (input$colorv == "Pastel1") {
         wordcloud_color <- brewer.pal(9, "Pastel1")
       }
       else if (input$colorv == "Pastel2") {
         wordcloud_color <- brewer.pal(8, "Pastel2")
       }
       else if (input$colorv == "Set1") {
         wordcloud_color <- brewer.pal(9, "Set1")
       }
       else if (input$colorv == "Set2") {
         wordcloud_color <- brewer.pal(8, "Set2")
       }
       else {
         wordcloud_color <- brewer.pal(12, "Set3")
       }
       
       #Now, Create a Wordcloud for the Characters data
       wordcloud(starwarsDialogue_wc$word, starwarsDialogue_wc$freq,
                 min.freq = input$min_freq, colors = wordcloud_color, 
                 random.order = input$random_order, max.words = input$max_word,
                 rot.per = .5)
     })
   })

   
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                        THEME: CHARACTERS                                    $#
#$                                                                             $#
#$                        GOAL: DISPLAYING THE CHARACTERS                      $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################   
   
   #We need to Display also the Characters as a Table
   #Function renderTable helps displays the characters
   output$starwars_text <- renderTable(striped = TRUE, hover = TRUE,
                                       bordered = TRUE,
   {
     #Getting Characters data to display
     starwarsCharacters_data()
     
   })
   
 
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                        THEME: CHARACTERS                                    $#
#$                                                                             $#
#$                        GOAL: DISPLAYING THE CHARACTERS' DIALOGUES           $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################   
   
   #We need to Display also the Characters' Dialogues as a Table
   #Function renderTable helps displays the Dialogues
   output$starwars_dialogs <- renderTable(striped = TRUE, hover = TRUE,
                                       bordered = TRUE,
   {
     #Getting Characters data to display
     starwarsDialogue_tdm()
     
   }) 
   
   
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                        THEME: DIALOGUES                                     $#
#$                                                                             $#
#$                        GOAL: DISPLAYING THE DIALOGUES TDM                   $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################   
   
   #We need to Display also the Dialogues as a Table
   #Function renderTable helps displays the Dialogues
   output$starwars_tdm <- renderTable(striped = TRUE, hover = TRUE,
                                       bordered = TRUE,
     {
       #Getting Characters data to display
       starwarsDialogue_tdm()
       
     })
   
   
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                        THEME: DIALOGUES                                     $#
#$                                                                             $#
#$                        GOAL: DISPLAYING THE DIALOGUES TDM                   $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################   
   
   #We need to Display also the Dialogues as a Table
   #Function renderTable helps displays the Dialogues
   output$starwars_tdm <- renderTable(striped = TRUE, hover = TRUE,
                                      bordered = TRUE,
      {
        #Getting Characters data to display
        starwars_file <- upload_function()
        
        #Creating a copy of our Data File
        starwars_data <- starwars_file
        
        #Set seed
        set.seed(1000)
        
        #Creating a Partition
        data_partition <- createDataPartition(y = starwars_data$dialogue, 0.50,
                                              list = FALSE)
        
        #Partitioning Data into training and testing Dataset
        train <- starwars_data[data_partition,]
        test <- starwars_data[-data_partition,]
        
      })
   
   
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                  THEME: SENTIMENT ANALYSIS                                  $#
#$                                                                             $#
#$                  GOAL:  BOX PLOT                                            $#
#$                                                                             $#
#$                  DETAILS: CREATING A BOX PLOT FOR THE CHARACTERS'           $#     
#$                                    SENTIMENT SCORES                         $#      
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################
    
    output$sentiments <- renderPlotly({
      
      input$update_visualization
      
      sentiments <- starwars_sentiments()
      
      
      #Try to analyze the data using a box plot
      #The Box plot will clearly show they polarity and as well the positivity and
      #Negativity of the characters
      
      char <- sentiments[1:input$frequency,2]
      
      sent <- sentiments[1:input$frequency,4]
      
      p <- ggplot(sentiments[1:input$frequency, c(2,4)],
                  aes(x = char, y = sent, fill = sent)) + 
        geom_boxplot(size = 0.75, outlier.shape = NA) +
        xlab("Characters") + ylab("Sentiments Score") +
        ggtitle("Box Plot For Sentiments")
      
      ggplotly(p)
      
      #boxplot(sentiments$Sentiments ~ sentiments$character, col = 'yellow',
       #       main = 'SENTIMENTS AS IN EPISODE IV',
       #       xlab = 'Charaters', ylab = 'Sentiments', 
        #      ylim = c(-1, 1), yaxs = 'i')
      
      
    })
   

#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                  THEME: EMOTIONAL SENTIMENT ANALYSIS                        $#
#$                                                                             $#
#$                  GOAL:  BAR PLOT                                            $#
#$                                                                             $#
#$                  DETAILS: CREATING A BAR PLOT TO REPRESENT THE              $#     
#$                                EMOTIONAL VARIANCE                           $#      
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################   
   
   #Visualization of Starwars Emotional Variance USING BarPlot ON PLOTLY
   output$starwars_emotions <- renderPlotly({
     
       #Creating Progress Bar Plus Message
     
       withProgress({
         
         #Setting Progress Message
         
         setProgress(message = "Plotting. Please Wait Patiently")
         
         #Getting the uploaded files.
         
         starwars_file <- upload_function()
         
         #Extracting the dialogs column only
         
         starwars_dialogs <- starwars_file$dialogue
         
         #Getting the Emotions using NRC dictionary
         
         emotions <- get_nrc_sentiment(starwars_dialogs)
         
         #Calculating the emotional counts
         
         emotion_counts = colSums(emotions)
         
         #Creating a Data.Frame for the emotions
         
         emotions_df = data.frame(count = emotion_counts, 
                                  emotions = names(emotion_counts))
         
         #Adding a new column "emotion" to the Data.Frame and sorting it
         
         emotions_df$emotion = factor(emotions_df$emotion,
                        levels = emotions_df$emotion[order(emotions_df$count,
                                                           decreasing = TRUE)])
         
         #Creating a Bar Plot using Plotly
         
         plot_ly(emotions_df, x =~ emotion, y =~ count, type = "bar",
                 color =~ emotion) %>%
           
           layout(xaxis = list(title = ""), showlegend = FALSE,
                  title = "Distribution Of Emotion Categories StarWars Movie")
         
       })
       
     })
   
   
   #################################################################################
   #$=============================================================================$#
   #$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
   #$                                                                             $#
   #$                  THEME: EMOTIONAL SENTIMENT ANALYSIS                        $#
   #$                                                                             $#
   #$                  GOAL:  BAR PLOT                                            $#
   #$                                                                             $#
   #$                  DETAILS: CREATING A BAR PLOT TO REPRESENT THE              $#     
   #$                                EMOTIONAL VARIANCE                           $#      
   #$                                                                             $#
   #$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
   #$=============================================================================$#
   #################################################################################   
   
   #Visualization of Starwars Emotional Variance USING BarPlot ON PLOTLY
   output$analysed_boxplot <- renderPlotly({
     
     #Creating Progress Bar Plus Message
     
     withProgress({
       
       #Setting Progress Message
       
       setProgress(message = "Creating BoxPlot. Please Wait Patiently")
       
       #Getting the uploaded files.
       
       starwars_file <- upload_function()
       
       #Extracting the dialogs column only
       
       starwars_dialogs <- starwars_file$dialogue
       
       #Getting the Emotions using NRC dictionary
       
       emotions <- get_nrc_sentiment(starwars_dialogs)
       
       #Calculating the emotional counts
       
       emotion_counts = colSums(emotions)
       
       #Creating a Data.Frame for the emotions
       
       emotions_df = data.frame(count = emotion_counts, 
                                emotions = names(emotion_counts))
       
       #Adding a new column "emotion" to the Data.Frame and sorting it
       
       emotions_df$emotion = factor(emotions_df$emotion,
                                    levels = emotions_df$emotion[order(emotions_df$count,
                                                                       decreasing = TRUE)])
       
       #Creating a Bar Plot using Plotly
       
       p <- ggplot(emotions_df, aes(emotion, count, fill = emotion)) + 
         geom_boxplot() + 
         ggtitle("Distribution of Emotions On Boxplot") +
         coord_flip()
       
      ggplotly(p)
       
       #plot_ly(emotions_df, x =~ emotion, y =~ count, type = "bar",
        #       color =~ emotion) %>%
        # 
        # layout(xaxis = list(title = ""), showlegend = FALSE,
         #       title = "Distribution Of Emotion Categories StarWars Movie")
       
     })
     
   })
   
   
   
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                  THEME: EMOTIONAL SENTIMENT ANALYSIS                        $#
#$                                                                             $#
#$                  GOAL:  WORD CLOUD                                          $#
#$                                                                             $#
#$                  DETAILS: CREATING A WORD CLOUD TO REPRESENT THE            $#     
#$                                EMOTIONAL VARIANCE                           $#      
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################   
   
   #Visualization of Starwars Emotional Variance USING WORD CLOUD
   output$emotions_wc <- renderPlot({
     
     input$update_analysis
     
     #Setting Progresss bar and Message
     
     withProgress({
       
       #Setting Progress Message
       
       setProgress(message = "Creating Comparison Word Cloud...")
       
       #Getting the uploaded files.
       
       starwars_file <- upload_function()
       
       #Extracting the dialogs column only
       
       starwars_dialogs <- starwars_file$dialogue
       
       #Getting the Emotions using NRC dictionary
       
       emotions <- get_nrc_sentiment(starwars_dialogs)
       
       #Combinning all the emotions so as to create a comparison word cloud
       
       emotions_all = c(
         paste(starwars_dialogs[emotions$anger > 0], collapse=" "),
         paste(starwars_dialogs[emotions$anticipation > 0], collapse=" "),
         paste(starwars_dialogs[emotions$disgust > 0], collapse=" "),
         paste(starwars_dialogs[emotions$fear > 0], collapse=" "),
         paste(starwars_dialogs[emotions$joy > 0], collapse=" "),
         paste(starwars_dialogs[emotions$sadness > 0], collapse=" "),
         paste(starwars_dialogs[emotions$surprise > 0], collapse=" "),
         paste(starwars_dialogs[emotions$trust > 0], collapse=" "))
       
       #First clean the Data
       
       starwars_dialogs <- Corpus(VectorSource(emotions_all))
       starwars_dialogs <- tm_map(starwars_dialogs, content_transformer(tolower))
       starwars_dialogs <- tm_map(starwars_dialogs, removePunctuation)
       starwars_dialogs <- tm_map(starwars_dialogs, removeNumbers)
       starwars_dialogs <- tm_map(starwars_dialogs, stripWhitespace)
       my_stopwords <- c(stopwords('english'), "they've", "we'll", "we're", 
                         "he'll","she'll", "you'll", "it's", "you're", "will",
                         "don't", "i'm", "i've", "what's", "didn't", "can", 
                         "there'll")
       starwars_dialogs <- tm_map(starwars_dialogs, removeWords, my_stopwords)
       starwars_dialogs <- tm_map(starwars_dialogs, stemDocument)
       
       #Creating a Term Document Matrix
       
       emotions_tdm <- TermDocumentMatrix(starwars_dialogs)
       
       #Converting the TDM as a Matrix
       
       emotions_matrix <- as.matrix(emotions_tdm)
       
       #Creating a new column names for the matrix
       
       colnames(emotions_matrix) = c('anger', 'anticipation', 'disgust', 'fear',
                                     'joy', 'sadness', 'surprise', 'trust')
       
       #Setting the Word Cloud Color Themes
       wordcloud_color <- brewer.pal(8, "Accent")
       
       #Obtain the Color Theme from the user
       if(input$colorv == "Accent"){
         wordcloud_color <- brewer.pal(8, "Accent")
       }
       else if (input$colorv == "Dark2") {
         wordcloud_color <- brewer.pal(8, "Dark2")
       }
       else if (input$colorv == "Paired") {
         wordcloud_color <- brewer.pal(12, "Paired")
       }
       else if (input$colorv == "Pastel1") {
         wordcloud_color <- brewer.pal(9, "Pastel1")
       }
       else if (input$colorv == "Pastel2") {
         wordcloud_color <- brewer.pal(8, "Pastel2")
       }
       else if (input$colorv == "Set1") {
         wordcloud_color <- brewer.pal(9, "Set1")
       }
       else if (input$colorv == "Set2") {
         wordcloud_color <- brewer.pal(8, "Set2")
       }
       else {
         wordcloud_color <- brewer.pal(12, "Set3")
       }
       
       #Comparison Word Cloud
       
       comparison.cloud(emotions_matrix, random.order = input$Random,
                        colors = wordcloud_color, min.freq = input$Min,
                        max.words = input$Max, rot.per = 0.3)
       
     })
     
   })
   
   
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                        THEME: CHARACTERS' SENTIMENTS DATA                   $#
#$                                                                             $#
#$                        GOAL: DISPLAYING THE DIALOGUES OF THE CHARACTERS     $#
#$                                       AND SENTIMENTS                        $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################
   
   output$trial <- renderTable({
     starwars_sentiments()
   })
   
   
   
#################################################################################
#$=============================================================================$#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$                                                                             $#
#$                        THEME: HISTOGRAM                                     $#
#$                                                                             $#
#$                        GOAL: DISPLAYING THE HISTOGRAM FOR SENTIMENTS        $#
#$                                                                             $#
#$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
#$=============================================================================$#
#################################################################################
  
   output$hist_sentiments <- renderPlot({
     
     starwars_sentiments <- starwars_sentiments()
     
     hist(starwars_sentiments$Sentiments)
     
     #plot(starwars_sentiments, type = "s", main = "Sentiments Score", col = "red",
         # xlab = "Percentage Duration", ylab = "Character's Emotional Variance")
     
   })
   
   output$Plots <- renderInfoBox({
     infoBox("Visualization Tools", "100", icon = icon("bar-chart-o"), 
             color = "olive")
   })
   
   output$Tasks <- renderValueBox({
     valueBox(100, "Tasks Handled", icon = icon("fire"), color = "lime")
   })
   
    output$histogram <- renderPlot({
      hist(faithful$eruptions, breaks = input$breaks)
    })
})
