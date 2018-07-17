#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(ggplot2)
library(car) 
library(corrgram)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = "bootstrap.css",
  navbarPage("STAR WARS MOVIE SCRIPTS ANALYSIS SYSTEM",
                           tabPanel("SCRIPT ANALYSIS SYSTEM",
   
   # Application title
   titlePanel(title = h1("STAR WARS MOVIE SCRIPTS ANALYSIS SYSTEM", align="center",style="font-family:Algerian"
                         )),
    
   sidebarLayout(
      sidebarPanel(
        tags$style("body{background-color:linen;color:blue}"),
        radioButtons(inputId = "visualise", label="visualisations on data", choices = c(Textplot="plot1",Wordcloud="plot2"),selected = 'Textplot'),
        tags$style("body{background-color:grey;color: brown}")
        

      ),
      mainPanel(
        plotOutput("distPlot")
      )
   )
                           ),
   
                 tabPanel("DATA VISUALIZATION",
            
                  titlePanel(title=h2("STAR WARS MOVIE SCRIPTS ANALYSIS SYSTEM",style="font-family:Algerian")),
                  sidebarLayout(
                    sidebarPanel(
                      checkboxInput(inputId = "stringsAsFactors", label = "stringAsFactors",value = FALSE)
                     
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                      tabsetPanel(
                        type = "tab",
                        tabPanel("Data", tableOutput("starwars_dialogs")),
                        tabPanel("Plot", plotOutput("cleanDialogs_wc"))
                      )
                    )
                    )
                  )
                 ),
   
              
   
                           tabPanel("CONCLUSIONS",
            
                                titlePanel(title = h2("CONCLUSION ON MOVIE SCRIPTS",style="font-family:Algerian",align="center")),
                                sidebarLayout(
                                  sidebarPanel(
                                    
                                    
                                  ),
                                  
                                  # Show a plot of the generated distribution
                                  mainPanel(
                                    plotOutput("conc_starwars")
                                  )
                                )
                    ),
   
         tabPanel("More Information",   # Information about data collection.
                  "Please for more information contact on:",
                  br(),
                  br(),
                  "Please see the code", 
                  a("this site", href="https://github.com/Domodan/BSE2301-Recess-2018-Group-BSE-01",
                  br(),
                  br(),
                  
                  "Any questions or comments can be sent to",
                  br(),
                  "KABENI EMMANUEL " ,
                  a("kabeniemmanuel@gmail.com", href="mailto:kabeniemmanuel@gmail.com"),
                  br(),
                  "OMACHOL JAMES: ",
                  a("omacholjames@gmail.com", href="mailto:omacholjames@gmail.com"),
                  br(),
                  "SSEBUUFU EDDY: ",
                  a("ssebuufueddyson@yahoo.com", href="ssebuufueddyson@yahoo.com")
                  
   )
   

))
# Define server logic required to draw a histogram

server <- function(input, output) {
  upload_function <- reactive({
    
    #Storing input file being uploaded
    upload_file <- input$file
    
    #Checking to see if the file is empty
    if(is.null(upload_file)) {
      return("Please choose file")
    }
    
    #Reading the file as a using a csv file
    read.csv(upload_file$datapath, header = input$header, sep = input$sep)
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
    
    
    head(starwarsDialogs_df)
    
    return(starwarsDialogs_df)
    
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
    
    #Setting Progress Bar
    withProgress({
      
      #Setting Progress message
      setProgress(message = "Creating Word Cloud")
      
      #Getting Characters data From starwarsCharacter_data reactive Function
      characters_wordcloud <- starwarsCharacters_data()
      
      #Now, Create a Wordcloud for the Characters data
      wordcloud(characters_wordcloud$word, characters_wordcloud$freq,
                min.freq = 1, colors = brewer.pal(8, "Accent"), 
                random.order = TRUE, max.words = 200, rot.per = .5)
    })
  })
  
  #################################################################################
  #$=============================================================================$#
  #$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
  #$                                                                             $#
  #$                  THEME: CHARACTERS' DIALOGS                                 $#
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
    
    #Setting Progress Bar
    withProgress({
      
      #Setting Progress message
      setProgress(message = "Creating Word Cloud")
      
      #Getting CLEAN dialog data From starwarsDialogue_data reactive Function
      cleanDialogs_wordcloud <- starwarsDialogue_data()
      
      #Now, Create a Wordcloud for the Clean Dialogue data
      wordcloud(cleanDialogs_wordcloud,
                min.freq = 1, colors = brewer.pal(8, "Accent"), 
                random.order = TRUE, max.words = 200, rot.per = .5)
    })
  })
  
  
  
  
  #################################################################################
  #$=============================================================================$#
  #$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
  #$                                                                             $#
  #$                  THEME: CHARACTERS' DIALOGS                                 $#
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
  output$characters <- renderPlot({
    
    #Setting Progress Bar
    withProgress({
      
      #Setting Progress message
      setProgress(message = "Creating Word Cloud")
      
      #Getting Characters data From starwarsDialogue_tdm reactive Function
      starwarsDialogue_wc <- starwarsDialogue_tdm()
      
      #Now, Create a Wordcloud for the Characters data
      wordcloud(starwarsDialogue_wc$word, starwarsDialogue_wc$freq,
                min.freq = 1, colors = brewer.pal(8, "Accent"), 
                random.order = TRUE, max.words = 200, rot.per = .5)
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
  #$                        THEME: CHARACTERS' DIALOGUES                         $#
  #$                                                                             $#
  #$                        GOAL: DISPLAYING THE DIALOGUES OF THE CHARACTERS     $#
  #$                                                                             $#
  #$+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++$#
  #$=============================================================================$#
  #################################################################################   
  
  #We need to Display also the Characters as a Table
  #Function renderTable helps displays the characters
  output$starwars_dialogs <- renderTable(striped = TRUE, hover = TRUE,
                                         bordered = TRUE,
                                         {
                                           #Getting Characters data to display
                                           starwarsDialogue_tdm()
                                           
                                         })
  output$conc_starwars<-renderText(hover=TRUE,{conc_starwars()})
}
shinyApp(ui = ui, server = server)

