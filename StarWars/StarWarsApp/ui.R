
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

#First thing first, Load the libraries to be used

library(shiny)
library(shinydashboard)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(title = "SWAS SYSTEM", #skin = c("blue", "black",
                                       #"purple", "green", "red", "yellow")
                skin = "purple",
    dashboardHeader(title = "SWAS SYSTEM"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName = "home"),
        menuItem("Upload File", tabName = "upload_file"),
        menuItem("Text Mining", tabName = "text_mining",
                 menuSubItem("Characters", tabName = "characters"),
                 menuSubItem("Dialogs", tabName = "dialogs")
                 ),
        menuItem("Visualize & Analyze Data", tabName = "visualize_data"),
        menuItem("Analyse Data", tabName = "analyze_data")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "upload_file",
          fluidPage(
            titlePanel("Upload File To Be Analyzed"),
            sidebarLayout(
              sidebarPanel(
                fileInput("file", "Upload Script File", multiple = TRUE,
                          accept = c("text/csv", "text/comma-seperated-values",
                          "text/plain", ".csv"),
                          placeholder = "File Unavailable. Please Upload",
                          buttonLabel = "Upload File..."
                          ),
                h4("Max File Size is 10 MB"),
                radioButtons("sep", "Separator", 
                             c("Comma" = ",", "Period" = ".", "Tilde" = "~")),
                checkboxInput("header", "Header?")
              ),
              mainPanel(
                tableOutput("input_file")
              )
            )
          )
        ),
        tabItem(
          tabName = "characters",
          fluidPage(
            titlePanel("Characters Page"),
            sidebarLayout(
              sidebarPanel(
                selectInput("characters", "SELECT KIND OF FILE TO USE",
                            c("Single File", "Multiple Files")),
                sliderInput("min.freq", "Set Minimum Word Frequency", 1, 20, 1),
                sliderInput("max.word", "Set Maximum Number of Words", 1, 1000,
                            50),
                checkboxInput("random", "Random Order?"),
                radioButtons("color", "Choose Color Theme TO Use",c("Accent",
                              "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
                              "Set2", "Set3")),
                actionButton("update_characters", "Update")
              ),
              mainPanel(
                tabsetPanel(
                  type = "tab",
                  tabPanel("Data", tableOutput("starwars_text")),
                  tabPanel("Plot", plotOutput("characters_wc"))
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "dialogs",
          fluidPage(
            titlePanel("Starwars Dialogues Page"),
            sidebarLayout(
              sidebarPanel(
                selectInput("dialogs", "SELECT KIND OF FILE TO USE",
                            c("Single File", "Multiple Files")),
                sliderInput("min", "Set Minimum Word Frequency", 1, 20, 1),
                sliderInput("max", "Set Maximum Number of Words", 1, 1000,
                            50),
                checkboxInput("random.order", "Random Order?"),
                radioButtons("colord", "Choose Color Theme TO Use", c("Accent",
                            "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
                            "Set2", "Set3")),
                actionButton("update_dialogs", "Update")
              ),
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
        tabItem(
          tabName = "visualize_data",
          fluidPage(
            titlePanel("Data Visualization & Analysis"),
            sidebarLayout(
              sidebarPanel(
                sliderInput("frequency", "Box Plot Controller", 1, 500, 20),
                sliderInput("min_freq", "Set Minimum Word Frequency", 1, 20, 1),
                sliderInput("max_word", "Set Maximum Number of Words", 1, 1000,
                            50),
                checkboxInput("random_order", "Random Order?"),
                radioButtons("colorv", "Choose Color Theme TO Use",c("Accent",
                            "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
                            "Set2", "Set3")),
                actionButton("update_visualization", "Update")
              ),
              mainPanel(
                tabsetPanel(
                  tabPanel("Data", tableOutput("starwars_tdm")),
                  tabPanel("Histogram", plotOutput("hist_sentiments")),
                  tabPanel("WordCloud", plotOutput("wordcloud_tdm")),
                  tabPanel("BoxPlot", plotlyOutput("sentiments"))
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "analyze_data",
          fluidPage(
            titlePanel("Sentimental Analysis & Visualization"),
            sidebarLayout(
              sidebarPanel(
                selectInput("Analysis", "SELECT KIND OF FILE TO USE",
                            c("Single File", "Multiple Files")),
                sliderInput("Min", "Set Minimum Word Frequency", 1, 20, 1),
                sliderInput("Max", "Set Maximum Number of Words", 1, 1000,
                            50),
                checkboxInput("Random", "Random Order?"),
                radioButtons("Color", "Choose Color Theme TO Use",
                             c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2",
                               "Set1", "Set2", "Set3")),
                actionButton("update_analysis", "Update")
              ),
              mainPanel(
                tabsetPanel(
                  tabPanel("Data", tableOutput("t")),
                  tabPanel("BarPlot", plotlyOutput("starwars_emotions")),
                  tabPanel("WordCloud", plotOutput("emotions_wc")),
                  tabPanel("BoxPlot", plotlyOutput("analysed_boxplot"))
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "home",
          fluidRow(
            infoBox("Characters", 50, icon = icon("thumbs-up"),
                    color = "light-blue"),
            infoBox("Dialogues", paste0("10,000"), icon = icon("info-circle"),
                    color = "orange"),
            infoBoxOutput("Plots")
          ),
          fluidRow(
            valueBox(paste0("< 5"),"Number Of Files Allowed", color = "purple", 
                     icon = icon("folder-open")),
            valueBoxOutput("Tasks")
          ),
          fluidRow(
            box(
              title = "HOME PAGE", status = "warning", solidHeader = TRUE,
              background = "fuchsia", plotOutput("histogram"),
              h1("WELCOME TO STAR WARS MOVIE SCRIPT ANALYSIS SYSTEM")
            ),
            box(
              title = "DESCRIPTION", status = "success", solidHeader = TRUE,
              background = "teal",
              sliderInput("breaks", "Number OF Breaks", 1, 100, 10),
              textInput("text", "Enter Comments", value = "comments"), 
              h2("Your Home of Movie Scripts Analysis")
            )
          ),
          fluidRow(
            h1("ENJOY YOUR ANALYSIS")
          )
        )
      )
    )
  )
)

