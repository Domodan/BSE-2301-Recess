
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

#First thing first, Load the libraries to be used

library(shiny)
library(shinydashboard)

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
                             c("Comma" = ",", "Period" = ".", "Tilde" = "~"),
                             inline = TRUE),
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
                            c("Single File", "Multiple Files"))
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
                selectInput("characters", "SELECT KIND OF FILE TO USE",
                            c("Single File", "Multiple Files"))
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
          tabName = "visualize_data"
        ),
        tabItem(
          tabName = "analyze_data",
          h2("Data Analysis Page Not Yet Implemented")
        ),
        tabItem(
          tabName = "home",
          fluidRow(
            box(
              title = "HOME PAGE", status = "danger", solidHeader = TRUE,
              background = "fuchsia",
              h1("WELCOME TO STAR WARS MOVIE SCRIPT ANALYSIS SYSTEM")
            ),
            box(
              title = "DESCRIPTION", status = "success", solidHeader = TRUE,
              background = "teal",
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

