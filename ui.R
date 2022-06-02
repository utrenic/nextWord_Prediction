#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
      # Application title
      titlePanel("Next Word Prediction"),
      sidebarLayout(
        sidebarPanel(
          # sliderInput 
          textInput("inputId", label="Enter input phrase:", NULL),
          verbatimTextOutput("inputText"),   
          sliderInput("numWords",
                      "Number of words:",
                       min = 0,
                       max = 20,
                       value = 5)
          
        ),
        mainPanel(
            tableOutput("nextWords"),
            plotOutput("wordcloud")
        )
      )
  )
)
  