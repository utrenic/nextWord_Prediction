#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

source("read_nGrams.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    observe ({
    
      # predict next word
      sentence <- input$inputId
    
      # show the input text to the GUI
      output$inputText <- renderText({ input$inputId})

      ans <- as.data.frame(predict_next_word(sentence)) %>% top_n(input$numWords)
    
      # nextWords table
      output$nextWords <- renderTable({ans})
    
      # output wordcloud
      wordcloud_r = repeatable(wordcloud)
      output$wordcloud = renderPlot(
                          wordcloud_r(
                          ans$nextWord,
                          ans$Prop,
                          colors = brewer.pal(8, "Dark2"),
                          min.freq = 0.01, 
                          random.order=TRUE
                          )
                        )
   })

})
