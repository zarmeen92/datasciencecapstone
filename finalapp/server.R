# =================================================
# : Coursera.org
# : Data Science Specialization - Capstone Project
# : January. 2016
# :
# : Shiny Application: Predicting Next Word
# :
# : Author  - Zarmeen
# =================================================

library(shiny)
source("functions.R")

dataLoadComplete=FALSE
print("loading data")
loadfiles()
dataLoadComplete=TRUE
print("Files loaded")


server <- function(input, output, session) {
  
  output$text <- renderText({
    paste("Input text is:", input$text)
  })
  dataInput <- reactive({
    print(paste("Data load", dataLoadComplete))
    if(dataLoadComplete) {
      textCleansed <- clean(input$text)
      if(textCleansed != "") 
      {
        output$cleaned <- renderText({
          paste0("After preprocessing : [",textCleansed,"]")
        })
        
        textCleansed <- gsub(" \\* "," ",textCleansed)
        
      predictWords <- predict_model(textCleansed)
      pred <- data.frame(word = predictWords$word)
      predd <- paste0("[",pred$word,"]")
      predd
      }
    } else {
      return(c("Loading data... Please wait"))
    }
  })
  
  observe({
    iniTime <- Sys.time()
    
      #output$table <- renderTable(pred)
      output$nextWords <- renderText(dataInput())
      endTime <- Sys.time()
     
      output$msg <- renderText({
        sprintf("Total time processing = %6.3f msecs",1000*(endTime-iniTime))
      })
      gc()
      
  })
}