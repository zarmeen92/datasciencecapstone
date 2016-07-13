library(shiny)
shinyUI <- fluidPage(
  # Application title
  titlePanel("Data Science Capstone Project - Next Word Prediction App"),
  
  # User interface controls1
  sidebarLayout(
    sidebarPanel(
      p("Type a word or sequence of words and press <ENTER> or click <Predict> to see the next word(s) suggestions:"),
      p("Note: The first run of the app could take a couple of secs for the next word to appear"),
      textInput(inputId="text", label = ""),
      submitButton("Predict"),
      HTML('<script type="text/javascript"> 
           document.getElementById("text").focus();
           </script>')
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("About", htmlOutput("help"),
                 tags$div(id="help", 
                          HTML("<div> This is a next word prediction application built for Data Science Capstone Project given by JohnHopkins@Coursera.
                               <br/>Model is built using quad gram model with Simple Good Turing Algorithm smoothing.
                               <br/> 
                               <br/> Note: The first run of the app could take a couple of secs for the next word to appear.</div>")
                          )
                          ),
        tabPanel("App", 
                 conditionalPanel(condition = "input.text != ''",
                                  verbatimTextOutput("text"),
                                  verbatimTextOutput("cleaned"), verbatimTextOutput("msg"),
                                  verbatimTextOutput("nextWords")
                 )
        )                 
       
      )
    )
      ),
  
  fluidRow(HTML("<div style='margin-left:18px;margin-bottom:12px;color:red;'><strong>Created By : Zarmeen Nasim</strong></div>") )
   )
