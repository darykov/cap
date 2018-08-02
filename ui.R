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
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text Prediction"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("text",
                "Enter text: then press enter", value = ""
      ),
      submitButton(text = 'Enter'),
      h3(" "),
      "Please note that the predictions in \"other options\" may include repeating words and the entries in lower lines may show higher probability.
      This is done for illustrational purposes. The repeating predictions would occur with an explicit switch to lower order N-gram" 
    ),
    h3(" "),
    
    ("The code can be seen at "),

    mainPanel(
      h4("The application produces several predictions, the predition with highest probability is selected as Primary."),
         h4("The predictions including primary are displayed in the table below for illustrative purposes."),
      h4("Primary - Highest Probability Predictor"),
      tableOutput("dataMain"), 
      h4("Other Options"), 
      tableOutput("data")
    
          )
    
      )
))
