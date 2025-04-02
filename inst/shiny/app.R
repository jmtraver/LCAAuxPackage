#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(stats)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),

    # Application title
    titlePanel("BCH Procedure for LCA with Auxilary Variables"),
    #Upload a File:
    fileInput("file1", "Choose a File"),

    varSelectInput(
      'DependentVar',
      'Select your dependent variable:', ""),

    varSelectInput(
      'ProbCols',
      'Select your posterior probability columns:', "",
      multiple = TRUE),


    #Select your model
    selectInput(
      "select",
      "Select your model:",
      list("Latent Class Model" = "1A", "Latent Profile Model" = "1B", "Growth Mixture Model" = "1C")
    ),

  mainPanel("Summary Output",
           verbatimTextOutput("RegOut")),

)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  #Reactive to store loaded data
  reactives <- reactiveValues(

    mydata = NULL

  )

  #Observe file being selected
  observeEvent(input$file1, {

    #Store loaded data in reactive
  reactives$mydata <- read.csv(file = input$file1$datapath)

    #Update select input
  updateSelectInput(session, inputId = 'DependentVar',
                      label = 'Select your dependent variable:',
                      choices  = colnames(reactives$mydata))
  updateSelectInput(session, inputId = 'ProbCols',
                      label = 'Select your posterior probability columns:',
                      choices  = colnames(reactives$mydata))

  })



  output$RegOut = renderPrint({
    req(input$file1) #require input file
    req(input$file1$DependentVar) #require dependent var
    summary(input$file1$DependentVar)})

}

# Run the application
shinyApp(ui = ui, server = server)
