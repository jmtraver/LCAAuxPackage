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
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  titlePanel( "BCH Procedure for LCA with Distal Outcome"),
  p("This shiny app can be used to conduct the BCH procedure on an LCA model to
    accurately predict a distal outcome."),
  p("To use the app, you first must fit your LCA (without an outcome) in the modeling software of your choice.
    Create a CSV file that includes your dependent variable and columns indicating the probaiblity of
    class membership."),

   sidebarLayout(
    sidebarPanel(
      # inputs
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
    ),

    mainPanel(

      # outputs
      tableOutput("contents"),
      plotOutput("plot")

    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output,session) {

  #Reactive to store loaded data
  reactives <- reactiveValues(mydata = NULL)

  #Observe file being selected
  observeEvent(input$file1, {

    #Store loaded data in reactive
    reactives$mydata <- read.csv(file = input$file1$datapath)

    #Update select input to reflect column names
    updateSelectInput(session, inputId = 'DependentVar',
                      label = 'Select your dependent variable:',
                      choices  = colnames(reactives$mydata))
    updateSelectInput(session, inputId = 'ProbCols',
                      label = 'Select your posterior probability columns:',
                      choices  = colnames(reactives$mydata))

  })

    output$contents <- renderTable(reactives$mydata)
    output$plot <- renderPlot(hist(reactives$mydata[,1]))

  }



# Run the application
shinyApp(ui = ui, server = server)
