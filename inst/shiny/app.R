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
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("var_select"),
      uiOutput("indep_select"),
      actionButton("submit_btn", "Let's Go!")
    ),
    mainPanel(
      h4("Regression Equation"),
      textOutput("regression_equation"),
      br(),
      h4("Regression Output"),
      verbatimTextOutput("regression_summary")
    )
  )
)

server <- function(input, output, session) {
  # Load data
  data_reactive <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  # UI for selecting variables
  output$var_select <- renderUI({
    req(data_reactive())
    selectInput("dep_var", "Dependent Variable:", choices = names(data_reactive()))
  })

  output$indep_select <- renderUI({
    req(data_reactive(), input$dep_var)
    selectInput("indep_vars", "Independent Variables:",
                choices = setdiff(names(data_reactive()), input$dep_var),
                multiple = TRUE)
  })

  # Store regression result
  values <- reactiveValues(
    equation = NULL,
    model = NULL
  )

  # Run regression after button click
  observeEvent(input$submit_btn, {
    req(input$dep_var, input$indep_vars)
    req(length(input$indep_vars) > 0)

    df <- data_reactive()

    # Make sure all selected columns exist in data
    if (!all(c(input$dep_var, input$indep_vars) %in% names(df))) {
      values$equation <- "Selected variables not found in the dataset."
      values$model <- NULL
      return()
    }

    # Fit model
    formula_str <- paste(input$dep_var, "~", paste(input$indep_vars, collapse = " + "))
    model <- lm(as.formula(formula_str), data = df)

    # Create equation string
    coef_names <- paste0("B", seq_along(input$indep_vars))
    equation_terms <- paste0(coef_names, "*", input$indep_vars, collapse = " + ")
    equation <- paste(input$dep_var, "=", equation_terms)

    values$equation <- equation
    values$model <- model
  })

  output$regression_equation <- renderText({
    req(values$equation)
    values$equation
  })

  output$regression_summary <- renderPrint({
    req(values$model)
    summary(values$model)
  })
}

shinyApp(ui, server)
