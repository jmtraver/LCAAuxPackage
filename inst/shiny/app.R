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
if (!requireNamespace("naniar", quietly = TRUE)) {
  install.packages("naniar")
}
library(naniar)

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
      uiOutput("member_select"),
      uiOutput("indep_select"),
      uiOutput('prob_select'),
      uiOutput('fam_select'),
      actionButton("submit_btn", "Let's Go!")
    ),
    mainPanel(
      h4("Regression Equation"),
      textOutput("regression_equation"),
      br(),
      h4("Regression Output"),
      verbatimTextOutput("regression_summary"),
      br(),
      h4("Missing Data Plot"),
      plotOutput("missing_plot")
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
    selectInput("dep_var", "Select Dependent Variable:", choices = names(data_reactive()))
  })

  output$member_select <- renderUI({
    req(data_reactive())
    selectInput("member_var", "Select Class Membership Variable:", choices = names(data_reactive()))
  })

  output$indep_select <- renderUI({
    req(data_reactive(), input$dep_var)
    selectInput("indep_vars", "Select Covariates:",
                choices = setdiff(names(data_reactive()), input$dep_var),
                multiple = TRUE)
  })

  output$prob_select <- renderUI({
    req(data_reactive())
    selectInput("prob_vars", "Select Class Probability Columns:",
                choices = setdiff(names(data_reactive()), input$dep_var),
                multiple = TRUE)
  })

  output$fam_select <- renderUI({
    req(data_reactive())
    selectInput("fam_choice", "Select Family:",
                choices = c("gaussian", "binomial", "poisson"),
                multiple = FALSE)
  })

  # Store regression result
  values <- reactiveValues(
    equation = NULL,
    model = NULL
  )

  # Run regression after button click
  observeEvent(input$submit_btn, {
    req(input$dep_var, input$member_var, input$prob_vars)

    df <- data_reactive()

    # Make sure all selected columns exist in data
    if (!all(c(input$dep_var, input$member_vars, input$prob_vars) %in% names(df))) {
      values$equation <- "Selected variables not found in the dataset."
      values$model <- NULL
      return()
    }

    observeEvent(input$submit_btn, {
      output$missing_plot <- renderPlot({
        df <- data_reactive()
        selectVars <- unique(c(input$dep_var, input$member_var, input$indep_vars, input$prob_vars))
        filteredDF <- df[,selectVars,drop=FALSE]
        vis_miss(filteredDF)+ theme(
          axis.text.x = element_text(size = 14, angle = 45, hjust = 0),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16)
        )

      })})

    # Fit model
    formula_str <- paste(input$dep_var, "~", paste(input$member_var, collapse = " + "), " + ",
                         paste(input$prob_vars, collapse = " + "))
    prob_list <- lapply(input$prob_vars, function(col) df[[col]])
    names(prob_list) <- input$prob_vars
    model <- mx_BCH(as.formula(formula_str), data = df, post.prob = prob_list)

    # Create equation string
    my_coefs <- c(input$member_var, input$indep_vars)
    coef_names <- paste0("B", seq_along(my_coefs))
    equation_terms <- paste0(coef_names, "*", my_coefs, collapse = " + ")

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
