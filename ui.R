library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Enhanced Linear Regression App with Multiple Sample Datasets"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      selectInput(
        "sample_data", 
        "Load Sample Dataset", 
        choices = c("None", "mtcars", "faithful"), # Dropdown for sample datasets
        selected = "None"
      ),
      selectInput("xvar", "Select X Variable", choices = NULL),
      selectInput("yvar", "Select Y Variable", choices = NULL),
      actionButton("run_regression", "Submit Regression"),
      hr(),
      uiOutput("dynamic_input"),
      actionButton("predict_btn", "Predict"),
      verbatimTextOutput("predicted_output"),
      hr(),
      downloadButton("download_summary", "Download Regression Summary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Scatterplot with Summary",
          plotOutput("scatter_plot"),
          hr(),
          uiOutput("model_details")
        ),
        tabPanel(
          "Dataset Summary",
          fluidRow(
            column(12, 
                   htmlOutput("dataset_summary_text"),  # Add wording at the top
                   hr(),
                   tableOutput("data_summary")         # Dataset summary table
            )
          )
        ),
        tabPanel(
          "Residuals & Analysis",
          fluidRow(
            tableOutput("residuals_table")        # Residuals table
          ),
          hr(),
          uiOutput("residual_analysis")          # Residual analysis below
        ),
        tabPanel(
          "Correlation Matrix & QQ Plot",
          verticalLayout(
            plotOutput("correlation_matrix"),
            hr(),
            plotOutput("qq_plot")
          )
        )
      )
    )
  )
)
