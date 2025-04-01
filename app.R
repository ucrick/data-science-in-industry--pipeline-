library(shiny)

# Define UI for app
ui <- fluidPage(
  # App title
  titlePanel("CSV Data Visualization"),
  
  # Sidebar with input file selection
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    ),
    
    # Main panel for displaying summaries and plots
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Plot", plotOutput("plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Read CSV file
  data <- reactive({
    file <- input$file1
    if (is.null(file)) {
      return(NULL)
    }
    read.csv(file$datapath, header = TRUE)
  })
  
  # Summary output
  output$summary <- renderPrint({
    data <- data()
    if (is.null(data)) {
      return(NULL)
    }
    summary(data)
  })
  
  # Plot output
  output$plot <- renderPlot({
    data <- data()
    if (is.null(data)) {
      return(NULL)
    }
    # Replace label1, label2, etc. with your actual column names
    hist(data$label1)
  })
}

# Run the app
shinyApp(ui, server)
