
# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Assignment 2 - mengyao liu 4341644"),

    mainPanel(
      tabsetPanel(
        tabPanel("EDA",
                 tabsetPanel(
                   
                   tabPanel("summary", 
                            verbatimTextOutput("summary_output"),
                            verbatimTextOutput("summary")
                            ),
                   
                   tabPanel("Table", DTOutput("mytable")),
                   
                   tabPanel("corrgram", 
                            selectizeInput("corrgramplot", "Select Variable:",
                                           choices = numeric_columns,
                                           selected = numeric_columns,
                                           multiple = TRUE),
                            radioButtons("correlationMethod", "Choose Correlation Method", choices = c("spearman", "kendall", "pearson"), selected = "pearson"),
                            selectizeInput("ordervar", "sort order :",
                                           choices = c("OLO", "HC"),
                                           selected = "HC",
                                           multiple = FALSE),
                            checkboxInput("absCheckbox", "Using absolute values", value = FALSE),
                            plotOutput("corrgram")
                   ),
                   
                   tabPanel("Miss",
                            
                            plotOutput("Miss"),
                            plotOutput("Miss_2")
                            ),
                   
                   tabPanel("GGally",
                            selectInput("GGallyvar", "Selection variable:", 
                                        choices = numeric_columns, 
                                        selected = "HEALTHCARE_COST", 
                                        multiple = TRUE, 
                                        selectize = TRUE),
                            selectInput("colorVar", "Select variables corresponding to colors:", 
                                        choices = factor_columns, 
                                        selected = "HEALTHCARE_BASIS", 
                                        selectize = TRUE),
                            plotOutput("GGally")
                   ),
                   
                   tabPanel("Boxplot", 
                            selectizeInput("boxplot", "Select Variable:",
                                           choices = numeric_columns,
                                           selected = numeric_columns,
                                           multiple = TRUE),
                            fluidRow(
                              sliderInput("outlierThreshold", "OutlierThreshold:", min = 1.5, max = 3, value = 1.5),
                              checkboxInput("center_0", "Center", value = TRUE),
                              checkboxInput("scale_0", "Scale", value = TRUE),
                              checkboxInput("showOutliers", "Show Outliers", value = TRUE),
                              plotOutput("Boxplot")
                            )
                   )

                )
          ),
        
        tabPanel("process",
                 tabsetPanel(
                   
                   tabPanel("1.shadow",
                            plotOutput("shadow")
                   ),

                   
                   
                   tabPanel("2.clean&model",
                            radioButtons("center", label = "center?",
                                         choices = c("yes", "no"),
                                         selected = "no"),
                            radioButtons("scale", label = "scale?",
                                         choices = c("yes", "no"),
                                         selected = "no"),
                            sliderInput(inputId = "VarThreshold", label = "Threshold of variable missingness", 
                                        min = 1, max = 100, value = 50, post = "%"),
                            sliderInput(inputId = "ObsThreshold", label = "Threshold of observations missingness", 
                                        min = 1, max = 100, value = 50, post = "%"),
                            selectInput(inputId = "ImpMethod", label = "Imputation method", 
                                        choices = c("KNN", "Partial Del", "Median"), selected = "KNN"),
                            radioButtons("folds", label = "how many folds",
                                         choices = c("5", "10"),
                                         selected = "5"),
                            actionButton(inputId = "Go", label = "Train", icon = icon("play")),
                            withSpinner(
                              plotOutput(outputId = "Missing")
                            ),
                            withSpinner(
                              verbatimTextOutput(outputId = "Summary")
                            )
                            
                   ),
                   tabPanel("3.tree",
                            plotOutput("tree")
                   ),
                   tabPanel("4.plots",
                     tabsetPanel(
                     tabPanel("prediction",
                        verbatimTextOutput("TestRMSE"),
                       plotOutput("prediction_vs_actual"),
                     ),
                     
                     tabPanel("Residualtrain",
                              
                              fluidRow(
                                sliderInput("outlierThreshold_2", "OutlierThreshold:", min = 1.5, max = 3, value = 1.5),
                                plotOutput("Residualtrain")
                              ),
                              
                     ),
                     
                     tabPanel("Residualtest",
                              
                              fluidRow(
                                sliderInput("outlierThreshold_3", "OutlierThreshold:", min = 1.5, max = 3, value = 1.5),
                                plotOutput("Residualtest")
                              ),
                              
                     ),
                     tabPanel("ResidualBoxPlot",

                              fluidRow(
                                sliderInput("outlierThreshold_1", "OutlierThreshold:", min = 1.5, max = 3, value = 1.5),
                                plotOutput("ResidualBoxPlot")
                              ),
                              
                              )
                     
                     
                     
                   )       
                   ),
                   
                   
                   
                   
                   
                   
                )
        )
                 
                 
                 
                 
        )
      
        
        
        

    )
      
      
)
