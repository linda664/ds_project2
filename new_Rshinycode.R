library(shiny)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)
library(ggExtra)
library(data.table)
library(DT) 
library(caret)
library(recipes)
library(shinyWidgets)
library(rsconnect)
#library(randomForest)
library(e1071)
library(ggplot2)
library(ranger)
library(pROC)
library(kernlab)
#library(xgboost)
library(vip)



data_initial <- read.csv("carseats_full.csv", header = T)


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #shiny-notification-panel {
        position: fixed; /* Override the default positioning */
        top: 50%; /* Center vertically */
        left: 50%; /* Center horizontally */
        transform: translate(-50%, -50%); /* Adjust the position to be truly centered */
        width: auto; /* Allow dynamic width */
      }
      .shiny-notification {
        font-size: 20px; /* Larger font size */
        width: 100%; /* Notification width to fit content */
        padding: 20px; /* Padding inside the notification */
        text-align: center; /* Center text inside the notification */
      }
    "))
  ),
  titlePanel("STAT 5243 - Project2 Shiny Application"),
  navbarPage(
    title = "STAT 5243",
    theme = shinytheme("flatly"),
    
    tabPanel("Overview", icon = icon("info-circle"),
             titlePanel("Overview: User Instructions"),
             HTML("<strong>Welcome to the STAT 5243 Project2 Application! </strong>: Need to rewrite."),
             tags$h3("Recommended Workflow:"),
             tags$ul(
               tags$li("Data Selection  →  Data Overview & Visualization  →  Data Preparation (return to previous steps to validate any changes)  →  Data Preprocessing  →  Model Training."),
               tags$li("Please use the navigation bar at the top to access different functionalities. Below is a brief walkthrough of each section to help you efficiently navigate through the application."),
               
             ),
             tags$ul(
               tags$h3("Step 1. Data Selection:"),
               tags$ul(
                 tags$li("Upload Data: Choose 'Car Seats' for a predefined dataset or 'Upload your own file' to import a data file from your computer. Supported file formats are CSV and TXT."),
                 tags$li("If uploading your own dataset: If applicable, please specify the string representing missing values (NAs) in your dataset.")
               )
             ),
             tags$ul(
               tags$h3("Step 2. Data Exploration & Visualization:"),
               tags$ul(
                 tags$li("Data Summary: This tab prints out the descriptive statistics summary for all variables in the dataset."),
                 tags$li("Variable Types: Review and modify data types as needed."),
                 tags$li("Variable Distribution: You can also select a variable of interest to view more closely of its descriptive statistics, and distribution plot. Adjust the histogram setting at the sidebar"),
                 tags$li("Scatterplots: You can explore relationships between variables by selecting a response (Y) and an explanatory (X) variable. Adjust transparency and opt to display marginal distributions at the sidebar.")
               )
             ),
             tags$ul(
               tags$h3("Step 3. Data Preparation & Manipulation:"),
               tags$ul(
                 tags$li("Checking Class Levels: Check if the target feature is binary, if not, perform neccesarily conversion at this tab"),
                 tags$li("Checking NAs: Check if there are columns with missing values and decide whether to drop them later."),
                 tags$li("Zero Variance: Identify columns with no variance."),
                 tags$li("Drop Columns: Remove unnecessary columns from the dataset.")
               )
             ),
             
             tags$ul(
               tags$h5("(Remember to return to Step 2 if sturctual changes has been applied to your dataset. Check the distributions to make sure that the changes were successfully applied.)")
             ),
             
             tags$ul(
               tags$h3("Step 4. Data Preprocessing:"),
               tags$ul(
                 tags$li("Set Target Feature: Choose the feature to predict."),
                 tags$li("Train/Test Split: Define the proportion of data split between training and testing."),
                 tags$li("Handle Missing Values: Decide on a strategy for missing values: ignore, drop, or impute using KNN (specify neighbor count).")
               )
             ),
             # tags$ul(
             #   tags$h3("Step 5. Model Training:"),
             #   tags$ul(
             #     tags$li("Choose a Model: Select from available models like Random Forest, SVM Linear, etc."),
             #     tags$li("Set Hyperparameters: Customize hyperparameters based on the chosen model."),
             #     tags$li("Train Model: Initiate model training by clicking 'Train Model.'")
             #   )
             # ),
             # tags$ul(
             #   tags$h3("Step 6: Results and Visualization:"),
             #   tags$ul(tags$li("Model Summary: Review performance metrics and summary statistics of the trained model."),
             #           tags$li("ROC Curve: Assess the classifier's performance through ROC curve visualization."),
             #           tags$li("Confusion Matrix: Analyze the accuracy of classifications for both training and testing phases.")
             #   )
             # ),
             tags$br(),
             tags$br()
    ),
    
    tabPanel("Data Selection", icon = icon("folder-open"),
             titlePanel("Upload Data"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset", "Dataset:", choices = c("Car Seats", "Upload your own file")),
                 conditionalPanel(
                   condition = "input.dataset == 'Upload your own file'",
                   fileInput("file", "Select your files:", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                   textInput("na_string", "Enter the NA string:", value = ""),
                   actionButton("confirm_na", "Confirm NA String")
                 )
               ),
               mainPanel(
                 dataTableOutput("data_preview")
               )
             )
    ),
    
    tabPanel("Data Exploration & Visualization",icon = icon("chart-simple"),
             titlePanel("Explore Your Data"),
             tabsetPanel(
               tabPanel("Data Summary",
                        titlePanel("Descriptive Statistics of All Variables"),
                        verbatimTextOutput("summaryOutput_all")
                        
               ),
               tabPanel("Variable Type",
                        fluidRow(
                          column(8, 
                                 # Display current data types
                                 tags$h3("Current Data Types"),
                                 HTML("Verify the current type of all variables in your data and modify as needed. Note that incorrect data type can lead to errors in modeling. 
                                         For example: Check if your target feature is a factor"),
                                 tags$br(),
                                 verbatimTextOutput("data_types_output")
                          ),
                          column(4,
                                 # UI for selecting a variable and changing its type
                                 tags$h3("Change the Data Type of:"),
                                 uiOutput("var_select"),  # UI to select variable
                                 tags$br(),  # Adding a line break for better spacing
                                 uiOutput("type_change_ui")  # UI for inputting new type and confirming change
                          )
                        )
               ),
               tabPanel("Variable Distribution",
                        titlePanel("Visualize Variable of Interest"),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("selectedVars", "Select Variables:", choices = names(data())),
                            tags$h3("Plotting the selected variables"),
                            numericInput("bins", "Number of bins for Histogram", min = 1, max = 50, step = 1, value = 10),
                            radioButtons("color", "Color of bins for Histogram:",
                                         choices = list("Blue" = "skyblue", "Pink" = "pink", "Orange" = "orange"),
                                         selected = "skyblue")
                          ),
                          mainPanel(
                            fluidRow(
                              column(6,
                                     verbatimTextOutput("summaryOutput")),
                              column(6,
                                     plotOutput("distPlot")))
                          )
                        )
                        
               ),
               tabPanel("Scatterplots",
                        titlePanel("Explore relationship between two variables (scatterplots)"),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("response", "Response Variable (Y) for Scatterplot", choices = NULL), 
                            selectInput("explanatory", "Explanatory Variable (X) for Scatterplot", choices = NULL),
                            sliderInput("shade", "Transparency Rate for Scatterplot", min = 0, max = 1, value = 0.5, step = 0.1),
                            checkboxInput("marginal", "Show Marginal Distributions in Scatterplot", value = FALSE)
                          ),
                          mainPanel(
                            plotOutput("relPlot")
                          )
                        )
               )
               
             )
             
    ),
    
    tabPanel("Data Preparation & Manipulation", icon = icon("gear"),
             titlePanel("Prepare and Manipulate Your Data"),
             tabsetPanel(id = "exploration_tabs",
                         tabPanel("Checking Class levels",
                                  titlePanel("Binary Level Conversion Tool"),
                                  HTML("Please make sure that the target feature is <strong>binary (a factor with only two classes)</strong>
                                  and the class names do not contain any special character or numbers. If your target
                                  feature is not binary (numeric or multinomial), use the conversion tool below to convert it into a binary factor."),
                                  tags$hr(),
                                  sidebarLayout(
                                    sidebarPanel(
                                      uiOutput("var_convert"),
                                      uiOutput("conversion_threshold_ui")
                                    ),
                                    mainPanel(
                                      dataTableOutput("level_conversion_output")
                                    )
                                  )
                         ),
                         tabPanel("Checking NAs",
                                  titlePanel("Check columns with NAs"),
                                  DTOutput("na_info_table")
                                  
                         ),
                         tabPanel("Zero Variance",
                                  titlePanel("Zero Variance Columns"),
                                  uiOutput("zero_variance_output")
                         ),
                         tabPanel("Drop Columns",
                                  titlePanel("Choose any columns to drop"),
                                  uiOutput("drop_var_select"), # Dropdown to select variables to drop
                                  actionButton("drop_vars", "Drop Selected Columns"),  # Button to trigger the drop
                                  dataTableOutput("dropped_data_preview") # Display the updated dataset
                         )
             )
    ),
    tabPanel("Preprocessing", icon = icon("code-branch"),
             titlePanel("Data Preprocessing for Machine Learning"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("target_feature", "Select Target Feature:", choices = NULL),
                 actionButton("confirm_target", "Confirm Target Feature"),
                 #numericInput("train_test_split", "Train/Test Split (%):", value = 70, min = 50, max = 90),
                 radioButtons("na_handling", "Handle Missing Values:", choices = c("There's no missing value.", "Drop NAs", "Impute with KNN")),
                 conditionalPanel(
                   condition = "input.na_handling == 'Impute with KNN'",
                   numericInput("k_neighbors", "Number of Neighbors:", value = 5, min = 1)
                 ),
                 actionButton("finish_preprocessing", "Finish Preprocessing")
               ),
               # mainPanel(
               #   tabsetPanel(
               #     tabPanel("Transformed Train Set", dataTableOutput("transformed_train")),
               #     tabPanel("Transformed Test Set", dataTableOutput("transformed_test"))
               #   )
               # )
                 mainPanel(
                 h4("Output will be displayed here")  
               )
             )
    ),
    # tabPanel("Model Training", icon = icon("brain"),
    #          titlePanel("Train Final Models"),
    #          sidebarLayout(
    #            sidebarPanel(
    #              selectInput("modelType", "Choose a Model:", choices = c("Random Forest", "SVM Linear", "SVM Polynomial", "SVM RBF", "XGBoost")),
    #              # Add conditional UI for Random Forest hyperparameters
    #              conditionalPanel(
    #                condition = "input.modelType == 'Random Forest'",
    #                textInput("mtry", "mtry (comma-separated values):", value = "3, 5, 10"),
    #                textInput("min.node.size", "min.node.size (comma-separated values):", value = "1, 2, 5"),
    #                selectInput("splitrule", "splitrule:", choices = c("gini", "extratrees"))
    #              ),
    #              
    #              # Add conditional UI for SVM Linear hyperparameters
    #              conditionalPanel(
    #                condition = "input.modelType == 'SVM Linear'",
    #                textInput("C_linear", "Cost (C) (comma-separated values):", value = "0.01, 0.1, 1, 10")
    #              ),
    #              
    #              # Add conditional UI for SVM Polynomial hyperparameters
    #              conditionalPanel(
    #                condition = "input.modelType == 'SVM Polynomial'",
    #                textInput("C_poly", "Cost (C) (comma-separated values):", value = "0.01, 0.1, 1, 10"),
    #                textInput("degree", "Degree (comma-separated values):", value = "2, 3, 4"),
    #                textInput("scale_poly", "Scale (comma-separated values):", value = "0.1, 0.5, 1")
    #              ),
    #              
    #              # Add conditional UI for SVM RBF hyperparameters
    #              conditionalPanel(
    #                condition = "input.modelType == 'SVM RBF'",
    #                textInput("C_rbf", "Cost (C) (comma-separated values):", value = "0.01, 0.1, 1, 10"),
    #                textInput("sigma", "Sigma (comma-separated values):", value = "0.01, 0.1, 1")
    #              ),
    #              # Add conditional UI for XGBoost hyperparameters
    #              conditionalPanel(
    #                condition = "input.modelType == 'XGBoost'",
    #                textInput("nrounds", "Number of Rounds (comma-separated values):", value = "100, 200, 300"),
    #                textInput("max_depth", "Max Depth (comma-separated values):", value = "4, 6, 8, 10"),
    #                textInput("eta", "Learning Rate (eta) (comma-separated values):", value = "0.05, 0.1, 0.2, 0.3"),
    #                textInput("min_child_weight", "Min Child Weight (comma-separated values):", value = "5, 10, 15")
    #              )
    #              ,
    #              actionButton("trainModel", "Train Model")
    #            ),
    #            
    #            mainPanel(
    #              tabsetPanel(
    #                tabPanel("Model Summary", verbatimTextOutput("modelSummary")),
    #                tabPanel("ROC Curve", plotOutput("rocPlot")),
    #                tabPanel("Training Confusion Matrix", verbatimTextOutput("trainConfMatrix")),
    #                tabPanel("Testing Confusion Matrix", verbatimTextOutput("testConfMatrix")),
    #                tabPanel("Feature Importance", plotOutput("featureImportancePlot"))
    #              )
    #            )
    #          )
    # )
  )
)




# Define Server

server <- function(input, output, session) {
  
  data <- reactiveVal()
  
  # Upload and manage dataset
  observe({
    if (input$dataset == 'Upload your own file') {
      req(input$file)
      File <- input$file
      
      observeEvent(input$confirm_na, {
        data(data.frame(rbindlist(lapply(File$datapath, function(path) fread(path, na.strings = input$na_string)), use.names = TRUE, fill = TRUE)))
      }, ignoreInit = TRUE)
      
      if (is.null(data())) {
        data(data.frame(rbindlist(lapply(File$datapath, fread), use.names = TRUE, fill = TRUE)))
      }
    } else {
      data(data_initial)
    }
  })
  
  output$data_preview <- renderDataTable({
    data()
  })
  
  
  ################################################################################
  #Data Overview and Plotting
  
  # Update UI for variable selection based on file upload
  
  observe({
    if (!is.null(data())) {
      updateSelectInput(session, "selectedVars", choices = names(data()))
      updateSelectInput(session, "response", choices = names(data()))
      updateSelectInput(session, "explanatory", choices = names(data()))
    }
  })
  
  
  # Display summary and distribution plots
  output$summaryOutput_all <- renderPrint({
    summary(data())
  })
  
  output$summaryOutput <- renderPrint({
    req(input$selectedVars)
    # Get the subset of the data based on selected variables
    selected_data <- data()[, input$selectedVars, drop = FALSE]
    # Loop through each selected variable to decide what to print
    lapply(names(selected_data), function(var) {
      if (is.factor(selected_data[[var]]) || is.character(selected_data[[var]])) {
        # For factor/character variables, print the count of each level
        cat(paste("Distribution of", var, ":"))
        table(selected_data[[var]])
      } else {
        # For numeric/integer variables, print the summary statistics
        cat(paste("Summary of", var, ":"))
        summary(selected_data[[var]])
      }
    })
      
  })
  
  ################################################################################
  
  #Change data Types
  output$data_types_output <- renderPrint({
    req(data())  # Ensure the reactive data is actually required here
    sapply(data(), class) # Return the data types to be printed in the UI
  })
  
  
  output$var_select <- renderUI({
    req(data())
    tagList(
      tags$div(style = "overflow-y: scroll; max-height: 300px;", 
               checkboxGroupInput("variable", "Variables",
                                  choices = names(data()), selected = NULL)),
      checkboxInput("selectAll", "Select All", value = FALSE)
    )
  })
  
  # Select all variables
  observeEvent(input$selectAll, {
    # Directly manage the selected values based on the state of the 'selectAll' checkbox
    if (input$selectAll) {
      # Select all variables
      updateCheckboxGroupInput(session, "variable", selected = names(data()))
    } else {
      # Deselect all variables
      updateCheckboxGroupInput(session, "variable", selected = character(0))
    }
  })
  
  
  output$type_change_ui <- renderUI({
    current_types <- sapply(input$variable, function(var) class(data()[[var]]))
    
    # Create descriptions for each selected variable
    description_tags <- Map(function(var, type) {
      tags$p(paste(var, "is currently", type, ". Change it to:"))
    }, var = input$variable, type = current_types)
    
    tagList(
      tags$div(style = "overflow-y: scroll; max-height: 200px;", 
               description_tags),
      selectInput("new_type", "Select new data type", 
                  choices = c("numeric", "character", "integer", "logical", "factor", "Date")),
      actionButton("confirm_type_change", "Confirm Change")
    )
  })
  
  # Update UI elements when data is updated
  observe({
    updateSelectInput(session, "variable", selected = input$variable)
  })
  
  
  # Observe event for changing the data type
  observeEvent(input$confirm_type_change, {
    req(input$new_type, data())
    
    # Debugging outputs
    print(paste("Changing type of", input$variable, "to", input$new_type))
    
    updated_data <- data()  
    # Apply the type change
    tryCatch({
      for (var_name in input$variable){
        updated_data[[var_name]] <- switch(input$new_type,
                                           "numeric" = as.numeric(updated_data[[var_name]]),
                                           "character" = as.character(updated_data[[var_name]]),
                                           "integer" = as.integer(updated_data[[var_name]]),
                                           "logical" = as.logical(updated_data[[var_name]]),
                                           "factor" = as.factor(updated_data[[var_name]]),
                                           "Date" = as.Date(updated_data[[var_name]]),
                                           updated_data[[var_name]])  # Default case to avoid alteration if type is not recognized
      }
      
      # Update the reactive variable with the new data
      data(updated_data)
      
      print("Data type change successful.")
      
      # Maintain the selected variables in the inputs after updates
      updateSelectInput(session, "variable", selected = input$variable)
    }) 
  })
  
  ################################################################################
  
  # Distribution plots
  output$distPlot <- renderPlot({
    req(input$selectedVars, input$bins, input$color)  # Ensure necessary inputs are available
    # Check if the variable is numeric or categorical
    if (is.numeric(data()[[input$selectedVars]])) {
      # Plot histogram for numeric data
      ggplot(data = data(), aes_string(x = input$selectedVars)) +
        geom_histogram(binwidth = diff(range(data()[[input$selectedVars]], na.rm = TRUE)) / input$bins, fill = input$color, color = "black") +
        labs(x = input$selectedVars, y = "Frequency", title = "Histogram") +
        theme_minimal()
    } else {
      # Plot bar chart for categorical data
      ggplot(data = data(), aes_string(x = input$selectedVars)) +
        geom_bar(fill = input$color) +
        labs(x = input$selectedVars, y = "Frequency", title = "Bar plot") +
        theme_minimal()
    }
  })
  
  # Explore relationship between two variables
  
  # Render the scatterplot
  output$relPlot <- renderPlot({
    req(input$explanatory, input$response, data())  # Ensure necessary inputs are available
    
    # Determine the type of the variables
    is_numeric_explanatory <- is.numeric(data()[[input$explanatory]])
    is_numeric_response <- is.numeric(data()[[input$response]])
    
    # Start the ggplot object with a general aesthetics setting
    p <- ggplot(data = data(), aes_string(x = input$explanatory, y = input$response))
    
    # Decide on the plot type based on the data types of variables
    if (is_numeric_explanatory && is_numeric_response) {
      # Both variables are numeric: use scatterplot
      p <- p + geom_point(alpha = input$shade) + theme_minimal()
    } else {
      # At least one variable is categorical: use boxplot
      # Adjust the aesthetics to group data if both variables are not numeric
      p <- p + geom_boxplot(aes(group = data()[[input$explanatory]]), alpha = input$shade) + theme_minimal()
    }
    
    # Optionally add marginal histograms if both variables are numeric and requested
    if (is_numeric_explanatory && is_numeric_response && input$marginal) {
      p <- ggMarginal(p, type = "histogram")
    }
    
    p  # Return the plot object
  })
  
  
  ################################################################################
  #conversion
  
  output$var_convert <- renderUI({
    selectInput("variable_to_convert", "Select Your Taget Variable", choices = names(data()))
  })
  
  output$conversion_threshold_ui <- renderUI({
    req(input$variable_to_convert)
    current_var <- data()[[input$variable_to_convert]]
    
    if (is.numeric(current_var)) {
      min_val <- min(current_var, na.rm = TRUE)
      max_val <- max(current_var, na.rm = TRUE)
      mean_val <- mean(current_var, na.rm = TRUE)
      
      tagList(
        tags$p(paste("The variable", input$variable_to_convert, "is numeric. Specify a threshold value (must be within the range of the selected variable) to create binary levels. 
                     Default threshold value is set to the mean.")),
        numericInput("level_threshold", "Define a threshold to divide the class:", min = min_val, max = max_val, value = mean_val),
        textInput("Level1_name", "Class name for values ≤ the threshold (Class 1)"),
        textInput("Level2_name", "Class name for values > the threshold (Class 2) "),
        actionButton("convert_binary", "Convert")
      )
    } else {
      var_levels <- unique(current_var)
      
      if (length(var_levels) == 2) {
        # Check if the variable is already binary
        tagList(
          tags$p(paste("The variable", input$variable_to_convert, "is a factor or character variable with exactly two levels: '", paste(var_levels, collapse = "', '"), "'. Your target feature is
                       already binary and does not need conversion."))
        )
      } else {
        tagList(
          tags$p(paste("The variable", input$variable_to_convert, " contains multiple levels. Select levels to combine")),
          selectInput("levels_to_combine1", "Levels to Combine into New class 1:", choices = var_levels, multiple = TRUE),
          textInput("new_level1_name", "New Class 1 Name "),
          selectInput("levels_to_combine2", "Levels to Combine into New Level 2:", choices = var_levels, multiple = TRUE),
          textInput("new_level2_name", "New Level 2 Name"),
          actionButton("combine_levels", "Combine Levels")
        )
      }
    }
  })
  
  observeEvent(input$convert_binary, {
    data_updated <- data()
    data_updated[[input$variable_to_convert]] <- ifelse(data_updated[[input$variable_to_convert]] <= input$level_threshold, input$Level1_name, input$Level2_name)
    data(data_updated)
    
    # Refresh data table output to reflect changes
    output$level_conversion_output <- renderDataTable({
      if (is.numeric(data_updated[[input$variable_to_convert]])) {
        df <- data.frame(
          Min = min(data_updated[[input$variable_to_convert]], na.rm = TRUE),
          Max = max(data_updated[[input$variable_to_convert]], na.rm = TRUE),
          Median = median(data_updated[[input$variable_to_convert]], na.rm = TRUE),
          Mean = mean(data_updated[[input$variable_to_convert]], na.rm = TRUE),
          SD = sd(data_updated[[input$variable_to_convert]], na.rm = TRUE)
        )
      } else {
        level_data <- table(data_updated[[input$variable_to_convert]])
        df <- data.frame(Level = names(level_data), Count = as.integer(level_data))
      }
      datatable(df, options = list(autoWidth = TRUE))
    })
    # Maintain the selected variables in the inputs after updates
    updateSelectInput(session, "variable_to_convert", selected = input$variable_to_convert)
  })
  
  
  observeEvent(input$combine_levels, {
    data_updated <- data()
    if (is.character(data_updated[[input$variable_to_convert]])) {
      data_updated[[input$variable_to_convert]] <- factor(data_updated[[input$variable_to_convert]])
    }
    levels(data_updated[[input$variable_to_convert]])[levels(data_updated[[input$variable_to_convert]]) %in% input$levels_to_combine1] <- input$new_level1_name
    levels(data_updated[[input$variable_to_convert]])[levels(data_updated[[input$variable_to_convert]]) %in% input$levels_to_combine2] <- input$new_level2_name
    data(data_updated)
    
    # Refresh data table output to reflect changes
    output$level_conversion_output <- renderDataTable({
      level_data <- table(data_updated[[input$variable_to_convert]])
      df <- data.frame(Level = names(level_data), Count = as.integer(level_data))
      datatable(df, options = list(autoWidth = TRUE))
    })
    
    # Maintain the selected variables in the inputs after updates
    updateSelectInput(session, "variable_to_convert", selected = input$variable_to_convert)
    
  })
  
  output$level_conversion_output <- renderDataTable({
    req(input$variable_to_convert)
    current_var <- data()[[input$variable_to_convert]]
    
    if (is.numeric(current_var)) {
      df <- data.frame(
        Min = min(current_var, na.rm = TRUE),
        Max = max(current_var, na.rm = TRUE),
        Median = median(current_var, na.rm = TRUE),
        Mean = mean(current_var, na.rm = TRUE),
        SD = sd(current_var, na.rm = TRUE)
      )
    } else {
      level_data <- table(current_var)
      if (length(level_data) == 0) {
        df <- data.frame(Level = character(0), Count = numeric(0))
      } else {
        df <- data.frame(Level = names(level_data), Count = as.integer(level_data))
      }
    }
    datatable(df, options = list(autoWidth = TRUE))
  })
  
  
  
  
  #################################################################################
  
  # Data previews and summaries
  output$data_preview <- renderDataTable({
    req(data())
    datatable(data())
  })
  
  na_stats <- reactive({
    req(data())  # Ensure that the dataset is loaded
    df <- data()
    
    # Calculate the count of missing values per column
    na_counts <- sapply(df, function(x) sum(is.na(x)))
    # Create a data frame to store columns with NAs and their counts
    na_data <- data.frame(
      `Column Name` = names(na_counts),
      `NA Count` = na_counts,
      stringsAsFactors = FALSE
    )
    na_data
  })
  
  # Render the data table showing columns with NAs
  output$na_info_table <- renderDT({
    datatable(na_stats(), options = list(pageLength = 5, autoWidth = TRUE), rownames = FALSE)
  })
  zero_variance_stats <- reactive({
    req(data())  # Ensure that the dataset is loaded
    df <- data()
    
    # Calculate variance for each column and identify those with zero variance
    variances <- sapply(df, function(x) if(is.numeric(x)) var(x, na.rm = TRUE) else NA)
    zero_var_cols <- names(variances)[variances == 0 & !is.na(variances)]
    
    # Prepare a response based on the presence of zero variance columns
    if (length(zero_var_cols) > 0) {
      return(data.frame(`Column Name` = zero_var_cols, stringsAsFactors = FALSE))
    } else {
      return(NULL)
    }
  })
  
  # Render output for zero variance statistics
  output$zero_variance_output <- renderUI({
    zero_var_data <- zero_variance_stats()
    if (!is.null(zero_var_data)) {
      # Show a table of columns with zero variance
      dataTableOutput("zero_variance_table")
    } else {
      # Display a message if there are no columns with zero variance
      HTML("There's no column with zero variance.")
    }
  })
  
  # If there are zero variance columns, render them in a DataTable
  output$zero_variance_table <- renderDataTable({
    datatable(zero_variance_stats(), options = list(pageLength = 5, autoWidth = TRUE), rownames = FALSE)
  })
  
  
  
  ################################################################################
  # Render UI to select variable to drop
  output$drop_var_select <- renderUI({
    req(data())  # Only display if data is available
    selectInput("vars_to_drop", "Select Columns to Drop:",
                choices = names(data()), multiple = TRUE)
  })
  
  # Observe event for dropping columns
  observeEvent(input$drop_vars, {
    req(data())  # Ensure data is available
    # Prevent dropping all columns accidentally
    if (!is.null(input$vars_to_drop) && length(input$vars_to_drop) < length(names(data()))) {
      data_updated <- data()[, -which(names(data()) %in% input$vars_to_drop), drop = FALSE]
      data(data_updated)  # Update the data reactive value
    }
  })
  
  # Data preview output
  output$dropped_data_preview <- renderDataTable({
    req(data())
    datatable(data(), options = list(scrollX = TRUE))
  })
  
  # Update other UI elements that depend on data columns
  observe({
    updateSelectInput(session, "drop_var_select", choices = names(data()))
  })
  
  
  ################################################################################
  #preprocessing
  data_preprocess <- reactiveVal()
  target_feature_name <- reactiveVal()
  # Update dropdown for selecting target feature dynamically
  observe({
    updateSelectInput(session, "target_feature", choices = names(data()))
  })
  
  observeEvent(input$confirm_target, {
    # Store the target feature name upon confirmation
    target_feature_name(input$target_feature)
    #showNotification("Target feature set to: " + input$target_feature, type = "success")
  })
  
  # observeEvent(input$finish_preprocessing, {
  #   
  #   
  #   # Validate train/test split input
  #   if (input$train_test_split < 50 || input$train_test_split > 90) {
  #     showNotification("Train/Test split percentage should be between 50 and 90.", type = "error")
  #     return()
  #   }
  #   preprocess_note <- showNotification("Preprocessing started, please wait...", type = "message", duration = NULL)
  #   # Access the stored target feature name
  #   req(target_feature_name())  # Ensure target feature is set
  #   
  #   df <- data()  # Make a copy of the data for processing
  #   
  #   # Ensure the target feature is a factor if not already
  #   if (!is.factor(df[[target_feature_name()]])) {
  #     df[[target_feature_name()]] <- as.factor(df[[target_feature_name()]])
  #   }
  #   if (input$na_handling == "Drop NAs") {
  #     df <- df %>% select(where(~ !any(is.na(.))))
  #   }
  #   
  #   # Create the recipe
  #   blueprint <- recipe(reformulate(termlabels = ".", response = target_feature_name()), data = df) %>%
  #     step_string2factor(all_nominal_predictors()) %>%
  #     step_nzv(all_predictors())
  #   if (input$na_handling == "Impute with KNN") {
  #     blueprint <- blueprint %>%
  #       step_impute_knn(all_predictors(), neighbors = input$k_neighbors)
  #   }
  #   blueprint <- blueprint %>%
  #     step_center(all_numeric_predictors()) %>%
  #     step_scale(all_numeric_predictors()) %>%
  #     step_pca(all_numeric_predictors(), threshold = 0.95) %>%
  #     step_dummy(all_nominal(), -all_of(target_feature_name()), one_hot = TRUE)
  #   
  #   # Prepare the recipe by estimating transformation parameters from the training data
  #   set.seed(123)  # For reproducible results
  #   training_rows <- createDataPartition(df[[target_feature_name()]], p = input$train_test_split / 100, list = FALSE)
  #   train_set <- df[training_rows, ]
  #   test_set <- df[-training_rows, ]
  #   
  #   blueprint_prep <- prep(blueprint, training = train_set, retain = TRUE)
  #   
  #   # Transform the training data
  #   transformed_train <- bake(blueprint_prep, new_data = train_set)
  #   
  #   # Transform the testing data
  #   transformed_test <- bake(blueprint_prep, new_data = test_set)
  #   # Save the processed datasets
  #   data_preprocess(list(train = transformed_train, test = transformed_test))
  #   removeNotification(preprocess_note)  # Remove the notification
  #   showNotification("Preprocessing complete.", type = "message", duration = 5)
  # })
  
#   # Render the processed train dataset
#   output$transformed_train <- renderDataTable({
#     req(data_preprocess())
#     datatable(data_preprocess()[["train"]])
#   })
#   
#   # Render the processed test dataset
#   output$transformed_test <- renderDataTable({
#     req(data_preprocess())
#     datatable(data_preprocess()[["test"]])
#   })
#   
#   ################################################################################
#   #training and predicting
#   trainedModel <- reactiveVal()  # Store the trained model
#   # Helper function to convert comma-separated string to numeric vector
#   parse_input <- function(input_str) {
#     as.numeric(unlist(strsplit(input_str, ",")))
#   }
#   
#   # Validation functions for each model type
#   validate_rf <- function(mtry, min_node_size, num_vars) {
#     error_msg <- NULL
#     if (!all(is.numeric(mtry)) || !all(is.numeric(min_node_size))) {
#       error_msg <- "All inputs must be numeric values."
#     } else {
#       if (any(is.na(mtry)) || any(mtry <= 0) || any(mtry > num_vars)) {
#         error_msg <- paste0("Error in mtry: Values must be positive integers and <= ", num_vars, ".")
#       }
#       if (any(is.na(min_node_size)) || any(min_node_size < 1)) {
#         error_msg <- paste0(ifelse(is.null(error_msg), "", paste0(error_msg, "\n")), "Error in min.node.size: Values must be >= 1.")
#       }
#     }
#     
#     error_msg
#   }
#   
#   validate_svm <- function(C) {
#     if (!all(is.numeric(C))) {
#       return("All inputs must be numeric values.")
#     }
#     if (any(is.na(C)) || any(C <= 0)) {
#       return("Error in C: Values must be positive.")
#     }
#     NULL
#   }
#   
#   validate_svm_poly <- function(C, degree, scale) {
#     messages <- c()
#     if (!all(is.numeric(c(C, degree, scale)))) {
#       messages <- c(messages, "All inputs must be numeric values.")
#     }
#     if (any(is.na(C)) || any(C <= 0)) {
#       messages <- c(messages, "Error in C: Values must be positive.")
#     }
#     if (any(is.na(degree)) || any(degree < 1)) {
#       messages <- c(messages, "Error in degree: Values must be positive integers.")
#     }
#     if (any(is.na(scale)) || any(scale <= 0)) {
#       messages <- c(messages, "Error in scale: Values must be positive.")
#     }
#     if (length(messages) > 0) return(paste(messages, collapse = "\n"))
#     NULL
#   }
#   
#   validate_svm_rbf <- function(C, sigma) {
#     messages <- c()
#     if (!all(is.numeric(c(C, sigma)))) {
#       messages <- c(messages, "All inputs must be numeric values.")
#     }
#     if (any(is.na(C)) || any(C <= 0)) {
#       messages <- c(messages, "Error in C: Values must be positive.")
#     }
#     if (any(is.na(sigma)) || any(sigma <= 0)) {
#       messages <- c(messages, "Error in sigma: Values must be positive.")
#     }
#     if (length(messages) > 0) return(paste(messages, collapse = "\n"))
#     NULL
#   }
#   
#   # Validation function for XGBoost
#   validate_xgboost <- function(nrounds, max_depth, eta, min_child_weight) {
#     messages <- c()
#     if (!all(is.numeric(c(nrounds, max_depth, eta, min_child_weight)))) {
#       messages <- c(messages, "All inputs must be numeric values.")
#     }
#     if (any(is.na(nrounds)) || any(nrounds < 1)) {
#       messages <- c(messages, "Error in Number of Rounds: Values must be positive integers.")
#     }
#     if (any(is.na(max_depth)) || any(max_depth < 1)) {
#       messages <- c(messages, "Error in Max Depth: Values must be positive integers.")
#     }
#     if (any(is.na(eta)) || any(eta <= 0)) {
#       messages <- c(messages, "Error in Learning Rate (eta): Values must be positive.")
#     }
#     if (any(is.na(min_child_weight)) || any(min_child_weight < 1)) {
#       messages <- c(messages, "Error in Min Child Weight: Values must be positive integers.")
#     }
#     if (length(messages) > 0) return(paste(messages, collapse = "\n"))
#     NULL
#   }
#   
#   observeEvent(input$trainModel, {
#     
#     req(data_preprocess())  # Ensure the data is preprocessed and available
#     
#     num_vars <- ncol(data_preprocess()[["train"]])  # Assuming preprocessing stores the train dataset and has been handled properly
#     error_message <- NULL
#     
#     if (input$modelType == "Random Forest") {
#       mtry_in <- parse_input(input$mtry)
#       min_node_size <- parse_input(input$min.node.size)
#       error_message <- validate_rf(mtry_in, min_node_size, num_vars)
#     } else if (input$modelType == "SVM Linear") {
#       C_linear <- parse_input(input$C_linear)
#       error_message <- validate_svm(C_linear)
#     } else if (input$modelType == "SVM Polynomial") {
#       C_poly <- parse_input(input$C_poly)
#       degree_in <- parse_input(input$degree)
#       scale_poly <- parse_input(input$scale_poly)
#       error_message <- validate_svm_poly(C_poly, degree_in, scale_poly)
#     } else if (input$modelType == "SVM RBF") {
#       C_rbf <- parse_input(input$C_rbf)
#       sigma_in <- parse_input(input$sigma)
#       error_message <- validate_svm_rbf(C_rbf, sigma_in)
#     } else if (input$modelType == "XGBoost") {
#       nrounds <- parse_input(input$nrounds)
#       max_depth <- parse_input(input$max_depth)
#       eta <- parse_input(input$eta)
#       min_child_weight <- parse_input(input$min_child_weight)
#       error_message <- validate_xgboost(nrounds, max_depth, eta, min_child_weight)
#     }
#     
#     # Show error message if validation failed
#     if (!is.null(error_message)) {
#       showNotification(error_message, type = "error")
#       return()
#     }
#     trainingNote <- showNotification("Training the model, please wait...", type = "message", duration = NULL)
#     output$trainingStatus <- renderText("Starting model training... Please wait.")
#     
#     # Setup for cross-validation
#     train_control <- trainControl(
#       method = "cv",
#       number = 5,
#       savePredictions = "final",
#       classProbs = TRUE,
#       summaryFunction = twoClassSummary
#     )
#     
#     # Hyperparameters
#     if (input$modelType == "Random Forest") {
#       hyper_grid_rf <- expand.grid(
#         mtry = mtry_in,  # Preselected optimal value
#         splitrule = input$splitrule,  # Preselected optimal criterion
#         min.node.size = min_node_size  # Preselected optimal node size
#       )
#     } else if (input$modelType == "SVM Linear") {
#       hyper_grid_linear <- expand.grid(
#         C = C_linear
#       )
#     } else if (input$modelType == "SVM Polynomial") {
#       hyper_grid_poly <- expand.grid(
#         C = C_poly,
#         degree = degree_in,
#         scale = scale_poly
#       )
#     } else if (input$modelType == "SVM RBF") {
#       hyper_grid_rbf <- expand.grid(
#         C = C_rbf,
#         sigma = sigma_in
#       ) 
#     } else if (input$modelType == "XGBoost") {
#       hyper_grid_xg <-expand.grid(
#         nrounds = nrounds,
#         max_depth = max_depth,
#         eta = eta,    
#         min_child_weight = min_child_weight, 
#         subsample = c(0.4, 0.6), 
#         gamma = 0,
#         colsample_bytree = 1)
#     }
#     
#     
#     
#     
#     if (input$modelType == "Random Forest") {
#       output$trainingStatus <- renderText("Training Random Forest...")
#       trainedModel(train(
#         reformulate(termlabels = ".", response = target_feature_name()),
#         data = data_preprocess()[["train"]],
#         method = "ranger",
#         trControl = train_control,
#         tuneGrid = hyper_grid_rf,
#         metric = "ROC",
#         importance = "impurity"
#       ))
#     } else if (input$modelType == "SVM Linear") {
#       output$trainingStatus <- renderText("Training SVM Linear...")
#       trainedModel(train(
#         reformulate(termlabels = ".", response = target_feature_name()),
#         data = data_preprocess()[["train"]],
#         method = "svmLinear",
#         trControl = train_control,
#         preProcess = "scale",
#         tuneGrid = hyper_grid_linear,
#         metric = "ROC"
#       ))
#     } else if (input$modelType == "SVM Polynomial") {
#       output$trainingStatus <- renderText("Training SVM Polynomial...")
#       trainedModel(train(
#         reformulate(termlabels = ".", response = target_feature_name()),
#         data = data_preprocess()[["train"]],
#         method = "svmPoly",
#         trControl = train_control,
#         tuneGrid = hyper_grid_poly,
#         metric = "ROC"
#       ))
#     } else if (input$modelType == "SVM RBF") {
#       output$trainingStatus <- renderText("Training SVM RBF...")
#       trainedModel(train(
#         reformulate(termlabels = ".", response = target_feature_name()),
#         data = data_preprocess()[["train"]],
#         method = "svmRadial",
#         trControl = train_control,
#         tuneGrid = hyper_grid_rbf,
#         metric = "ROC"
#       ))
#     } else if (input$modelType == "XGBoost") {
#       output$trainingStatus <- renderText("Training XGBoost...")
#       trainedModel(train(
#         reformulate(termlabels = ".", response = target_feature_name()),
#         data = data_preprocess()[["train"]],
#         method = "xgbTree",
#         verbose = FALSE,
#         trControl = train_control,
#         tuneGrid = hyper_grid_xg,
#         metric = "ROC"
#       ))
#     }
#     removeNotification(trainingNote)  # Remove the notification
#     showNotification("Model training complete.", type = "message", duration = 5)
#     # After training
#     output$trainingStatus <- renderText({paste(input$modelType, "training complete.")})
#     
#     # Generate predictions and compute confusion matrices
#     train_predictions <- predict(trainedModel(), newdata = data_preprocess()[["train"]])
#     test_predictions <- predict(trainedModel(), newdata = data_preprocess()[["test"]])
#     
#     train_conf_matrix <- confusionMatrix(train_predictions, data_preprocess()[["train"]][[target_feature_name()]])
#     test_conf_matrix <- confusionMatrix(test_predictions, data_preprocess()[["test"]][[target_feature_name()]])
#     
#     # Output results
#     output$modelSummary <- renderPrint({
#       trainedModel()
#     })
#     
#     # Replace the existing ROC plot rendering logic
#     output$rocPlot <- renderPlot({
#       if (input$modelType == "SVM Linear") {
#         showNotification("Plotting is not supported for this model.", type = "warning")
#         return(NULL)  # Return NULL to prevent further plotting actions
#       }
#       req(trainedModel())  # Ensure the model is trained and available
#       
#       
#       plot(trainedModel(), metric = "ROC", plotType = "level")
#     })
#     
#     output$featureImportancePlot <- renderPlot({
#       req(trainedModel())  # Ensure the model is available
#       
#       # Check if the model supports feature importance extraction
#       if (input$modelType == "XGBoost") {
#         vip(trainedModel())
#       } else if (input$modelType == "Random Forest") {
#         vip(trainedModel())
#       } else if (input$modelType == "SVM Linear"| input$modelType == "SVM Polynomial" | 
#                  input$modelType == "SVM RBF") {
#         
#         
#         # Function to determine the reference class from the data
#         get_level_names <- function(data, target_feature_name) {
#           levels(data()[[target_feature_name()]])
#         }
#         
#         # Wrapper function to predict probabilities for the first class
#         prob_first_class <- function(object, newdata) {
#           predicted_probs <- predict(object, newdata = newdata, type = "prob")
#           predicted_probs[, 1]  # Extract probabilities of the first class
#         }
#       
#         #vip for SVM
#         vip(trainedModel(), method = "permute", train = data_preprocess()[["test"]], target = target_feature_name(),
#             metric = "roc_auc", reference_class = get_level_names[,1], pred_wrapper = prob_first_class)
#         
#       } else {
#         plot(NULL)  # If the model type does not support feature importance, plot nothing
#         title("Feature importance not available for this model type")
#       }
#     })
#     
#     output$trainConfMatrix <- renderPrint({ train_conf_matrix })
#     output$testConfMatrix <- renderPrint({ test_conf_matrix })
#   })
 }

# Run the application 
shinyApp(ui = ui, server = server)