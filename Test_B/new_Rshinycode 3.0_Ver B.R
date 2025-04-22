library(shiny)
library(shinythemes)
library(tidyverse)
library(ggExtra)
library(data.table)
library(DT)
library(shinyWidgets)
library(ggplot2)



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
             HTML("<strong>Welcome to the STAT 5243 Project2 Application! </strong>"),
             tags$div(
               style = "margin: 30px 0;",
               # Visual workflow using HTML/CSS - Now horizontal (left to right)
               tags$div(
                 style = "display: flex; flex-direction: row; justify-content: space-between; align-items: center; max-width: 1000px; margin: 0 auto; overflow-x: auto; padding: 20px 0;",
                 
                 # Step 1
                 tags$div(
                   style = "display: flex; flex-direction: column; align-items: center;",
                   tags$div(
                     style = "background-color: #3498db; color: white; padding: 15px; border-radius: 10px; width: 200px; text-align: center; margin-bottom: 10px;",
                     tags$h4("1. Data Selection"),
                     tags$p(style = "font-size: 0.9em;", "Upload your dataset or use a built-in example")
                   )
                 ),
                 
                 # Arrow 1
                 tags$div(
                   style = "width: 40px; display: flex; justify-content: center; align-items: center;",
                   HTML('<i class="fa fa-arrow-right" style="font-size: 24px; color: #666;"></i>')
                 ),
                 
                 # Step 2
                 tags$div(
                   style = "display: flex; flex-direction: column; align-items: center;",
                   tags$div(
                     style = "background-color: #2ecc71; color: white; padding: 15px; border-radius: 10px; width: 200px; text-align: center; margin-bottom: 10px;",
                     tags$h4("2. Cleaning & Preprocessing"),
                     tags$p(style = "font-size: 0.9em;", "Handle missing values, drop columns, convert data types")
                   )
                 ),
                 
                 # Arrow 2
                 tags$div(
                   style = "width: 40px; display: flex; justify-content: center; align-items: center;",
                   HTML('<i class="fa fa-arrow-right" style="font-size: 24px; color: #666;"></i>')
                 ),
                 
                 # Step 3
                 tags$div(
                   style = "display: flex; flex-direction: column; align-items: center;",
                   tags$div(
                     style = "background-color: #f39c12; color: white; padding: 15px; border-radius: 10px; width: 200px; text-align: center; margin-bottom: 10px;",
                     tags$h4("3. Feature Engineering"),
                     tags$p(style = "font-size: 0.9em;", "Create new features through transformations or interactions")
                   )
                 ),
                 
                 # Arrow 3
                 tags$div(
                   style = "width: 40px; display: flex; justify-content: center; align-items: center;",
                   HTML('<i class="fa fa-arrow-right" style="font-size: 24px; color: #666;"></i>')
                 ),
                 
                 # Step 4
                 tags$div(
                   style = "display: flex; flex-direction: column; align-items: center;",
                   tags$div(
                     style = "background-color: #e74c3c; color: white; padding: 15px; border-radius: 10px; width: 200px; text-align: center; margin-bottom: 10px;",
                     tags$h4("4. Exploratory Data Analysis"),
                     tags$p(style = "font-size: 0.9em;", "Visualize distributions and explore relationships")
                   )
                 )
               )
             ),
             tags$br(),
             tags$h3("Additional Information:"),
             tags$ul(
               tags$li("Use the navigation tabs at the top to move between different stages"),
               tags$li("Each tab contains specific tools for that stage of the data analysis process"),
               tags$li("You can return to any stage at any time to make adjustments")
             ),
             tags$br()
    ),
    
    tabPanel("Data Selection", icon = icon("folder-open"),
             titlePanel("Upload Data"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset", "Dataset:", choices = c("Car Seats", "Upload your own file")),
                 conditionalPanel(
                   condition = "input.dataset == 'Upload your own file'",
                   fileInput("file", "Select your files (Max size: 5MB):", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                   textInput("na_string", "Enter the NA string (optional):", value = ""),
                   actionButton("confirm_na", "Get the preview", class = "btn-primary")
                 )
               ),
               mainPanel(
                 tags$div(
                   style = "height: 600px; overflow-y: auto; overflow-x: auto;",
                   dataTableOutput("data_preview")
                 )
               )
             )
    ),
    
    tabPanel("Cleaning & Preprocessing", icon = icon("broom"),
             titlePanel("Clean and Preprocess Your Data"),
             tabsetPanel(id = "clean_preprocess_tabs", 
               tabPanel("Cleaning",
                        # --- Nested navlistPanel for Cleaning sub-functions ---
                        navlistPanel(id = "cleaning_nav", widths = c(3, 9),
                          tabPanel("Check NAs",
                                   h4("Check columns with NAs"),
                                   DTOutput("na_info_table")
                          ),
                          tabPanel("Check Duplicates",
                                   h4("Identify and Handle Duplicate Rows"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       radioButtons("dup_handling", "Handle Duplicates:",
                                                    choices = c("Keep all (no action)" = "keep_all",
                                                                "Keep first occurrence" = "keep_first",
                                                                "Remove all duplicates" = "remove_all"),
                                                    selected = "keep_all"),
                                       actionButton("apply_dup_handling", "Apply Handling", class = "btn-primary")
                                     ),
                                     mainPanel(
                                       DTOutput("dup_info_table"),
                                       tags$br(),
                                       verbatimTextOutput("dup_handling_message")
                                     )
                                   )
                          ),
                          tabPanel("Zero Variance",
                                   h4("Zero Variance Columns"),
                                   uiOutput("zero_variance_output")
                          ),
                          tabPanel("Drop Columns",
                                   h4("Choose any columns to drop"),
                                   uiOutput("drop_var_select"), 
                                   actionButton("drop_vars", "Drop Selected Columns", class = "btn-primary"),  
                                   dataTableOutput("dropped_data_preview") 
                           ),
                           # --- Ensure Finish Cleaning is the last tabPanel within the navlistPanel ---
                           tabPanel("Finish Cleaning", 
                                    tags$p("Click to confirm data cleaning."),
                                    actionButton("finish_cleaning", "Finish Cleaning", class = "btn-success", style="width:50%; margin-top: 20px;") 
                           )
                        ) # <<< This parenthesis closes navlistPanel(id = "cleaning_nav", ...)
               ),
               tabPanel("Preprocessing",
                        # --- Nested navlistPanel for Preprocessing sub-functions ---
                        navlistPanel(id = "preprocessing_nav", widths = c(3, 9),
                          tabPanel("Binary Level Conversion",
                                   h4("Binary Level Conversion Tool"),
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
                          tabPanel("Handling NAs",
                                   h4("Handling NAs & Target Feature Selection"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("target_feature", "Select Target Feature:", choices = NULL),
                                       actionButton("confirm_target", "Confirm Target Feature", class = "btn-primary"),
                                       tags$hr(),
                                       radioButtons("na_handling", "Handle Missing Values:", choices = c("There's no missing value.", "Drop NAs", "Impute with KNN")),
                                       conditionalPanel(
                                         condition = "input.na_handling == 'Impute with KNN'",
                                         numericInput("k_neighbors", "Number of Neighbors:", value = 5, min = 1)
                                       )
                                     ),
                                     mainPanel(
                                        verbatimTextOutput("preprocessing_message") # Output message area
                                     )
                                   )
                          ),
                           # --- Add Finish Preprocessing as a navlist item ---
                          tabPanel("Finish Preprocessing",
                                   tags$p("Click to confirm data preprocessing."),
                                   actionButton("finish_preprocessing", "Finish Preprocessing", class = "btn-success", style="width:50%; margin-top: 20px;")
                          )
                        ) # Closing parenthesis for preprocessing_nav navlistPanel
                        # --- Finish Preprocessing Button REMOVED from here ---
               )
             )
    ),
    
    tabPanel("Feature Engineering", icon = icon("cogs"),
             titlePanel("Feature Engineering"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("feature_operation", "Select Operation:", 
                            choices = c("Create Interaction Term" = "interaction",
                                        "Polynomial Features" = "polynomial",
                                        "Log Transformation" = "log",
                                        "Standardize/Normalize" = "standardize")),
                 
                 # UI for interaction terms
                 conditionalPanel(
                   condition = "input.feature_operation == 'interaction'",
                   selectInput("interaction_var1", "First Variable:", choices = NULL),
                   selectInput("interaction_var2", "Second Variable:", choices = NULL),
                   textInput("interaction_name", "New Feature Name:", value = "interaction_term"),
                   actionButton("create_interaction", "Create Interaction", class = "btn-primary")
                 ),
                 
                 # UI for polynomial features
                 conditionalPanel(
                   condition = "input.feature_operation == 'polynomial'",
                   selectInput("poly_var", "Variable to Transform:", choices = NULL),
                   numericInput("poly_degree", "Polynomial Degree:", value = 2, min = 2, max = 5),
                   textInput("poly_prefix", "Prefix for New Features:", value = "poly_"),
                   actionButton("create_poly", "Create Polynomial Features", class = "btn-primary")
                 ),
                 
                 # UI for log transformation
                 conditionalPanel(
                   condition = "input.feature_operation == 'log'",
                   selectInput("log_var", "Variable to Transform:", choices = NULL),
                   radioButtons("log_type", "Transformation Type:", 
                               choices = c("Natural Log (ln)" = "natural", 
                                          "Log Base 10" = "log10")),
                   textInput("log_name", "New Feature Name:", value = "log_transformed"),
                   actionButton("create_log", "Create Log Transform", class = "btn-primary")
                 ),
                 
                 # UI for standardization
                 conditionalPanel(
                   condition = "input.feature_operation == 'standardize'",
                   selectInput("std_vars", "Variables to Transform:", choices = NULL, multiple = TRUE),
                   radioButtons("std_method", "Method:", 
                               choices = c("Z-score standardization" = "zscore", 
                                          "Min-Max Normalization" = "minmax")),
                   textInput("std_prefix", "Prefix for New Features:", value = "std_"),
                   actionButton("create_std", "Standardize/Normalize", class = "btn-primary")
                 )
               ),
               mainPanel(
                 tags$div(
                   style = "margin-bottom: 20px;",
                   h4("Created Features"),
                   p("New features will appear in the table below:")
                 ),
                 DTOutput("engineered_features_table"),
                 tags$div(
                   style = "margin-top: 20px;",
                   verbatimTextOutput("feature_eng_message")
                 )
               )
             )
    ),
    
    tabPanel("Exploratory Data Analysis", icon = icon("chart-simple"),
             titlePanel("Explore Your Data"),
             tabsetPanel(
               tabPanel("Data Summary",
                        tags$div(
                          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                          h4("Dataset Overview"),
                          p("This table shows summary statistics for all variables in your dataset.")
                        ),
                        tags$div(
                          style = "height: 600px; overflow-y: auto; background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0px 0px 10px rgba(0,0,0,0.1);",
                          verbatimTextOutput("summaryOutput_all")
                        )
               ),
               tabPanel("Variable Type",
                        fluidRow(
                          column(8, 
                                 tags$div(
                                   style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h3("Current Data Types"),
                                   HTML("Verify the current type of all variables in your data and modify as needed. Note that incorrect data type can lead to errors in modeling. 
                                         For example: Check if your target feature is a factor")
                                 ),
                                 tags$div(
                                   style = "height: 400px; overflow-y: auto; background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0px 0px 10px rgba(0,0,0,0.1);",
                                   DTOutput("data_types_table")
                                 )
                          ),
                          column(4,
                                 tags$div(
                                   style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                                   h3("Change the Data Type of:"),
                                   uiOutput("var_select"),
                                   tags$br(),
                                   uiOutput("type_change_ui")
                                 )
                          )
                        )
               ),
               tabPanel("Variable Distribution",
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons("var_type_filter", "Filter by variable type:", 
                                         choices = c("All" = "all", "Numeric" = "numeric", "Categorical" = "categorical")),
                            selectInput("selectedVars", "Select Variables:", choices = NULL),
                            
                            # New dynamic filter controls
                            uiOutput("filter_controls"),
                            
                            tags$hr(),
                            
                            tags$h3("Plotting the selected variables"),
                            selectInput("plot_type", "Select Plot Type:", 
                                       choices = c("Histogram" = "histogram", 
                                                  "Box Plot" = "boxplot", 
                                                  "Pie Chart" = "pie")),
                            conditionalPanel(
                              condition = "input.plot_type == 'histogram'",
                              numericInput("bins", "Number of bins:", min = 1, max = 50, step = 1, value = 10),
                              radioButtons("color", "Color of bins:",
                                           choices = list("Blue" = "skyblue", "Pink" = "pink", "Orange" = "orange"),
                                           selected = "skyblue")
                            )
                          ),
                          mainPanel(
                            fluidRow(
                              column(6,
                                     tags$div(
                                       style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0px 0px 10px rgba(0,0,0,0.1);",
                                       verbatimTextOutput("summaryOutput")
                                     )),
                              column(6,
                                     tags$div(
                                       style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0px 0px 10px rgba(0,0,0,0.1);",
                                       plotOutput("distPlot")
                                     ))
                            )
                          )
                        )
               ),
               tabPanel("Explore Correlations",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("plot_correlation_type", "Select Plot Type:", 
                                        choices = c("Scatter Plot" = "scatter", 
                                                   "Heatmap" = "heatmap")),
                            conditionalPanel(
                              condition = "input.plot_correlation_type == 'scatter'",
                              selectInput("response", "Response Variable (Y):", choices = NULL), 
                              selectInput("explanatory", "Explanatory Variable (X):", choices = NULL)
                            ),
                            conditionalPanel(
                              condition = "input.plot_correlation_type == 'heatmap'",
                              checkboxGroupInput("heatmap_vars", "Select variables for heatmap:", choices = NULL),
                              actionButton("select_all_heatmap", "Select All", class = "btn-sm btn-primary"),
                              actionButton("deselect_all_heatmap", "Deselect All", class = "btn-sm btn-secondary")
                            ),
                            conditionalPanel(
                              condition = "input.plot_correlation_type == 'scatter'",
                              sliderInput("shade", "Transparency Rate:", min = 0, max = 1, value = 0.5, step = 0.1),
                              checkboxInput("marginal", "Show Marginal Distributions", value = FALSE)
                            ),
                            
                            # New dynamic filter controls for correlation
                            tags$hr(),
                            uiOutput("correlation_dynamic_filter_ui")
                          ),
                          mainPanel(
                            tags$div(
                              style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0px 0px 10px rgba(0,0,0,0.1);",
                              plotOutput("relPlot", height = "500px")
                            )
                          )
                        )
               )
             )
    )
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
  output$data_types_table <- renderDT({
    req(data())
    df_types <- data.frame(
      Variable = names(data()),
      Type = sapply(data(), class),
      stringsAsFactors = FALSE
    )
    datatable(df_types, options = list(pageLength = 10, autoWidth = TRUE))
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
    req(input$selectedVars, input$plot_type)
    
    # Use filtered data if available, otherwise use original data
    plot_data <- if (!is.null(filtered_data())) filtered_data() else data()
    
    var_to_plot <- input$selectedVars
    
    if (input$plot_type == "histogram") {
      if (is.numeric(plot_data[[var_to_plot]])) {
        ggplot(data = plot_data, aes_string(x = var_to_plot)) +
          geom_histogram(binwidth = diff(range(plot_data[[var_to_plot]], na.rm = TRUE)) / input$bins, 
                         fill = input$color, color = "black") +
          labs(x = var_to_plot, y = "Frequency", title = paste("Histogram of", var_to_plot)) +
          theme_minimal()
      } else {
        ggplot(data = plot_data, aes_string(x = var_to_plot)) +
          geom_bar(fill = input$color) +
          labs(x = var_to_plot, y = "Frequency", title = paste("Bar plot of", var_to_plot)) +
          theme_minimal()
      }
    } else if (input$plot_type == "boxplot") {
      if (is.numeric(plot_data[[var_to_plot]])) {
        ggplot(data = plot_data, aes_string(y = var_to_plot)) +
          geom_boxplot(fill = input$color) +
          labs(y = var_to_plot, title = paste("Box plot of", var_to_plot)) +
          theme_minimal()
      } else {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Boxplot requires numeric data") +
          theme_void()
      }
    } else if (input$plot_type == "pie") {
      if (!is.numeric(plot_data[[var_to_plot]]) || is.factor(plot_data[[var_to_plot]])) {
        # For categorical variables
        count_data <- as.data.frame(table(plot_data[[var_to_plot]]))
        names(count_data) <- c("Category", "Count")
        
        ggplot(count_data, aes(x = "", y = Count, fill = Category)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          labs(title = paste("Pie chart of", var_to_plot)) +
          theme_minimal() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                axis.ticks = element_blank())
      } else {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Pie chart requires categorical data") +
          theme_void()
      }
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
  # Duplicate Detection and Handling
  dup_stats <- reactive({
    req(data())
    df <- data()
    
    # Identify all duplicate rows (including first occurrences)
    dup_indices <- duplicated(df) | duplicated(df, fromLast = TRUE)
    # Count only the rows that are not the first occurrence (removable duplicates)
    dup_rows <- sum(duplicated(df))
    total_rows <- nrow(df)
    unique_rows <- total_rows - dup_rows
    
    # Create summary table
    dup_summary <- data.frame(
      "Metric" = c("Total Rows", "Duplicate Rows", "Unique Rows"),
      "Count" = c(total_rows, dup_rows, unique_rows),
      stringsAsFactors = FALSE
    )
    
    dup_summary
  })
  
  output$dup_info_table <- renderDT({
    datatable(dup_stats(), options = list(pageLength = 5, autoWidth = TRUE, searching = FALSE, paging = FALSE), rownames = FALSE)
  })
  
  observeEvent(input$apply_dup_handling, {
    req(data())
    df <- data()
    
    if (input$dup_handling == "keep_first") {
      df <- df[!duplicated(df), , drop = FALSE]
      data(df)
      output$dup_handling_message <- renderText({
        "Duplicates handled: Kept only the first occurrence of each duplicate row."
      })
    } else if (input$dup_handling == "remove_all") {
      df <- df[!duplicated(df) & !duplicated(df, fromLast = TRUE), , drop = FALSE]
      data(df)
      output$dup_handling_message <- renderText({
        "Duplicates handled: Removed all duplicate rows entirely."
      })
    } else {
      output$dup_handling_message <- renderText({
        "No action taken: All rows, including duplicates, are retained."
      })
    }
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
    updateSelectInput(session, "drop_var_select", choices = names(data()), selected=input$vars_to_drop) # Keep selection if possible
  })
  
  
  ################################################################################
  #preprocessing (Now mainly NA handling and target selection)
  data_preprocess <- reactiveVal() # Potentially use this later if separating data states
  target_feature_name <- reactiveVal()
  
  # Update dropdown for selecting target feature dynamically
  observe({
     req(data()) # Depends on the main data
     updateSelectInput(session, "target_feature", choices = names(data()), selected = target_feature_name()) # Keep selection
  })
  
  observeEvent(input$confirm_target, {
    req(input$target_feature)
    target_feature_name(input$target_feature)
    showNotification(paste("Target feature set to:", input$target_feature), type = "message")
     output$preprocessing_message <- renderText({
       paste("Target feature confirmed:", input$target_feature)
     })
  })

  # --- NA Handling Logic ---
  # Note: The original 'finish_preprocessing' button seemed to be intended to trigger NA handling.
  # Now, NA handling needs to be triggered explicitly, perhaps with a dedicated button,
  # or implicitly when 'Finish Preprocessing' is clicked (as partially added above).
  # Let's add placeholder logic triggered by the Finish button for now.

  # (Add observeEvent for KNN imputation or Drop NAs if needed, 
  # potentially triggered by a new dedicated button within the "Handling NAs" section,
  # or integrate into the 'finish_preprocessing' event)

  # Placeholder for messages in the preprocessing main panel
  output$preprocessing_message <- renderText({
    "Select target feature and NA handling method."
  })

  # --- End NA Handling Logic ---

  # Filtered data for variable distribution
  filtered_data <- reactiveVal()
  
  observeEvent(input$apply_filter, {
    req(data(), input$selectedVars, input$filter_value)
    tryCatch({
      filter_expr <- paste("data() %>% filter(", input$filter_value, ")")
      filtered_data(eval(parse(text = filter_expr)))
      showNotification("Filter applied successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error in filter:", e$message), type = "error")
      filtered_data(data())
    })
  })
  
  # Variables for heatmap
  observe({
    req(data())
    numeric_vars <- names(data())[sapply(data(), is.numeric)]
    updateCheckboxGroupInput(session, "heatmap_vars", choices = numeric_vars)
  })
  
  # Select/Deselect all for heatmap
  observeEvent(input$select_all_heatmap, {
    numeric_vars <- names(data())[sapply(data(), is.numeric)]
    updateCheckboxGroupInput(session, "heatmap_vars", choices = numeric_vars, selected = numeric_vars)
  })
  
  observeEvent(input$deselect_all_heatmap, {
    updateCheckboxGroupInput(session, "heatmap_vars", selected = character(0))
  })
  
  # Filtered data for correlation plots
  correlation_filtered_data <- reactiveVal()
  
  observeEvent(input$apply_correlation_filter, {
    req(data(), input$correlation_filter)
    tryCatch({
      filter_expr <- paste("data() %>% filter(", input$correlation_filter, ")")
      correlation_filtered_data(eval(parse(text = filter_expr)))
      showNotification("Filter applied successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error in filter:", e$message), type = "error")
      correlation_filtered_data(data())
    })
  })
  
  # Immediately update variable choices when filter type changes
  observeEvent(input$var_type_filter, {
    req(data())
    if (input$var_type_filter == "all") {
      updateSelectInput(session, "selectedVars", choices = names(data()))
    } else if (input$var_type_filter == "numeric") {
      numeric_vars <- names(data())[sapply(data(), is.numeric)]
      updateSelectInput(session, "selectedVars", choices = numeric_vars)
    } else if (input$var_type_filter == "categorical") {
      cat_vars <- names(data())[sapply(data(), function(x) is.factor(x) || is.character(x))]
      updateSelectInput(session, "selectedVars", choices = cat_vars)
    }
  })
  
  # Update correlation plots based on plot type
  output$relPlot <- renderPlot({
    req(input$plot_correlation_type)
    
    # Use filtered data if available, otherwise use original data
    plot_data <- if (!is.null(correlation_filtered_data())) correlation_filtered_data() else data()
    
    if (input$plot_correlation_type == "scatter") {
      req(input$explanatory, input$response)
      
      # Determine the type of the variables
      is_numeric_explanatory <- is.numeric(plot_data[[input$explanatory]])
      is_numeric_response <- is.numeric(plot_data[[input$response]])
      
      # Start the ggplot object with a general aesthetics setting
      p <- ggplot(data = plot_data, aes_string(x = input$explanatory, y = input$response))
      
      # Decide on the plot type based on the data types of variables
      if (is_numeric_explanatory && is_numeric_response) {
        # Both variables are numeric: use scatterplot
        p <- p + geom_point(alpha = input$shade) + theme_minimal()
      } else {
        # At least one variable is categorical: use boxplot
        p <- p + geom_boxplot(aes(group = plot_data[[input$explanatory]]), alpha = input$shade) + theme_minimal()
      }
      
      # Optionally add marginal histograms if both variables are numeric and requested
      if (is_numeric_explanatory && is_numeric_response && input$marginal) {
        p <- ggMarginal(p, type = "histogram")
      }
      
      p  # Return the plot object
    } else if (input$plot_correlation_type == "heatmap") {
      req(input$heatmap_vars, length(input$heatmap_vars) > 1)
      
      # Subset to only numeric columns selected for heatmap
      heatmap_data <- plot_data[, input$heatmap_vars, drop = FALSE]
      
      # Calculate correlation matrix
      cor_matrix <- cor(heatmap_data, use = "pairwise.complete.obs")
      
      # Create heatmap with correlation values
      melted_cor <- reshape2::melt(cor_matrix)
      
      # Create heatmap
      ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                            midpoint = 0, limit = c(-1,1), space = "Lab", 
                            name="Correlation") +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        coord_fixed() +
        labs(title = "Correlation Heatmap")
    }
  })
  
  # Feature Engineering functionality
  
  # Update variable selections for feature engineering
  observe({
    req(data())
    all_vars <- names(data())
    numeric_vars <- names(data())[sapply(data(), is.numeric)]
    
    # Update all select inputs for feature engineering
    updateSelectInput(session, "interaction_var1", choices = all_vars)
    updateSelectInput(session, "interaction_var2", choices = all_vars)
    updateSelectInput(session, "poly_var", choices = numeric_vars)
    updateSelectInput(session, "log_var", choices = numeric_vars)
    updateSelectInput(session, "std_vars", choices = numeric_vars)
  })
  
  # Store engineered features
  engineered_features <- reactiveVal(data.frame())
  
  # Create interaction term
  observeEvent(input$create_interaction, {
    req(data(), input$interaction_var1, input$interaction_var2, input$interaction_name)
    
    tryCatch({
      # Create interaction term
      var1 <- data()[[input$interaction_var1]]
      var2 <- data()[[input$interaction_var2]]
      
      interaction_var <- var1 * var2
      
      # Add to engineered features
      new_feature <- data.frame(interaction_var)
      names(new_feature) <- input$interaction_name
      
      # Update the engineered features dataframe
      current_features <- engineered_features()
      if (ncol(current_features) == 0) {
        engineered_features(new_feature)
      } else {
        engineered_features(cbind(current_features, new_feature))
      }
      
      # Add to main dataset
      data(cbind(data(), new_feature))
      
      # Show success message
      output$feature_eng_message <- renderText({
        paste("Successfully created interaction term:", input$interaction_name)
      })
    }, error = function(e) {
      output$feature_eng_message <- renderText({
        paste("Error creating interaction term:", e$message)
      })
    })
  })
  
  # Create polynomial features
  observeEvent(input$create_poly, {
    req(data(), input$poly_var, input$poly_degree)
    
    tryCatch({
      # Get original variable
      var_to_transform <- data()[[input$poly_var]]
      
      # Create polynomial features
      new_features <- data.frame(var_to_transform)
      for (i in 2:input$poly_degree) {
        new_col <- var_to_transform^i
        new_features <- cbind(new_features, new_col)
      }
      
      # Name the features
      feature_names <- c(input$poly_var)
      for (i in 2:input$poly_degree) {
        feature_names <- c(feature_names, paste0(input$poly_prefix, input$poly_var, "_", i))
      }
      names(new_features) <- feature_names
      
      # Keep only the new polynomial features (exclude the original)
      new_features <- new_features[, -1, drop = FALSE]
      
      # Update the engineered features dataframe
      current_features <- engineered_features()
      if (ncol(current_features) == 0) {
        engineered_features(new_features)
      } else {
        engineered_features(cbind(current_features, new_features))
      }
      
      # Add to main dataset
      data(cbind(data(), new_features))
      
      # Show success message
      output$feature_eng_message <- renderText({
        paste("Successfully created polynomial features for", input$poly_var)
      })
    }, error = function(e) {
      output$feature_eng_message <- renderText({
        paste("Error creating polynomial features:", e$message)
      })
    })
  })
  
  # Create log transformation
  observeEvent(input$create_log, {
    req(data(), input$log_var, input$log_name)
    
    tryCatch({
      # Get variable to transform
      var_to_transform <- data()[[input$log_var]]
      
      # Check if all values are positive
      if (any(var_to_transform <= 0, na.rm = TRUE)) {
        stop("Log transformation requires all values to be positive")
      }
      
      # Apply transformation
      if (input$log_type == "natural") {
        transformed_var <- log(var_to_transform)
      } else { # log10
        transformed_var <- log10(var_to_transform)
      }
      
      # Create new feature
      new_feature <- data.frame(transformed_var)
      names(new_feature) <- input$log_name
      
      # Update the engineered features dataframe
      current_features <- engineered_features()
      if (ncol(current_features) == 0) {
        engineered_features(new_feature)
      } else {
        engineered_features(cbind(current_features, new_feature))
      }
      
      # Add to main dataset
      data(cbind(data(), new_feature))
      
      # Show success message
      output$feature_eng_message <- renderText({
        paste("Successfully created log transformation:", input$log_name)
      })
    }, error = function(e) {
      output$feature_eng_message <- renderText({
        paste("Error creating log transformation:", e$message)
      })
    })
  })
  
  # Create standardized/normalized features
  observeEvent(input$create_std, {
    req(data(), input$std_vars, length(input$std_vars) > 0)
    
    tryCatch({
      # Get variables to transform
      vars_to_transform <- data()[, input$std_vars, drop = FALSE]
      
      # Apply transformation
      if (input$std_method == "zscore") {
        # Z-score standardization
        transformed_vars <- as.data.frame(scale(vars_to_transform))
      } else { # minmax
        # Min-Max normalization
        transformed_vars <- as.data.frame(lapply(vars_to_transform, function(x) {
          (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
        }))
      }
      
      # Name the new features
      names(transformed_vars) <- paste0(input$std_prefix, input$std_vars)
      
      # Update the engineered features dataframe
      current_features <- engineered_features()
      if (ncol(current_features) == 0) {
        engineered_features(transformed_vars)
      } else {
        engineered_features(cbind(current_features, transformed_vars))
      }
      
      # Add to main dataset
      data(cbind(data(), transformed_vars))
      
      # Show success message
      output$feature_eng_message <- renderText({
        paste("Successfully standardized/normalized", length(input$std_vars), "variables")
      })
    }, error = function(e) {
      output$feature_eng_message <- renderText({
        paste("Error standardizing/normalizing variables:", e$message)
      })
    })
  })
  
  # Display table of engineered features
  output$engineered_features_table <- renderDT({
    req(engineered_features())
    if (ncol(engineered_features()) > 0) {
      datatable(engineered_features(), options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  # Create dynamic filter UI based on variable type
  output$dynamic_filter_ui <- renderUI({
    req(data(), input$selectedVars)
    
    var_to_filter <- input$selectedVars
    var_data <- data()[[var_to_filter]]
    
    if(is.numeric(var_data)) {
      # For numeric variables, use a range slider
      min_val <- floor(min(var_data, na.rm = TRUE))
      max_val <- ceiling(max(var_data, na.rm = TRUE))
      
      tagList(
        h4("Filter by range:"),
        sliderInput(
          inputId = "numeric_filter_range",
          label = NULL,
          min = min_val,
          max = max_val,
          value = c(min_val, max_val),
          step = (max_val - min_val) / 100,
          round = -1
        ),
        checkboxInput("exclude_outliers", "Exclude outliers", value = FALSE)
      )
    } else if(is.factor(var_data) || is.character(var_data)) {
      # For categorical variables, use checkboxes
      unique_values <- sort(unique(as.character(var_data)))
      tagList(
        h4("Select categories:"),
        checkboxGroupInput(
          inputId = "categorical_filter_values",
          label = NULL,
          choices = unique_values,
          selected = unique_values
        ),
        actionButton("select_all_cats", "Select All", class = "btn-sm btn-primary"),
        actionButton("deselect_all_cats", "Deselect All", class = "btn-sm btn-secondary")
      )
    } else {
      # For other types
      p("Filtering not available for this variable type")
    }
  })
  
  # Create dynamic correlation filter UI
  output$correlation_dynamic_filter_ui <- renderUI({
    req(data(), input$plot_correlation_type)
    
    filter_ui_list <- list()
    
    if(input$plot_correlation_type == "scatter") {
      req(input$explanatory, input$response)
      
      # X variable filter
      x_var <- input$explanatory
      x_data <- data()[[x_var]]
      
      # Y variable filter
      y_var <- input$response
      y_data <- data()[[y_var]]
      
      filter_ui_list[[1]] <- h4("Filter data:")
      
      # X variable UI
      if(is.numeric(x_data)) {
        min_x <- floor(min(x_data, na.rm = TRUE))
        max_x <- ceiling(max(x_data, na.rm = TRUE))
        
        filter_ui_list[[2]] <- tags$div(
          style = "margin-bottom: 15px;",
          tags$b(paste(x_var, ":")),
          sliderInput(
            inputId = "x_numeric_filter",
            label = NULL,
            min = min_x,
            max = max_x,
            value = c(min_x, max_x),
            step = (max_x - min_x) / 100
          )
        )
      } else if(is.factor(x_data) || is.character(x_data)) {
        unique_x <- sort(unique(as.character(x_data)))
        filter_ui_list[[2]] <- tags$div(
          style = "margin-bottom: 15px;",
          tags$b(paste(x_var, ":")),
          selectInput(
            inputId = "x_categorical_filter",
            label = NULL,
            choices = unique_x,
            selected = unique_x,
            multiple = TRUE
          )
        )
      }
      
      # Y variable UI
      if(is.numeric(y_data)) {
        min_y <- floor(min(y_data, na.rm = TRUE))
        max_y <- ceiling(max(y_data, na.rm = TRUE))
        
        filter_ui_list[[3]] <- tags$div(
          style = "margin-bottom: 15px;",
          tags$b(paste(y_var, ":")),
          sliderInput(
            inputId = "y_numeric_filter",
            label = NULL,
            min = min_y,
            max = max_y,
            value = c(min_y, max_y),
            step = (max_y - min_y) / 100
          )
        )
      } else if(is.factor(y_data) || is.character(y_data)) {
        unique_y <- sort(unique(as.character(y_data)))
        filter_ui_list[[3]] <- tags$div(
          style = "margin-bottom: 15px;",
          tags$b(paste(y_var, ":")),
          selectInput(
            inputId = "y_categorical_filter",
            label = NULL,
            choices = unique_y,
            selected = unique_y,
            multiple = TRUE
          )
        )
      }
    } else if(input$plot_correlation_type == "heatmap") {
      # No specific filters needed for heatmap, as it uses all selected variables
      filter_ui_list[[1]] <- p("Filtering is handled through variable selection for heatmaps.")
    }
    
    do.call(tagList, filter_ui_list)
  })
  
  # Distribution tab - Variable selection and filtering
  output$filter_controls <- renderUI({
    req(data(), input$var_type_filter)
    
    if(input$var_type_filter == "all") {
      all_vars <- names(data())
      updateSelectInput(session, "selectedVars", choices = all_vars)
    } else if(input$var_type_filter == "numeric") {
      numeric_vars <- names(data())[sapply(data(), is.numeric)]
      updateSelectInput(session, "selectedVars", choices = numeric_vars)
    } else if(input$var_type_filter == "categorical") {
      cat_vars <- names(data())[sapply(data(), function(x) is.factor(x) || is.character(x))]
      updateSelectInput(session, "selectedVars", choices = cat_vars)
    }
    
    # Return the UI for the filter
    uiOutput("dynamic_filter_ui")
  })
  
  # Handler for categorical filters select/deselect all
  observeEvent(input$select_all_cats, {
    req(data(), input$selectedVars)
    var_data <- data()[[input$selectedVars]]
    if(is.factor(var_data) || is.character(var_data)) {
      unique_values <- sort(unique(as.character(var_data)))
      updateCheckboxGroupInput(session, "categorical_filter_values", selected = unique_values)
    }
  })
  
  observeEvent(input$deselect_all_cats, {
    updateCheckboxGroupInput(session, "categorical_filter_values", selected = character(0))
  })
  
  # Apply filters to variable distribution data
  filtered_dist_data <- reactive({
    req(data(), input$selectedVars)
    
    # Start with the original data
    filtered <- data()
    var_to_filter <- input$selectedVars
    var_data <- filtered[[var_to_filter]]
    
    # Apply appropriate filters based on variable type
    if(is.numeric(var_data)) {
      req(input$numeric_filter_range)
      range_min <- input$numeric_filter_range[1]
      range_max <- input$numeric_filter_range[2]
      
      filtered <- filtered[filtered[[var_to_filter]] >= range_min & 
                          filtered[[var_to_filter]] <= range_max, ]
      
      # Handle outlier exclusion if selected
      if(input$exclude_outliers) {
        q1 <- quantile(filtered[[var_to_filter]], 0.25, na.rm = TRUE)
        q3 <- quantile(filtered[[var_to_filter]], 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        lower_bound <- q1 - 1.5 * iqr
        upper_bound <- q3 + 1.5 * iqr
        
        filtered <- filtered[filtered[[var_to_filter]] >= lower_bound & 
                            filtered[[var_to_filter]] <= upper_bound, ]
      }
    } else if(is.factor(var_data) || is.character(var_data)) {
      req(input$categorical_filter_values)
      if(length(input$categorical_filter_values) > 0) {
        filtered <- filtered[filtered[[var_to_filter]] %in% input$categorical_filter_values, ]
      }
    }
    
    return(filtered)
  })
  
  # Apply correlation tab filters
  filtered_corr_data <- reactive({
    req(data())
    
    # Start with the original data
    filtered <- data()
    
    if(input$plot_correlation_type == "scatter") {
      req(input$explanatory, input$response)
      
      x_var <- input$explanatory
      y_var <- input$response
      
      # Apply X variable filters
      if(is.numeric(filtered[[x_var]]) && !is.null(input$x_numeric_filter)) {
        x_min <- input$x_numeric_filter[1]
        x_max <- input$x_numeric_filter[2]
        filtered <- filtered[filtered[[x_var]] >= x_min & filtered[[x_var]] <= x_max, ]
      } else if((is.factor(filtered[[x_var]]) || is.character(filtered[[x_var]])) && 
                !is.null(input$x_categorical_filter)) {
        filtered <- filtered[filtered[[x_var]] %in% input$x_categorical_filter, ]
      }
      
      # Apply Y variable filters
      if(is.numeric(filtered[[y_var]]) && !is.null(input$y_numeric_filter)) {
        y_min <- input$y_numeric_filter[1]
        y_max <- input$y_numeric_filter[2]
        filtered <- filtered[filtered[[y_var]] >= y_min & filtered[[y_var]] <= y_max, ]
      } else if((is.factor(filtered[[y_var]]) || is.character(filtered[[y_var]])) && 
                !is.null(input$y_categorical_filter)) {
        filtered <- filtered[filtered[[y_var]] %in% input$y_categorical_filter, ]
      }
    }
    
    return(filtered)
  })
  
  # Update the distribution plot to use the new filtered data
  output$distPlot <- renderPlot({
    req(input$selectedVars, input$plot_type)
    
    # Use filtered data 
    plot_data <- filtered_dist_data()
    
    var_to_plot <- input$selectedVars
    
    if (input$plot_type == "histogram") {
      if (is.numeric(plot_data[[var_to_plot]])) {
        ggplot(data = plot_data, aes_string(x = var_to_plot)) +
          geom_histogram(binwidth = diff(range(plot_data[[var_to_plot]], na.rm = TRUE)) / input$bins, 
                         fill = input$color, color = "black") +
          labs(x = var_to_plot, y = "Frequency", title = paste("Histogram of", var_to_plot)) +
          theme_minimal()
      } else {
        ggplot(data = plot_data, aes_string(x = var_to_plot)) +
          geom_bar(fill = input$color) +
          labs(x = var_to_plot, y = "Frequency", title = paste("Bar plot of", var_to_plot)) +
          theme_minimal()
      }
    } else if (input$plot_type == "boxplot") {
      if (is.numeric(plot_data[[var_to_plot]])) {
        ggplot(data = plot_data, aes_string(y = var_to_plot)) +
          geom_boxplot(fill = input$color) +
          labs(y = var_to_plot, title = paste("Box plot of", var_to_plot)) +
          theme_minimal()
      } else {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Boxplot requires numeric data") +
          theme_void()
      }
    } else if (input$plot_type == "pie") {
      if (!is.numeric(plot_data[[var_to_plot]]) || is.factor(plot_data[[var_to_plot]])) {
        # For categorical variables
        count_data <- as.data.frame(table(plot_data[[var_to_plot]]))
        names(count_data) <- c("Category", "Count")
        
        ggplot(count_data, aes(x = "", y = Count, fill = Category)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          labs(title = paste("Pie chart of", var_to_plot)) +
          theme_minimal() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                axis.ticks = element_blank())
      } else {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Pie chart requires categorical data") +
          theme_void()
      }
    }
  })
  
  # Update the correlation plots to use the new filtered data
  output$relPlot <- renderPlot({
    req(input$plot_correlation_type)
    
    # Use filtered correlation data
    plot_data <- filtered_corr_data()
    
    if (input$plot_correlation_type == "scatter") {
      req(input$explanatory, input$response)
      
      # Determine the type of the variables
      is_numeric_explanatory <- is.numeric(plot_data[[input$explanatory]])
      is_numeric_response <- is.numeric(plot_data[[input$response]])
      
      # Start the ggplot object with a general aesthetics setting
      p <- ggplot(data = plot_data, aes_string(x = input$explanatory, y = input$response))
      
      # Decide on the plot type based on the data types of variables
      if (is_numeric_explanatory && is_numeric_response) {
        # Both variables are numeric: use scatterplot
        p <- p + geom_point(alpha = input$shade) + theme_minimal()
      } else {
        # At least one variable is categorical: use boxplot
        p <- p + geom_boxplot(aes(group = plot_data[[input$explanatory]]), alpha = input$shade) + theme_minimal()
      }
      
      # Optionally add marginal histograms if both variables are numeric and requested
      if (is_numeric_explanatory && is_numeric_response && input$marginal) {
        p <- ggMarginal(p, type = "histogram")
      }
      
      p  # Return the plot object
    } else if (input$plot_correlation_type == "heatmap") {
      req(input$heatmap_vars, length(input$heatmap_vars) > 1)
      
      # Subset to only numeric columns selected for heatmap
      heatmap_data <- plot_data[, input$heatmap_vars, drop = FALSE]
      
      # Calculate correlation matrix
      cor_matrix <- cor(heatmap_data, use = "pairwise.complete.obs")
      
      # Create heatmap with correlation values
      melted_cor <- reshape2::melt(cor_matrix)
      
      # Create heatmap
      ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                            midpoint = 0, limit = c(-1,1), space = "Lab", 
                            name="Correlation") +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        coord_fixed() +
        labs(title = "Correlation Heatmap")
    }
  })
  
  #################################################################################
  # Finish Buttons Logic (Simple Notifications for now)
  
  observeEvent(input$finish_cleaning, {
      showNotification("Cleaning stage finished. Current data state saved.", type = "message", duration = 5)
      # Potentially add logic here if you want to create a snapshot of 'cleaned_data'
  })

  observeEvent(input$finish_preprocessing, {
      req(target_feature_name()) # Ensure target is confirmed before finishing
      
      # Placeholder for actual NA handling logic trigger if needed, 
      # assuming NA handling happens reactively or on a separate button press now.
      # Example: Trigger KNN imputation if selected
      if(input$na_handling == 'Impute with KNN') {
          # Add KNN imputation logic here, or ensure it runs before this point
          # For example, you might have another button like "Apply NA Handling"
          # If imputation is complex, it should probably have its own button/trigger
          showNotification("Attempting KNN Imputation (logic needs implementation)...", type = "warning") 
      } else if (input$na_handling == 'Drop NAs') {
           # Add Drop NA logic here, or ensure it runs before this point
           showNotification("Attempting to Drop NAs (logic needs implementation)...", type = "warning") 
      }
      
      # Show final notification
      showNotification("Preprocessing stage finished. Data ready for Feature Engineering.", type = "success", duration = 5)
      
      # Update the reactive value 'data_preprocess' if you implement separate data states
      # data_preprocess(data()) 
       output$preprocessing_message <- renderText({
         paste("Preprocessing finished. Target feature:", target_feature_name(), ". NA Handling method:", input$na_handling)
       })
  })
 }

# Run the application 
shinyApp(ui = ui, server = server)