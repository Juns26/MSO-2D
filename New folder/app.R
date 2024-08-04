library(shiny)
library(bs4Dash)
library(readxl)
library(plotly)
library(ggplot2)
library(readr)
library(DT)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(fresh)
library(igraph)
library(tidyr)
library(networkD3)
library(htmlwidgets)
source('construction.R')

theme <- create_theme(
  bs4dash_status(
    danger = '#263F6B'
  )
)

ui <- dashboardPage(
  freshTheme = theme,
  fullscreen=TRUE,
  help = NULL,
  dark = NULL,
  title = "Project Management",
  header = dashboardHeader(
    
    title = dashboardBrand(
      title = "Project Management",
      color = "danger",
    )
  ),
  sidebar = dashboardSidebar(
    skin = "light",
    status = "danger",
    title = "Menu",
    sidebarMenu(
      menuItem(
        text = "Home",
        tabName = "home",
        icon = icon("home")
      ),
      menuItem(
        text = "Data",
        tabName = "data",
        icon = icon("database")
      ),
      menuItem(
        text = "Dashboard",
        tabName = "dashboard",
        icon = icon("fas fa-chart-bar")
      )
    )
  ),
  footer = dashboardFooter(
    left = "Project Management System",
    right = Sys.Date()
  ),
  
  body = dashboardBody(
    tags$head(tags$link(rel='stylesheet', type='text/css',href='styles.css')),
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons")
    ),
    tabItems(
      
      #HOME PAGE TAB
      tabItem(
        tabName = "home",
        jumbotron(
          title = tagList(icon("project-diagram"), " Welcome!"),
          status = 'danger',
          lead = 'Make scheduling project more seemless',
          btnName = NULL,
          fluidRow(
            column(
              width=6,
              HTML(
                "
                <h3>
                  <i class='material-icons' style='vertical-align'>navigation</i> &nbsp; <b> Navigating our interface </b>
                </h3>
                
                <br>
                
                <h5> 1. &nbsp; <i class='fas fa-home'></i> &nbsp; Home </h5>
                
                <div class='indent'>
                  a. Find out about the interface
                  <br>
                  b. Download prepared data
                </div>
                
                <br>
                
                <h5> 2. &nbsp; <i class='fas fa-database'></i> &nbsp; Data input </h5>
                
                <div class='indent'>
                  a. Input csv / xlsx files
                  <br>
                  b. Manual input of data
                </div>
                
                <br>
                
                <h5> 3. &nbsp; <i class='fas fa-chart-bar'></i> &nbsp; Dashboard </h5>
                
                <div class='indent'>
                  a. View project schedule
                  <br>
                  b. Crash and/or resource level project
                  <br>
                  c. Compute total cost of project

                </div>
                
                "
              )
              ),
            column(
              width=6,
              HTML( 
              "
              <h3><i class='fas fa-file-upload'></i> &nbsp; <b> Data </b> </h3>
              
              <br> 
              
              <h5> <b> Parameters </b> </h5>
              1. <b>Activity</b>: Unique activity for schedule
              <br>
              2. <b>Duration</b>: Time to complete activity
              <br>
              3. <b>Predecessor</b>: Prior activity needed to complete
              <br>
              4. <b>Cost</b>: Cost to complete activity
              <br>
              5. <b>Resource</b>: Resource needed to complete activity
              <br>
              6. <b>Crashable duration</b>: Available duration to shorten
              <br>
              7. <b>Crash cost per period</b>: Cost incurred to speed up activity completion
              
              <br>
              <br>
              
              <h5> <b> Basic scheduling requires: </b></h5>
              <ul> 
              <li>Activity</li>
              <li>Duration</li>
              <li>Predecessor </li>
              </ul>
    
              Additional parameter for resource leveling
              <ul>
              <li>Resource</li>
              </ul>
              
              Additional parameter for crashing
              <ul>
              <li>Crashable duration</li>
              </ul>
              ")
            )
          ),
          class = 'homepage_box'
          ),
        fluidRow(        
          box(
            title = tags$b('Test Data 1'),
            status = 'danger',
            tags$i("Download data for basic scheduling and resource leveling"),
            br(),
            br(),
            downloadButton("downloadData1","Download"),
            class = 'box_table_small'
          ),
          box(
            title = tags$b('Test Data 2'),
            status = 'danger',
            tags$i("Download data for resource leveling and crashing"),
            br(),
            br(),
            downloadButton("downloadData2","Download"),
            class = 'box_table_small'
          )
          )
      ),
      
      #DATA PAGE TAB
      tabItem(
        tabName = "data",
        box(
          title = 'Input data and select parameters',
          status='danger',
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 12,
          # Checking which manual entry of upload file
          fluidRow(
            column(
              width = 12,  # Full width for the radio buttons
              radioButtons("dataInputType", "Data Input Type:",
                           choices = c("Upload File" = "file", "Manual Entry" = "manual"),
                           selected = "file")  # Default selection
            )
          ),
          
          # Conditional UI based on radio button selection then it will show which
          conditionalPanel(
            condition = "input.dataInputType == 'file'", 
            fluidRow(
              column(
                width = 5,
                fileInput("file", label = NULL, accept = c(".csv", ".xlsx")),
                radioButtons("fileType", "File type:",
                             choices = c("CSV" = "csv", "Excel" = "xlsx"),
                             selected = "csv")
              ),
              column(
                width = 7,
                uiOutput('parameter_selection')
              )
            )
          ),
          
          # UI for manual entry
          conditionalPanel(
            condition = "input.dataInputType == 'manual'",
            
            # Input for Number of Rows
            fluidRow(
              column(width = 12,
                     numericInput("numRows", "Number of Rows:", value = 5, min = 1),
                     actionButton("generateRows", "Generate Rows"))
            ),
            hr(),
            # Fixed Headers
            fluidRow(
              column(width = 2, h5("Activity")),
              column(width = 2, h5("Predecessor"),
                     tags$div(style = "margin-top: -10px;", "Only 1 '-' as predecessor")),
              column(width = 2, h5("Duration")),
              column(width = 2, h5("Resource")),
              column(width = 2, h5("Cost"))
            ),
            
            hr(),  # Horizontal line for separation
            # Data Entry Table (Dynamically Generated)
            uiOutput("manualEntryTableWithHeaders"),  # Dynamically create the table
            
            # "Done" Button
            fluidRow(
              column(width = 12, actionButton("manualDataDone", "Update"))
            )
          )
          
        ),
        
        box(
          title = 'Preview input data',
          status='danger',
          solidHeader = TRUE,
          width=12,
          uiOutput('contents'),
          class = 'box_table'
        ),
        
        box(
          title = 'Preview selected parameter data',
          status='danger',
          solidHeader = TRUE,
          width=12,
          uiOutput('missing_param'),
          class = 'box_table'
        ),
        
        ),
      
      #DASHBOARD PAGE TAB
      tabItem(
        tabName = "dashboard",
        fluidRow(
          infoBoxOutput('cost_value_box'),
          infoBoxOutput('resource_value_box'),
          infoBoxOutput('targetduration_value_box'),
        ),
        fluidRow(
          box(
            width=6,
            title=tags$b("Modify Target Duration"),
            status='purple',
            uiOutput('duration_slider')
          ),
          box(
            width=6,
            title=tags$b("Modify Resource"),
            status='orange',
            uiOutput('resource_slider'),
            class = 'charts_container'
          )
        ),
        box(
          width=12,
          title = tags$b("Gantt Chart"),
          status = 'info',
          fluidRow(
            column(width = 6, uiOutput("date_toggle")),
            column(width = 6, uiOutput("date_selection"))
          ),
          uiOutput("gantt_chart_plot"),
          class = 'charts_container'
        ),
        box(
          width=12,
          title=tags$b("Resource Chart"),
          status = 'orange',
          uiOutput("resource_chart"),
          class = 'charts_container'
        ),
        box(
          width=12,
          title=tags$b("Cost Chart"),
          status = 'success',
          uiOutput("cost_chart"),
          class = 'charts_container'
        ),          
        box(
          width = 12,
          title = tags$b("Network Diagram"),
          status = 'primary',
          uiOutput("cpm_network_plot"),
          class = 'charts_container'
          ),
        box(
          width = 12,
          title=tags$b('Finalised Schedule'),
          status='info',
          uiOutput("final_table"),
          uiOutput('download_finaldata'),
          class = 'charts_container'
        )
      )
    )
  )
)

server <- function(input, output) {
  
  #vvvvvvvvvvvv HOME PAGE TAB vvvvvvvvvvvv#
  output$downloadData1 <- downloadHandler(
    filename <- function() {
      "Construction.csv"
    },
    content <- function(file) {
      file.copy("www/Construction.csv",file)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename <- function() {
      "Construction2.csv"
    },
    content <- function(file) {
      file.copy("www/Construction2.csv",file)
    }
  )
  #^^^^^^^^^^^^ HOME PAGE TAB ^^^^^^^^^^^^#
  
  
  
  #vvvvvvvvvvvv DATA PAGE TAB vvvvvvvvvvvv#
  # Reactive value for manual data entry
  manual_data <- reactiveVal(data.frame(
    Jobs = character(),
    Predecessor = character(),
    Time = numeric(),
    Labour = numeric(),
    Cost = numeric()
  ))
  
  # Reactive to create the table for manual data
  manual_data_reactive <- reactive({
    manual_data()
  })
  
  # Reactive for handling data input from file or manual entry
  data <- reactive({
    if (input$dataInputType == "file") {
      req(input$file)
      fileType <- input$fileType
      file <- input$file$datapath
      
      if (fileType == "csv") {
        df <- read_csv(file)
      } else {
        df <- read_excel(file)
      }
      return(as.data.frame(df))
    } 
    else if (input$dataInputType == "manual") {
      req(input$manualDataDone)
      return(manual_data_reactive()) # Return manual data if available
    } 
    else {
      return(NULL) # Return NULL if no data available
    }
  })
  
  # Render the manual entry table dynamically
  output$manualEntryTableWithHeaders <- renderUI({
    
    num_rows <- input$numRows
    
    # Create table rows with input fields and corresponding headers
    rows <- lapply(1:num_rows, function(i) {
      fluidRow(
        column(width = 2, textInput(paste0("Activity", i), label = NULL, value = ifelse(is.na(manual_data_reactive()[i, 1]), "", manual_data_reactive()[i, 1]))),
        column(width = 2, textInput(paste0("Predecessor", i), label = NULL, value = ifelse(is.na(manual_data_reactive()[i, 2]), "", manual_data_reactive()[i, 2]))),
        column(width = 2, numericInput(paste0("Duration", i), label = NULL, value = ifelse(is.na(manual_data_reactive()[i, 3]), 1, manual_data_reactive()[i, 3]), min = 0)),
        column(width = 2, numericInput(paste0("Resource", i), label = NULL, value = ifelse(is.na(manual_data_reactive()[i, 4]), 1, manual_data_reactive()[i, 4]), min = 0)),
        column(width = 2, numericInput(paste0("Cost", i), label = NULL, value = ifelse(is.na(manual_data_reactive()[i, 5]), 0, manual_data_reactive()[i, 5]), min = 0))
      )
    })
    
    # Combine rows into a table (no header row here)
    tagList(tags$table(tags$tbody(rows)))
  })
  
  # Observe the "Enter Data" button to update manual data
  observeEvent(input$manualDataDone, {
    req(input$numRows)
    
    new_data <- data.frame(
      Jobs = sapply(1:input$numRows, function(i) input[[paste0("Activity", i)]]),
      Predecessor = sapply(1:input$numRows, function(i) input[[paste0("Predecessor", i)]]),
      Time = as.numeric(sapply(1:input$numRows, function(i) input[[paste0("Duration", i)]])),
      Resource = as.numeric(sapply(1:input$numRows, function(i) input[[paste0("Resource", i)]])),
      Cost = as.numeric(sapply(1:input$numRows, function(i) input[[paste0("Cost", i)]]))
    )
    manual_data(new_data)
  })
  
  # Observe the Generate button to add new rows dynamically
  observeEvent(input$generateRows, {
    req(input$numRows)
    
    # Get the current data
    current_data <- manual_data()
    
    # If there are fewer rows than requested, add new rows
    if (nrow(current_data) < input$numRows) {
      new_rows <- input$numRows - nrow(current_data)
      new_data <- data.frame(
        Jobs = c(current_data$Jobs, LETTERS[seq(nrow(current_data) + 1, length.out = new_rows)]),  # Add new activity codes
        Predecessor = rep("", new_rows),
        Time = rep(1, new_rows),
        Labour = rep(1, new_rows),
        Cost = rep(0, new_rows)
      )
      current_data <- rbind(current_data, new_data)
    } 
    
    # If there are more rows than requested, remove the extra rows
    else if (nrow(current_data) > input$numRows) {
      current_data <- current_data[1:input$numRows, ]
    }
    
    manual_data(current_data)
  })
  
  # Reactive function to process the data input (file or manual)
  new_input_data <- reactive({
    data_input <- data()
    # Check if data is available and if 'Done' button is clicked (for manual input)
    if (input$dataInputType == "manual" && is.null(input$manualDataDone)) {
      return(NA)  # Don't process manual data until "Done" is clicked
    }
    
    if (!is.null(data_input) && is.data.frame(data_input)) {
      # Determine if using manual data and set defaults for missing headers
      is_manual <- input$dataInputType == "manual"
      default_headers <- list(Jobs = "Jobs", Predecessor = "Predecessor", Time = "Time", Labour = "Labour", Cost = "Cost")
      
      # Get header selections, using defaults for manual input
      activity_header    <- if (is_manual) default_headers$Jobs     else input$activity_header
      predecessor_header <- if (is_manual) default_headers$Predecessor else input$predecessor_header
      duration_header   <- if (is_manual) default_headers$Time      else input$duration_header
      resource_header   <- if (is_manual) default_headers$Labour    else input$resource_header
      cost_header       <- if (is_manual) default_headers$Cost       else input$cost_header
      crashduration_header <- if (is_manual) "NA" else input$crashduration_header
      crashcost_header <- if (is_manual) "NA" else input$crashcost_header
      
      # Check if essential headers are provided, if not, return NA
      if (is.na(activity_header) || is.na(predecessor_header) || is.na(duration_header)) {
        return(NA)
      }
      # Call the `input_data` function to process the data
      a <- input_data(
              data_input,
              activity_header = activity_header,
              predecessor_header = predecessor_header,
              duration_header = duration_header,
              cost_header = cost_header,
              crashduration_header = crashduration_header,       # Not used in manual entry
              crashcost_header = crashcost_header,           # Not used in manual entry
              resource_header = resource_header
              )
      return (a)
    } else {
      return(NA) 
    }
    
  })
  
  # Render parameter selection UI dynamically for both manual and file
  output$parameter_selection <- renderUI({
    req(data())  # Requires either file or manual data to be available
    div(
      selectInput(inputId = "activity_header",
                  label = "Activity *",
                  choices = c(colnames(data()), NA),
                  selected = if (input$dataInputType == "manual") "Jobs" else NA),
      
      selectInput(inputId = "predecessor_header",
                  label = "Predecessor *",
                  choices = c(colnames(data()), NA),
                  selected = if (input$dataInputType == "manual") "Predecessor" else NA),
      
      selectInput(inputId = "duration_header",
                  label = "Duration *",
                  choices = c(colnames(data()), NA),
                  selected = if (input$dataInputType == "manual") "Time" else NA),
      
      selectInput(inputId = "resource_header",
                  label = "Resource",
                  choices = c(colnames(data()), NA),
                  selected = if (input$dataInputType == "manual") "Labour" else NA),
      
      selectInput(inputId = "cost_header",
                  label = "Cost",
                  choices = c(colnames(data()), NA),
                  selected = if (input$dataInputType == "manual") "Cost" else NA),
      
      selectInput(inputId = "crashcost_header",
                  label = "Crash Cost",
                  choices = c(colnames(data()), NA),
                  selected = NA), # No default selection for crash cost
      
      selectInput(inputId = "crashduration_header",
                  label = "Crash Duration",
                  choices = c(colnames(data()), NA),
                  selected = NA), # No default selection for crash duration
      
      class = "parameter-selection"
    )
  })
  
  # Render the data table
  output$contents <- renderUI({
    req(data())
    renderTable(data())
  })
  
  #process the data
  activity_header <- reactive(if (input$dataInputType == "manual") "Jobs" else input$activity_header)
  duration_header <- reactive(if (input$dataInputType == "manual") "Time" else  input$duration_header)
  predecessor_header <- reactive(if (input$dataInputType == "manual") "Predecessor" else input$predecessor_header)
  resource_header <- reactive(if (input$dataInputType == "manual") "Labour" else input$resource_header)
  cost_header <- reactive(if (input$dataInputType == "manual") "Cost" else input$cost_header)
  crashcost_header <- reactive(if (input$dataInputType=='manual') "NA" else input$crashcost_header)
  crashduration_header <- reactive(if (input$dataInputType == "manual") "NA" else input$crashduration_header)
  headers_current <- reactive(c(activity_header(),duration_header(),predecessor_header(),resource_header(),cost_header(),crashcost_header(),crashduration_header()))
  headers_name <- reactive(c("Activity","Duration","Predecessor","Resource","Cost","Crash Cost","Crash duration"))
  
  
  output$missing_param <- renderUI({
    req(data())  # Make sure data exists before proceeding
    
    # Check if any of the essential headers are missing (regardless of input type)
    missing_compulsory_parameters <- headers_name()[1:3][headers_current()[1:3] == "NA"]
    
    # If manual input, check if the "Done" button is clicked
    if (input$dataInputType == "manual" && is.null(input$manualDataDone)) {
      missing_compulsory_parameters <- c(missing_compulsory_parameters, "Please click 'Done' after entering data")
    }
    
    # Only check for additional parameters if the main data is valid
    if (!is.null(new_input_data()) && is.data.frame(new_input_data())) {
      missing_additional_parameters <- headers_name()[4:7][headers_current()[4:7] == "NA"]
    } else {
      missing_additional_parameters <- NULL  # No additional parameters if data is invalid
    }
    
    #when data input is NULL due to same columns chosen
    if (is.null(new_input_data())) {
      HTML('<span style="color:red; font-weight:bold;">Incorrect inputs parameters. Check the data inputs and parameter selections</span>')
    }
    
    # Display messages if there are missing parameters
    else if (length(missing_compulsory_parameters) > 0) {
      missing_compulsory_parameters <- paste(missing_compulsory_parameters, collapse = ", ")
      HTML(paste0("<p><span style='color:black; font-weight:bold;'>Missing Mandatory Parameters: </span><span style='color:red; font-weight:bold;'>", missing_compulsory_parameters, "</span></p>"))
    } 
    else if (length(missing_additional_parameters) > 0) { 
      if (all(is.na(missing_additional_parameters))) {
        missing_additional_parameters <- NULL
        } 
      else {
        missing_additional_parameters <- paste(missing_additional_parameters, collapse = ", ")  
      }
      div(
        renderTable({new_input_data()}),
        tags$b(paste0("Additional Parameter(s) missing: ", missing_additional_parameters))
      )
    } 
    else {
      renderTable({new_input_data()})  # Render the table if no parameters are missing
    }
  })
  
  
  # Output table containing ES, EF, LS, LF, CP if at least 3 basic parameters are available, else return NA
  cpm_processed_data <- reactive({
    if (!is.data.frame(new_input_data())) {
      return(NA)
    } else {
      processed_data(new_input_data())
    }
  })
  
  # CPM base datas
  resource_usage_cpm <- reactive(
    if (is.data.frame(cpm_processed_data())){
      resource_vs_time(cpm_processed_data())
    })
  cpm_max_resource <- reactive(
    if (is.data.frame(cpm_processed_data())){
      max(resource_usage_cpm()$resource_usage)
    })
  min_resource <- reactive(
    if (is.data.frame(cpm_processed_data())){
      max(cpm_processed_data()$Resource)
    })
  
  duration_cpm <- reactive(
  if (is.data.frame(cpm_processed_data())){
    max(cpm_processed_data()$EF)
  })
  
  min_duration <- reactive({
    if (is.data.frame(cpm_processed_data())){
      computed_duration <- duration_cpm() - sum(cpm_processed_data()$`Crash Duration`)
      ifelse(computed_duration < 0, 0, computed_duration)
      }
    })
  #^^^^^^^^^^^^ DATA PAGE TAB ^^^^^^^^^^^^#
  
  
  #vvvvvvvvvvvv DASHBOARD PAGE TAB vvvvvvvvvvvv#
  
  #based on table inputs and/or sliders input, store as final_dataset()
  final_dataset <- reactive({
    req(cpm_processed_data())
    #case 1.1 when its bare minimum cpm with no crashing or leveling
    if (crashduration_header()=="NA" & resource_header()=="NA") {
      a <- cpm_processed_data()
    }
    
    #case 1.2 when only crashing duration is provided, can only crashing
    else if (resource_header()=="NA") {
      a <- crashing_function(input$duration,cpm_processed_data())
    }
    
    #case 1.3 when only resource is provided, can only do resource leveling
    else if (crashduration_header()=="NA"){
      a <- resource_leveling(cpm_processed_data(),resource_allocated = input$resource,cpm_max_resource())
    }
    
    #case 2.1 when it is default cpm, both resource and crash duration provided
    else if (input$duration == duration_cpm() & input$resource == cpm_max_resource()) {
      a <- cpm_processed_data()
    }
    
    #case 2.2 when only duration differs, resource = cpm_resource,crashing and leveling function involved
    else if (input$duration!=duration_cpm() & input$resource==cpm_max_resource()) {
      a <- final_data(cpm_processed_data(),input$resource,input$duration)
    }
    #case 2.3 when only resource differs, duration = cpm_duration,leveling function involved
    else if (input$duration==duration_cpm() & input$resource!=cpm_max_resource()) {
      a <- final_data(cpm_processed_data(),input$resource,input$duration)
    }
    
    #case 3 when both duration and resource differs from CPM
    else if (input$duration!=duration_cpm() & input$resource!=cpm_max_resource()) {
      a <- final_data(cpm_processed_data(),input$resource,input$duration)
    }
    
    # input$start_date is null initially
    if (!is.null(input$start_date)){
      if (input$date_format){
        a$ES <- as.Date(input$start_date + a$ES)
        a$EF <- as.Date(input$start_date + a$EF)
        a$LS <- as.Date(input$start_date + a$LS)
        a$LF <- as.Date(input$start_date + a$LF)
      }
    }
    data.frame(a)
  })
  
  editable_table <- reactive({
    final_dataset()
  })
  

  output$cost_value_box <- renderInfoBox({
    if (is.null(input$file) & input$dataInputType=='file') {
      value = tags$div("Missing data input",class='warning_msg')
    }
    else if (!is.data.frame(cpm_processed_data())) {
      value = tags$div("Incorrect entries. Check the data inputs and parameter selections",class='warning_msg')
    }
    if (!is.data.frame(cpm_processed_data())) {
      value = tags$div("Missing compulsory parameters", class='warning_msg')
    }
    else if (cost_header()=="NA") {
      value = tags$div("Missing cost parameter", class='warning_msg')
    }
    else {
      value = paste("$",sum(final_dataset()$Cost))
    }
    infoBox(tags$h5(tags$b(value)), "Total Cost", icon = icon("dollar-sign"), color='olive')
  })

  output$resource_value_box <- renderInfoBox({
    
    if (!is.data.frame(cpm_processed_data())) {
      value = tags$div("Missing compulsory parameters", class='warning_msg')
    }
    else if (resource_header()=='NA') {
      value = tags$div("Missing resource parameter", class='warning_msg')
    }
    else {
      value = input$resource
    }
    infoBox(tags$h5(tags$b(value)), 'Resource utilised',icon = icon("user"),color = 'orange') 
  })
    
  output$targetduration_value_box <- renderInfoBox({
    #when missing the first 3 compulsory parameter
    if (!is.data.frame(cpm_processed_data())) {
      value = tags$div("Missing compulsory parameters", class='warning_msg')
    }

    #no crashing and leveling involved
    #Need to take into consideration returning an empty vector of length zero, no selection
    else if (length(crashduration_header()) > 0 && crashduration_header() == "NA" &
             length(resource_header()) > 0 && resource_header() == "NA"){
      value = max(final_dataset()$EF)
    }

    #when crashing and/or leveling involved
    else {
      value = max(final_dataset()$EF)
    }
    msg <- "Target completion duration"

    if (!is.null(input$date_format)) {
      if (input$date_format){
        msg <- "Target completion date"
        
      }
    }
    infoBox(tags$h5(tags$b(value)),msg, icon = icon("calendar-alt"),color = 'purple')
  })

  output$resource_slider <- renderUI({
    # Check conditions separately to avoid the error
    if (is.null(input$file) & input$dataInputType=='file') {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (input$dataInputType=='manual' & input$manualDataDone==0) {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (input$dataInputType=='file' & input$manualDataDone==1) {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else {
      data_is_valid <- is.data.frame(cpm_processed_data()) && "Resource" %in% colnames(cpm_processed_data())
      manual_input_done <- input$dataInputType == "manual" && !is.null(input$manualDataDone)
      # Render only if data is valid and either file input or manual input is done
      if (data_is_valid && (input$dataInputType == "file" || manual_input_done)) {
        div(
          class = 'slider-container',
          sliderInput(
            inputId = 'resource',
            label = 'Resource Allocation',
            min = min_resource(),  
            max = cpm_max_resource(),
            value = cpm_max_resource(),
            step = 1
          )
        )
      } else {
        tags$div("Missing resource parameter", class = "warning_msg") 
      } 
    }
  })
  
  output$duration_slider <- renderUI({
    # Ensure a file is uploaded before proceeding
    # Currently manual dont accept crashing
    if (is.null(input$file) & input$dataInputType=='file') {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (input$dataInputType=='manual' & input$manualDataDone==0) {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (input$dataInputType=='file' & input$manualDataDone==1) {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else {
      data_is_valid <- is.data.frame(cpm_processed_data()) && "Crash Duration" %in% colnames(cpm_processed_data())
      manual_input_done <- input$dataInputType == "manual" && !is.null(input$manualDataDone)
      
      if (data_is_valid) {
        div(class = 'slider-container',
            sliderInput(inputId = 'duration',
                        label = "Target Duration for completion",
                        min=min_duration(), 
                        max=sum(cpm_processed_data()$Duration), 
                        value=duration_cpm(),
                        step=1) 
        )
      } else {
        tags$div("Missing crashing duration parameter", class = "warning_msg")  
      } 
    }
  })

  output$date_toggle <- renderUI ({
    req(data())
    req(is.data.frame(cpm_processed_data()))#bare minimum of 3 parameters input
    prettySwitch('date_format',"Date Format",value=FALSE,status = 'success',fill = TRUE,bigger = TRUE)
  })

  output$gantt_chart_plot <- renderUI ({
    if (is.null(input$file) & input$dataInputType=='file') {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (input$dataInputType=='manual' & input$manualDataDone==0) {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (input$dataInputType=='file' & input$manualDataDone==1) {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (!is.data.frame(cpm_processed_data())) {
      HTML('<span style="color:red; font-weight:bold;">Incorrect entries. Check the data inputs and parameter selections</span>')
    }
    else{
      renderPlotly(
        gantt_chart(final_dataset(),input$start_date,input$date_format)
      ) 
    }
  })

  output$date_selection <- renderUI ({
    req(data())
    req(is.data.frame(cpm_processed_data())) #bare minimum of 3 parameters input
    if (input$date_format) {
      date <- if (is.null(input$start_date)) Sys.Date() else input$start_date #to prevent constantly changing back to today's date due to renderUI
      airDatepickerInput('start_date',"Select Start Date",value = date)
    }
  })

  output$resource_chart <- renderUI ({
    if (is.null(input$file) & input$dataInputType=='file') {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (input$dataInputType=='manual' & input$manualDataDone==0) {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (input$dataInputType=='file' & input$manualDataDone==1) {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (!is.data.frame(cpm_processed_data())) {
      HTML('<span style="color:red; font-weight:bold;">Incorrect entries. Check the data inputs and parameter selections</span>')
    }
    
    else if (resource_header()=="NA") {
      tags$div("Missing resource parameter", class='warning_msg')
    }
    else if (is.numeric(final_dataset()$Resource)){
      renderPlotly(
        resource_chart(final_dataset(),input$start_date,input$date_format)
      )
    }
  })

  output$cost_chart <- renderUI ({
  
    if (is.null(input$file) & input$dataInputType=='file') {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (input$dataInputType=='manual' & input$manualDataDone==0) {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (input$dataInputType=='file' & input$manualDataDone==1) {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (!is.data.frame(cpm_processed_data())) {
      HTML('<span style="color:red; font-weight:bold;">Incorrect entries. Check the data inputs and parameter selections</span>')
    }
    
    else if (cost_header()=="NA") {
      tags$div("Missing cost parameter", class='warning_msg')
    }
    else if (is.numeric(final_dataset()$Cost)){
      renderPlotly(
        cost_chart(final_dataset(),input$start_date,input$date_format)
      )
    }
    else {
      tags$div("Cost Parameter is not of the correct format",class='warning_msg')
    }
  })
  

  output$final_table <- renderUI ({
    if (is.null(input$file) & input$dataInputType=='file') {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (input$dataInputType=='manual' & input$manualDataDone==0) {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (input$dataInputType=='file' & input$manualDataDone==1) {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (!is.data.frame(cpm_processed_data())) {
      HTML('<span style="color:red; font-weight:bold;">Incorrect entries. Check the data inputs and parameter selections</span>')
    }
    else {
      final_table <- final_dataset() %>% select(-c("cp")) %>% relocate("LS", .before = "LF")
      renderDT(final_table)
    }
  })
  
  
  # Render the CPM Network Diagram
  output$cpm_network_plot <- renderUI({
    if (is.null(input$file) & input$dataInputType=='file') {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (input$dataInputType=='manual' & input$manualDataDone==0) {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (input$dataInputType=='file' & input$manualDataDone==1) {
      HTML('<span style="color:red; font-weight:bold;">Missing data input</span>')
    }
    else if (!is.data.frame(cpm_processed_data())) {
      HTML('<span style="color:red; font-weight:bold;">Incorrect entries. Check the data inputs and parameter selections</span>')
    }
    else {
      network_data <- prepare_network_data(final_dataset())
      network <- forceNetwork(
        Links = network_data$links,
        Nodes = network_data$nodes,
        Source = "source",
        Target = "target",
        NodeID = "name",
        Group = "group",
        opacity = 0.8,
        zoom = TRUE,
        fontSize = 18,
        colourScale = JS("d3.scaleOrdinal()
                        .domain(['Root', 'Node'])
                        .range(['#FF6347', '#1f77b4']);")  # Color for Root: Tomato, Nodes: Default D3 color
      )
      
      # Add custom JavaScript to include smaller arrows for the links
      network <- onRender(network, '
    function(el, x) {
      // Select the SVG element
      var svg = d3.select(el).select("svg");
      
      // Append a marker definition for the arrows
      svg.append("defs").append("marker")
        .attr("id", "arrowhead")
        .attr("viewBox", "-0 -5 10 10")
        .attr("refX", 7)  // Adjusted to make the arrow smaller
        .attr("refY", 0)
        .attr("orient", "auto")
        .attr("markerWidth", 7)  // Adjusted to make the arrow smaller
        .attr("markerHeight", 7)  // Adjusted to make the arrow smaller
        .attr("xoverflow", "visible")
        .append("svg:path")
        .attr("d", "M 0,-5 L 10 ,0 L 0,5")
        .attr("fill", "#999")
        .style("stroke", "none");

      // Add the marker-end attribute to the links
      d3.select(el).selectAll(".link")
        .attr("marker-end", "url(#arrowhead)");
    }
  ')
      
      # Render the network plot in the UI
      network
    }
  })
  
  output$download_finaldata <- renderUI({
    if (is.null(input$file) & input$dataInputType == 'file') {
      return(NULL)
    } else if (input$dataInputType == 'manual' & input$manualDataDone == 0) {
      return(NULL)
    } else if (input$dataInputType == 'file' & input$manualDataDone == 1) {
      return(NULL)
    } else if (!is.data.frame(cpm_processed_data())) {
      return(NULL)
    } else {
      return(downloadButton('download_finaltable', 'Download schedule'))
    }
  })
  
  output$download_finaltable <- downloadHandler(
    filename = function() {
      paste('final_schedule_', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(final_dataset(), file, row.names = FALSE)
    }
  )
  
  #^^^^^^^^^^^^ DASHBOARD PAGE TAB ^^^^^^^^^^^^#
}

shinyApp(ui, server)
