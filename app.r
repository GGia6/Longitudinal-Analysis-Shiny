source("Functions.R")
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
## Upload up to 50 MB
options(shiny.maxRequestSize = 50 * 1024^2)


# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "Survey Wave Dashy",

  # uploading the big wave merged csv files
  tabPanel(
    "Data Upload",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload Survey ",
          multiple = FALSE,
          accept = c(".csv", ".xlsx")
        ),
        hr(),
        selectizeInput(
          inputId = "var_sum",
          label = "Select Variable",
          choices = NULL
        ),
        hr(),
        selectizeInput(inputId = "wave_filter", 
                    label = "Select Wave", 
                    choices = NULL,
                    multiple = TRUE)
                   
                    
        
      ),
      mainPanel(
        h4("Dataset Information"),
        DTOutput("table"),
        hr(),
        verbatimTextOutput("info")
      )
    )
  ),
  tabPanel("Wave Comparison",
    sidebarLayout(
      sidebarPanel(
        selectInput("Wave", "Select Wave(s):",
                    choices = NULL,
                    multiple = TRUE),
        hr(),
        selectizeInput("variable", "Select Variable:",
                    choices = NULL),
        hr(),
        radioButtons(
          inputId = "chart_type",
          label = "Select Graph Type",
          choices = c("Stacked Bar Graph" = "bar", "Line Graph"= "line", "Grouped Bar Graph"= "box"),
          selected = "bar"
        )
      ),
      mainPanel(
        h4("Wave Trends"),
        plotOutput("stackedPlot")
      )
    )
    
  ),
  tabPanel(
    "Compare Waves",
    plotOutput("plot")
  ),
  tabPanel(
    "Statistical Analysis",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "bayes_waveA",
          "WaveA:",
          choices = NULL
        ),
        selectInput("bayes_waveB",
                    "WaveB:",
                    choices = NULL),
        selectizeInput(
        "bayes_variable",
        "Binary Variable: ",
        choices = NULL),
        actionButton("run_bayes", "Run Bayseian Analysis" )),
      mainPanel(
        h4("Posterior Summary"),
        tableOutput("bayes_summary"),
        hr(),
        h4("comparison"),
        verbatimTextOutput("bayes_results"))
      )
    ),
  tabPanel(
    "Dashboard",
    dataTableOutput("dashboard")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ## Add the reactive element so that you can upload file
  df <- reactive({
    req(input$file)
    data <- read.csv(
      input$file$datapath,
      stringsAsFactors = FALSE
    )
    expected_col_names <- c("Wave", "MonthYear")
    given_col_names <- names(data)

    if (all(expected_col_names %in% given_col_names)) {
      showModal(
        modalDialog(
          title = "Success",
          p(paste(
            "Correct File Uploaded. File contains the following columns:",
            paste(expected_col_names, collapse = ", ")
          ))
        )
      )
    } else {
      showModal(
        modalDialog(
          title = "Error",
          p(paste(
            "Incorrect File Uploaded. Please make sure your file contains the following columns:",
            paste(expected_col_names, collapse = ", ")
          ))
        )
      )
    }
    data
  })
  observe({
    req(df())
    updateSelectizeInput(
      session = session,
      inputId = "var_sum",
      choices = names(df())
    )
  })
  
  observe({
    req(df())
    wave_choices<- c("All Waves"= "all", sort(unique(df()$Wave)))
    updateSelectizeInput(session = session,
                         inputId = "wave_filter",
                         choices = wave_choices,
                         selected = "all")
  })
  ## The element to preview the file on the data upload page
  output$table <- renderDT({
    req(df())
    req(input$wave_filter)
    
    filtered_df<- if (input$wave_filter == "all"){
      df()
    } else{
      df() %>% filter(Wave == input$wave_filter)
    }
    datatable(
      head(filtered_df, 100),
      options = list(pageLength = 10, scrollX = TRUE),
      selection = list(target = "column")
    ) 
  })
  ## Get a summary of variables
  output$info <- renderPrint({
    req(df(), input$var_sum)
    x <- df()[[input$var_sum]]
    if (is.character(x)) {
      x <- as.factor(x)
    }
    if (is.factor(x)) {
      cat("Frequency Table\n\n")
      print(table(x, useNA = "ifany"))
    } else if ((is.numeric(x) || is.integer(x)) &&
      length(unique(na.omit(x))) <= 10) {
      x <- factor(x)
      cat("Frequency Table\n\n")
      print(table(x, useNA = "ifany"))
    } else if (is.numeric(x) || is.integer(x)) {
      cat("Numerical Summary\n\n")
      print(summary(x))
    } else {
      cat("Unsupported Variable Type")
    }
  })
  ## Creating the wave and variable comparison charts
  observe({
    req(df())
    updateSelectInput(
      session = session,
      inputId = "Wave",
      choices = levels(as.factor(df()$Wave)),
      selected = levels(as.factor(df()$Wave))
    )
    updateSelectizeInput(
      session = session,
      inputId = "variable",
      choices = setdiff(names(df()), c("Wave", "MonthYear", "IDNO"))
    )
  })
#Code for plots and graphs  
   output$stackedPlot <- renderPlot({
    req(df(), input$Wave, input$variable, input$chart_type)
    
    ## Identify wave and response variable
    datawave <- df() %>%
      filter(Wave %in% input$Wave)
    datawave$response <- as.factor(datawave[[input$variable]])

    plotdf <- datawave %>%
      group_by(Wave, response) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(Wave) %>%
      mutate(proportion = n / sum(n))

    if (input$chart_type == "bar") {
      ggplot(plotdf, aes(x = Wave, y = proportion, fill = response)) +
        geom_bar(stat = "identity", position = "fill") +
        labs(
          x = "Wave", y = "Proportion",
          fill = input$variable
        ) +
        theme_classic()
    } else if (input$chart_type == "line") {
      ggplot(plotdf, aes(x = Wave, y = proportion, color = response, group = response)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        labs(
          x = "Wave",
          y = "Proportion",
          color = input$variable
        ) +
        theme_classic()
    }else if (input$chart_type == "box"){
      ggplot(plotdf, aes(x = Wave, y = proportion, fill = factor(response))) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        labs(x = "Wave", y = "Proportion of Response", fill = "Response") +
        theme_classic()
    }
    ## create stacked bar plot of responses by wave, as well as line graph, include more plot types later suhc as box or scatterplot
  })
  ##update inputs for stat analysis 
  ## Stat analysis to code for binary variables 
  observe({
    req(df())
   
     updateSelectInput(
      session = session,
      inputId = "bayes_waveA",
      choices = sort(unique(df()$Wave)))
     updateSelectInput(
       session = session,
       inputId = "bayes_waveB",
       choices = sort(unique(df()$Wave)))
     binary_vars<- names(df())[sapply(df(), function(x){
       detect_var(x)== "binary var"
     })]
     
     updateSelectizeInput(
       session = session,
       inputId = "bayes_variable",
       choices = binary_vars
     )})
  
  bayes_results<- eventReactive(input$run_bayes, {
    req(input$bayes_waveA,
        input$bayes_waveB,
        input$bayes_variable)
    result<-bayes_binaryvar(
      data = df(),
      wave_var = "Wave",
      outcome_var = input$bayes_variable,
      waveA = input$bayes_waveA,
      WaveB = input$bayes_waveB
    )
    print(result)
    
    result
  })
  output$bayes_summary<- renderTable({
    req(bayes_results())
    bayes_results()$summary
  })
  ##End of binary analysis code excluding the text comparisons 
  ##Text outputs interpreting comparison
  
  output$bayes_results<-renderPrint({
    
    probs<-paste("Posterior probability that", input$bayes_waveB,
        "has a larger success proportion than", input$bayes_waveA,
        ":", bayes_results()$Probability_greater)
   
    
   
    
    meandiff<-paste("Posterior mean difference:", bayes_results()$mean_difference)
    

    
    confi<-paste("95% Credible Interval:", bayes_results()$lower_diff, ",",
          bayes_results()$upper_diff)
    
    cat(probs, "\n")
    cat(meandiff, "\n")
    cat(confi, "\n")
    
  })
  #End of text output code 
  
}

# Run the application
shinyApp(ui = ui, server = server)
