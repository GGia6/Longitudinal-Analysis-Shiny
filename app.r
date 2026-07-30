source("Functions.R")
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(ordinal)
library(gofcat)
library(periscope2)
library(VGAM)
library(haven)
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
                  accept = c(".sav", ".csv") ## copy and  change to .csv
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
        verbatimTextOutput("info"),
        hr(),
        DTOutput("table")
      )
    )
  ),
  ## Wave comparison tab 
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
               plotOutput("stackedPlot"),
               downloadButton("downloadStacked", "Download Plot")
             )
           )
           
  ),
  #ordinal analy tab 
  tabPanel(
    "Ordinal Categorical Analysis",
    sidebarLayout(
      
      sidebarPanel(
        
        selectInput(
          "analysis_var",
          "Select Survey Question",
          choices = NULL),
        
        selectInput("Wave_cat", "Select Wave(s) to Analyze",
                    choices = NULL,
                    multiple = TRUE)
        
      ),
      
      mainPanel( 
        DT::dataTableOutput("ordinal_table"),
        hr(),
        h4("Proportional Odds Assumption Check"),
        htmlOutput("brant_message")
      )
    )
    
  ),
  ## VGLM Analysis
  tabPanel( "Generalized Categorical Analysis",
            sidebarLayout(
              sidebarPanel(
                selectizeInput(inputId = "glm_var",
                               label = "Select Survey Question",
                               choices = NULL),
                hr(),
                selectInput(inputId = "glm_wave",
                            label = "Select Waves to Analyze",
                            choices = NULL,
                            multiple = TRUE)),
              mainPanel(
                DT::dataTableOutput("glm_datatable"),
                hr(),
                h4("Error Assumption Check"),
                htmlOutput("glm_assum"))
            )
  ),
  ##Plots and Interpretations Tab
  tabPanel("Intepretations",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 inputId = "test_type",
                 label = "Select working test type",
                 choices = c("Ordinal Analysis"= "OA", "Generalized Linear Model" = "GLM")),
               hr(),
               radioButtons(inputId = "interprets",
                            label = "Select Interpretation Type",
                            choices = c("Predictive plot"= "predi_plot",
                                        "Text Interpretations" = "text_int"))),
             mainPanel(
               conditionalPanel(condition = "input.interprets == 'predi_plot'",
                                plotOutput("predict_plot"),
                                downloadButton("downloadPlot", "Download Plot")),
               conditionalPanel(condition = "input.interprets == 'text_int'",
                                htmlOutput("takeaways"))
             )
           )
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ## Add the reactive element so that you can upload file
  df <- reactive({
    req(input$file)
    data <- read_sav( ## read_sav from haven package 
      input$file$datapath
    )
    expected_col_names <- c("Wave", "IDNO")
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
  ## Adding df labelled so that display tables and graphs can display labels
  df_labelled <- reactive({
    req(df())
    
    df() %>%
      mutate(across(where(haven::is.labelled), haven::as_factor))
  })
  
  observe({
    req(df())
    updateSelectizeInput(
      session = session,
      inputId = "var_sum",
      choices = names(df()))
  })
  
  observe({
    req(df())
    
    updateSelectizeInput(session = session,
                         inputId = "wave_filter",
                         choices = levels(as_factor(df()$Wave)),
                         selected = levels(as_factor(df()$Wave)))
  })
  ## The element to preview the file on the data upload page
  output$table <- renderDT({
    req(df())
    req(input$wave_filter)
    
    filtered_df <- df() %>%
      filter(as_factor(Wave) %in% input$wave_filter) %>%
      mutate(across(where(haven::is.labelled), haven::as_factor))
    
    datatable(
      head(filtered_df, 1000),
      extensions = "Buttons",
      options = list(dom= "Bfrtip", buttons = c("csv", "excel", "pdf", "copy"), pageLength = 10, scrollX = TRUE),
      class = c("display","nowrap"),
      selection = list(target = "column"),
      width = "400px")
  })
  ## Get a summary of variables
  output$info <- renderPrint({
    req(df(), input$var_sum)
    x <- df_labelled()[[input$var_sum]]
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
      choices = setdiff(names(df()), c("Wave", "IDNO"))
    )
  })
  #Code for plots and graphs  
  stackedPlot_reactive <- reactive({
    req(df(), input$Wave, input$variable, input$chart_type)
    datawave <- df_labelled() %>%
      filter(as_factor(Wave) %in% input$Wave)
    datawave$response <- datawave[[input$variable]]
    
    plotdf <- datawave %>%
      group_by(Wave, response) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(Wave) %>%
      mutate(proportion = n / sum(n))
    
    if (input$chart_type == "bar") {
      ggplot(plotdf, aes(x = Wave, y = proportion, fill = response)) +
        geom_bar(stat = "identity", position = "fill") +
        labs(
          x = "Wave", y = "Proportion of Response",
          fill = input$variable
        ) +
        theme_classic()
    } else if (input$chart_type == "line") {
      ggplot(plotdf, aes(x = Wave, y = proportion, color = response, group = response)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        labs(
          x = "Wave",
          y = "Proportion of Response",
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
  
  ##Download Stacked plot 
  output$stackedPlot<- renderPlot({
    stackedPlot_reactive()
  })
  
  output$downloadStacked<- downloadHandler(
    filename = function(){
      paste0("stackedPlot_", input$chart_type, "_", Sys.Date(), ".png" )
    },
    content = function(file){
      ggsave(file, plot = stackedPlot_reactive(), device = "png", 
             width = 8, height = 6, dpi = 300)
    }
  )
  
  ##update inputs for stat analysis 
  
  
  ## Start of categorical analysis 
  #Update categorical selectuon
  observe({
    
    req(df())
    
    ordinal_vars <- names(df())[sapply(df(), function(x) {
      detect_var(x) == "categorical var"
    })]
    updateSelectInput(
      session =session,
      inputId = "analysis_var",
      choices = ordinal_vars
    )
    
    updateSelectInput(
      session = session,
      inputId = "Wave_cat",
      choices = levels(as.factor(df()$Wave)),
      selected = levels(as.factor(df()$Wave))
    )
    
  })
  ##get ordinal analysis  results 
  ordinal_results <- reactive({
    
    req(df())
    req(input$analysis_var)
    req(input$Wave_cat)
    
    datawave_cat <- df() %>%
      filter(Wave %in% input$Wave_cat)
    
    ordinal_analysis(
      data = datawave_cat,
      outcome = input$analysis_var
    )
    
  })
  ## make summary page of estimates and odds ratios 
  output$ordinal_table <- DT::renderDataTable({
    
    req(ordinal_results())
    
    datatable( ordinal_results()$results,
               extensions = "Buttons",
               options = list(dom= "Bfrtip", buttons = c("csv", "excel", "pdf", "copy"),
                              pagelength = 10,
                              scrollX = TRUE
               ))%>% 
      formatStyle("p_value", backgroundColor = styleInterval(0.05, c("#8B0000", "white")))
    
  })
  
  
  ## Start of assumptions analysis
  
  ##get brant test results 
  
  brant_test<- reactive({
    
    req(df())
    
    req(input$analysis_var)
    
    brant_func(data = df(),
               outcome = input$analysis_var)
  })
  
  ## Output brant testresults at bottom of ord tab
  
  output$brant_message<- renderUI({
    req(brant_test())
    
    tags$div(
      tags$br(),
      tags$p(brant_test()$Error)
    )
  })
  ## Done with brant results 
  
  
  ## VGLM Analysis reactive for sidebar
  observe({
    
    req(df())
    
    ordinal_vars <- names(df())[sapply(df(), function(x) {
      detect_var(x) == "categorical var"
    })]
    updateSelectInput(
      session =session,
      inputId = "glm_var",
      choices = ordinal_vars
    )
    
    updateSelectInput(
      session = session,
      inputId = "glm_wave",
      choices = levels(as.factor(df()$Wave)),
      selected = levels(as.factor(df()$Wave))
    )
    
  })
  
  ## Doing the vglm analysis func
  
  glm_results <- reactive({
    
    req(df())
    req(input$glm_var)
    req(input$glm_wave)
    
    datawave_cat <- df() %>%
      filter(Wave %in% input$glm_wave)
    
    glm_analysis(
      data = datawave_cat,
      outcome = input$glm_var
    )
    
  })
  
  ##Gettign vglm table 
  
  output$glm_datatable <- DT::renderDataTable({
    
    req(glm_results())
    
    datatable(glm_results()$vglm_table,
              extensions = "Buttons",
              options = list(dom= "Bfrtip", buttons = c("csv", "excel", "pdf", "copy"),
                             pagelength = 10,
                             scrollX = TRUE
              ))%>% 
      formatStyle("Odds_Ratio", 
                  backgroundColor = styleInterval(5, c("white", "#8B0000")))
  })
  
  
  ## Printing the vglm assumption underneath the table 
  output$glm_assum<- renderUI({
    req(glm_results())
    
    tags$div(
      tags$br(),
      tags$p(glm_results()$Error)
    )
  })
  
  ##Print predictive plot on interpret tab 
  
  predict_plot_reactive <- reactive({
    req(input$test_type)
    if (input$test_type == "OA") {
      req(ordinal_results(), input$Wave_cat)
      filtered <- df_labelled() %>% filter(Wave %in% input$Wave_cat)
      oa_pred_plot(model = ordinal_results()$model, data = filtered)
    } else {
      req(glm_results(), input$glm_wave)
      filtered <- df_labelled() %>% filter(Wave %in% input$glm_wave)
      glm_pred_plot(model = glm_results()$model, data = filtered)
    }
  })
  
  output$predict_plot<- renderPlot({
    predict_plot_reactive()
  })
  
  output$downloadPlot<- downloadHandler(
    filename = function(){
      paste0("predict_plot_", input$test_type, "_", Sys.Date(), ".png" )
    },
    content = function(file){
      ggsave(file, plot = predict_plot_reactive(), device = "png", 
             width = 8, height = 6, dpi = 300)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
