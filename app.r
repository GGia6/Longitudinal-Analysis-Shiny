
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
        )
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
        selectizeInput("variable", "Select Variable:",
                    choices = NULL)
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
    verbatimTextOutput("static")
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
    data<- read.csv(
      input$file$datapath,
      stringsAsFactors = FALSE
    )
    expected_col_names <- c("Wave", "MonthYear")
    given_col_names <- names(data)
    
    if(all(expected_col_names %in% given_col_names)) {
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
  ## The element to preview the file on the data upload page
  output$table <- renderDT({
    req(df())
    datatable(
      head(df(), 100),
      options= list(pageLength = 10, scrollX = TRUE),
      selection = list(target= 'column')
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
  ##Creating the wave and variable comparison charts 
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
  output$stackedPlot<- renderPlot({
    req(df(), input$Wave, input$variable)
    
    datawave<- df()%>%
      filter(Wave %in% input$Wave)
    datawave$response<- as.factor(datawave[[input$variable]])
    
    plotdf<- datawave %>%
      group_by(Wave, response)%>%
      summarise(n= n(), .groups = "drop")%>%
      group_by(Wave) %>%
      mutate(proportion = n/sum(n))
    
    ggplot(plotdf, aes(x= Wave, y = proportion, fill = response))+
      geom_bar(stat = "identity", position = "fill")+
      labs(x = "Wave", y= "Proportion",
           fill = input$variable)+ 
      theme_classic()
  })
  
  
  }

# Run the application
shinyApp(ui = ui, server = server)
