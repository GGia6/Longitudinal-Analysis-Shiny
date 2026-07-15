
library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "Survey Wave Dashy",
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Home",
      verbatimTextOutput("contents")
    ),
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
          selectInput(
            inputId = "var_sum",
            label = "Select Variable",
            choices = NULL
          )
        ),
        mainPanel(
          h4("Dataset Information"),
          tableOutput("table"),
          hr(),
          verbatimTextOutput("info")
        )
      )
    ),
    tabPanel(
      "Survey Summary",
      verbatimTextOutput("summary")
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
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
## Add the reactive element so that you can upload file   
  df <- reactive({
    req(input$file)
    read.csv(
      input$file$datapath,
      stringsAsFactors = FALSE)
    })
  observe({
    req(df())
    updateSelectInput(
      session = session,
      inputId = "var_sum",
      choices = names(df())
    )
  })
## The element to preview the file on the data upload page   
   output$table<- renderTable({
     req(df())
     head(df())
   })
  ##Get a summary of 
    output$info<- renderPrint({
      req(input$var_sum)
      summary(df()[[input$var_sum]])
    })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
