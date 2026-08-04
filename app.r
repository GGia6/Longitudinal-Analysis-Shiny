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
library(scales)
library(bslib)
library(viridis)
library(shinycssloaders)
## Upload up to 50 MB
options(shiny.maxRequestSize = 50 * 1024^2)

##Make company theme colors for shiny 
theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  
  # Brand colors
  primary   = "#D7191C",  # Company Red
  secondary = "#858789",  # Dark Gray
  success   = "#9FC7DE",  # Blue
  info      = "#F9A25A",  # Orange
  warning   = "#F8A798",  # Light Coral
  danger    = "#D7191C",
  
  # Background
  bg = "#FFFFFF",
  fg = "#2B2B2B",
  
  # Fonts
  base_font = font_google("Open Sans"),
  heading_font = font_google("Open Sans")
)

# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = theme,
  title = "Survey Wave Explorer",
  
  # uploading the big wave merged csv files
  tabPanel(
    "Data Upload",
    sidebarLayout(
      sidebarPanel(
        tags$div(
          style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
          tags$strong("Warning: "),
          "This tool only accepts .sav files with a numeric unlabeled Wave column."
        ),
        hr(),
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
        withSpinner(DTOutput("table"), type = 8, color = "#D7191C")
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
               withSpinner(plotOutput("stackedPlot"), type = 8, color = "#D7191C"),
               downloadButton("downloadStacked", "Download Plot"),
               br(),
               hr(),
               h4("Missingness Summary"),
               DTOutput("missingSummary")
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
                    multiple = TRUE),
        tags$div(
          style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
          tags$strong("Warning: "),
          "For model accuracy, use Wave Comparison to deselect Wave(s) where survey question wasn't asked."
        ),
        hr(),
        tags$div(
          style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
          tags$strong("Note: "),
          "Significant p-vals highlighted in gray. High Odds Ratios highlighted in orange. 
                  Low Odds Ratios highlighted in blue."
        )
        
      ),
      
      mainPanel( 
        withSpinner(DT::dataTableOutput("ordinal_table"), type = 8, color = "#D7191C"),
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
                            multiple = TRUE),
                tags$div(
                  style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
                  tags$strong("Warning: "),
                  "For model accuracy, use Wave Comparison to deselect Wave(s) where survey question wasn't asked."
                ),
                hr(),
                tags$div(
                  style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
                  tags$strong("Note: "),
                  "Significant p-vals highlighted in gray. High Odds Ratios highlighted in orange. 
                  Low Odds Ratios highlighted in blue."
                )),
              mainPanel(
                withSpinner(DT::dataTableOutput("glm_datatable"), type = 8, color = "#D7191C"),
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
                                        "Text Interpretations" = "text_int")),
               tags$div(
                 style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
                 tags$strong("Note: "),
                 "Text Interpretations exclude all Wave:Response pairs that aren't statistically significant."
               )),
             mainPanel(
               conditionalPanel(condition = "input.interprets == 'predi_plot'",
                                withSpinner(plotOutput("predict_plot"), type = 8, color = "#D7191C"),
                                downloadButton("downloadPlot", "Download Plot"),
                                hr(),
                                h4("Predicted Probability Metrics per Wave"),
                                DT::dataTableOutput("predict_table")),
               conditionalPanel(condition = "input.interprets == 'text_int'",
                                uiOutput("interpretation"))
             )
           )
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
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
      choices = names(df())
    )
  })

  observe({
    req(df())

    updateSelectizeInput(
      session = session,
      inputId = "wave_filter",
      choices = levels(as_factor(df()$Wave)),
      selected = levels(as_factor(df()$Wave))
    )
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
      options = list(dom = "Bfrtip", buttons = list(
        list(
          extend = "copy",
          exportOptions = list(modifier = list(page = "all"))
        ),
        list(
          extend = "csv",
          exportOptions = list(modifier = list(page = "all"))
        ),
        list(
          extend = "excel",
          exportOptions = list(modifier = list(page = "all"))
        ),
        list(
          extend = "pdf",
          exportOptions = list(modifier = list(page = "all"))
        )
      ), pageLength = 10, scrollX = TRUE),
      class = c("display", "nowrap"),
      selection = list(target = "column"),
      width = "400px"
    )
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
      choices = levels(haven::as_factor(df()$Wave)),
      selected = levels(haven::as_factor(df()$Wave))
    )
    updateSelectizeInput(
      session = session,
      inputId = "variable",
      choices = setdiff(names(df()), c("Wave", "IDNO"))
    )
  })
  # Code for plots and graphs
  
  okabe_ito <- c("#F8A798","#F9A25A",  "#D7191C", "#9FC7DE",
                 "#2B7EB6", "#25418c", "#BCBCBC", "#858789")
  
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
        scale_fill_manual(values = okabe_ito)+
        labs( title = "Wave Response Trend: Stacked Histogram",
          x = "Wave", y = "Proportion of Response",
          fill = input$variable
        ) +
        theme_classic()
    } else if (input$chart_type == "line") {
      ggplot(plotdf, aes(x = Wave, y = proportion, color = response, group = response)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_color_manual(values = okabe_ito) + 
        labs( title = "Wave Response Trend: Line Graph",
          x = "Wave",
          y = "Proportion of Response",
          color = input$variable
        ) +
        theme_classic()
    } else if (input$chart_type == "box") {
      ggplot(plotdf, aes(x = Wave, y = proportion, fill = factor(response))) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        scale_fill_manual(values = okabe_ito)+
        labs( title = "Wave Response Trend: Grouped Bar Graph", 
              x = "Wave", y = "Proportion of Response", fill = "Response") +
        theme_classic()
    }
    ## create stacked bar plot of responses by wave, as well as line graph, include more plot types later suhc as box or scatterplot
  })

  ## Download Stacked plot
  output$stackedPlot <- renderPlot({
    stackedPlot_reactive()
  })

  output$downloadStacked <- downloadHandler(
    filename = function() {
      paste0("stackedPlot_", input$chart_type, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file,
        plot = stackedPlot_reactive(), device = "png",
        width = 8, height = 6, dpi = 300
      )
    }
  )
  ## Make missingness summary at the bottom of the plots
  missingSummary_reactive <- reactive({
    req(input$variable, input$Wave)
    
    datawave <- df() %>%
      filter(haven::as_factor(Wave) %in% input$Wave)%>%
      mutate(Wave = haven::as_factor(Wave)) 
    
    var_vals <- datawave[[input$variable]]
    
    # Pull the value-label mapping (e.g. c("Refused (vol.)" = 98, "Don't know (vol.)" = 99))
    val_labels <- attr(var_vals, "labels")
    
    # Dynamically find whatever numeric code corresponds to each missing label,
    # regardless of whether it's 8/9 or 98/99 for this particular variable
    refused_code  <- val_labels[grepl("refus", names(val_labels), ignore.case = TRUE)]
    dontknow_code <- val_labels[grepl("don.t know|dk", names(val_labels), ignore.case = TRUE)]
    
    var_num <- as.numeric(haven::zap_labels(var_vals))
    
    datawave %>%
      mutate(.varnum = var_num) %>%
      group_by(Wave) %>%
      summarise(
        n_total     = n(),
        n_na        = sum(is.na(.varnum)),
        n_refused   = sum(.varnum %in% refused_code, na.rm = TRUE),
        n_dontknow  = sum(.varnum %in% dontknow_code, na.rm = TRUE),
        n_missing   = n_na + n_refused + n_dontknow,
        pct_missing = round(100 * n_missing / n_total, 1)
      )
  })
  
  output$missingSummary <- renderDT({
   
     ms <- missingSummary_reactive()
     
     ms_display <- ms %>%
       mutate(
         `% Missing` = paste0(pct_missing, "%"),
         `Refused (98)` = n_refused,
         `Don't Know (99)` = n_dontknow,
         `True NA` = n_na,
         `Total N` = n_total
       ) %>%
       select(Wave, `% Missing`, `True NA`, `Refused (98)`, `Don't Know (99)`, `Total N`)
     
     datatable(
       ms_display,
       rownames = FALSE,
       options = list(
         dom = "t",          # just the table, no search/pagination clutter
         pageLength = -1,     # show all rows
         ordering = TRUE
       ),
       class = "display"
     ) %>%
       formatStyle(
         "% Missing",
         backgroundColor = styleInterval(c(10, 25), c("#e8f5e9", "#fff3e0", "#ffebee")) # green/yellow/red by severity
       )
  })


  ## Start of categorical analysis
  # Update categorical selectuon
  observe({
    req(df())

    ordinal_vars <- names(df())[sapply(df(), function(x) {
      detect_var(x) == "categorical var"
    })]
    updateSelectInput(
      session = session,
      inputId = "analysis_var",
      choices = ordinal_vars
    )

    updateSelectInput(
      session = session,
      inputId = "Wave_cat",
      choices = levels(haven::as_factor(df()$Wave)),
      selected = levels(haven::as_factor(df()$Wave))
    )
  })
  ## get ordinal analysis  results
  ordinal_results <- reactive({
    req(df())
    req(input$analysis_var)
    req(input$Wave_cat)
    
    datawave_cat <- df() %>%
      filter(haven::as_factor(Wave) %in% input$Wave_cat)
    
    ordinal_analysis(
      data = datawave_cat,
      outcome = input$analysis_var
    )
  })
  ## make summary page of estimates and odds ratios
  output$ordinal_table <- DT::renderDataTable({
    req(ordinal_results())

    datatable(ordinal_results()$results,
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",  buttons = list(
          list(
            extend = "copy",
            exportOptions = list(modifier = list(page = "all"))
          ),
          list(
            extend = "csv",
            exportOptions = list(modifier = list(page = "all"))
          ),
          list(
            extend = "excel",
            exportOptions = list(modifier = list(page = "all"))
          ),
          list(
            extend = "pdf",
            exportOptions = list(modifier = list(page = "all"))
          )
        ),
        pageLength = 10,
        scrollX = TRUE
      )
    ) %>%
      formatStyle("Odds_Ratio",
                  backgroundColor = styleInterval(5, c("white", "#F8A798"))) %>%
      formatStyle("p_value", backgroundColor = styleInterval(0.05, c("#BCBCBC", "white"))) %>%
      formatStyle("Odds_Ratio", backgroundColor = styleInterval(0.40, c("#9FC7DE", "white")))
  })


  ## Start of assumptions analysis

  ## get brant test results

  brant_test <- reactive({
    req(df())

    req(input$analysis_var)

    brant_func(
      data = df(),
      outcome = input$analysis_var
    )
  })

  ## Output brant testresults at bottom of ord tab

  output$brant_message <- renderUI({
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
      session = session,
      inputId = "glm_var",
      choices = ordinal_vars
    )

    updateSelectInput(
      session = session,
      inputId = "glm_wave",
      choices = levels(haven::as_factor(df()$Wave)),
      selected = levels(haven::as_factor(df()$Wave))
    )
  })

  ## Doing the vglm analysis func

  glm_results <- reactive({
    req(df())
    req(input$glm_var)
    req(input$glm_wave)

    datawave_cat <- df() %>%
      filter(haven::as_factor(Wave) %in% input$glm_wave)

    glm_analysis(
      data = datawave_cat,
      outcome = input$glm_var
    )
  })

  ## Gettign vglm table

  output$glm_datatable <- DT::renderDataTable({
    req(glm_results())

    datatable(glm_results()$vglm_table,
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip", buttons = list(
          list(
            extend = "copy",
            exportOptions = list(modifier = list(page = "all"))
          ),
          list(
            extend = "csv",
            exportOptions = list(modifier = list(page = "all"))
          ),
          list(
            extend = "excel",
            exportOptions = list(modifier = list(page = "all"))
          ),
          list(
            extend = "pdf",
            exportOptions = list(modifier = list(page = "all"))
          )
        ),
        pagelength = 10,
        scrollX = TRUE
      )
    ) %>%
      formatStyle("Odds_Ratio",
        backgroundColor = styleInterval(5, c("white", "#F8A798"))) %>%
      formatStyle("p_value", backgroundColor = styleInterval(0.05, c("#BCBCBC", "white"))) %>%
      formatStyle("Odds_Ratio", backgroundColor = styleInterval(0.40, c("#9FC7DE", "white")))
  })


  ## Printing the vglm assumption underneath the table
  output$glm_assum <- renderUI({
    req(glm_results())

    tags$div(
      tags$br(),
      tags$p(glm_results()$Error)
    )
  })

  ## Print predictive plot on interpret tab

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
  

  output$predict_plot <- renderPlot({
    predict_plot_reactive()
  })
  ## Add download option for the predictive plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("predict_plot_", input$test_type, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file,
        plot = predict_plot_reactive(), device = "png",
        width = 8, height = 6, dpi = 300
      )
    }
  )
  ## Start of text interpretations code

  interpretation_reactive <- reactive({
    req(input$test_type)

    if (input$test_type == "OA") {
      req(ordinal_results())

      res <- ordinal_results()$results
      sig <- res %>% filter(p_value < 0.05)

      if (nrow(sig) == 0) {
        return("No statistically significant wave effects were found.")
      }

      top_row <- sig[which.max(sig$Odds_Ratio), ]
      bottom_row <- sig[which.min(sig$Odds_Ratio), ]

      interpret_row <- function(row) {
        or <- round(row$Odds_Ratio, 2)
        wave <- row$Predictor
        if (or < 1) {
          pct <- round((1 - or) * 100, 1)
          sprintf(
            "In %s, the odds of being in a higher response category are %.2f times the baseline wave — the odds of being in a higher ordinal category (i.e. levels >2 or >3) decrease by about %.1f%%.",
            wave, or, pct
          )
        } else {
          pct <- round((or - 1) * 100, 1)
          sprintf(
            "In %s, the odds of being in a higher response category are %.2f times the baseline wave — the odds of being in a higher ordinal category (i.e. levels >2 or >3) increase by about %.1f%%.",
            wave, or, pct
          )
        }
      }

      if (nrow(sig) == 1) {
        interpret_row(sig)
      } else {
        paste0(
          "<b>Largest Significant Effect:</b> ", interpret_row(top_row), "<br><br>",
          "<b>Smallest Significant Effect:</b> ", interpret_row(bottom_row)
        )
      }
    } else {
      req(glm_results())
      tab <- glm_results()$vglm_table
      sig <- tab %>% filter(p_value < 0.05)

      if (nrow(sig) == 0) {
        return("No statistically significant wave effects were found.")
      }

      top_row <- sig[which.max(sig$Odds_Ratio), ]
      bottom_row <- sig[which.min(sig$Odds_Ratio), ]

      interpret_row <- function(row, direction) {
        or <- round(row$Odds_Ratio, 2)
        wave <- row$Predictor
        trend <- if (direction == "higher") "a sharp decline" else "a sharp increase"
        sprintf(
          "In %s, the odds of choosing a  response level greater than this one were %.2f times %s than the baseline — 
          by this Wave there was %s in people choosing this as their response.",
          wave, or, direction, trend
        )
      }

      if (nrow(sig) == 1) {
        interpret_row(sig, if (sig$Odds_Ratio >= 1) "higher" else "lower")
      } else {
        paste0(
          "<b>Highest Significant Odds Ratio:</b> ", interpret_row(top_row, "higher"), "<br><br>",
          "<b>Lowest Significant Odds Ratio:</b> ", interpret_row(bottom_row, "lower")
        )
      }
    }
  })
##End of interpretations code

  ## Print interpretation in the tab 
  output$interpretation <- renderUI({
    HTML(interpretation_reactive())
  })
  
  predict_table_reactive <- reactive({
    req(input$test_type)
    
    if (input$test_type == "OA") {
      req(ordinal_results(), input$Wave_cat)
      filtered <- df_labelled() %>% filter(Wave %in% input$Wave_cat)
      model <- ordinal_results()$model
      pred_type <- "prob"
    } else {
      req(glm_results(), input$glm_wave)
      filtered <- df_labelled() %>% filter(Wave %in% input$glm_wave)
      model <- glm_results()$model
      pred_type <- "response"
    }
    
    wave_var <- "Wave"
    
    new_data <- data.frame(
      Wave = factor(sort(unique(filtered[[wave_var]])),
                    levels = levels(as.factor(filtered[[wave_var]])))
    )
    names(new_data) <- wave_var
    
    probs <- predict(model, newdata = new_data, type = pred_type)
    
    if (is.null(dim(probs))) {
      probs_df <- data.frame(Probability = probs)
    } else {
      probs_df <- as.data.frame(probs)
    }
    
    probs_df$Wave <- new_data[[wave_var]]
    
    probs_df %>%
      tidyr::pivot_longer(cols = -Wave,
                          names_to = "Response_Category",
                          values_to = "Probability") %>%
      tidyr::pivot_wider(names_from = Response_Category,
                         values_from = Probability) %>%
      dplyr::mutate(dplyr::across(-Wave, ~ scales::percent(.x, accuracy = 0.1)))
  })
  
  output$predict_table <- DT::renderDataTable({
    
     datatable(predict_table_reactive(), 
               extensions = "Buttons",
               options = list(
                 dom = "Bfrtip", buttons = list(
                   list(
                     extend = "copy",
                     exportOptions = list(modifier = list(page = "all"))
                   ),
                   list(
                     extend = "csv",
                     exportOptions = list(modifier = list(page = "all"))
                   ),
                   list(
                     extend = "excel",
                     exportOptions = list(modifier = list(page = "all"))
                   ),
                   list(
                     extend = "pdf",
                     exportOptions = list(modifier = list(page = "all"))
                   )
                 ),
                 pagelength = 10,
                 scrollX = TRUE
               ))
    })
  
  
  


  
}

# Run the application
shinyApp(ui = ui, server = server)
