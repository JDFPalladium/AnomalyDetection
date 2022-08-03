# #Utils Packages#
# library(dplyr)
# library(tidyr)
# library(modi)
# library(reshape2)
# library(openxlsx)
# library(data.table)

#App Packages#
library(shiny)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title = "Aomaly Detection"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("type", "Type",
                  c('Recommender', 'Time Series')),
      #### Recommender Menu ####
      conditionalPanel(
        condition = "input.type == 'Recommender'",
        menuItem(tabName = "recommender", startExpanded = TRUE,
                 tags$br(),
                 tags$b("Data Upload"),
                 textInput("OU", label = " ", value = "Enter OU..."),
                 textInput("Year", label = " ", value = "Enter data year..."),
                 textInput("Quarter", label = " ", value = "Enter data quarter..."),
                 tags$br(),
                 fileInput("file1", "Choose MER file",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 tags$b("Switch ON if you want to run,"), tags$br(),
                 tags$b("OFF if you do not"),tags$br(),tags$br(),
                 
                 "Select the analyses to run with MER", tags$br(),
                 "data disaggregated by sex and age", tags$br(), 
                 
                 menuItem("Observations", tabName = "observations", startExpanded = TRUE,
                          "Each obs. compared against all",
                          tags$br(),
                          "obs.",
                          switchInput(inputId = "obs", value = FALSE)
                 ),
                 menuItem("Sex", tabName = "sex",startExpanded = TRUE,
                          "Each obs. compared against all",
                          tags$br(),
                          "obs. of the same sex",
                          switchInput(inputId = "sex", value = FALSE)),
                 menuItem("Age", tabName = "age",
                          "Each obs. compared against all", tags$br(),
                          "obs. of the same age group",
                          startExpanded = TRUE,
                          switchInput(inputId = "age", value = FALSE)),
                 
                 "Select the analyses to run with MER", tags$br(),
                 "data aggregated at the facility", tags$br(),
                 
                 menuItem("Facility", tabName = "facility",startExpanded = TRUE,
                          "Each obs. compared against all",
                          tags$br(),
                          "obs.",
                          switchInput(inputId = "facility", value = FALSE)),
                 menuItem("PSNU", tabName = "psnu",startExpanded = TRUE,
                          "Each obs. compared against all",
                          tags$br(),
                          "obs. of the same PSNU",
                          switchInput(inputId = "psnu", value = FALSE)),
                 
                 "Switch ON to discount indicators", tags$br(),
                 "with very low values.",tags$br(), 
                 menuItem("Minimum Threshold", tabName = "min_thresh",startExpanded = TRUE,
                          switchInput(inputId = "min_thresh", value = FALSE)),
                 
                 "Switch ON if you want outputs to ", tags$br(),
                 "include both anomalous and non-anomalous", tags$br(),
                 "non-anomalous obs.",
                 menuItem("Return", tabName = "return",startExpanded = TRUE,
                          
                          switchInput(inputId = "return", value = FALSE))
        )
      ),
      #### Time Series Menu ####
      conditionalPanel(
        condition = "input.type == 'Time Series'",
        menuItem(tabName = "time", startExpanded = TRUE,
                 tags$br(),
                 tags$b("Data Upload"),
                 textInput("OU", label = " ", value = "Enter OU..."),
                 textInput("Year", label = " ", value = "Enter data year..."),
                 textInput("Quarter", label = " ", value = "Enter data quarter..."),
                 tags$br(),
                 fileInput("file2", "Choose files",
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 numericInput("datasets", "Number of datasets:", 10, min =1, max = 10),
                 "Switch ON to discount indicators", tags$br(),
                 "with very low values.",tags$br(), 
                 menuItem("Minimum Threshold", tabName = "min_thresh",startExpanded = TRUE,
                          switchInput(inputId = "min_thresh", value = TRUE)),
                 
                 "Switch ON if you want outputs to ", tags$br(),
                 "include both anomalous and non-anomalous", tags$br(),
                 "non-anomalous obs.",
                 menuItem("Return", tabName = "return",startExpanded = TRUE,
                          
                          switchInput(inputId = "return", value = TRUE))
        )
      )
      
    )
  ),
  dashboardBody(
    h2(textOutput('title')),
    conditionalPanel(
      condition = "output.rec_sum",
      box(title = "Summary Table:..", width = 12,
          "Summary Table or Score Card")
    ),
    conditionalPanel(
      condition = "output.rec1",
      box(title = "Observations: Each obs. compared against all obs.", width = 12,
          "All Observations Disaggregate table")
    ),
    conditionalPanel(
      condition = "output.rec2",
      box(title = "Sex: Each obs. compared against all obs. of the same sex", width = 12,
          "Sex Disaggregate Table")
    ),
    conditionalPanel(
      condition = "output.rec3",
      box(title = "Age: Each obs. compared against all obs. of the same age group", width = 12,
          "Age Disaggregate Table")
    ),
    conditionalPanel(
      condition = "output.rec4",
      box(title = "Facility: Each obs. compared against all obs.", width = 12,
          "Facility Disaggregate Table")
    ),
    conditionalPanel(
      condition = "output.rec5",
      box(title = "PSNU: Each obs. compared against all obs. of the same PSNU", width = 12,
          "PSNU Disaggregate Table")
    ),
    conditionalPanel(
      condition = "output.ts1",
      box(title = "Times Series: ", width = 12,
          "...")
    )
    
    
  )
)

server <- function(input, output) {
  
  output$title <- renderText({
    if(input$type=="Recommender"){
      paste("Recommender Anomaly Detetction")
    }
    else if(input$type=="Time Series"){
      paste("Time Series Anomaly Detection")
    }
  })
  
  output$plot <- renderPlot({
    plot(rnorm(100))
  }) 
  
  # observeEvent(switchInput$obs, {
  #   toggle("plot")
  # })
  
  #RECOMMENDER OBSERVATION Each observation compared against all observations
  output$rec_sum <- reactive({
    input$type == 'Recommender' # Add whatever condition you want here. Must return TRUE or FALSE
  })
  
  outputOptions(output, 'rec_sum', suspendWhenHidden = FALSE)
  
  output$rec1 <- reactive({
    input$obs == 'TRUE' & input$type == 'Recommender' # Add whatever condition you want here. Must return TRUE or FALSE
  })
  
  outputOptions(output, 'rec1', suspendWhenHidden = FALSE)
  
  #RECOMMENDER SEX
  output$rec2 <- reactive({
    input$sex == 'TRUE' & input$type == 'Recommender' # Add whatever condition you want here. Must return TRUE or FALSE
  })
  
  outputOptions(output, 'rec2', suspendWhenHidden = FALSE)
  
  #RECOMMENDER AGE
  output$rec3 <- reactive({
    input$age == 'TRUE' & input$type == 'Recommender' # Add whatever condition you want here. Must return TRUE or FALSE
  })
  
  outputOptions(output, 'rec3', suspendWhenHidden = FALSE)
  
  #RECOMMENDER FACILITY
  output$rec4 <- reactive({
    input$facility == 'TRUE' & input$type == 'Recommender' # Add whatever condition you want here. Must return TRUE or FALSE
  })
  
  outputOptions(output, 'rec4', suspendWhenHidden = FALSE)
  
  #RECOMMENDER PSNU
  output$rec5 <- reactive({
    input$psnu == 'TRUE' & input$type == 'Recommender' # Add whatever condition you want here. Must return TRUE or FALSE
  })
  
  outputOptions(output, 'rec5', suspendWhenHidden = FALSE)
  
  
  
  #Time Series
  output$ts1 <- reactive({
    input$type == 'Time Series' # Add whatever condition you want here. Must return TRUE or FALSE
  })
  
  outputOptions(output, 'ts1', suspendWhenHidden = FALSE)
  
}
# Complete app with UI and server components
shinyApp(ui, server)

