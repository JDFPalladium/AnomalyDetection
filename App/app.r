library(shiny)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title = "Aomaly Detection"),
  dashboardSidebar(
    sidebarMenu(
      #### Recommender Menu ####
      menuItem("Recommender", tabName = "recommender",
               tags$br(),
               tags$b("Data Upload"),
                        textInput("OU", label = " ", value = "Enter OU..."),
                        textInput("Year", label = " ", value = "Enter data year..."),
                        textInput("Quarter", label = " ", value = "Enter data quarter..."),
                        tags$br(),
                        fileInput("file1", "Choose CSV File",
                                  multiple = TRUE,
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
                 switchInput(inputId = "obs", value = TRUE)),
        menuItem("Sex", tabName = "sex",startExpanded = TRUE,
                 "Each obs. compared against all",
                 tags$br(),
                 "obs. of the same sex",
                 switchInput(inputId = "sex", value = TRUE)),
        menuItem("Age", tabName = "age",
                 "Each obs. compared against all", tags$br(),
                 "obs. of the same age group",
                 startExpanded = TRUE,
                 switchInput(inputId = "age", value = TRUE)),
        
                "Select the analyses to run with MER", tags$br(),
                "data aggregated at the facility", tags$br(),
        
        menuItem("Facility", tabName = "facility",startExpanded = TRUE,
                 "Each obs. compared against all",
                 tags$br(),
                 "obs.",
                 switchInput(inputId = "facility", value = TRUE)),
        menuItem("PSNU", tabName = "psnu",startExpanded = TRUE,
                 "Each obs. compared against all",
                 tags$br(),
                 "obs. of the same PSNU",
                 switchInput(inputId = "psnu", value = TRUE)),
        
        "Switch ON to discount indicators", tags$br(),
        "with very low values.",tags$br(), 
        menuItem("Minimum Threshold", tabName = "min_thresh",startExpanded = TRUE,
                 switchInput(inputId = "min_thresh", value = TRUE)),
        
        "Switch ON if you want outputs to ", tags$br(),
        "include both anomalous and non-anomalous", tags$br(),
        "non-anomalous obs.",
        menuItem("Return", tabName = "return",startExpanded = TRUE,
            
                 switchInput(inputId = "return", value = TRUE))
        ),
      #### Time Series Menu ####
      menuItem("Times Series", tabName = "time",
               tags$br(),
               tags$b("Data Upload"),
               textInput("OU", label = " ", value = "Enter OU..."),
               textInput("Year", label = " ", value = "Enter data year..."),
               textInput("Quarter", label = " ", value = "Enter data quarter..."),
               tags$br(),
               fileInput("file1", "Choose CSV File",
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
  ),
  dashboardBody()
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
}

# Complete app with UI and server components
shinyApp(ui, server)

