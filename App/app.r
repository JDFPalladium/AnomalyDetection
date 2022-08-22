#Utils Packages#
library(dplyr)
library(tidyr)
library(modi)
library(reshape2)
library(openxlsx)
library(data.table)

#App Packages#
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rintrojs)
library(vroom)

#Global Settings
#In file statement upload size#
options(shiny.maxRequestSize = 2000*1024^2)

ui <- dashboardPage(
  dashboardHeader(title = "Anomaly Detection",
                  tags$li(class = "dropdown", id= "download",
                          dropMenu(
                            dropdownButton("Info",  icon = icon('download')),
                            conditionalPanel(condition = "input.type == 'Recommender'",
                            actionButton("download_rec", "Click to download Recommender outputs.")
                            ),
                            conditionalPanel(condition = "input.type == 'Time Series'",
                            actionButton("download_rec", "Click to download Times Series outputs.")
                            ),
                            placement = "bottom",
                            arrow = TRUE
                            )
                          
                  ),
                  # dropdownMenu(
                  #   type = "notifications", 
                  #   headerText = strong("Download"), 
                  #   icon = icon("download"), 
                  #   badgeStatus = NULL,
                  #   notificationItem(
                  #     actionButton("download", "Click to download all selected"),
                  #     icon = icon("download")
                  #   )),
                  tags$li(
                    a(
                      strong("INFO"),
                      height = 40,
                      href = "https://github.com/JDFPalladium/AnomalyDetection/blob/main/README.md",
                      title = "",
                      target = "_blank",
                      ),
                    class = "dropdown"
                  )
                    ),
  ####Recommender Sidebar ####
  dashboardSidebar(
    introjsUI(),
    sidebarMenu(
      fluidRow(column(12, offset =1,
        conditionalPanel(
          condition = "input.type == 'Recommender'",
          tags$br(),
      actionButton("help", HTML("Press for Recommender <br/> instructions"))))),
      fluidRow(column(12, offset =1,
                      conditionalPanel(
                        condition = "input.type == 'Time Series'",
                        tags$br(),
                        actionButton("help2", HTML("Press for Time Series <br/> instructions"))))),
      fluidRow(column(12,
                      div(id="step4",
          selectInput("type", "Type",
                  c('Recommender', 'Time Series'))
          ))),
      #### Recommender Menu ####
      
        conditionalPanel(condition = "input.type == 'Recommender'",
          fluidRow(column(12,div(id="step2",
        menuItem(tabName = "recommender", startExpanded = TRUE,
                 tags$br(),
                 fluidRow(column(12,
                                 div(id="step5",
                 tags$b("Data Upload"),
                 numericInput("year", label = "Data Fiscal Year", value = 2022),
                 selectInput("quarter", "Quarter",
                             c('Q1','Q2','Q3','Q4')),
                 tags$br(),
                 fileInput("file1", "Choose MER file",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv",
                                      ".xlsx",
                                      ".txt"))
                 )))
        )))),
                 fluidRow(column(8, offset =2,
                                 div(id="step6",
                 actionButton("recdatacheck", "Run Data Check"),
                                 ))),
                  tags$br(),
        #Step 7
                
                 fluidRow(column(12, offset=1,
                                 tags$b("Switch ON if you want to run,"), tags$br(),
                                  tags$b("OFF if you do not"),tags$br(),tags$br()
                 )),
        
        menuItem(tabName="disaggregations", id="step7",startExpanded = TRUE,
                 tags$br(),
                 tags$h5("Observation Disaggregations"),
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
                          switchInput(inputId = "age", value = FALSE))
                 ),
                 tags$br(),
                   
        menuItem(tabName="disaggregations",startExpanded = TRUE, id="step8",
                 tags$br(),
                 tags$h5("Facility Disaggregations"),
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
                          switchInput(inputId = "psnu", value = FALSE))
        )
                 ,
        menuItem(tabName = "settings", startExpanded = TRUE, collapsible=FALSE,
                 tags$br(),
                 tags$h5("Detection Settings"),
                 menuItem("Minimum Threshold", tabName = "min_thresh", id = "step9",startExpanded = TRUE,
                          "Switch ON to discount indicators", tags$br(),
                          "with very low values.",tags$br(), 
                          numericInput(inputId = "min_thresh", label = "", value = 10)),
                 tags$br(),
                 menuItem("Return", tabName = "return", id = "step10", startExpanded = TRUE, offset=2,
                          "Switch ON if you want outputs to ", tags$br(),
                          "include both anomalous and", tags$br(),
                          "non-anomalous obs.",         
                          switchInput(inputId = "return", value = FALSE))
                        )
        )
        
    
      ,
      #### Time Series Menu ####
      conditionalPanel(
        condition = "input.type == 'Time Series'",
        menuItem(tabName = "time", startExpanded = TRUE,
                 tags$br(),
                 fluidRow(column(12,
                                 div(id="tsstep4",
                 tags$b("Data Upload"),
                 numericInput("year", label = "Data Fiscal Year", value = 2022),
                 selectInput("quarter", "Quarter",
                             c('Q1','Q2','Q3','Q4')),
                 tags$br(),
                 fileInput("file2", "Choose files",
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv"))
                 )))
                 ),
                 fluidRow(column(12,
                                 div(id="tsstep5",
                 numericInput("datasets", "Number of datasets:", 10, min =1, max = 10)
                 ))),
          menuItem(tabName = "settings", startExpanded = TRUE,  collapsible=FALSE,
                  tags$br(),
                  tags$h5("Detection Settings"),
                 menuItem("Minimum Threshold", tabName = "min_thresh", id="tsmintresh", startExpanded = TRUE,
                          "Switch ON to discount indicators", tags$br(),
                          "with very low values.",tags$br(), 
                                                    numericInput(inputId = "min_thresh", label = "", value = 10)),
                 menuItem("Return", tabName = "return", id="tsreturn", startExpanded = TRUE,
                          "Switch ON if you want outputs to ", tags$br(),
                          "include both anomalous and non-anomalous", tags$br(),
                          "non-anomalous obs.",
                          switchInput(inputId = "return", value = TRUE))
        )
      )
      
    )
  ),
  dashboardBody(
    fluidRow(div(id="body_title",
                 (h2(textOutput('title')))
                 )),
    fluidRow(div(id="rec_datachecks",
      conditionalPanel(id="rec_datachecks",
      condition = "output.rec_data",
      box(title = "Data Checks", width = 12,
          "Summary Table or Score Card",
          textOutput('year_check'))
    )
    )),
    fluidRow(div(id="rec_summarytable",
      conditionalPanel(
      condition = "output.rec_sum",
      box(title = "Summary Table:..", width = 12, 
          "Summary Table or Score Card")
    )
    )),
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
    fluidRow(div(id="ts_summary",
      conditionalPanel(
      condition = "output.ts1",
      box(title = "Times Series: ", width = 12,
          "...")
    )
    ))
    
    
  )
)

server <- function(input, output, session) {
  #### Recommender IntroJS 
  steps <- reactive(
    data.frame(
      element=c(".main-header", ".sidebar-toggle", "#step4", "#step5", "#step6", "#step7", "#step8", "#step9", "#step10", "#body_title", "#rec_datachecks", "#rec_summarytable", "#center", "#download"),
      intro=c(
        HTML("The original Anomaly Detection tools were created as a series of R studio scripts. This ShinyApp converts the same functions of the scripts into an online application.<br/><br/> <b>These are the Recommender Tool instructions.</b> <br/> Click next to continue with instructions or click skip and change the 'Type' for Time Series instructions."),
        "This is a button that allows to close and open the sidebar. <br/>The sidebar menu is where you will upload data, define disaggregations and other detection settings.",
        "Here you can choose which type of analysis you want to run either Recommender or Times Series.<br/><br/> The sidebar menu options will change based on this selection",
        "Here you will upload your data for a Recommender analysis. <br/> 1. Enter the four-digit fiscal year. <br/> 2. Choose the correct quarter for the data. <br/> 3. Locate the file on your computer. <br/> <br/> NOTE: Only .csv, .xlsx, or .txt MER data files work function properly.",
        "Click the Run Data Check button after uploading your data. This will double check the file to make sure the named variables are included. <br/><br/> A notice will appear in the Data Checks box to confirm data consistency.",
        "Here are the three primary disggregation options: Observation, Sex, and Age",
        "Here are the two additional disggregation options: Facility, and Age",
        "After the disaggregations are selected, there are two more settings to define. <br/> First, you can decide to discount indicators with very low values. This is automaticaly set to ten but can be altered depending on the context",
        "Second, you can chose whether or not to display both anomalous and non-anomalous outputs in the final tables. <br/> For large data sets, we would highly recommend to leave this setting off.",
        "Here are all of the output will appear. Each box will contain a different output.",
        "This is the box for Data Checks. After uploading and running the data check, the content here will tell you in the appropriate variables are included in the uploaded dataset.",
        "This is the Summary Table which will provide a broad overview of the detection. This box will always appear.",
        "All other outputs for each of the disaggregations will appear ONLY when the switch is turned ON. The the rest of the outputs and their display boxes will not populate until the corresponding swtiches are turned on.",
        "After you have defined all desired disaggregations and settings, click here to download all outputs. <br/> <br/> NOTE: Before clicking, make sure all output boxes display the corresponding data table!"
        ),
      position=c("right", "bottom", "right", "right", "right", "right", "right", "right", "right", "top", "bottom","bottom", "bottom", "left")
    )
  )
  observeEvent(input$help,
               introjs(session,
                       options = list(steps=steps(),
                                      "nextLabel"="Next",
                                      "prevLabel"="Previous",
                                      "skipLabel"="Skip"
                       )
                       # ,
                       # events = list("oncomplete"=I('alert("Done")'))
               )
  )
  
  #### Time Series IntroJS
  steps2 <- reactive(
    data.frame(
      element=c(".main-header", ".sidebar-menu", ".sidebar-toggle", "#tsstep4", "#tsstep5", "#tsmintresh", "#tsreturn", "#ts_summary", "#download"),
      intro=c(
        "The original Anomaly Detection tools were created as a series of R studio scripts. This ShinyApp converts the same functions of the scripts into an online application.<br/><br/> <b>These are the Time Series Tool instructions.</b> <br/> <br/> Click next to continue with instructions or click skip and change the 'Type' for Recommender instructions.",
        "This is the main interactive sidebar menu. This is where you will upload data sets and define your detection settings.",
        "This button allows you to close and open the sidebar.",
        "Here you will upload your data for a Time Series analysis. <br/> 1. Enter the four-digit fiscal year. <br/> 2. Choose the correct quarter for the data. <br/> 3. Locate the files on your computer. Make sure all files are in the same folder on your computer. <br/> <br/> NOTE: Only .csv, .xlsx, or .txt MER data files work function properly. For ease of use, make sure all files are the same format.",
        "Enter the number of datasets/files you uploaded in the previous step. The default number is 10 and the minimum number requried is 2.",
        "After the the data upload, there are two settings to define. <br/> First, you can decide to discount indicators with very low values. This is automaticaly set to ten but can be altered depending on the context",
        "Second, you can chose whether or not to display both anomalous and non-anomalous outputs in the final tables. <br/> For large data sets, we would highly recommend to leave this setting off.",
        "Second, you can chose whether or not to display both anomalous and non-anomalous outputs in the final tables. <br/> For large data sets, we would highly recommend to leave this setting off.",
        "Click here to download all Time Series outputs. <br/> <br/> NOTE: Before clicking, make sure all output boxes display the corresponding data table!"
        ),
      position=c("right", "right", "bottom", "bottom", "right", "right", "right", "bottom", "left")
    )
  )
  observeEvent(input$help2,
               introjs(session,
                       options = list(steps=steps2(),
                                      "nextLabel"="Next",
                                      "prevLabel"="Previous",
                                      "skipLabel"="Skip"
                       )
                       )
  )
  
  output$title <- renderText({
    if(input$type=="Recommender"){
      paste("Recommender Anomaly Detetction")
    }
    else if(input$type=="Time Series"){
      paste("Time Series Anomaly Detection")
    }
  })
  
  output$title <- renderText({
    if(input$type=="Recommender"){
      paste("Recommender Anomaly Detetction")
    }
    else if(input$type=="Time Series"){
      paste("Time Series Anomaly Detection")
    }
  })
  
  RecData <- reactive({
    file <-input$file1
    ext <- tools::file_ext(input$file1$name)
    switch(ext,
           csv = vroom::vroom(input$file1$datapath, delim = ","),
           tsv = vroom::vroom(input$file1$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .txt file")
    )
    
  })
  
  observeEvent(input$recdatacheck, {
    
    runChecks <- reactive({
      function(dat=RecData(),
                          year_for_analysis=input$year
                          # qtr_for_analysis = input$quarter
                          # type_check = type,
                          # facility_strings_tmp = facility_strings
                          ){
      
      # Check to confirm if fiscal year selected by user for analysis exists in the dataset
      if(!year_for_analysis %in% unique(dat$fiscal_year)){
        stop(
          output$year_check <- renderText({
            paste("Please confirm the fiscal year selected is included in the file uploaded.")
          })
        )
      }
      
      # Check to confirm age_groups is exactly "Over/Under 15" or "Five Year"
      # if(!age_groups %in% c("Over/Under 15", "Five Year")) {
      #   stop("Please confirm that your age_groups entry is an exact match with either 'Over/Under 15' or 'Five Year' (case-sensitive)")
      # }
      # 
      # # Check to confirm if quarter selected by user for analysis exists in the dataset
      # if(!qtr_for_analysis %in% names(dat)){
      #   stop("Please confirm the quarter selected is included in the file uploaded.")
      # }
      # 
      # # Check to confirm if other required variables exist in the dataset
      # if(any(!c("sitename","psnu","facility","indicator","numeratordenom",
      #           "disaggregate","ageasentered","sex","primepartner") %in% names(dat))){
      #   stop("Please confirm the file selected contains the required columns: 
      #    sitename,psnu,facility,indicator,numeratordenom,disaggregate,ageasentered,sex,primepartner")
      # }
      
      # If user chooses to run analysis by facility type, check to confirm if facility type
      # descriptions exist in the dataset
      # if(type_check == TRUE){
      #   for(i in facility_strings_tmp){
      #     if(sum(grepl(i, unique(tolower(dat$facility))))==0){
      #       print(sprintf("Facility type %s not found. All types should be lowercase.", i))
      #     }
      #   }
      # }
      
    }
    runChecks(dat=RecData(),
              year_for_analysis=input$year
              # qtr_for_analysis =input$quarter
              # type_check = type,
              # facility_strings_tmp = facility_strings
              )
  }
  )
  })
  
  #RECOMMENDER DATA CHECK
  output$rec_data <- reactive({
    input$type == 'Recommender' # Add whatever condition you want here. Must return TRUE or FALSE
  })
  
  outputOptions(output, 'rec_data', suspendWhenHidden = FALSE)
  
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

