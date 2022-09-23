#Utils Packages#
library(dplyr)
library(tidyr)
library(modi)
library(reshape2)
library(openxlsx)
library(data.table)
library(forecast)
library(imputeTS)
library(zoo)

#App Packages#
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rintrojs) #Tutorial package
library(htmltools)
library(tidyverse)
library(shinyalert) #Used in the data checks portion
library(shinycssloaders) #Used from the loading spinning symbols throughout
library(shinyBS)
library(writexl)
library(DT)

#Global Settings
#In file statement upload size#
options(shiny.maxRequestSize = 2000 * 1024 ^ 2)

source("utils.R")

# Specify which variables identify a unique observation
keys_disag <-
  c('facility', 'ageasentered', 'sex', 'kp', 'psnu', 'primepartner')
keys_facility <- c('facility', 'psnu', 'primepartner')
keysts <-
  c("psnu",
    "facility",
    "primepartner",
    "indicator",
    "lower99",
    "upper99",
    "outlier")
quarterly_indicators <-
  c(
    "HTS_INDEX",
    "HTS_RECENT",
    "HTS_TST",
    "HTS_TST_NEG",
    "HTS_TST_POS",
    "PMTCT_ART",
    "PMTCT_EID",
    "PMTCT_HEI_POS",
    "PMTCT_HEI_POS_2MO",
    "PMTCT_HEI_POS_ART",
    "PMTCT_STAT",
    "PMTCT_STAT_POS",
    "PrEP_CURR",
    "PrEP_NEW",
    "TB_ART",
    "TB_STAT",
    "TB_STAT_POS",
    "TX_CURR",
    "TX_ML",
    "TX_NET_NEW",
    "TX_NEW",
    "TX_PVLS",
    "TX_RTT",
    "VMMC_CIRC"
  )

ui <- dashboardPage(
  dashboardHeader(
    title = "Anomaly Detection",
    tags$li(
      class = "dropdown",
      id = "download",
      dropMenu(
        dropdownButton("Info",  icon = icon('download')),
        conditionalPanel(
          condition = "input.type == 'Recommender'",
          downloadButton("download_rec_sum", "Download SUMMARY Recommender outputs."),
          tags$br(),
          tags$br(),
          downloadButton("download_rec_all", "Download ALL Recommender outputs.")
        ),
        conditionalPanel(
          condition = "input.type == 'Time Series'",
          downloadButton("download_ts_sum", "Download SUMMARY Times Series outputs."),
          tags$br(),
          tags$br(),
          downloadButton("download_ts_all", "Download All Times Series outputs.")
        ),
        placement = "bottom",
        arrow = TRUE
      )
      
    ),
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
      fluidRow(column(
        12,
        offset = 1,
        conditionalPanel(condition = "input.type == 'Recommender'",
                         tags$br(),
                         actionButton("help", HTML(
                           "Press for instructions"
                         )))
      )),
      fluidRow(column(
        12,
        offset = 1,
        conditionalPanel(condition = "input.type == 'Time Series'",
                         tags$br(),
                         actionButton("help2", HTML(
                           "Press for instructions"
                         )))
      )),
      fluidRow(column(12,
                      div(
                        id = "step4",
                        selectInput("type", "Type",
                                    c('Recommender', 'Time Series'))
                      ))),
      #### Recommender Menu ####
      
      conditionalPanel(
        condition = "input.type == 'Recommender'",
        fluidRow(column(12, div(
          id = "step2",
          menuItem(
            tabName = "recommender",
            startExpanded = TRUE,
            tags$br(),
            fluidRow(column(
              12,
              div(
                id = "step5",
                tags$b("Data Upload"),
                numericInput("year", label = "Data Fiscal Year", value = 2021),
                selectInput("quarter", "Quarter",
                            c('qtr1', 'qtr2', 'qtr3', 'qtr4')),
                fileInput(
                  "file1",
                  "Choose MER file",
                  multiple = FALSE,
                  accept = c(".csv",
                             ".xlsx",
                             ".txt")
                )
                
              )
            ))
          )
        ))),
        fluidRow(column(
          8, offset = 2,
          div(id = "step6",
              actionButton("recdatacheck", "Run Data Check"), )
        )),
        fluidRow(column(
          8, offset = 2,
          div(id = "runmodel",
              actionButton("recrun", "Run Model"), )
        )),
        menuItem(
          tabName = "disaggregations",
          id = "step7",
          startExpanded = TRUE,
          tags$br(),
          "Select the analyses to run:",
          tags$br(),
          menuItem(
            "Observations",
            tabName = "observations",
            startExpanded = TRUE,
            "Each obs. compared against all",
            tags$br(),
            "obs.",
            switchInput(inputId = "obs", value = TRUE)
          ),
          menuItem(
            "Sex",
            tabName = "sex",
            startExpanded = TRUE,
            "Each obs. compared against all",
            tags$br(),
            "obs. of the same sex",
            switchInput(inputId = "sex", value = FALSE)
          ),
          menuItem(
            "Age",
            tabName = "age",
            "Each obs. compared against all",
            tags$br(),
            "obs. of the same age group",
            startExpanded = TRUE,
            switchInput(inputId = "age", value = FALSE)
          ),
          menuItem(
            "Facility",
            tabName = "facility",
            startExpanded = TRUE,
            "Each facility compared against all",
            tags$br(),
            "other facilities",
            switchInput(inputId = "facility", value = FALSE)
          )
        ),
        tags$br(),
        menuItem(
          tabName = "settings",
          startExpanded = TRUE,
          collapsible = FALSE,
          tags$br(),
          tags$h5("Detection Settings"),
          menuItem(
            "Minimum Threshold",
            tabName = "min_thresh",
            id = "step9",
            startExpanded = TRUE,
            "Switch ON to discount indicators",
            tags$br(),
            "with very low values.",
            tags$br(),
            numericInput(
              inputId = "min_thresh",
              label = "",
              value = 10
            )
          ),
          menuItem(
            "Return",
            tabName = "return",
            id = "step10",
            startExpanded = TRUE,
            offset = 2,
            "Switch ON if you want outputs to ",
            tags$br(),
            "include both anomalous and",
            tags$br(),
            "non-anomalous obs.",
            switchInput(inputId = "return", value = TRUE)
          ),
          menuItem(
            "Funding",
            tabName = "funding",
            id = "step11",
            startExpanded = TRUE,
            offset = 2,
            "ON limits to USAID-funded sites.",
            switchInput(inputId = "recfunder", value = TRUE)
          )
          
        )
      )
      
      
      ,
      #### Time Series Menu ####
      conditionalPanel(
        condition = "input.type == 'Time Series'",
        menuItem(
          tabName = "time",
          startExpanded = TRUE,
          tags$br(),
          fluidRow(column(
            12,
            div(
              id = "tsstep4",
              tags$b("Data Upload"),
              numericInput("tsyear", label = "Data Fiscal Year", value = 2021),
              selectInput("tsquarter", "Quarter",
                          c('qtr1', 'qtr2', 'qtr3', 'qtr4')),
              tags$br(),
              fileInput(
                "file2",
                "Choose files",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
              )
            )
          ))
        ),
        fluidRow(column(
          8, offset = 2,
          div(id = "tsstep6",
              actionButton("tsdatacheck", "Run Data Check"), )
        )),
        fluidRow(column(
          8, offset = 2,
          div(id = "tsstep6.5",
              actionButton("tsrun", "Run Model"), )
        )),
        tags$br(),
        menuItem(
          tabName = "settings",
          startExpanded = TRUE,
          collapsible = FALSE,
          tags$br(),
          tags$h5("Detection Settings"),
          menuItem(
            "Minimum Threshold",
            tabName = "min_thresh",
            id = "tsmintresh",
            startExpanded = TRUE,
            "Do not flag as anomalies",
            tags$br(),
            "values below: .",
            tags$br(),
            numericInput(
              inputId = "tsminthresh",
              label = "",
              value = 10
            )
          ),
          menuItem(
            "Return",
            tabName = "return",
            id = "tsreturn",
            startExpanded = TRUE,
            "Switch ON if you want outputs to ",
            tags$br(),
            "include both anomalous and non-anomalous",
            tags$br(),
            "non-anomalous obs.",
            switchInput(inputId = "tsreturn", value = TRUE)
          ),
          menuItem(
            "Funding",
            tabName = "funding",
            id = "tsfunder",
            startExpanded = TRUE,
            offset = 2,
            "ON limits to USAID-funded sites.",
            switchInput(inputId = "tsfunder", value = TRUE)
          )
        )
      )
      
    )
  ),
  dashboardBody(
    fluidRow(div(id = "body_title",
                 (h2(
                   textOutput('title')
                 )))),
    conditionalPanel(condition = "input.type == 'Recommender'",
                     fluidRow(div(
                       id = "rec_summary",
                       tabsetPanel(
                         tabPanel(
                           h4(icon("list-check"), "Summary"),
                           conditionalPanel(
                             condition = "output.rec6",
                             box(
                               title = "Summary of outliers with data run at disaggregated levels.",
                               width = 12,
                               "Disaggregate Summary Table",
                               collapsible = TRUE,
                               shinycssloaders::withSpinner(DT::DTOutput('rec6'))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.rec7",
                             box(
                               title = "Summary of outliers with data run at facility level.",
                               width = 12,
                               "Facility Summary Table",
                               collapsible = TRUE,
                               shinycssloaders::withSpinner(DT::DTOutput('rec7'))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.rec8",
                             box(
                               title = "Facility Scorecard",
                               width = 12,
                               "Scorecard Table",
                               collapsible = TRUE,
                               shinycssloaders::withSpinner(DT::DTOutput('rec8'))
                             )
                           )
                         ),
                         tabPanel(
                           h4(icon("circle-user"), "Observation"),
                           conditionalPanel(
                             condition = "output.rec1",
                             box(
                               title = "Observations: Each obs. compared against all obs.",
                               width = 12,
                               "All Observations Disaggregate table",
                               collapsible = TRUE,
                               shinycssloaders::withSpinner(DT::DTOutput('rec1'))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.rec2",
                             box(
                               title = "Sex: Each obs. compared against all obs. of the same sex",
                               width = 12,
                               "Sex Disaggregate Table",
                               collapsible = TRUE,
                               shinycssloaders::withSpinner(DT::DTOutput('rec2'))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.rec3",
                             box(
                               title = "Age: Each obs. compared against all obs. of the same age group",
                               width = 12,
                               "Age Disaggregate Table",
                               collapsible = TRUE,
                               shinycssloaders::withSpinner(DT::DTOutput('rec3'))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.rec4",
                             box(
                               title = "Facility: Each obs. compared against all obs.",
                               width = 12,
                               "Facility Disaggregate Table",
                               collapsible = TRUE,
                               shinycssloaders::withSpinner(DT::DTOutput('rec4'))
                             )
                           )
                         )
                       )
                     ))),
    conditionalPanel(condition = "input.type == 'Time Series'",
                     fluidRow(div(
                       id = "ts_summary",
                       tabsetPanel(
                         tabPanel(
                           h4(icon("list-check"), "Summary"),
                           conditionalPanel(
                             condition = "output.ts5",
                             box(
                               title = "Summary of Outliers",
                               width = 12,
                               collapsible = TRUE,
                               "Summary of Outliers",
                               shinycssloaders::withSpinner(DT::DTOutput('ts5'))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.ts6",
                             box(
                               title = "Facility Scorecard",
                               width = 12,
                               collapsible = TRUE,
                               "Facility Scorecard",
                               shinycssloaders::withSpinner(DT::DTOutput('ts6'))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.ts7",
                             box(
                               title = "IP Scorecard",
                               width = 12,
                               collapsible = TRUE,
                               "IP Scorecard",
                               shinycssloaders::withSpinner(DT::DTOutput('ts7'))
                             )
                           )
                         ),
                         tabPanel(
                           h4(icon("circle-user"), "Observation"),
                           conditionalPanel(
                             condition = "output.ts2",
                             box(
                               title = "Observations: ARIMA Outputs",
                               width = 12,
                               collapsible = TRUE,
                               "ARIMA Outputs",
                               shinycssloaders::withSpinner(DT::DTOutput('ts2'))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.ts3",
                             box(
                               title = "Observations: ETS Outputs",
                               width = 12,
                               collapsible = TRUE,
                               "ETS Outputs",
                               shinycssloaders::withSpinner(DT::DTOutput('ts3'))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.ts4",
                             box(
                               title = "Observations: STL Outputs",
                               width = 12,
                               collapsible = TRUE,
                               "STL Outputs",
                               shinycssloaders::withSpinner(DT::DTOutput('ts4'))
                             )
                           )
                         )
                       )
                     )))
    
    
  )
)

server <- function(input, output, session) {
  observeEvent("", {
    showModal(modalDialog(includeHTML("intro_text.html"),
                          easyClose = TRUE))
  })
  
  observeEvent(input$intro, {
    removeModal()
  })
  
  
  #### Recommender IntroJS
  steps <- reactive(data.frame(
    element = c(
      ".main-header",
      "#step5",
      "#step6",
      "#step7",
      "#runmodel",
      "#rec_summary",
      "#download"
    ),
    intro = c(
      HTML(
        "This sidebar menu is the main interactive element of the dashboard. Here you define the settings of the anomaly detection, upload and validate your data, and run the model."
      ),
      "Here you will upload your data for a Recommender analysis. <br/> 1. Enter the four-digit fiscal year. <br/> 2. Choose the correct quarter for the data. <br/> 3. Locate the file on your computer. <br/> <br/> NOTE: Only .csv, .xlsx, or .txt MER data files work function properly.",
      "Click the Run Data Check button after uploading your data. This will double check the file to make sure the named variables are included.",
      "The primary disggregation options include All (disaggregated by both sex and age), Sex, Age, and Facilitiy level.  <br/><br/> Switch each disaggregation to 'ON' if you want to use that disaggregation.",
      "After all disaggregations have been selected for the model, click the here to run the model and to produce outputs. Click this button once.",
      "All outputs are organized in two tabs.<br/><br/> The Summary tab will provide a data table for high level review of anomalous data. <br/><br/> The Observations tab will provide specific outputs based on the observation, age, and sex.<br/><br/> Outputs will only be generated if their corresponding switch has been turned on.",
      "After reviewing all outputs, click here to download either summary outputs or all outputs."
    ),
    position = c("right", "right", "right", "right", "right", "bottom", "left")
  ))
  observeEvent(input$help,
               introjs(
                 session,
                 options = list(
                   steps = steps(),
                   "nextLabel" = "Next",
                   "prevLabel" = "Previous",
                   "skipLabel" = "Skip"
                 )
               ))
  
  #### Time Series IntroJS
  steps2 <- reactive(data.frame(
    element = c(
      ".main-header",
      "#tsstep4",
      "#tsminthresh",
      "#tsreturn",
      "#ts_summary",
      "#download"
    ),
    intro = c(
      HTML(
        "This sidebar menu is the main interactive element of the dashboard. Here you define the settings of the anomaly detection, upload and validate your data, and run the model."
      ),
      "Here you will upload your data for a Time Series analysis. <br/> 1. Enter the four-digit fiscal year. <br/> 2. Choose the correct quarter for the data. <br/> 3. Locate the file on your computer. <br/> <br/> NOTE: Only .csv, .xlsx, or .txt MER data files work function properly.",
      "You can decide to discount indicators with very low values (Minimum Threshhold).",
      "You can chose whether or not to display both anomalous and non-anomalous outputs in the final tables.",
      "All outputs are organized in two tabs.<br/><br/> The Summary tab will provide a data table for highlevel review of anomalous data. <br/><br/> The Observations tab will provide results for each of ARIMA, ETS, and STL models. <br/> <br/> Outputs will only be generated if their corresponding switch has been turned on.",
      "After reviewing all outputs, click here to download either summary outputs or all outputs."
    ),
    position = c("right", "bottom", "bottom", "right", "bottom", "left")
  ))
  observeEvent(input$help2,
               introjs(
                 session,
                 options = list(
                   steps = steps2(),
                   "nextLabel" = "Next",
                   "prevLabel" = "Previous",
                   "skipLabel" = "Skip"
                 )
               ))
  
  output$title <- renderText({
    if (input$type == "Recommender") {
      paste("Recommender Anomaly Detetction")
    }
    else if (input$type == "Time Series") {
      paste("Time Series Anomaly Detection")
    }
  })
  
  output$title <- renderText({
    if (input$type == "Recommender") {
      paste("Recommender Anomaly Detetction")
    }
    else if (input$type == "Time Series") {
      paste("Time Series Anomaly Detection")
    }
  })
  
  datasetInput <- reactive({
    infile <- input$file1
    print(infile)
    ext <- tools::file_ext(infile$datapath)
    print(ext)
    if (is.null(infile)) {
      return(NULL)
    } else if (ext == "txt") {
      # print(head(read.delim(infile$datapath)))
      read.delim(infile$datapath)
    } else if (ext == "csv") {
      # print(head(read.csv(infile$datapath, stringsAsFactors = FALSE)))
      read.csv(infile$datapath, stringsAsFactors = FALSE)
    } else if (ext == "xlsx") {
      # print(head(read.xlsx(infile$datapath)))
      read.xlsx(infile$datapath)
    }
  })
  
  output$test <- DT::renderDT(datasetInput(),
                              options = list(scrollX = TRUE))
  
  
  
  ####Recommender DATA CHECK FUNCTION####
  observeEvent(input$recdatacheck, {
    withProgress(message = 'Running Checks', value = 0.5, {
      runChecks(
        dat = datasetInput(),
        year_for_analysis = input$year,
        qtr_for_analysis = input$quarter
      )
    })
  })
  
  #### Recommender DATA PREP #####
  dat_out <- reactive({
    # keep only the columns we need
    cols_to_keep <-
      c(
        "sitename",
        "psnu",
        "facility",
        "indicator",
        "numeratordenom",
        "fundingagency",
        "disaggregate",
        "ageasentered",
        "sex",
        "fiscal_year",
        "primepartner",
        "otherdisaggregate_sub",
        input$quarter
      )
    dat <- datasetInput()[, cols_to_keep]
    
    if (input$recfunder) {
      dat <- dat %>% filter(fundingagency == "USAID")
    }
    
    # Confirm they are strings and not factors
    dat$sitename <- as.character(dat$sitename)
    dat$psnu <- as.character(dat$psnu)
    dat$facility <- as.character(dat$facility)
    dat$indicator <- as.character(dat$indicator)
    dat$ageasentered <- as.character(dat$ageasentered)
    dat$sex <- as.character(dat$sex)
    dat$primepartner <- as.character(dat$primepartner)
    dat$disaggregate <- as.character(dat$disaggregate)
    dat$numeratordenom <- as.character(dat$numeratordenom)
    dat$otherdisaggregate_sub <-
      as.character(dat$otherdisaggregate_sub)
    
    
    # filter to the fiscal year entered by the user
    dat <- dat %>% filter(fiscal_year == input$year)
    
    # remove the rows that report on Total Numerator or Total Denominator
    dat <-
      dat %>% filter(!disaggregate %in% c("Total Numerator", "Total Denominator"))
    dat <-
      dat %>% filter(tolower(facility) != "data reported above facility level")
    
    # remove rows that are aggregates of age groups (e.g. 15+ and 50+)
    dat <- dat[-grep("\\+", dat$ageasentered),]
    
    # label indicators with N and D for those for which both numerators and denominators are reported
    dat$indicator <- paste0(dat$indicator, "_", dat$numeratordenom)
    
    # add transgender to the sex column
    dat$tg <-
      ifelse(grepl("TG", dat$otherdisaggregate_sub), "Transgender", "")
    dat$sex2 <- paste(dat$sex, dat$tg, sep = "")
    cols_to_keep <-
      c(
        "sitename",
        "psnu",
        "facility",
        "indicator",
        "numeratordenom",
        "disaggregate",
        "ageasentered",
        "sex2",
        "fiscal_year",
        "primepartner",
        input$quarter
      )
    dat <- dat[, cols_to_keep]
    dat <- dat %>% rename(sex = sex2)
    
    dat
    
  })
  
  dat_disag_out <- reactive({
    dat <- dat_out()
    
    # for disaggregate output - create a column for the key population disaggregate
    dat$kp <- ifelse(grepl("KeyPop", dat$disaggregate), "Yes", "No")
    
    # for disaggregate output - drop disaggregate and numeratordenom columns
    cols_to_drop <- c("numeratordenom", "disaggregate")
    dat <- dat[,!(names(dat) %in% cols_to_drop)]
    
    # for disaggregate output - we'll need the qtr variable - drop the 1/2/3/4 from quarter name
    # so that we can reference the variable regardless of the quarter selected
    names(dat) <- gsub("[0-9]", "", names(dat))
    dat <- dat %>%
      filter(!is.na(qtr))
    
    # group by facility, age, sex, and indicator, kp, and psnu, and then summarize qtr (before pivot)
    dat_grouped <-
      dat %>% group_by(facility,
                       ageasentered,
                       sex,
                       indicator,
                       kp,
                       psnu,
                       primepartner) %>%
      summarise(qtr_sum = sum(qtr, na.rm = TRUE))
    
    # for disaggregate output - pivot wider to get MER indicators in wide format
    dat_out <- dat_grouped %>%
      pivot_wider(names_from = "indicator", values_from = "qtr_sum") %>%
      as.data.frame()
    
    dat_out
    
  })
  
  dat_facility_out <- reactive({
    dat_facility <- dat_out()
    
    # facility level file - filter for just the quarter of interest
    names(dat_facility) <- gsub("[0-9]", "", names(dat_facility))
    dat_facility <- dat_facility %>%
      filter(!is.na(qtr))
    
    # facility level file - group by facility, psnu and indicator, and then summarize qtr 2 (before pivot)
    dat_facility <-
      dat_facility %>% group_by(facility, indicator, psnu, primepartner) %>%
      summarise(qtr_sum = sum(qtr, na.rm = TRUE))
    
    # facility level file - pivot wider to get indicators in wide format
    dat_facility_out <- dat_facility %>%
      pivot_wider(names_from = "indicator", values_from = "qtr_sum") %>%
      as.data.frame()
    
    dat_facility_out
    
  })
  
  returnAll <- reactive({
    if (input$return) {
      TRUE
    } else {
      FALSE
    }
  })
  
  #### Run Rec Model ####
  
  forout_reactive <- reactiveValues()
  
  observeEvent(input$recrun, {
    forout_reactive <- reactiveValues()
  })
  
  
  observeEvent(input$recrun, {
    all_outputs <- NULL
    site_sex_outliers <- NULL
    site_age_outliers <- NULL
    facility_outputs <- NULL
    
    withProgress(message = 'Running Models', value = 0, {
      # Scenario can take on values set by user including "all", "sex", and "age"
      if (input$obs) {
        incProgress(.2, detail = paste("Running Model with All Disaggregrates"))
        # Apply the runRecAnalysis function on entire dataset disaggregated by sex and age
        all_outputs <-
          runRecAnalysis(dat = dat_disag_out(), keys = keys_disag)
        # Sort outputs by anomalous distance
        all_outputs <- tryCatch({
          sortOutputs(
            all_outputs,
            keys = keys_disag,
            scenario_tmp = "all",
            return_all = returnAll(),
            min_thresh = input$min_thresh
          )
        }, error = function(cond) {
          message("No Outliers Found with All Disags")
          message(cond)
        })
        output$rec1 = DT::renderDT(
          datatable(
            all_outputs,
            filter = "top",
            options = list(scrollX = TRUE,
                           columnDefs = list(list(
                             visible = FALSE, targets = c(grep("^D_", colnames(
                               all_outputs
                             )),
                             grep("^E_", colnames(
                               all_outputs
                             )))
                           )))
          ) %>%
            formatStyle(
              7:(6 + length(grep(
                "^D_", colnames(all_outputs)
              ))),
              grep("^D_", colnames(all_outputs)),
              backgroundColor = styleInterval(
                as.numeric(quantile(
                  all_outputs[, grep("^D_", colnames(all_outputs))],
                  probs = c(.8, .9, 1),
                  na.rm = T
                )),
                c(
                  "rgb(255,255,255)",
                  "rgb(255,170,170)",
                  "rgb(255,80,80)",
                  "rgb(255,0,0)"
                )
              )
            )
        )
        forout_reactive$all_outputs <- all_outputs %>%
          select(names(all_outputs)[!grepl("^E_|^D_|^MD|outlier_sp", names(all_outputs))],
                 names(all_outputs)[grepl("^MD", names(all_outputs))],
                 names(all_outputs)[grepl("outlier_sp", names(all_outputs))])
      }
      
      if (input$sex) {
        incProgress(.2, detail = paste("Running Model with Sex Disaggregrates"))
        
        dat <- dat_disag_out()
        
        # Confirm sex is a string and not a factor
        dat$sex <- as.character(dat$sex)
        
        # Limit to observations that are male or female
        dat <- dat[dat$sex %in% c("Male", "Female"), ]
        
        # Split dataset by sex and run Recommender analysis on each subset
        site_split <- split(dat, dat$sex)
        site_out <- list()
        for (j in 1:length(site_split)) {
          site_out[[j]] <- runRecAnalysis(site_split[[j]],
                                          keys = keys_disag)
        }
        # stack the outputs
        site_sex_outliers <- do.call(plyr::rbind.fill, site_out)
        
        site_sex_outliers <- tryCatch({
          sortOutputs(
            site_sex_outliers,
            keys = keys_disag,
            scenario_tmp = "sex",
            return_all = returnAll(),
            min_thresh = input$min_thresh
          )
        }, error = function(cond) {
          message("No Outliers Found for Sex Disag")
          message(cond)
        })
        if (nrow(site_sex_outliers) > 0) {
          output$rec2 = DT::renderDT(
            datatable(
              site_sex_outliers,
              filter = "top",
              options = list(scrollX = TRUE,
                             columnDefs = list(
                               list(
                                 visible = FALSE,
                                 targets = c(grep(
                                   "^D_", colnames(site_sex_outliers)
                                 ),
                                 grep(
                                   "^E_", colnames(site_sex_outliers)
                                 ))
                               )
                             ))
            ) %>%
              formatStyle(
                7:(6 + length(grep(
                  "^D_", colnames(site_sex_outliers)
                ))),
                grep("^D_", colnames(site_sex_outliers)),
                backgroundColor = styleInterval(
                  as.numeric(quantile(
                    site_sex_outliers[, grep("^D_", colnames(site_sex_outliers))],
                    probs = c(.8, .9, 1),
                    na.rm = T
                  )),
                  c(
                    "rgb(255,255,255)",
                    "rgb(255,170,170)",
                    "rgb(255,80,80)",
                    "rgb(255,0,0)"
                  )
                )
              )
          )
          
          forout_reactive$site_sex_outliers <- site_sex_outliers %>%
            select(
              names(site_sex_outliers)[!grepl("^E_|^D_|^MD|outlier_sp",
                                              names(site_sex_outliers))],
              names(site_sex_outliers)[grepl("^MD", names(site_sex_outliers))],
              names(site_sex_outliers)[grepl("outlier_sp", names(site_sex_outliers))]
            )
        } else {
          shinyalert("Proceed",
                     "Completed Sex Disag. No outliers found.",
                     type = "success")
        }
      }
      
      if (input$age) {
        incProgress(.2, detail = paste("Running Model with Age Disaggregrates"))
        
        dat <- dat_disag_out()
        
        # Create "agegroup" variable which takes value of Under 15 of Over 15 based on "ageasentered"
        dat <-
          cbind(
            "agegroup" = ifelse(
              dat$ageasentered %in% c("01-04", "05-09", "10-14"),
              "Under 15",
              "Over 15"
            ),
            dat,
            stringsAsFactors = FALSE
          )
        
        # Split dataset by age group
        site_split <- split(dat, factor(dat$agegroup))
        
        # Loop through list and run Recommender analysis on each
        site_out <- list()
        
        for (j in 1:length(site_split)) {
          site_out[[j]] <- tryCatch({
            # append "agegroup" to vector of keys
            runRecAnalysis(site_split[[j]],
                           keys = c(keys_disag, "agegroup"))
          }, error = function(cond) {
            message(paste(
              "Insufficient Data to Run Disag for Age Group:",
              names(site_split)[j]
            ))
            message(cond)
          })
        }
        
        # stack the outputs and drop the age group variable so that outputs from all runs can be appropriately stacked
        site_age_outliers <-
          do.call(plyr::rbind.fill, site_out) %>% select(-agegroup)
        
        # Sort outputs by anomalous distance
        site_age_outliers <- tryCatch({
          sortOutputs(
            site_age_outliers,
            keys = keys_disag,
            scenario_tmp = "age",
            return_all = returnAll(),
            min_thresh = input$min_thresh
          )
        }, error = function(cond) {
          message("No Outliers Found for Disag for Age Group:")
          message(cond)
        })
        
        if (nrow(site_age_outliers) > 0) {
          output$rec3 = DT::renderDT(
            datatable(
              site_age_outliers,
              filter = "top",
              options = list(scrollX = TRUE,
                             columnDefs = list(
                               list(
                                 visible = FALSE,
                                 targets = c(grep(
                                   "^D_", colnames(site_age_outliers)
                                 ),
                                 grep(
                                   "^E_", colnames(site_age_outliers)
                                 ))
                               )
                             ))
            ) %>%
              formatStyle(
                7:(6 + length(grep(
                  "^D_", colnames(site_age_outliers)
                ))),
                grep("^D_", colnames(site_age_outliers)),
                backgroundColor = styleInterval(
                  as.numeric(quantile(
                    site_age_outliers[, grep("^D_", colnames(site_age_outliers))],
                    probs = c(.8, .9, 1),
                    na.rm = T
                  )),
                  c(
                    "rgb(255,255,255)",
                    "rgb(255,170,170)",
                    "rgb(255,80,80)",
                    "rgb(255,0,0)"
                  )
                )
              )
          )
    forout_reactive$site_age_outliers <- site_age_outliers %>%
      select(
        names(site_age_outliers)[!grepl("^E_|^D_|^MD|outlier_sp",
                                        names(site_age_outliers))],
        names(site_age_outliers)[grepl("^MD", names(site_age_outliers))],
        names(site_age_outliers)[grepl("outlier_sp", names(site_age_outliers))]
      )
        } else {
          shinyalert("Proceed",
                     "Completed Age Disag. No outliers found.",
                     type = "success")
        }
      }
      
      if (input$facility) {
        incProgress(.2, detail = paste("Running Model at Facility Level"))
        
        facility_outputs <- runRecAnalysis(dat = dat_facility_out(),
                                           keys = keys_facility)
        facility_outputs <- tryCatch({
          sortOutputs(
            facility_outputs,
            keys = keys_facility,
            scenario_tmp = "facility",
            return_all = returnAll(),
            min_thresh = input$min_thresh
          )
        }, error = function(cond) {
          message("No Outliers Found at Facility Level")
          message(cond)
        })
        
        if (nrow(facility_outputs) > 0) {
          output$rec4 = DT::renderDT(
            datatable(
              facility_outputs,
              filter = "top",
              options = list(scrollX = TRUE,
                             columnDefs = list(
                               list(
                                 visible = FALSE,
                                 targets = c(grep(
                                   "^D_", colnames(facility_outputs)
                                 ),
                                 grep(
                                   "^E_", colnames(facility_outputs)
                                 ))
                               )
                             ))
            ) %>%
              formatStyle(
                7:(6 + length(grep(
                  "^D_", colnames(facility_outputs)
                ))),
                grep("^D_", colnames(facility_outputs)),
                backgroundColor = styleInterval(
                  as.numeric(quantile(
                    facility_outputs[, grep("^D_", colnames(facility_outputs))],
                    probs = c(.8, .9, 1),
                    na.rm = T
                  )),
                  c(
                    "rgb(255,255,255)",
                    "rgb(255,170,170)",
                    "rgb(255,80,80)",
                    "rgb(255,0,0)"
                  )
                )
              )
          )
          forout_reactive$facility_outputs <- facility_outputs %>%
            select(
              names(facility_outputs)[!grepl("^E_|^D_|^MD|outlier_sp", names(facility_outputs))],
              names(facility_outputs)[grepl("^MD", names(facility_outputs))],
              names(facility_outputs)[grepl("outlier_sp", names(facility_outputs))]
            )
        } else {
          shinyalert("Proceed",
                     "Completed Facility Run. No outliers found.",
                     type = "success")
        }
        
      }
      
      disags_list <-
        list(all_outputs, site_sex_outliers, site_age_outliers)
      disags_list <- disags_list[lengths(disags_list) != 0]
      
      facility_list <- list(facility_outputs)
      facility_list <- facility_list[lengths(facility_list) != 0]
      
      if (length(disags_list) > 0) {
        incProgress(.2, detail = paste("Creating Summary Disaggregate Tab"))
        disags_summary <-
          createSummaryTab(dat_summary_list = disags_list)
        output$rec6 = DT::renderDT(
          disags_summary$summary,
          filter = "top",
          options = list(scrollX = TRUE)
        )
        forout_reactive$disags_summary <- disags_summary$summary
      }
      
      # Create summary tab for anomalies found using data aggregated at facility level
      if (length(facility_list) > 0) {
        incProgress(.2, detail = paste("Creating Summary Facility Tab"))
        facility_summary <-
          createSummaryTab(dat_summary_list = facility_list,
                           disag = FALSE)
        output$rec7 = DT::renderDT(
          facility_summary$summary,
          filter = "top",
          options = list(scrollX = TRUE)
        )
        forout_reactive$facility_summary <- facility_summary$summary
      }
      
      # Prior to creating scorecard, pull the summary tabs that were created in previous step
      dat_tmp <- list()
      if (exists("disags_summary")) {
        dat_tmp[['disag']] <- disags_summary$scorecard
      }
      if (exists("facility_summary")) {
        dat_tmp[['facility']] <- facility_summary$scorecard
      }
      
      # Combine lists into dataframe
      dat_tmp <- rbindlist(dat_tmp)
      
      # Generate scorecard
      incProgress(.2, detail = paste("Creating Scorecard"))
      scorecard <- createScoreCard(scorecard_in = dat_tmp)
      output$rec8 = DT::renderDT(scorecard,
                                 filter = "top",
                                 options = list(scrollX = TRUE))
      forout_reactive$scorecard <- scorecard
      
      shinyalert("Proceed", "Completed Recommender Models", type = "success")
      
    })
  })

#RECOMMENDER DATA CHECK
output$rec_data <- reactive({
  input$type == 'Recommender'
})

outputOptions(output, 'rec_data', suspendWhenHidden = FALSE)

#RECOMMENDER OBSERVATION Each observation compared against all observations
output$rec_sum <- reactive({
  input$type == 'Recommender'
})

outputOptions(output, 'rec_sum', suspendWhenHidden = FALSE)

output$rec1 <- reactive({
  input$obs == 'TRUE' & input$type == 'Recommender'
})

outputOptions(output, 'rec1', suspendWhenHidden = FALSE)

#RECOMMENDER SEX
output$rec2 <- reactive({
  input$sex == 'TRUE' & input$type == 'Recommender'
})

outputOptions(output, 'rec2', suspendWhenHidden = FALSE)

#RECOMMENDER AGE
output$rec3 <- reactive({
  input$age == 'TRUE' & input$type == 'Recommender'
})

outputOptions(output, 'rec3', suspendWhenHidden = FALSE)

#RECOMMENDER FACILITY
output$rec4 <- reactive({
  input$facility == 'TRUE' & input$type == 'Recommender'
})

outputOptions(output, 'rec4', suspendWhenHidden = FALSE)

#RECOMMENDER PSNU
output$rec5 <- reactive({
  input$psnu == 'TRUE' & input$type == 'Recommender'
})

outputOptions(output, 'rec5', suspendWhenHidden = FALSE)

#RECOMMENDER Disag Summary
output$rec6 <- reactive({
  (input$obs == 'TRUE' |
     input$age == 'TRUE' |
     input$sex == 'TRUE') & input$type == 'Recommender'
})

outputOptions(output, 'rec6', suspendWhenHidden = FALSE)

#RECOMMENDER Facility Summary
output$rec7 <- reactive({
  input$facility == 'TRUE' & input$type == 'Recommender'
})

outputOptions(output, 'rec7', suspendWhenHidden = FALSE)

#RECOMMENDER Facility Summary
output$rec8 <- reactive({
  (
    input$obs == 'TRUE' |
      input$age == 'TRUE' |
      input$sex == 'TRUE' |
      input$facility == 'TRUE'
  ) & input$type == 'Recommender'
})

outputOptions(output, 'rec8', suspendWhenHidden = FALSE)



#Time Series
output$ts1 <- reactive({
  input$type == 'Time Series'
})

outputOptions(output, 'ts1', suspendWhenHidden = FALSE)

output$ts2 <- reactive({
  input$type == 'Time Series'
})

outputOptions(output, 'ts2', suspendWhenHidden = FALSE)

output$ts3 <- reactive({
  input$type == 'Time Series'
})

outputOptions(output, 'ts3', suspendWhenHidden = FALSE)

output$ts4 <- reactive({
  input$type == 'Time Series'
})

outputOptions(output, 'ts4', suspendWhenHidden = FALSE)

output$ts5 <- reactive({
  input$type == 'Time Series'
})

outputOptions(output, 'ts5', suspendWhenHidden = FALSE)

output$ts6 <- reactive({
  input$type == 'Time Series'
})

outputOptions(output, 'ts6', suspendWhenHidden = FALSE)

output$ts7 <- reactive({
  input$type == 'Time Series'
})

outputOptions(output, 'ts7', suspendWhenHidden = FALSE)

output$ts8 <- reactive({
  input$type == 'Time Series'
})

outputOptions(output, 'ts8', suspendWhenHidden = FALSE)

datasetInputTS <- reactive({
  infile <- input$file2
  
  mer_list <- list()
  for (i in 1:nrow(infile)) {
    ext <- tools::file_ext(infile[i, 'datapath'])
    if (is.null(infile)) {
      return(NULL)
    } else if (ext == "txt") {
      # print(head(read.delim(infile$datapath)))
      mer_list[[i]] <- read.delim(infile[i, 'datapath'])
    } else if (ext == "csv") {
      # print(head(read.csv(infile$datapath, stringsAsFactors = FALSE)))
      mer_list[[i]] <-
        read.csv(infile[i, 'datapath'], stringsAsFactors = FALSE)
    } else if (ext == "xlsx") {
      # print(head(read.xlsx(infile$datapath)))
      mer_list[[i]] <- read.xlsx(infile[i, 'datapath'])
    }
  }
  
  mer_data <- rbindlist(mer_list) %>% as.data.frame()
  
  cols_to_keep <-
    c(
      "facility",
      "indicator",
      "psnu",
      "numeratordenom",
      "disaggregate",
      "statushiv",
      "ageasentered",
      "primepartner",
      "fiscal_year",
      "qtr1",
      "qtr2",
      "qtr3",
      "qtr4",
      "fundingagency"
    )
  mer_data <- mer_data[, cols_to_keep]
  mer_data
})

#### Time Series DATA CHECK FUNCTION####
observeEvent(input$tsdatacheck, {
  withProgress(message = 'Running Checks', value = .5, {
    cols_to_keep <-
      c(
        "facility",
        "indicator",
        "psnu",
        "numeratordenom",
        "disaggregate",
        "statushiv",
        "ageasentered",
        "primepartner",
        "fiscal_year",
        "qtr1",
        "qtr2",
        "qtr3",
        "qtr4",
        "fundingagency"
      )
    
    
    if (sum(!cols_to_keep %in% names(datasetInputTS())) > 0) {
      shinyalert(
        "Check the data file",
        "One or more of the required columns are missing from your dataset. Please make sure
        that your dataset contains all the following columns: facility, indicator, psnu,
        frequency, numeratordenom, disaggregate, fiscal_year, qtr1, qtr2, qtr3, and qtr4.",
        type = "error"
      )
    } else {
      shinyalert("Proceed", "Continue to run models.", type = "success")
    }
  })
})

tsdata <- reactive({
  mer_data <- datasetInputTS()
  
  recent_year <- input$tsyear
  recent_qtr <- input$tsquarter
  
  # Keep only USAID sites
  if (input$tsfunder) {
    mer_data <- mer_data %>% filter(fundingagency == "USAID")
  }
  # mer_data <- datasetInputTS() %>% filter(fundingagency == "USAID")
  
  mer_data$indicator <- as.character(mer_data$indicator)
  mer_data <- mer_data %>%
    filter(indicator %in% quarterly_indicators)
  
  mer_data$indicator <-
    paste0(mer_data$indicator, "_", mer_data$numeratordenom)
  
  # remove the rows that report on Total Numerator or Total Denominator data
  mer_data <-
    mer_data %>% filter(!disaggregate %in% c("Total Numerator", "Total Denominator"))
  
  mer_data <-
    mer_data %>% filter(tolower(facility) != "data reported above facility level")
  
  # remove rows that are aggregates of age groups (e.g. 15+) but keep 50+
  mer_data <-
    rbind(mer_data[-grep("\\+", mer_data$ageasentered), ],
          mer_data[grep("50+", mer_data$ageasentered), ])
  
  mer_data <- mer_data[-grep("<+[0-9]", mer_data$ageasentered), ]
  
  # For HTS_INDEX, keep only those rows where statushiv is positive or negative
  mer_data <- rbind(mer_data[mer_data$indicator != 'HTS_INDEX', ],
                    mer_data[mer_data$indicator == 'HTS_INDEX' &
                               (mer_data$statushiv %in% c('Negative', 'Positive')), ])
  
  # Drop deduplication rows from prime partner
  mer_data <-
    mer_data[!mer_data$primepartner %in% c("Dedup", "TBD"),]
  
  mer_data <-
    mer_data[,!names(mer_data) %in% c("numeratordenom",
                                      "disaggregate",
                                      "ageasentered",
                                      "statushiv")]
  
  # Pivot longer to get all the values in one column
  mer_data_long <- pivot_longer(mer_data,
                                cols = names(mer_data)[grepl("qtr", names(mer_data))],
                                names_to = "qtr")
  
  # What are the indicators reported in the most recent quarter
  indicators_to_keep <- mer_data_long %>%
    filter(fiscal_year == recent_year) %>%
    filter(qtr == recent_qtr) %>%
    filter(!is.na(value)) %>%
    .$indicator %>%
    unique()
  
  if (length(indicators_to_keep) == 0) {
    shinyalert(
      "No data found in target year and quarter.",
      "Please confirm that the dataset includes the year and quarter selected.",
      type = "error"
    )
  }
  
  facilities_to_keep <- mer_data_long %>%
    filter(fiscal_year == recent_year) %>%
    filter(qtr == recent_qtr) %>%
    filter(!is.na(value)) %>%
    .$facility %>%
    unique() %>%
    as.character()
  # Only keep these indicators
  mer_data_long <-
    mer_data_long[mer_data_long$indicator %in% indicators_to_keep,]
  
  mer_data_long <-
    mer_data_long[as.character(mer_data_long$facility) %in% facilities_to_keep,]
  
  # Turn quarter into a number for sorting
  mer_data_long$qtr <-
    as.numeric(gsub(".*?([0-9]+).*", "\\1", mer_data_long$qtr))
  
  # Take primepartner from most recent quarter in case it changed
  mer_data_long <- mer_data_long %>% arrange(desc(fiscal_year))
  mer_data_long <- mer_data_long %>%
    group_by(facility, indicator) %>%
    mutate(primepartner = primepartner[1])
  
  # summarize by facility/indicator/fiscal_year/qtr
  mer_data_long <- mer_data_long %>%
    group_by(psnu, primepartner, facility, indicator, fiscal_year, qtr) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop")
  
  earliest_year <- min(mer_data_long$fiscal_year)
  shell <-
    expand.grid(fiscal_year = earliest_year:recent_year, qtr = 1:4) %>%
    filter(!(fiscal_year >= recent_year &
               qtr > as.numeric(
                 gsub(".*?([0-9]+).*", "\\1", recent_qtr)
               ))) %>%
    arrange(desc(fiscal_year), desc(qtr)) %>%
    mutate(rownum = row_number()) %>% filter(rownum <= 12) %>% select(-rownum) %>%
    mutate(keep = paste0(fiscal_year, qtr))
  
  obs_to_keep <- mer_data_long %>%
    mutate(yrqtr = paste0(fiscal_year, qtr)) %>%
    mutate(keep = ifelse(yrqtr %in% shell$keep, 1, 0)) %>%
    group_by(psnu, facility, indicator) %>%
    mutate(count = sum(keep)) %>%
    filter(count >= 10) %>%
    ungroup() %>%
    select(-count,-yrqtr,-keep)
  
  # convert facility to character string
  obs_to_keep$facility <- as.character(obs_to_keep$facility)
  
  obs_to_keep
  
})

returnAllts <- reactive({
  if (input$tsreturn) {
    TRUE
  } else {
    FALSE
  }
})

forout_reactive_ts <- reactiveValues()

observeEvent(input$tsrun, {
  tsoutputs <- runTimeSeries(
    dat = tsdata(),
    recent_year = input$tsyear,
    recent_qtr = input$tsquarter,
    MIN_THRESH = input$tsminthresh,
    RETURN_ALL = returnAllts(),
    keys = keysts
  )
  print("end")
  output$ts2 = DT::renderDT(tsoutputs$ARIMA,
                            filter = "top",
                            options = list(scrollX = TRUE))
  
  output$ts3 = DT::renderDT(tsoutputs$ETS,
                            filter = "top",
                            options = list(scrollX = TRUE))
  
  output$ts4 = DT::renderDT(tsoutputs$STL,
                            filter = "top",
                            options = list(scrollX = TRUE))
  
  output$ts5 = DT::renderDT(tsoutputs$Summary,
                            filter = "top",
                            options = list(scrollX = TRUE))
  
  output$ts6 = DT::renderDT(
    tsoutputs$facility_scorecard,
    filter = "top",
    options = list(scrollX = TRUE)
  )
  
  output$ts7 = DT::renderDT(tsoutputs$ip_scorecard,
                            filter = "top",
                            options = list(scrollX = TRUE))
  
  forout_reactive_ts$ARIMA <- tsoutputs$ARIMA
  forout_reactive_ts$ETS <- tsoutputs$ETS
  forout_reactive_ts$STL <- tsoutputs$STL
  forout_reactive_ts$Summary <- tsoutputs$Summary
  forout_reactive_ts$FacilityScorecard <-
    tsoutputs$facility_scorecard
  forout_reactive_ts$IPScorecard <- tsoutputs$ip_scorecard
  
  shinyalert("Proceed", "Completed Time series Models", type = "success")
  
})

data_list_rec_summary <- reactive({
  tmp <- list(
    "Scorecard" = forout_reactive$scorecard,
    "Summary by Facility" = forout_reactive$facility_summary,
    "Summary by Disag" = forout_reactive$disags_summary
  )
  tmp <- tmp[lengths(tmp) != 0]
  tmp
})

output$download_rec_sum <- downloadHandler(
  filename = function() {
    "recommender_summary.xlsx"
  },
  content = function(file) {
    writexl::write_xlsx(data_list_rec_summary(),
                        path = file)
  }
)

data_list_rec_all <- reactive({
  tmp <- list(
    "Scorecard" = forout_reactive$scorecard,
    "Summary By Facility" = forout_reactive$facility_summary,
    "Summary By Disag" = forout_reactive$disags_summary,
    "Outliers All Disags" = forout_reactive$all_outputs,
    "Outliers Sex Disags" = forout_reactive$site_sex_outliers,
    "Outliers Age Disags" = forout_reactive$site_age_outliers,
    "Outliers Facility Level" = forout_reactive$facility_outputs
  )
  tmp <- tmp[lengths(tmp) != 0]
  tmp
})

output$download_rec_all <- downloadHandler(
  filename = function() {
    "recommender_all.xlsx"
  },
  content = function(file) {
    writexl::write_xlsx(data_list_rec_all(), path = file)
  }
)

data_list_ts_sum <- reactive({
  tmp <- list(
    "IP Scorecard" = forout_reactive_ts$IPScorecard,
    "Facility Scorecard" = forout_reactive_ts$FacilityScorecard,
    "Summary" = forout_reactive_ts$Summary
  )
  tmp <- tmp[lengths(tmp) != 0]
  tmp
})

output$download_ts_sum <- downloadHandler(
  filename = function() {
    "timeseries_summary.xlsx"
  },
  content = function(file) {
    writexl::write_xlsx(data_list_ts_sum(), path = file)
  }
)

data_list_ts_all <- reactive({
  tmp <- list(
    "IP Scorecard" = forout_reactive_ts$IPScorecard,
    "Facility Scorecard" = forout_reactive_ts$FacilityScorecard,
    "Summary" = forout_reactive_ts$Summary,
    "Outliers ARIMA" = forout_reactive_ts$ARIMA,
    "Outliers ETS" = forout_reactive_ts$ETS,
    "Outliers STL" = forout_reactive_ts$STL
  )
  tmp <- tmp[lengths(tmp) != 0]
  tmp
})

output$download_ts_all <- downloadHandler(
  filename = function() {
    "timeseries_all.xlsx"
  },
  content = function(file) {
    writexl::write_xlsx(data_list_ts_all(), path = file)
  }
)

}
# Complete app with UI and server components
shinyApp(ui, server)
