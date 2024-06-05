#### Script to create UI components for Shiny app ####

ui <- dashboardPage(
  # First, creating dashboard header, along top of the application
  dashboardHeader(
    title = "Anomaly Detection",
    tags$li(
      class = "dropdown",
      id = "download",
      # Button at top right of application to download model results
      dropMenu(
        dropdownButton("Info",  icon = icon('download')),
        # Button label and outputs will update to reflect whether Recommender or Time Series models are selected
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
    # Info button - links to Github repository
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
  #### Sidebar ####
  dashboardSidebar(
    # tags$head(includeHTML(("google-analytics.html"))),
    shinyjs::useShinyjs(),
    introjsUI(),
    # Button to trigger walk through of instructions for using the app
    # button ID is "help" if recommender, "help2" for time series
    sidebarMenu(
      fluidRow(column(
        12,
        offset = 0,
        conditionalPanel(condition = "input.type == 'Recommender'",
                         tags$br(),
                         actionButton("help", HTML(
                           "Recommender instructions"
                         )))
      )),
      fluidRow(column(
        12,
        offset = 0,
        conditionalPanel(condition = "input.type == 'Time Series'",
                         tags$br(),
                         actionButton("help2", HTML(
                           "Time Series instructions"
                         )))
      )),
      # This is where user selects type of recommender or time series. Most UI items are wrapped in 
      # conditionalPanel so that labels and values updated accordingly
      fluidRow(column(12,
                      div(
                        # id = "step4",
                        selectInput("type", "Type",
                                    c('Recommender', 'Time Series'))
                      ))),
      
      #### Recommender Sidebar Menu ####
      conditionalPanel(
        condition = "input.type == 'Recommender'",
        fluidRow(column(12, div(
          menuItem(
            tabName = "recommender",
            startExpanded = TRUE,
            tags$br(),
            # Data upload section
            fluidRow(column(
              12,
              div(
                id = "step1",
                tags$b("Data Upload"),
                # Dropdown to select country or region
                selectInput("country_selected",
                            "Select OU", 
                            choices = sort(COUNTRIES), # options set in global.R
                            selected = NULL)))),
            # If region is selected, then dropdown appears to select country/countries within region
            fluidRow(column(
              12,
              div(
              conditionalPanel(
                condition = "input.country_selected == 'Asia'",
                pickerInput("asiafilter",
                            "Select from Region",
                            choices = ASIA,
                            selected = ASIA,
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE)),
              conditionalPanel(
                condition = "input.country_selected == 'West Africa'",
                pickerInput("westafricafilter",
                            "Select from Region",
                            choices = WESTAFRICA,
                            selected = WESTAFRICA,
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE)),
              conditionalPanel(
                condition = "input.country_selected == 'Western Hemisphere'",
                pickerInput("westernhemishpherefilter",
                            "Select from Region",
                            choices = WESTERNHEMISPHERE,
                            selected = WESTERNHEMISPHERE,
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE))))),
            # Button to upload data 
            fluidRow(column(
              12,
              div(
                actionButton(
                  "rec_upload",
                  "Load Data")))),
                br(), 
            # Once data is uploaded, proceed to Data Checks section
              fluidRow(column(
                12,
                div(id = "step2",
                  tags$b("Data Checks"),
                    # Numeric Input to select fiscal year - range limited in server.R based on data uploaded
                numericInput("year",
                            label = "Select Fiscal Year",
                            value = 2023),
                    # Dropdown for quarter - options limited in server.R based on data uploaded
                selectInput("quarter",
                            "Select Quarter",
                            c('qtr1', 'qtr2', 'qtr3', 'qtr4')),
                    # Run data checks button
          div(
              actionButton("recdatacheck", "Run Data Check"))))),
        br(),
            # Final section - set parameters and then run model
        fluidRow(column(
          12,
          div(
            id = "step3",
            tags$b("Set Parameters"),
            # Numeric input to set value below which variables will not be flagged as driving determination that
            # observation is anomalous - informs color coding of results
            numericInput(
              inputId = "min_thresh",
              label = "Ignore values below",
              value = 10),
            # Dropdown to select which funders to considers - updates in server.R based on data uploaded
            pickerInput(inputId = "recfunder",
                        label = "Select supported sites",
                        choices = NULL,
                        selected = NULL,
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)
          ))), br(),
            # Run model button
        fluidRow(column(
          12,
          tags$b("Run Model"),
          div(id = "step4",
              actionButton("recrun", "Run Model"), )
        )))))))
      
      
      ,
      #### Time Series Menu ####
      # Similar flow to recommender, will highlight differences
      # Notice each UI object has "ts" appended to ID to differentiate from recommender
      conditionalPanel(
        condition = "input.type == 'Time Series'",
        menuItem(
          tabName = "time",
          startExpanded = TRUE,
          tags$br(),
          fluidRow(column(
            12,
            div(
              id = "tsstep1",
              tags$b("Data Upload"),
              selectInput("country_selected_ts",
                          "Select OU", 
                          choices = sort(COUNTRIES),
                          selected = NULL)))),
          fluidRow(column(
            12,
            div(
              conditionalPanel(
                condition = "input.country_selected_ts == 'Asia'",
                pickerInput("asiafilter_ts",
                            "Select from Region",
                            choices = ASIA,
                            selected = ASIA,
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE)),
              conditionalPanel(
                condition = "input.country_selected_ts == 'West Africa'",
                pickerInput("westafricafilter_ts",
                            "Select from Region",
                            choices = WESTAFRICA,
                            selected = WESTAFRICA,
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE)),
              conditionalPanel(
                condition = "input.country_selected_ts == 'Western Hemisphere'",
                pickerInput("westernhemishpherefilter_ts",
                            "Select from Region",
                            choices = WESTERNHEMISPHERE,
                            selected = WESTERNHEMISPHERE,
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE))))),
          # As opposed to recommender, for time series users select indicators to evaluate. 
          # This is the dropdown to select indicators
          fluidRow(column(
            12,
            div(
            pickerInput("ts_vars",
                        label = "Select Indicators",
                        choices = quarterly_indicators,
                        selected = NULL,
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)))),
          fluidRow(column(
            12,
            div(
              actionButton(
                "ts_upload",
                "Load Data")))),
          br(), 
          fluidRow(column(
            12,
            div(id = "tsstep2",
              tags$b("Data Checks"),
              numericInput("tsyear",
                           label = "Select Fiscal Year",
                           value = 2023),
              selectInput("tsquarter",
                          "Select Quarter",
                          c('qtr1', 'qtr2', 'qtr3', 'qtr4')),
              div(id = "step6",
                  actionButton("tsdatacheck", "Run Data Check")))))
        ),
        br(),
        fluidRow(column(
          12,
          div(
            id = "tsstep3",
            tags$b("Set Parameters"),
            numericInput(
              inputId = "min_threshts",
              label = "Ignore values below",
              value = 10),
            pickerInput(inputId = "tsfunder",
                        label = "Select supported sites",
                        choices = NULL,
                        selected = NULL,
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)
          ))), br(),
        fluidRow(column(
          12,
          tags$b("Run Model"),
          div(id = "tsstep4",
              actionButton("tsrun", "Run Model"))
        )))
    )
  ),

  #### Main panel of the application - Results of analysis ####
  dashboardBody(
    fluidRow(div(id = "body_title",
                 (h2(
                   textOutput('title')
                 )))),
    # Outputs items when Recommender is run
    conditionalPanel(condition = "input.type == 'Recommender'",
                     fluidRow(div(
                       id = "rec_summary",
                       # Two tabs. First, UI objects created for the Summary tab
                       tabsetPanel(
                         tabPanel(
                           h4(icon("list-check"), "Summary"),
                           # Output table show aggregate counts of anomalies by facility, sex, age combinations
                           conditionalPanel(
                             condition = "output.rec6",
                             box(
                               title = "Outliers from analysis using age group patterns, sex-based patterns, and overall patterns.",
                               width = 12,
                               "Disaggregate Summary Table",
                               collapsible = TRUE,
                               shinycssloaders::withSpinner(DT::DTOutput('rec6'))
                             )
                           ),
                           # Output table showing number of anomalies when considering patterns rolled up to facility level
                           conditionalPanel(
                             condition = "output.rec7",
                             box(
                               title = "Outliers from analysis using facility-level patterns.",
                               width = 12,
                               "Facility Summary Table",
                               collapsible = TRUE,
                               shinycssloaders::withSpinner(DT::DTOutput('rec7'))
                             )
                           ),
                           # Facility scorecard table
                           conditionalPanel(
                             condition = "output.rec8",
                             box(
                               title = "Facility Scorecard",
                               width = 12,
                               "Scorecard Table",
                               collapsible = TRUE,
                               shinycssloaders::withSpinner(DT::DTOutput('rec8'))
                             )
                           ),
                           # Implementing Partner Scorecard
                           conditionalPanel(
                             condition = "output.rec9",
                             box(
                               title = "IP Scorecard",
                               width = 12,
                               collapsible = TRUE,
                               "IP Scorecard",
                               shinycssloaders::withSpinner(DT::DTOutput('rec9'))
                             )
                           )
                         ),
                         # Second tab, UI objects created for the Observation tab
                         tabPanel(
                           h4(icon("circle-user"), "Observation"),
                           # Results when considering each disaggregate as a unique observations
                           conditionalPanel(
                             condition = "output.rec1",
                             box(
                               title = "Observations: The number for each outcome is shown, with the expected value shown in parentheses. Red cells show the largest deviations.",
                               width = 12,
                               "All Disaggregates table",
                               collapsible = TRUE,
                               shinycssloaders::withSpinner(DT::DTOutput('rec1'))
                             )
                           ),
                           # results when calculating patterns separately by sex
                           conditionalPanel(
                             condition = "output.rec2",
                             box(
                               title = "Observations: The number for each outcome is shown, with the expected value shown in parentheses. Red cells show the largest deviations.",
                               width = 12,
                               "Sex Disaggregate Table",
                               collapsible = TRUE,
                               shinycssloaders::withSpinner(DT::DTOutput('rec2'))
                             )
                           ),
                           # Results when calculating patterns separately by age group
                           conditionalPanel(
                             condition = "output.rec3",
                             box(
                               title = "Observations: The number for each outcome is shown, with the expected value shown in parentheses. Red cells show the largest deviations.",
                               width = 12,
                               "Age Disaggregate Table",
                               collapsible = TRUE,
                               shinycssloaders::withSpinner(DT::DTOutput('rec3'))
                             )
                           ),
                           # Results when calculating patterns by facility
                           conditionalPanel(
                             condition = "output.rec4",
                             box(
                               title = "Observations: The number for each outcome is shown, with the expected value shown in parentheses. Red cells show the largest deviations.",
                               width = 12,
                               "Facility Disaggregate Table",
                               collapsible = TRUE,
                               shinycssloaders::withSpinner(DT::DTOutput('rec4'))
                             )
                           )
                         )
                       )
                     ))),
    # UI objects that present results from time series model
    conditionalPanel(condition = "input.type == 'Time Series'",
                     fluidRow(div(
                       id = "ts_summary",
                       # As with recommender, there are two tabs, one for summary results and one for observation-level results
                       tabsetPanel(
                         # First, summary results
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
