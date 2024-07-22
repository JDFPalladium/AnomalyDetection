library(shiny)

# ui <- shinyUI(
#   uiOutput("ui")
# )

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
    # tags$head(includeHTML(("google-analytics.html"))),
    shinyjs::useShinyjs(),
    introjsUI(),
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
      fluidRow(column(12,
                      div(
                        # id = "step4",
                        selectInput("type", "Type",
                                    c('Recommender', 'Time Series'))
                      ))),
      #### Recommender Menu ####

      conditionalPanel(
        condition = "input.type == 'Recommender'",
        fluidRow(column(12, div(
          menuItem(
            tabName = "recommender",
            startExpanded = TRUE,
            tags$br(),
            fluidRow(column(
              12,
              div(
                id = "step1",
                tags$b("Data Upload"),
                selectInput("country_selected",
                            "Select OU",
                            choices = sort(COUNTRIES),
                            selected = NULL)))),
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
            fluidRow(column(
              12,
              div(
                actionButton(
                  "rec_upload",
                  "Load Data")))),
                br(),
              fluidRow(column(
                12,
                div(id = "step2",
                  tags$b("Data Checks"),
                numericInput("year",
                            label = "Select Fiscal Year",
                            value = 2023),
                selectInput("quarter",
                            "Select Quarter",
                            c('qtr1', 'qtr2', 'qtr3', 'qtr4')),
          div(
              actionButton("recdatacheck", "Run Data Check"))))),
        br(),
        fluidRow(column(
          12,
          div(
            id = "step3",
            tags$b("Set Parameters"),
            numericInput(
              inputId = "min_thresh",
              label = "Ignore values below",
              value = 10),
            pickerInput(inputId = "recfunder",
                        label = "Select supported sites",
                        choices = NULL,
                        selected = NULL,
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)
          ))), br(),
        fluidRow(column(
          12,
          tags$b("Run Model"),
          div(id = "step4",
              actionButton("recrun", "Run Model"), )
        )))))))


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
  dashboardBody(
    uiOutput("password_modal_ui"),
    uiOutput("ui_redirect"),
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
                               title = "Outliers from analysis using age group patterns, sex-based patterns, and overall patterns.",
                               width = 12,
                               "Disaggregate Summary Table",
                               collapsible = TRUE,
                               # shinycssloaders::withSpinner(DT::DTOutput('rec6'))
                               shinycssloaders::withSpinner(reactable::reactableOutput("rec6"))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.rec7",
                             box(
                               title = "Outliers from analysis using facility-level patterns.",
                               width = 12,
                               "Facility Summary Table",
                               collapsible = TRUE,
                               #shinycssloaders::withSpinner(DT::DTOutput('rec7'))
                               shinycssloaders::withSpinner(reactable::reactableOutput("rec7"))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.rec8",
                             box(
                               title = "Facility Scorecard",
                               width = 12,
                               "Scorecard Table",
                               collapsible = TRUE,
                               # shinycssloaders::withSpinner(DT::DTOutput('rec8'))
                               shinycssloaders::withSpinner(reactable::reactableOutput("rec8"))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.rec9",
                             box(
                               title = "IP Scorecard",
                               width = 12,
                               collapsible = TRUE,
                               "IP Scorecard",
                               # shinycssloaders::withSpinner(DT::DTOutput('rec9'))
                               shinycssloaders::withSpinner(reactable::reactableOutput("rec9"))
                             )
                           )
                         ),
                         tabPanel(
                           h4(icon("circle-user"), "Observation"),
                           conditionalPanel(
                             condition = "output.rec1",
                             box(
                               title = "Observations: The number for each outcome is shown, with the expected value shown in parentheses. Red cells show the largest deviations.",
                               width = 12,
                               "All Disaggregates table",
                               collapsible = TRUE,
                               #shinycssloaders::withSpinner(DT::DTOutput('rec1'))
                               #shinycssloaders::withSpinner(uiOutput("rec1"))
                               shinycssloaders::withSpinner(reactable::reactableOutput("rec1"))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.rec2",
                             box(
                               title = "Observations: The number for each outcome is shown, with the expected value shown in parentheses. Red cells show the largest deviations.",
                               width = 12,
                               "Sex Disaggregate Table",
                               collapsible = TRUE,
                               #shinycssloaders::withSpinner(DT::DTOutput('rec2'))
                               shinycssloaders::withSpinner(reactable::reactableOutput("rec2"))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.rec3",
                             box(
                               title = "Observations: The number for each outcome is shown, with the expected value shown in parentheses. Red cells show the largest deviations.",
                               width = 12,
                               "Age Disaggregate Table",
                               collapsible = TRUE,
                               #shinycssloaders::withSpinner(DT::DTOutput('rec3'))
                               shinycssloaders::withSpinner(reactable::reactableOutput("rec3"))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.rec4",
                             box(
                               title = "Observations: The number for each outcome is shown, with the expected value shown in parentheses. Red cells show the largest deviations.",
                               width = 12,
                               "Facility Disaggregate Table",
                               collapsible = TRUE,
                               #shinycssloaders::withSpinner(DT::DTOutput('rec4'))
                               shinycssloaders::withSpinner(reactable::reactableOutput("rec4"))
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
                               # shinycssloaders::withSpinner(DT::DTOutput('ts5'))
                               shinycssloaders::withSpinner(reactable::reactableOutput("ts5"))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.ts6",
                             box(
                               title = "Facility Scorecard",
                               width = 12,
                               collapsible = TRUE,
                               "Facility Scorecard",
                               # shinycssloaders::withSpinner(DT::DTOutput('ts6'))
                               shinycssloaders::withSpinner(reactable::reactableOutput("ts6"))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.ts7",
                             box(
                               title = "IP Scorecard",
                               width = 12,
                               collapsible = TRUE,
                               "IP Scorecard",
                               # shinycssloaders::withSpinner(DT::DTOutput('ts7'))
                               shinycssloaders::withSpinner(reactable::reactableOutput("ts7"))
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
                               # shinycssloaders::withSpinner(DT::DTOutput('ts2'))
                               shinycssloaders::withSpinner(reactable::reactableOutput("ts2"))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.ts3",
                             box(
                               title = "Observations: ETS Outputs",
                               width = 12,
                               collapsible = TRUE,
                               "ETS Outputs",
                               # shinycssloaders::withSpinner(DT::DTOutput('ts3'))
                               shinycssloaders::withSpinner(reactable::reactableOutput("ts3"))
                             )
                           ),
                           conditionalPanel(
                             condition = "output.ts4",
                             box(
                               title = "Observations: STL Outputs",
                               width = 12,
                               collapsible = TRUE,
                               "STL Outputs",
                               # shinycssloaders::withSpinner(DT::DTOutput('ts4'))
                               shinycssloaders::withSpinner(reactable::reactableOutput("ts4"))
                             )
                           )
                         )
                       )
                     )))


  )
)