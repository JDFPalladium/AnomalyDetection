# add read parquet
# qtr columns are automatically read in
# h <- 1000  # Default chunk size
# 
# # Function to read parquet file more efficiently
# read_parquet_file <- function(my_file, chunk_size = h, columns_to_read = NULL) {
#   print(paste0("Reading parquet file in ", h, " chunks: ", my_file))
# 
#   # Initialize a list to collect chunks
#   all_chunks <- list()
# 
#   # Define a function to process each chunk and add it to the list
#   process_chunk <- function(chunk) {
#     # Convert the chunk to data.table
#     dt_chunk <- as.data.table(chunk)
#     rm(dt_chunk)
# 
#     # Append the chunk to the list
#     all_chunks <<- append(all_chunks, list(dt_chunk))
# 
#     # Optionally, call garbage collection after processing each chunk
#     # gc()
#   }
# 
#   # Function to read and process the Parquet file in chunks
#   process_file_in_chunks <- function(file_path, chunk_size) {
#     # Read the entire Parquet file as an Arrow Table
#     table <- arrow::read_parquet(file_path)
# 
#     # Automatically include any column that starts with "qtr"
#     qtr_columns <- grep("^qtr", colnames(table), value = TRUE)
#     if (!is.null(columns_to_read)) {
#       table <- table[, union(columns_to_read, qtr_columns), drop = FALSE]
#     }
# 
#     # Convert Arrow Table to data.table directly
#     dt_table <- as.data.table(table)
# 
#     # Post-process parquet data to replace empty quotes with NA in specific columns
#     replace_empty_with_na <- function(x) {
#       fifelse(x == "", NA_character_, x)
#     }
# 
#     # Check and replace empty strings with NA for "qtr" columns
#     cols_to_check <- c("qtr1", "qtr2", "qtr3", "qtr4")
#     existing_cols <- cols_to_check[cols_to_check %in% colnames(dt_table)]
# 
#     if (length(existing_cols) > 0) {
#       dt_table[, (existing_cols) := lapply(.SD, replace_empty_with_na), .SDcols = existing_cols]
#     }
# 
#     # Convert the data.table to chunks
#     num_rows <- nrow(dt_table)
#     for (start in seq(1, num_rows, by = chunk_size)) {
#       end <- min(start + chunk_size - 1, num_rows)
#       chunk <- dt_table[start:end, ]
#       process_chunk(chunk)
#     }
#   }
# 
#   # Read and process the Parquet file from S3
#   aws.s3::s3read_using(
#     FUN = function(object) {
#       process_file_in_chunks(object, chunk_size)
#     },
#     bucket = Sys.getenv("S3_READ"),
#     object = my_file
#   )
# 
#   # Return the collected chunks as a single data.table
#   return(rbindlist(all_chunks, use.names = TRUE, fill = TRUE))
# }

# old read parquet file ---
read_parquet_file <- function(my_file, n_rows = Inf, columns_to_read = NULL) {
  print(paste0("reading parquet file: ", my_file))

  # Read the entire Parquet file
  parquet_data <- aws.s3::s3read_using(
    FUN = function(object) {
      arrow::read_parquet(object)
    },
    bucket = Sys.getenv("S3_READ"),
    object = my_file
  )
  
  print("file read complete")
  gc()

  # Limit the number of rows if n_rows is specified
  if (!is.infinite(n_rows) && n_rows > 0) {
    parquet_data <- parquet_data[1:min(n_rows, nrow(parquet_data)), ]
  }

  # Filter the columns after reading the entire file
  # Automatically include any column that starts with "qtr"
  qtr_columns <- grep("^qtr", colnames(parquet_data), value = TRUE)
  if (!is.null(columns_to_read)) {
    parquet_data <- parquet_data[, union(columns_to_read, qtr_columns), drop = FALSE]
  }

  # Post-process parquet data to replace empty quotes with NA in specific columns
  replace_empty_with_na <- function(x) {
    ifelse(x == "", NA, x)
  }

  # Check if parquet_data is a data frame and if the required columns exist
  if (inherits(parquet_data, "data.frame")) {
    cols_to_check <- c("qtr1", "qtr2", "qtr3", "qtr4")
    existing_cols <- cols_to_check[cols_to_check %in% colnames(parquet_data)]

    if (length(existing_cols) > 0) {
      parquet_data <- parquet_data %>%
        mutate(across(all_of(existing_cols), replace_empty_with_na))
    }
  }

  return(parquet_data)
}



# connect to s3
tryCatch({
  pdaprules::s3_connect()
},
error = function(e) {
  print(e)
})

# test connection
# my_items <- s3_list_bucket_items(bucket = Sys.getenv("S3_READ"), filter_parquet = TRUE)
# sample_file <- my_items[10,2]
# data_recent <- aws.s3::s3read_using(FUN = readr::read_delim, "|", escape_double = FALSE,
#                                     trim_ws = TRUE, col_types = readr::cols(.default = readr::col_character()), 
#                                     bucket = Sys.getenv("S3_READ"),
#                                     object = sample_file)
#data_recent <- read_parquet_file(sample_file)



################ OAuth Client information #####################################
if (interactive()) {
  # testing url
  options(shiny.port = 3123)
  APP_URL <- "http://127.0.0.1:3123/"# This will be your local host path
} else {
  # deployed URL
  APP_URL <- Sys.getenv("APP_URL") #This will be your shiny server path
}

{
  
  oauth_app <- httr::oauth_app(Sys.getenv("OAUTH_APPNAME"),
                               key = Sys.getenv("OAUTH_KEYNAME"),        # dhis2 = Client ID
                               secret = Sys.getenv("OAUTH_SECRET"), #dhis2 = Client Secret
                               redirect_uri = APP_URL
  )
  
  oauth_api <- httr::oauth_endpoint(base_url = paste0(Sys.getenv("BASE_URL"),"uaa/oauth"),
                                    request=NULL,# Documentation says to leave this NULL for OAuth2
                                    authorize = "authorize",
                                    access="token"
  )
  
  oauth_scope <- "ALL"
}

has_auth_code <- function(params) {
  
  return(!is.null(params$code))
}



server <- function(input, output, session) {

  user_input  <-  reactiveValues(authenticated = FALSE,
                                 status = "",
                                 d2_session = NULL,
                                 memo_authorized = FALSE,
                                 #modal = TRUE
                                 )
  # User and mechanisms reactive value pulled only once ----
  user <- reactiveValues(type = NULL)
  mechanisms <- reactiveValues(my_cat_ops = NULL)
  userGroups <- reactiveValues(streams = NULL)
  
  
  
  #### App Landing Page ---------------  
  
  output$password_modal_ui <- renderUI({
    if (!user_input$authenticated) {
      showModal(
        modalDialog(
          id = "passwordModal",
          title = "Please Login",
          footer = actionButton("login_button_oauth", "Log in with DATIM")
        )
      )
    }
  })

  # observeEvent("",{
  #   showModal(modalDialog(
  #     id = "passwordModal",
  #     title = "Login",
  #     textInput("username", "Username"),
  #     passwordInput("password", "Password"),
  #     footer = actionButton("submitBtn", "Submit", class = "btn-primary")
  #   )
  #   )
  # })
  # 
  
  # observeEvent(user_input$modal, {
  #   
  #   print(
  #     paste0(
  #       "your modal value is: ",
  #       user_input$modal
  #     )
  #   )
  #   
  #   if (user_input$modal) {
  #     showModal(
  #       modalDialog(
  #       id = "passwordModal",
  #       title = "Login",
  #       # textInput("username", "Username"),
  #       # passwordInput("password", "Password"),
  #       footer = actionButton("login_button_oauth", "Log in with DATIM")
  #       )
  #     )
  #   }
  # })
  
  #UI that will display when redirected to OAuth login agent
  output$ui_redirect <- renderUI({
    #print(input$login_button_oauth) useful for debugging
    if (!is.null(input$login_button_oauth)) { # nolint
      if (input$login_button_oauth > 0) { # nolint
        url <- httr::oauth2.0_authorize_url(oauth_api, oauth_app, scope = oauth_scope)
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))
      } else NULL
    } else NULL
  })
  
  # # is the user authenticated?
  # output$ui <- renderUI({
  #   if(user_input$authenticated == FALSE) {
  #     uiOutput("uiLogin")
  #   } else {
  #     uiOutput("authenticated")
  #   }
  # })
  
  
  observeEvent(input$submitBtn > 0, {
    
    print("submitting to modal...")
    #print(session$clientData$url_search)
    
    #Grabs the code from the url
    params <- parseQueryString(session$clientData$url_search)
    #Wait until the auth code actually exists
    req(has_auth_code(params))
    
    #Manually create a token
    token <- httr::oauth2.0_token(
      app = oauth_app,
      endpoint = oauth_api,
      scope = oauth_scope,
      use_basic_auth = TRUE,
      oob_value = APP_URL,
      cache = FALSE,
      credentials = httr::oauth2.0_access_token(endpoint = oauth_api,
                                                app = oauth_app,
                                                code = params$code,
                                                use_basic_auth = TRUE)
    )
    
    #print("here is your token...")
    #print(token)
    
    loginAttempt <- tryCatch({
      print("attempting to login")
      user_input$uuid <- uuid::UUIDgenerate()
      datimutils::loginToDATIMOAuth(base_url =  Sys.getenv("BASE_URL"),
                                    token = token,
                                    app = oauth_app,
                                    api = oauth_api,
                                    redirect_uri = APP_URL,
                                    scope = oauth_scope,
                                    d2_session_envir = parent.env(environment())
      )
      
      # we remove the login modal as we continue to validate access
      removeModal()
      showModal(
        modalDialog(
          id = "transitionModal",
          title = "One moment while we validate credentials ...",
          footer = NULL
        )
      )
      
      # DISALLOW USER ACCESS TO THE APP-----
      
      # store data so call is made only once
      userGroups$streams <-  datimutils::getMyStreams()
      user$type <- datimutils::getMyUserType()
      mechanisms$my_cat_ops <- datimutils::listMechs()
      
      # if a user is not to be allowed deny them entry
      if (!user$type %in% c(USG_USERS, PARTNER_USERS
                            )) {
        
        # alert the user they cannot access the app
        sendSweetAlert(
          session,
          title = "YOU CANNOT LOG IN",
          text = "You are not authorized to use this application",
          type = "error"
        )
        
        # log them out
        Sys.sleep(3)
        flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
        user_input$authenticated  <-  FALSE
        user_input$user_name <- ""
        user_input$authorized  <-  FALSE
        user_input$d2_session  <-  NULL
        d2_default_session <- NULL
        gc()
        session$reload()
        
      }
    },
    # This function throws an error if the login is not successful
    error = function(e) {
      flog.info(paste0("User ", input$user_name, " login failed. ", e$message), name = "usgpartners")
    }
    )
    
    if (exists("d2_default_session")) {
      
      removeModal()
      user_input$authenticated  <-  TRUE
      user_input$d2_session  <-  d2_default_session$clone()
      d2_default_session <- NULL
      
      #Need to check the user is a member of the PRIME Data Systems Group, COP Memo group, or a super user
      user_input$memo_authorized <-
        grepl("VDEqY8YeCEk|ezh8nmc4JbX", user_input$d2_session$me$userGroups) |
        grepl(
          "jtzbVV4ZmdP",
          user_input$d2_session$me$userCredentials$userRoles
        )
      flog.info(
        paste0(
          "User ",
          user_input$d2_session$me$userCredentials$username,
          " logged in."
        ),
        name = "usgpartners"
      )
      
      flog.info(
        paste0(
          "User ",
          user_input$d2_session$me$userCredentials$username,
          " logged in."
        ),
        name = "usgpartners"
      )
    }
    
  })
  
  
    # observeEvent(input$submitBtn, {
  #   
  #   # Check if entered username and password match
  #   tryCatch({
  #       datimutils::loginToDATIM(base_url = Sys.getenv("BASE_URL"),
  #                                username = input$username,
  #                                password = input$password
  #       )
  # 
  #       # store data so call is made only once
  #       user$type <- datimutils::getMyUserType()
  # 
  #       if(user$type %in% c(USG_USERS, PARTNER_USERS)){
  #         
  #         removeModal()
  #         showModal(modalDialog(includeHTML("intro_text.html"),
  #                               easyClose = TRUE))
  # 
  #       }
  #       },
  #       # This function throws an error if the login is not successful
  #       error = function(e) {
  #         showNotification("Incorrect username or password. Please try again.", type = "warning")
  #         flog.info(paste0("User ", input$username, " login failed."), name = "datapack")
  #       }
  #   )
  # 
  # })
  
  # observeEvent("", {
  #   showModal(modalDialog(includeHTML("intro_text.html"),
  #                         # footer = modalButton("Login")))
  #                         easyClose = TRUE))
  # })
  
  # observeEvent(input$intro, {
  #   removeModal()
  # })
  
  #### Recommender IntroJS ------
  steps <- reactive(data.frame(
    element = c(
      ".main-header",
      "#step1",
      "#step2",
      "#step3",
      "#step4",
      "#rec_summary",
      "#download"
    ),
    intro = c(
      HTML(
        "This sidebar menu is the main interactive element of the dashboard. Here you define the settings of the anomaly detection, upload and validate your data, and run the model."
      ),
      "Here you will select data for Recommender analysis. Select the country for analysis. If part of a region, select the region and identify country. Then, click Load Data",
      "Select fiscal year and quarter and click the Run Data Check button to make sure the required variables are included.",
      "Select whether you want to ignore low values and sites from which supporters to include.",
      "Click the here to run the model.",
      "All outputs are organized in two tabs.<br/><br/> The Summary tab will provide a data table for high level review of anomalous data. <br/><br/> The Observations tab will provide specific outputs based on the observation, age, and sex.",
      "After reviewing all outputs, click here to download either summary outputs or all outputs."
    ),
    position = c("right", "right", "right", "right", "right", "bottom", "left")
  ))

  # Code triggered when Recommender Instructions button is pressed, which has an ID "help" defined in ui.r
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
  
  # Time Series Instructions ----
  # Set up instructions for time series
  steps2 <- reactive(data.frame(
    element = c(
      ".main-header",
      "#tsstep1",
      "#tsstep2",
      "#tsstep3",
      "#tsstep4",
      "#ts_summary",
      "#download"
    ),
    intro = c(
      HTML(
        "This sidebar menu is the main interactive element of the dashboard. Here you define the settings of the anomaly detection, upload and validate your data, and run the model."
      ),
      "Here you will select data for Time Series analysis. Select the country for analysis. If part of a region, select the region and identify country. 
      Select Indicators for analysis. Then, click Load Data.",
      "Select fiscal year and quarter and click the Run Data Check button to make sure the required variables are included.",
      "Select whether you want to ignore low values and sites from which supporters to include.",
      "Click the here to run the model.",
      "All outputs are organized in two tabs.<br/><br/> The Summary tab will provide a data table for high level review of anomalous data. <br/><br/> The Observations tab will provide specific outputs based on the time series model.",
      "After reviewing all outputs, click here to download either summary outputs or all outputs."
    ),
    position = c("right", "bottom",
                 "right", "right", "right",
                 "bottom", "left")
  ))

  # Trigger instructions walkthrough when Time Series Instructions button is pressed
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

  # Global UI Updates ----

  # Update title of application at top based on value user selects from dropdown with ID "type"
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

    # Toogling UI output ----
  # Some global parameters that swaps what UI output objects appear based whether Recommender or TS is selected
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
    (input$type == 'Recommender')
  })
  
  outputOptions(output, 'rec1', suspendWhenHidden = FALSE)
  
  #RECOMMENDER SEX
  output$rec2 <- reactive({
    (input$type == 'Recommender')
  })
  
  outputOptions(output, 'rec2', suspendWhenHidden = FALSE)
  
  #RECOMMENDER AGE
  output$rec3 <- reactive({
    (input$type == 'Recommender')
  })
  
  outputOptions(output, 'rec3', suspendWhenHidden = FALSE)
  
  #RECOMMENDER FACILITY
  output$rec4 <- reactive({
    (input$type == 'Recommender')
  })
  
  outputOptions(output, 'rec4', suspendWhenHidden = FALSE)
  
  #RECOMMENDER PSNU
  output$rec5 <- reactive({
    (input$type == 'Recommender')
  })
  
  outputOptions(output, 'rec5', suspendWhenHidden = FALSE)
  
  #RECOMMENDER Disag Summary
  output$rec6 <- reactive({
    (input$type == 'Recommender')
  })
  
  outputOptions(output, 'rec6', suspendWhenHidden = FALSE)
  
  #RECOMMENDER Facility Summary
  output$rec7 <- reactive({
    (input$type == 'Recommender')
  })
  
  outputOptions(output, 'rec7', suspendWhenHidden = FALSE)
  
  #RECOMMENDER Facility Summary
  output$rec8 <- reactive({
    (input$type == 'Recommender')
  })
  
  outputOptions(output, 'rec8', suspendWhenHidden = FALSE)
  
  #RECOMMENDER IP Summary
  output$rec9 <- reactive({
    (input$type == 'Recommender')
  })
  
  outputOptions(output, 'rec9', suspendWhenHidden = FALSE)
  
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
  
  # Recommender Pipeline -------------------------------

  # Initiate list to hold intermediate dataframes needed to run recommender analysis
  input_reactive <- reactiveValues()

  # When data upload button is pressed, re-initialize list to clear out data from previously selected OU
  observeEvent(input$rec_upload, {
    input_reactive <- reactiveValues()
  })

  # When upload data button is selected, trigger following code
  observeEvent(input$rec_upload, {

    
    cols_to_read <- c(
      "facility", "indicator", "psnu", "operatingunit", "country",
      "numeratordenom", "standardizeddisaggregate", "statushiv", "ageasentered",
      "prime_partner_name", "fiscal_year", "funding_agency",
      "sitename", "sex", "otherdisaggregate_sub"
    )
    
    withProgress(message = 'Loading Data', value = 0.5, {

    my_items <- s3_list_bucket_items(bucket = Sys.getenv("S3_READ"), filter_parquet = TRUE)

    # Select table name that contains name of OU, which is input$country_selected,
    # contains "Site" as opposed to aggregate data, and "Recent" as opposed to historical
    # my_data_recent is then the table name to extract
    # western hemisphere does not exist in s3
    # it is made up of south america, and carribean
    if (input$country_selected == "Western Hemisphere") {
      
      ous <- c(
        "Central and South America Region",
        "Caribbean"
      )
      
      ous_pattern <- paste(ous, collapse = "|")
      
      my_data_recent <- my_items[
        grepl(ous_pattern, my_items$file_names, ignore.case = TRUE) &
          grepl("Site", my_items$file_names, ignore.case = TRUE) &
          grepl("Recent", my_items$file_names, ignore.case = TRUE),
      ]$path_names
      
      my_data_recent
    } else {
      
      my_data_recent <- my_items[grepl(input$country_selected, my_items$file_names, ignore.case = TRUE) &
                                   grepl("Site", my_items$file_names, ignore.case = TRUE) &
                                   grepl("Recent", my_items$file_names, ignore.case = TRUE),]$path_names
    }
    
    print(paste0("recommender uploading: ", my_data_recent))
    
    # Check if more than one file exists
    if (length(my_data_recent) > 1) {
      # Loop through and read each file
      data_recent <- lapply(my_data_recent, function(file) {
        read_parquet_file(file, columns_to_read = cols_to_read)
      })
      # Combine the data (assuming they have the same structure)
      data_recent <- do.call(rbind, data_recent)
    } else {
      # If only one file exists, read it directly
      data_recent <- read_parquet_file(my_data_recent, columns_to_read = cols_to_read)
    }
    data_recent <- dplyr::bind_rows(data_recent)


    #data_recent <- read_parquet_file(my_data_recent, columns_to_read = cols_to_read)
    
    # Conditionally rename columns if they exist
    column_renames <- c("prime_partner_name" = "primepartner",
                        "funding_agency" = "fundingagency",
                        "countryname" = "country")
    
    existing_columns <- intersect(names(data_recent), names(column_renames))
    data_recent <- data_recent %>%
      rename_at(vars(all_of(existing_columns)), ~ column_renames[existing_columns])
    
    # drop extraneous rows from funding agency
    data_recent <- data_recent[!(tolower(data_recent$fundingagency) %in% c("dedup", "default", "data reported above facility level")), ]
    
    # drop keep only major indicators (avoid correlations with others)
    data_recent <- data_recent[data_recent$indicator %in% quarterly_indicators, ]
    
    input_reactive$data_loaded <- data_recent
    rm(data_recent)
    gc()
  
    print(table(input_reactive$data_loaded$country))
    
    
    if (input_reactive$data_loaded$operatingunit[1] == "Asia Region") {
      input_reactive$data_loaded <- input_reactive$data_loaded %>% filter(country %in% input$asiafilter)
    }
    if (input_reactive$data_loaded$operatingunit[1] == "West Africa Region") {
      input_reactive$data_loaded <- input_reactive$data_loaded %>% filter(country %in% input$westafricafilter)
    }
    if (input_reactive$data_loaded$operatingunit[1] == "Western Hemisphere Region") {
      input_reactive$data_loaded <- input_reactive$data_loaded %>% filter(country %in% input$westernhemishpherefilter)
    }

    
    
    print(nrow(input_reactive$data_loaded))
    updateNumericInput(session,
                       "year",
                       value = max(input_reactive$data_loaded$fiscal_year),
                       min = min(input_reactive$data_loaded$fiscal_year),
                       max = max(input_reactive$data_loaded$fiscal_year))

    # Update funder options in dropdown based on funding agencies that appear in dataset
    updatePickerInput(session,
                       "recfunder",
                       choices = unique(input_reactive$data_loaded$fundingagency),
                      selected = unique(input_reactive$data_loaded$fundingagency),
                      options = list(`actions-box` = TRUE))

    # Update default quarter displayed based on most recent quarter with data in dataset
    # Because quarters are wide, this is not as straightforward as with year, so we must check
    # for presence of values in those quarters.
    # Quarters can have values present that are calculated based on data from previous quarters. So,
    # we cannot select the most recent quarter with any values present. Instead, we check for the most recent
    # quarter where at least 50% of values are present.

    # First, take quarters 2, 3, and 4- if the year is present, then quarter 1 will have values and will be our
    # default 
    most_recent_quarter <- input_reactive$data_loaded %>%
      filter(fiscal_year == max(input_reactive$data_loaded$fiscal_year)) %>%
      select(qtr2, qtr3, qtr4)

    # If most values in quarter 4 are present (not NA), then update displayed quarter to qtr4
    if((sum(is.na(most_recent_quarter$qtr4))/nrow(most_recent_quarter))<.5){
      
      updateSelectInput(session,
                        "quarter",
                        choices = c('qtr1', 'qtr2', 'qtr3', 'qtr4'),
                        selected = 'qtr4')

    # If most values in quarter 4 are NA, then check for quarter 3 and update displayed quarter accordingly
    } else if((sum(is.na(most_recent_quarter$qtr3))/nrow(most_recent_quarter))<.5){
      
      updateSelectInput(session,
                        "quarter",
                        choices = c('qtr1', 'qtr2', 'qtr3', 'qtr4'),
                        selected = 'qtr3')
    # If most values in quarter 3 are NA, then check for quarter 2 and update displayed quarter accordingly
    }  else if((sum(is.na(most_recent_quarter$qtr2))/nrow(most_recent_quarter))<.5){
      
      updateSelectInput(session,
                        "quarter",
                        choices = c('qtr1', 'qtr2', 'qtr3', 'qtr4'),
                        selected = 'qtr2')

    # If most values in quarter 2 are NA, then display quarter 1
    } else {
      
      updateSelectInput(session,
                        "quarter",
                        choices = c('qtr1', 'qtr2', 'qtr3', 'qtr4'),
                        selected = 'qtr1')
      
    }

    # Create popup for user to indicate that upload process is complete
    shinyalert("Proceed",
               "Select Year/Quarter for Analysis and Run Data Checks.",
               type="success")
    
    })
  })


  # Recommender - Information Prompts for Users ----

  # Three kinds of prompts are included:
  # 1. If user has already uploaded data and then selects different OU, they must reupload data
  # 2. If user has run data checks and then changes year selected, they must rerun data checks
  # 3. If user has run data checks and then changes quarter selected, they must rerun data checks

  # Intitailize list of values to keep track of user selections to inform informational prompts to avoid user error
  values <- reactiveValues()
  
  # Counter of country selections, year selections, and quarter selections
  values$num_country_selections <- 0
  values$num_year_selections <- 0
  values$num_quarter_selections <- 0

  # Prompt to reupload data when country selection changes. Code triggered when value of country selected changes
  observeEvent(input$country_selected, {
    # Increment counter
    values$num_country_selections <- values$num_country_selections + 1
    # If data is uploaded and country selection changes, then inform user data must be reuploaded to pull
    # in data for newly selected country
    if(values$num_country_selections >= 2 & input$rec_upload >= 1){
      shinyalert("Reminder",
                 "Remember to reload data and rerun data checks after changing the country selected!",
                 type = "warning")
    }
  })

  # When data is uploaded, reset year and quarter selection counters as prompts are relevant for a given dataset.
  observeEvent(input$rec_upload, {
    values$num_year_selections <- 0
    values$num_quarter_selections <- 0
  })

  # When year selected changes, increment counter. If run data check has already been pressed, then prompt user
  # to rerun data checks. Otherwise, previously selected year will be used.
  observeEvent(input$year, {
    values$num_year_selections <- values$num_year_selections + 1
    if(values$num_year_selections >= 2 & input$recdatacheck >= 1){
      shinyalert("Reminder",
                 "Remember to rerun data checks after changing the year selected!",
                 type = "warning")
    }
  })

  # When quarter selected changes, increment counter. If run data check has already been pressed, then prompt user
  # to rerun data checks. Otherwise, previously selected quarter will be used.
  observeEvent(input$quarter, {
    values$num_quarter_selections <- values$num_quarter_selections + 1
    if(values$num_quarter_selections >= 3 & input$recdatacheck >= 1){
      shinyalert("Reminder",
                 "Remember to rerun data checks after changing the quarter selected!",
                 type = "warning")
    }
  })

  # We also disable buttons that cannot be run, to avoid user error
  # For example, data checks cannot be run until data is uploaded.
  # When rec_upload is 0, meaning data has not been uploaded yet, then datacheck button is disabled.
  # Once data is uploaded, button is enabled. 
  # Same logic applies later with data checks and run model button.
  observe({
    if(input$rec_upload == 0){
      disable("recdatacheck")
    }
    else{
      enable("recdatacheck")
    }
  })

  # Recommender - Data Checks ------------

  # This code processes observations from the year and quarter selected and is triggered when the data check button is pressed
  observeEvent(input$recdatacheck, {

    # Send progress updates to users
    withProgress(message = 'Beginning Checks', value = 0.3, {

      # Create a copy of the uploaded dataset for processing
      input_reactive$data_recent <- input_reactive$data_loaded
    
      incProgress(.3, detail = paste("Processing Data"))
      
    # Confirm they are strings and not factors
    input_reactive$data_recent <- input_reactive$data_recent %>%
      mutate(across(c(sitename, psnu, facility, indicator, ageasentered, sex, primepartner, standardizeddisaggregate, numeratordenom, otherdisaggregate_sub), as.character)) %>%
      mutate(across(c(qtr1, qtr2, qtr3, qtr4), as.numeric))
      
      # Trigger garbage collection to free up memory
      gc()
      print(names(input_reactive$data_recent))
    
    # filter to the fiscal year entered by the user
    input_reactive$data_recent <- input_reactive$data_recent %>% filter(fiscal_year == input$year)
    
    # remove the rows that report on Total Numerator or Total Denominator
    # remove rows that are aggregates of age groups (e.g. 15+ and 50+)
    input_reactive$data_recent <- input_reactive$data_recent %>%
      filter(fiscal_year == input$year,
             !standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
             !tolower(facility) == "data reported above facility level",
             !grepl("\\+", ageasentered))
    
    # label indicators with N and D for those for which both numerators and denominators are reported
    input_reactive$data_recent$indicator <- paste0(input_reactive$data_recent$indicator, "_", input_reactive$data_recent$numeratordenom)
    
    # add transgender to the sex column
    input_reactive$data_recent$tg <-
      ifelse(grepl("TG", input_reactive$data_recent$otherdisaggregate_sub), "Transgender", "")

    
    #input_reactive$data_recent$sex2 <- paste(input_reactive$data_recent$sex, input_reactive$data_recent$tg, sep = "")
    input_reactive$data_recent$sex2 <- paste0(input_reactive$data_recent$sex, input_reactive$data_recent$tg)
    
    cols_to_keep <-
      c(
        "sitename",
        "psnu",
        "facility",
        "indicator",
        "numeratordenom",
        "standardizeddisaggregate",
        "ageasentered",
        "sex2",
        "fiscal_year",
        "primepartner",
        "fundingagency",
        input$quarter
      )
    input_reactive$data_recent <- input_reactive$data_recent[, cols_to_keep]
    input_reactive$data_recent <- input_reactive$data_recent %>% dplyr::rename(sex = sex2)
    
    input_reactive$dat_disag_out1 <- input_reactive$data_recent %>%
      mutate(kp = ifelse(grepl("KeyPop", standardizeddisaggregate), "Yes", "No")) %>%
      select(-numeratordenom, -standardizeddisaggregate) %>%
      rename_with(~ gsub("[0-9]", "", .x), matches("[0-9]")) %>%
      filter(!is.na(qtr)) %>%
      group_by(facility, ageasentered, sex, indicator, kp, psnu, primepartner, fundingagency) %>%
      summarise(qtr_sum = sum(qtr, na.rm = TRUE))
    

    # for disaggregate output - pivot wider to get MER indicators in wide format
    input_reactive$dat_disag_out <- input_reactive$dat_disag_out1 %>%
      pivot_wider(names_from = "indicator", values_from = "qtr_sum") %>%
      as.data.frame()


    input_reactive$dat_facility_out <- input_reactive$data_recent %>%
      rename_with(~ gsub("[0-9]", "", .x), matches("[0-9]")) %>%
      filter(!is.na(qtr)) %>%
      group_by(facility, indicator, psnu, primepartner, fundingagency) %>%
      summarise(qtr_sum = sum(qtr, na.rm = TRUE)) %>%
      pivot_wider(names_from = indicator, values_from = qtr_sum) %>%
      as.data.frame()
    

    # Run checks ---------------
    incProgress(.3, detail = paste("Checking Names and Dimensions"))

    # Run checks
      
    # Check to confirm if fiscal year selected by user for analysis exists in the dataset
    if(!input$year %in% unique(input_reactive$data_recent$fiscal_year)){
      shinyalert("Check the data file", "Please confirm the fiscal year selected is included in the file uploaded.", type="error")
    } else
    if(nrow(input_reactive$data_recent) < 10){
      shinyalert("Check the data file", "Please confirm the year and quarter selected has data popuated.", type = "error")
    } else
    # Check to confirm if quarter selected by user for analysis exists in the dataset
    if(!input$quarter %in% names(input_reactive$data_recent)){
      shinyalert("Check the data file", "Please confirm the quarter selected is included in the file uploaded.", type="error")
    } else
    if(!"sitename" %in% names(input_reactive$data_recent)){
      shinyalert("Check the data file", "Please confirm the variable 'sitename' is included", type = "error")
      } else
    if(!"psnu" %in% names(input_reactive$data_recent)){
      shinyalert("Check the data file", "Please confirm the variable 'psnu' is included", type = "error")
      } else
    if(!"facility" %in% names(input_reactive$data_recent)){
      shinyalert("Check the data file", "Please confirm the variable 'facility' is included", type = "error")
      } else
    if(!"indicator" %in% names(input_reactive$data_recent)){
      shinyalert("Check the data file", "Please confirm the variable 'indicator' is included", type = "error")
      } else
    if(!"numeratordenom" %in% names(input_reactive$data_recent)){
      shinyalert("Check the data file", "Please confirm the variable 'numeratordenom' is included", type = "error")
    } else
    if(!"standardizeddisaggregate" %in% names(input_reactive$data_recent)){
      shinyalert("Check the data file", "Please confirm the variable 'standardizeddisaggregate' is included", type = "error")
    } else
    if(!"ageasentered" %in% names(input_reactive$data_recent)){
      shinyalert("Check the data file", "Please confirm the variable 'ageasentered' is included", type = "error")
    } else
    if(!"sex" %in% names(input_reactive$data_recent)){
      shinyalert("Check the data file", "Please confirm the variable 'sex' is included", type = "error")
    } else
    if(!"primepartner" %in% names(input_reactive$data_recent)){
      shinyalert("Check the data file", "Please confirm the variable 'primepartner' is included", type = "error")
    } else
    if(n_distinct(input_reactive$dat_disag_out1$indicator) < 3 | nrow(input_reactive$dat_disag_out) < 100){
      shinyalert("Check the data file", "There are fewer than three indicators present and/or fewer than 100 observations.", type="error")
      } else
    shinyalert("Proceed", "Continue to Run Models", type="success")

    
    # Set data_recent and data_disag_out1 to NULL since we no longer need these intermediary tables
    # We retain data_loaded, so that we can rerun this process should the user change the year or quarter
    # We retain dat_disag_out and dat_facility_out which we'll feed to the modeling code
    input_reactive$data_recent <- NULL
    input_reactive$data_disag_out1 <- NULL
    gc()
    
  })
  })

  # After data checks are run, enable Run Model button
  observe({
    if(input$recdatacheck == 0){
      disable("recrun")
    }
    else{
      enable("recrun")
    }
  })
  
  # Run Recommender Model ----

  # Initialize list to store outputs of each model run and summary tables
  forout_reactive <- reactiveValues()

  # Whenever model is run (e.g. for a second time), re-initialize list to clear out prior results
  observeEvent(input$recrun, {
    forout_reactive <- reactiveValues()
  })

  # Code triggered when Run Model button is pressed
  observeEvent(input$recrun, {

    gc()
    all_outputs <- NULL
    site_sex_outliers <- NULL
    site_age_outliers <- NULL
    facility_outputs <- NULL

    # Provide users with progress updates, indicating which model type is being run
    withProgress(message = 'Running Models', value = 0, {
      # Print progress update
      incProgress(.2, detail = paste("Running Model with All Disaggregrates"))

      # Additional check - if there are not enough values compared to indicators, there is not enough data to run analysis
      if(nrow(input_reactive$dat_disag_out)/ncol(input_reactive$dat_disag_out) < 3){
        shinyalert(title = "Warning",
                   text = "Insufficient Data to Run Analysis with All Disaggregates",
                   type = "warning")
      }

      # If there is enough data, proceed
      if(nrow(input_reactive$dat_disag_out)/ncol(input_reactive$dat_disag_out) > 3){

        # Apply the runRecAnalysis function on entire dataset, creating a single set of global parameters
        # In a tryCatch because code assumes outliers will be found. If none are, then print error message
        all_outputs <- tryCatch({
          # runRecAnalysis is defined in utils.R
          runRecAnalysis(dat = input_reactive$dat_disag_out, 
                         keys = keys_disag) 
        }, error = function(cond){
          message("No Outliers Found with All Disags")
          #message(cond)
        })
        
        # Sort outputs by distance metric - SortOutputs function defined in utils.R
        # Again, in tryCatch, in case no outliers were found
        all_outputs <- tryCatch({
          sortOutputs(
            all_outputs,
            keys = keys_disag,
            scenario_tmp = "all",
            return_all = TRUE,
            min_thresh = input$min_thresh,
            fund = input$recfunder
          )
        }, error = function(cond) {
          message("No Outliers Found with All Disags")
          #message(cond)
        })

        # If outliers were found, then all_outputs will exist
        # Generate table that displays observation-level results and return to UI as output$rec1
        # if(exists("all_outputs")){
        if(!is.null(all_outputs)){

          z <- all_outputs %>% mutate(outlier_sp = ifelse(outlier_sp == 1, "Yes", "No"))

          output$rec1 <- reactable::renderReactable({
            reactable::reactable(z)
          })

          forout_reactive$all_outputs <- all_outputs 
        } #else {
        #   shinyalert("Proceed",
        #              "No outliers found with all disaggregates",
        #              type = "warning")
        # }
      }

      # Update progress for user to indicate next set of models are being run
      incProgress(.2, detail = paste("Running Model with Sex Disaggregates"))

      ## Run models generating patterns by sex
      dat <- input_reactive$dat_disag_out
      
      # Confirm sex is a string and not a factor
      dat$sex <- as.character(dat$sex)
      
      # Limit to observations that are male or female
      dat <- dat[dat$sex %in% c("Male", "Female"), ]
      
      # Split dataset by sex and run Recommender analysis on each subset
      site_split <- split(dat, dat$sex)

      # Initiate list to store results for each sex
      site_out <- list()
      for (j in 1:length(site_split)) {

        # Same check as before - make sure there are at least 3X columns as observations
        # Otherwise throw warning to user and don't proceed with analysis
        if(nrow(site_split[[j]])/ncol(site_split[[j]]) < 3){
          shinyalert(title = "Warning",
                     text = paste("Insufficient Data to Run Analysis for Sex Subgroup"),
                     type = "warning")
        }

        # If there is sufficent data, then proceed
        if(nrow(site_split[[j]])/ncol(site_split[[j]]) > 3){
          site_out[[j]] <- tryCatch({
            runRecAnalysis(site_split[[j]],
                           keys = keys_disag)
          }, error = function(cond) {
            message("No Outliers Found for Sex Disag")
            #message(cond)
          })
        }
      }
      
      # stack the outputs for male and female
      site_sex_outliers <- do.call(plyr::rbind.fill, site_out)

      # Sort outputs as before
      site_sex_outliers <- tryCatch({
        sortOutputs(
          site_sex_outliers,
          keys = keys_disag,
          scenario_tmp = "sex",
          return_all = TRUE,
          min_thresh = input$min_thresh,
          fund = input$recfunder
        )
      }, error = function(cond) {
        message("No Outliers Found for Sex Disag")
        #message(cond)
      })

      # If outliers are found, create output table to send back to UI and store table for download
      # if (exists("site_sex_outliers")) {
      if (!is.null(site_sex_outliers)) {
        output$rec2 <- reactable::renderReactable({
            reactable::reactable(site_sex_outliers %>% mutate(outlier_sp = ifelse(outlier_sp == 1, "Yes", "No")))
          })

        forout_reactive$site_sex_outliers <- site_sex_outliers 
      } #else {
      #   shinyalert("Proceed",
      #              "No outliers found by sex disaggregates",
      #              type = "success")
      # }
      
      # Update progress for users
      incProgress(.2, detail = paste("Running Model with Age Disaggregrates"))

      ## Run model with age disaggregates
      # Filter for observations with relevant age categories
      dat <- input_reactive$dat_disag_out %>%
        filter(ageasentered %in% c("01-04", "05-09", "10-14", "15-19", "20-24",
                                   "25-29", "30-34", "35-39", "40-44", "45-49",
                                   "50+"))
      
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
      
      if(length(site_split)>=1){
        
        # Loop through list and run Recommender analysis on each
        site_out <- list()
        
        for (j in 1:length(site_split)) {
          
          if(nrow(site_split[[j]])/ncol(site_split[[j]]) < 3){
            shinyalert(title = "Warning",
                       text = paste("Insufficient Data to Run Analysis for Age Group:", site_split[[j]]$agegroup[1]),
                       type = "warning")
          }
          
          if(nrow(site_split[[j]])/ncol(site_split[[j]]) >= 3){
            
            site_out[[j]] <- tryCatch({
              # append "agegroup" to vector of keys
              runRecAnalysis(site_split[[j]],
                             keys = c(keys_disag, "agegroup"))
            }, error = function(cond) {
              message(paste(
                "Insufficient Data to Run Disag for Age Group:",
                names(site_split)[j]
              ))
              # message(cond)
            })
          }
        }
        
        
        # stack the outputs and drop the age group variable so that outputs from all runs can be appropriately stacked
        site_age_outliers <- tryCatch({
          do.call(plyr::rbind.fill, site_out) %>% select(-agegroup)
        }, error = function(cond){
          message("Insufficient data to run by either age disaggregate.")
        })
        
        # Sort outputs by anomalous distance
        site_age_outliers <- tryCatch({
          sortOutputs(
            site_age_outliers,
            keys = keys_disag,
            scenario_tmp = "age",
            return_all = TRUE,
            min_thresh = input$min_thresh,
            fund = input$recfunder
          )
        }, error = function(cond) {
          message("No Outliers Found for Disag for Age Group:")
          # message(cond)
        })

        # If outliers are found, create table to send back to UI
        # if (exists("site_age_outliers")) {
        if (!is.null(site_age_outliers)) {
          
          output$rec3 <- reactable::renderReactable({
            reactable::reactable(site_age_outliers %>% mutate(outlier_sp = ifelse(outlier_sp == 1, "Yes", "No")))
          })


          forout_reactive$site_age_outliers <- site_age_outliers 
        } #else {
        #   shinyalert("Proceed",
        #              "No outliers found by age disaggregate",
        #              type = "warning")
        # }
      }
      
      # update progress for users
      incProgress(.2, detail = paste("Running Model at Facility Level"))

      # Fun facility-level analysis
      if(nrow(input_reactive$dat_facility_out)/ncol(input_reactive$dat_facility_out) < 3){
        shinyalert(title = "Warning",
                   text = "Insufficient Data to Run Analysis at Facility Level",
                   type = "warning")
      }
      
      if(nrow(input_reactive$dat_facility_out)/ncol(input_reactive$dat_facility_out) > 3){
        
        facility_outputs <- tryCatch({
          runRecAnalysis(dat = input_reactive$dat_facility_out,
                         keys = keys_facility)
        }, error = function(cond) {
          message("No Outliers Found at Facility Level")
          #message(cond)
        })
        facility_outputs <- tryCatch({
          sortOutputs(
            facility_outputs,
            keys = keys_facility,
            scenario_tmp = "facility",
            return_all = TRUE,
            min_thresh = input$min_thresh,
            fund = input$recfunder
          )
        }, error = function(cond) {
          message("No Outliers Found at Facility Level")
          #message(cond)
        })
        
        # if (exists("facility_outputs")) {
        if (!is.null(facility_outputs)) {
          
          output$rec4 <- reactable::renderReactable({
            reactable::reactable(facility_outputs %>% mutate(outlier_sp = ifelse(outlier_sp == 1, "Yes", "No")))
          })

          forout_reactive$facility_outputs <- facility_outputs 
        } #else {
        #   shinyalert("Proceed",
        #              "No outliers found at facility level.",
        #              type = "warning")
        # }
        
      }

      ## Now that all model runs are complete, create summary tables
      # First, combine output tables in a list and keep only those that are non-null, meaning they have results
      disags_list <-
        list(all_outputs, site_sex_outliers, site_age_outliers)
      disags_list <- disags_list[lengths(disags_list) != 0]
      
      facility_list <- list(facility_outputs)
      facility_list <- facility_list[lengths(facility_list) != 0]

      # If there are any results, then create a summary table
      # createSummaryTab is defined in utils.R
      if (length(disags_list) > 0) {
        incProgress(.2, detail = paste("Creating Summary Disaggregate Tab"))
        disags_summary <-
          createSummaryTab(dat_summary_list = disags_list)
        
        
        output$rec6 <- reactable::renderReactable({
          reactable::reactable(disags_summary$summary)
        })
        
        forout_reactive$disags_summary <- disags_summary$summary
      }
      
      # Create summary tab for anomalies found using data aggregated at facility level
      if (length(facility_list) > 0) {
        incProgress(.2, detail = paste("Creating Summary Facility Tab"))
        facility_summary <-
          createSummaryTab(dat_summary_list = facility_list,
                           disag = FALSE)
        
        output$rec7 <- reactable::renderReactable({
          reactable::reactable(facility_summary$summary)
        })
        
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
      
      # Generate scorecard - createScoreCard defined in utils
      incProgress(.2, detail = paste("Creating Scorecard"))
      scorecard <- createScoreCard(scorecard_in = dat_tmp)

      
      output$rec8 <- reactable::renderReactable({
        reactable::reactable(scorecard)
      })
      forout_reactive$scorecard <- scorecard
      
      # Create IP scorecard sheet - summarize number of outliers by indicator by partner
      cover_ip <- dat_tmp %>%
        group_by(primepartner, Indicator) %>%
        summarize(Outliers = n(), .groups = "drop") %>% 
        mutate(primepartner = as.character(primepartner))

      # Then, for each IP, get the five most commonly flagged indicators
      ips <- unique(cover_ip$primepartner)
      ip_cover <- data.frame()
      for(i in 1:length(ips)){
        dat_tmp <- cover_ip %>% filter(primepartner == ips[i]) %>%
          arrange(desc(Outliers)) %>%
          mutate(rownum = row_number()) %>%
          filter(rownum <= 5)
        
        ip_cover[1:(min(length(dat_tmp$Indicator), 5)), i] <- dat_tmp$Indicator
        names(ip_cover)[i] <- ips[i]
      }

      
      # output$rec9 = DT::renderDT(ip_cover)
      output$rec9 <- reactable::renderReactable({
        reactable::reactable(ip_cover)
      })
      
      forout_reactive$ipcover <- ip_cover

      # notify user that model runs are complete
      shinyalert("Proceed", "Completed Recommender Models", type = "success")
      
    })
  })


  # Time Series ----------------------

  # initialize list to store tables
  input_reactive_ts <- reactiveValues()

  # any time new data is uploaded, re-initialize list to remove what's there
  observeEvent(input$ts_upload, {
    input_reactive_ts <- reactiveValues()
  })

  # if no indicators are selected, then there's nothing to upload, so grey out upload button
  observe({
    if(is.null(input$ts_vars)){
      disable("ts_upload")
    }
    else{
      enable("ts_upload")
    }
  })

  # code chunk executed when TS data upload button is pressed
  observeEvent(input$ts_upload, {

    # for time series, we want to pull in historical data, so we do two pulls
    # first, we'll retreive data from the table that contains the string "Recent"
    # then, we'll retrieve data from the table that contains the string "Historical"
    
    cols_to_read <- c(
      "facility", "indicator", "psnu", "operatingunit", "country",
      "numeratordenom", "standardizeddisaggregate", "statushiv", "ageasentered",
      "prime_partner_name", "fiscal_year", "funding_agency"
    )
    
    
    withProgress(message = 'Loading Recent Data', value = 0.3, {

      
      my_items <- s3_list_bucket_items(bucket = Sys.getenv("S3_READ"), filter_parquet = TRUE)

      # Select table name that contains name of OU, which is input$country_selected,
      # contains "Site" as opposed to aggregate data, and "Recent" as opposed to historical
      # my_data_recent is then the table name to extract
      # western hemisphere does not exist in s3
      # it is made up of south america, and carribean
      if (input$country_selected_ts == "Western Hemisphere") {
        
        ous <- c(
          "Central and South America Region",
          "Caribbean"
        )
        
        ous_pattern <- paste(ous, collapse = "|")
        
        my_data_recent <- my_items[
          grepl(ous_pattern, my_items$file_names, ignore.case = TRUE) &
            grepl("Site", my_items$file_names, ignore.case = TRUE) &
            grepl("Recent", my_items$file_names, ignore.case = TRUE),
        ]$path_names
        
        my_data_recent
      } else {
        
        my_data_recent <- my_items[grepl(input$country_selected_ts, my_items$file_names, ignore.case = TRUE) &
                                     grepl("Site", my_items$file_names, ignore.case = TRUE) &
                                     grepl("Recent", my_items$file_names, ignore.case = TRUE),]$path_names
      }

      print(paste0("upload: ", my_data_recent))

      
      #data_recent <- read_parquet_file(my_data_recent, columns_to_read = cols_to_read)
      # Check if more than one file exists
      if (length(my_data_recent) > 1) {
        # Loop through and read each file
        data_recent <- lapply(my_data_recent, function(file) {
          read_parquet_file(file, columns_to_read = cols_to_read)
        })
        # Combine the data (assuming they have the same structure)
        data_recent <- do.call(rbind, data_recent)
      } else {
        # If only one file exists, read it directly
        data_recent <- read_parquet_file(my_data_recent, columns_to_read = cols_to_read)
      }
      
      data_recent <- dplyr::bind_rows(data_recent)
      
      
      print(dim(data_recent))
      print(names(data_recent))
      
      if("prime_partner_name" %in% names(data_recent)){
        data_recent$primepartner <- data_recent$prime_partner_name
      }
      if("funding_agency" %in% names(data_recent)){
        data_recent$fundingagency <- data_recent$funding_agency
      }
      if("countryname" %in% names(data_recent)){
        data_recent$country <- data_recent$countryname
      }
      
    })

    # # now, repeat with earlier dataset
    # withProgress(message = 'Loading Historical Data', value = 0.7, {
    #   
    #   # Select table name that contains name of OU, which is input$country_selected,
    #   # contains "Site" as opposed to aggregate data, and "Recent" as opposed to historical
    #   # my_data_recent is then the table name to extract
    #   # western hemisphere does not exist in s3
    #   # it is made up of south america, and carribean
    #   if (input$country_selected_ts == "Western Hemisphere") {
    #     
    #     ous <- c(
    #       "Central and South America Region",
    #       "Caribbean"
    #     )
    #     
    #     ous_pattern <- paste(ous, collapse = "|")
    #     
    #     my_data_historical <- my_items[
    #       grepl(ous_pattern, my_items$file_names, ignore.case = TRUE) &
    #         grepl("Site", my_items$file_names, ignore.case = TRUE) &
    #         grepl("Historic", my_items$file_names, ignore.case = TRUE),
    #     ]$path_names
    #     
    #     my_data_historical
    #   } else {
    #     
    #     my_data_historical <- my_items[grepl(input$country_selected_ts, my_items$file_names, ignore.case = TRUE) &
    #                                      grepl("Site", my_items$file_names, ignore.case = TRUE) &
    #                                      grepl("Historic", my_items$file_names, ignore.case = TRUE),]$path_names
    #   }
    #   
    #   
    #   print(paste0("historical: ", my_data_historical))
    # 
    #   #data_historical <- read_parquet_file(my_data_historical, columns_to_read = cols_to_read)
    #   # Check if more than one file exists
    #   if (length(my_data_historical) > 1) {
    #     # Loop through and read each file
    #     data_historical <- lapply(my_data_historical, function(file) {
    #       read_parquet_file(file, columns_to_read = cols_to_read)
    #     })
    #     # Combine the data (assuming they have the same structure)
    #     data_historical <- do.call(rbind, data_historical)
    #   } else {
    #     # If only one file exists, read it directly
    #     data_historical <- read_parquet_file(my_data_historical, columns_to_read = cols_to_read)
    #   }
    #   
    #   data_historical <- dplyr::bind_rows(data_historical)
    #   
    #   print(dim(data_historical))
    #   
    #   if("prime_partner_name" %in% names(data_historical)){
    #     data_historical$primepartner <- data_historical$prime_partner_name
    #   }
    #   if("funding_agency" %in% names(data_historical)){
    #     data_historical$fundingagency <- data_historical$funding_agency
    #   }
    #   if("countryname" %in% names(data_historical)){
    #     data_historical$country <- data_historical$countryname
    #   }
# 
#       # Combine the two datasets
#       data_all <- bind_rows(data_recent, data_historical)
      
    data_all <- data_recent
    
      # drop extraneous rows from funding agency
      data_all <- data_all[!(tolower(data_all$fundingagency) %in% c("dedup", "default", "data reported above facility level")), ]
      
      # add data to list so it persists
      input_reactive_ts$data_loaded <- data_all

      # If region is selected, filter for country/countries
      if (input_reactive_ts$data_loaded$operatingunit[1] == "Asia Region") {
        input_reactive_ts$data_loaded <- input_reactive_ts$data_loaded %>% filter(country == input$asiafilter_ts)
      }
      
      if (input_reactive_ts$data_loaded$operatingunit[1] == "West Africa Region") {
        input_reactive_ts$data_loaded <- input_reactive_ts$data_loaded %>% filter(country == input$westafricafilter_ts)
      }
      
      if (input_reactive_ts$data_loaded$operatingunit[1] == "Western Hemisphere Region") {
        input_reactive_ts$data_loaded <- input_reactive_ts$data_loaded %>% filter(country == input$westernhemishpherefilter_ts)
      }
      
      ## Logic to calculate what years and quarters are valid
      # For time series, we need to have at least 12 previous quarters of data
      # Get unique years and quarters, arrange in reverse chronological order
      yrmt_check <- input_reactive_ts$data_loaded %>%
        filter(indicator %in% input$ts_vars) %>% # filter for selected indicators
        select(fiscal_year, qtr1, qtr2, qtr3, qtr4) %>%
        pivot_longer(cols = !fiscal_year,
                     names_to = "qtr",
                     values_to = "value",
                     values_drop_na = TRUE) %>%
        mutate(qtr = factor(qtr, levels = c("qtr1", "qtr2", "qtr3", "qtr4"))) %>%
        group_by(fiscal_year, qtr) %>%
        summarize(count = n()) %>%
        arrange(desc(fiscal_year), desc(qtr)) 

      # keep rows that are not in the bottom twelve - this will leave any year-quarter with at least 12 prior observations
      yrmt_keep <- yrmt_check[1:(nrow(yrmt_check)-12), ]

      # now, update year selection to year-quarter combinations that have are valid
      updateNumericInput(session,
                         "tsyear",
                         value = max(yrmt_keep$fiscal_year),
                         min = min(yrmt_keep$fiscal_year),
                         max = max(yrmt_keep$fiscal_year))
      
      # then, update quarter options that are valid for that fiscal year
      # create a reactive value that will return valid quarter options for the year selected
      updateSelectInputChoices <- reactive({
        
        numeric_value <- input$tsyear
        
        # If year selected is most recent year, then get unique quarters for that year from table of valid year-quarters
        choices <- if(input$tsyear == max(yrmt_keep$fiscal_year)){
          as.character(unique(yrmt_keep$qtr[yrmt_keep$fiscal_year==max(yrmt_keep$fiscal_year)]))
        # If year selected is earliest year, get unique valid quarters from that year
        } else if(input$tsyear == min(yrmt_keep$fiscal_year)){
          as.character(unique(yrmt_keep$qtr[yrmt_keep$fiscal_year==min(yrmt_keep$fiscal_year)])) 
        # If selected year is in the middle, then all four quarters are valid options
        } else {c('qtr1', 'qtr2', 'qtr3', 'qtr4')}
        
        return(choices)
      })
      
      # Update quarter options whenever the numericInput changes
      observe({
        choices <- updateSelectInputChoices()
        updateSelectInput(session, "tsquarter", choices = choices)
      })

      # update dropdown for funding agency options based on dataset loaded
      updatePickerInput(session,
                        "tsfunder",
                        choices = unique(input_reactive_ts$data_loaded$fundingagency),
                        selected = unique(input_reactive_ts$data_loaded$fundingagency),
                        options = list(`actions-box` = TRUE))
      
      # indicate to user that data is uploaded and to continue with year and quarter selection
      shinyalert("Proceed",
                 "Select Year/Quarter for Analysis and Run Data Checks.",
                 type="success")
      
    # })
  })
  
  
  # Time Series data processing ----

  # generate user prompts to avoid erros. same as with recommender, except we also want to remind user to
  # re-upload data when they change indicators selected
      
  # As with recommender, initailize counters to generate helpful user prompts to avoid errors
  values$num_country_selections_ts <- 0
  values$num_year_selections_ts <- 0
  values$num_quarter_selections_ts <- 0
  values$num_indicator_selections_ts <- 0

  # when country selection changes after data is uploaded, remind user to re-upload
  observeEvent(input$country_selected_ts, {
    values$num_country_selections_ts <- values$num_country_selections_ts + 1
    if(values$num_country_selections_ts >= 2 & input$ts_upload >= 1){
      shinyalert("Reminder",
                 "Remember to reload data and rerun data checks after changing the country selected!",
                 type = "warning")
    }
  })

  # when new data is uploaded, re-set counters for year, quarter, and indicator selections
  observeEvent(input$ts_upload, {
    values$num_year_selections_ts <- 0
    values$num_quarter_selections_ts <- 0
    values$num_indicator_selections_ts <- 0
  })

  # when year selected changes after data checks button was pressed, increment counter
  # and remind users to rerun data checks buttons (which actually preps data for modeling)
  observeEvent(input$tsyear, {
    values$num_year_selections_ts <- values$num_year_selections_ts + 1
    if(values$num_year_selections_ts >= 2 & input$tsdatacheck >= 1){
      shinyalert("Reminder",
                 "Remember to rerun data checks after changing the year selected!",
                 type = "warning")
    }
  })

  # when quarter selected changes after data checks button was pressed, increment counter
  # and remind users to rerun data checks buttons (which actually preps data for modeling)
  observeEvent(input$tsquarter, {
    values$num_quarter_selections_ts <- values$num_quarter_selections_ts + 1
    if(values$num_quarter_selections_ts >= 2 & input$tsdatacheck >= 1){
      shinyalert("Reminder",
                 "Remember to rerun data checks after changing the quarter selected!",
                 type = "warning")
    }
  })

  # when indicators selected changes after data checks button was pressed, increment counter
  # and remind users to rerun data checks buttons (which actually preps data for modeling)
  observeEvent(input$ts_vars, {
    values$num_indicator_selections_ts <- values$num_indicator_selections_ts + 1
    if(values$num_indicator_selections_ts >= 1 & input$tsdatacheck >= 1){
      shinyalert("Reminder",
                 "Remember to rerun data checks after changing the variables selected!",
                 type = "warning")
    }
  })
  
  # only once data is uploaded to we enable the data check button. 
  observe({
    if(input$ts_upload == 0){
      disable("tsdatacheck")
    }
    else{
      enable("tsdatacheck")
    }
  })

  ## code that executes when data check button is pressed
  observeEvent(input$tsdatacheck, {
    
    withProgress(message = 'Running Checks', value = 0.5, {

    # create copy, as before, so we retain the original dataset in case the user changes year or quarter and 
    # needs to rerun from here
    input_reactive_ts$mer_data <- input_reactive_ts$data_loaded

    # get the user selected year and quarter
    recent_year <- input$tsyear
    recent_qtr <- input$tsquarter

    # filter to user-selected funding agencies and indicators
    input_reactive_ts$mer_data <- input_reactive_ts$mer_data[input_reactive_ts$mer_data$fundingagency %in% input$tsfunder,]
    input_reactive_ts$mer_data <- input_reactive_ts$mer_data[input_reactive_ts$mer_data$indicator %in% input$ts_vars, ]

    # make sure indicator is a string
    input_reactive_ts$mer_data$indicator <- as.character(input_reactive_ts$mer_data$indicator)

    # make sure indicator is a quarterly indicator
    input_reactive_ts$mer_data <- input_reactive_ts$mer_data %>%
      filter(indicator %in% quarterly_indicators)

    # concatenate indicator with N-D so we can track them separately
    input_reactive_ts$mer_data$indicator <-
      paste0(input_reactive_ts$mer_data$indicator, "_", input_reactive_ts$mer_data$numeratordenom)
    
    # remove the rows that report on Total Numerator or Total Denominator data
    input_reactive_ts$mer_data <-
      input_reactive_ts$mer_data %>% filter(!standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"))

    # remove rows where facility is actually an aggregate
    input_reactive_ts$mer_data <-
      input_reactive_ts$mer_data %>% filter(tolower(facility) != "data reported above facility level")
    
    # remove rows that are aggregates of age groups (e.g. 15+) but keep 50+
    input_reactive_ts$mer_data <-
      rbind(input_reactive_ts$mer_data[-grep("\\+", input_reactive_ts$mer_data$ageasentered), ],
            input_reactive_ts$mer_data[grep("50+", input_reactive_ts$mer_data$ageasentered), ])

    # remove rows that contain this no longer used age group of 0-9
    input_reactive_ts$mer_data <- input_reactive_ts$mer_data[-grep("<+[0-9]", input_reactive_ts$mer_data$ageasentered), ]
    
    # For HTS_INDEX, keep only those rows where statushiv is positive or negative
    input_reactive_ts$mer_data <- rbind(input_reactive_ts$mer_data[input_reactive_ts$mer_data$indicator != 'HTS_INDEX', ],
                                        input_reactive_ts$mer_data[input_reactive_ts$mer_data$indicator == 'HTS_INDEX' &
                                 (input_reactive_ts$mer_data$statushiv %in% c('Negative', 'Positive')), ])
    
    # Drop deduplication rows from prime partner
    input_reactive_ts$mer_data <-
      input_reactive_ts$mer_data[!input_reactive_ts$mer_data$primepartner %in% c("Dedup", "TBD"),]

    
    # # drop columns no longer needed
    # input_reactive_ts$mer_data <-
    #   input_reactive_ts$mer_data[,!names(input_reactive_ts$mer_data) %in% c("numeratordenom",
    #                                     "standardizeddisaggregate",
    #                                     "ageasentered",
    #                                     "statushiv")]
    
    input_reactive_ts$mer_data <- input_reactive_ts$mer_data %>%
      select(-numeratordenom, -standardizeddisaggregate, -statushiv, -ageasentered)
    
    # Pivot longer to get all the values in one column
    input_reactive_ts$mer_data_long <- pivot_longer(input_reactive_ts$mer_data,
                                  cols = names(input_reactive_ts$mer_data)[grepl("qtr", names(input_reactive_ts$mer_data))],
                                  names_to = "qtr",
                                  values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    # What are the indicators reported in the most recent quarter
    indicators_to_keep <- input_reactive_ts$mer_data_long %>%
      filter(fiscal_year == recent_year) %>%
      filter(qtr == recent_qtr) %>%
      filter(!is.na(value)) %>%
      .$indicator %>%
      unique()

    # precautionary step to make sure there is data in the time period selected - should not be triggered based on 
    # steps added above, but keeping just in case
    if (length(indicators_to_keep) == 0) {
      shinyalert(
        "No data found in target year and quarter.",
        "Please confirm that the dataset includes the year and quarter selected.",
        type = "error"
      )
    }

    # only keep facilities that had values present in the selected year and quarter
    facilities_to_keep <- input_reactive_ts$mer_data_long %>%
      filter(fiscal_year == recent_year) %>%
      filter(qtr == recent_qtr) %>%
      filter(!is.na(value)) %>%
      .$facility %>%
      unique() %>%
      as.character()
      
    # Only keep these indicators that are present in the year and quarter selected
    input_reactive_ts$mer_data_long <-
      input_reactive_ts$mer_data_long[input_reactive_ts$mer_data_long$indicator %in% indicators_to_keep,]

    # Only keep these facilities that are present in the year and quarter selected
    input_reactive_ts$mer_data_long <-
      input_reactive_ts$mer_data_long[as.character(input_reactive_ts$mer_data_long$facility) %in% facilities_to_keep,]
    
    # Turn quarter into a number for sorting
    input_reactive_ts$mer_data_long$qtr <-
      as.numeric(gsub(".*?([0-9]+).*", "\\1", input_reactive_ts$mer_data_long$qtr))
    
    # Take primepartner from most recent quarter in case it changed
    # some facilities are present for 13+ quarters but the IP in charge changed during this period
    input_reactive_ts$mer_data_long <- input_reactive_ts$mer_data_long %>% arrange(desc(fiscal_year))
    input_reactive_ts$mer_data_long <- input_reactive_ts$mer_data_long %>%
      group_by(facility, indicator) %>%
      mutate(primepartner = last(primepartner),
             fundingagency = last(fundingagency))
    
    updatePickerInput(session,
                      "tsfunder",
                      choices = unique(input_reactive_ts$mer_data_long$fundingagency),
                      selected = unique(input_reactive_ts$mer_data_long$fundingagency),
                      options = list(`actions-box` = TRUE))

    # summarize indicator value by facility/indicator/fiscal_year/qtr
    input_reactive_ts$mer_data_long <- input_reactive_ts$mer_data_long %>%
      group_by(psnu, primepartner, facility, indicator, fiscal_year, qtr) %>%
      summarize(value = sum(value, na.rm = TRUE), .groups = "drop")

    ## create a shell so that we can get all valid year-quarter combinations for later filtering
    earliest_year <- min(input_reactive_ts$mer_data_long$fiscal_year)
    shell <-
      expand.grid(fiscal_year = earliest_year:recent_year, qtr = 1:4) %>% # all years and quarters
      filter(!(fiscal_year >= recent_year & # now filter out rows where the year-quarter is later than the one selected
                 qtr > as.numeric(
                   gsub(".*?([0-9]+).*", "\\1", recent_qtr)
                 ))) %>%
      arrange(desc(fiscal_year), desc(qtr)) %>%
      mutate(keep = paste0(fiscal_year, qtr))

    # take our long dataset and keep values from the timeperiods in the shell we just created
    input_reactive_ts$obs_to_keep <- input_reactive_ts$mer_data_long %>%
      mutate(yrqtr = paste0(fiscal_year, qtr)) %>%
      mutate(keep = ifelse(yrqtr %in% shell$keep, 1, 0)) %>%
      group_by(psnu, facility, indicator) %>%
      mutate(count = sum(keep)) %>%
      filter(count >= 10) %>%
      ungroup() %>%
      # added this to remove future observations
      filter(keep == 1) %>%
      select(-count,-yrqtr,-keep)
    
    # convert facility to character string
    input_reactive_ts$obs_to_keep$facility <- as.character(input_reactive_ts$obs_to_keep$facility)

    input_reactive_ts$obs_to_keep
    
      cols_to_keep <-
        c(
          "facility",
          "indicator",
          "psnu",
          "operatingunit",
          "country",
          "numeratordenom",
          "standardizeddisaggregate",
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
      
      # run some final checks - make sure all necessary columns are there
      if (sum(!cols_to_keep %in% names(input_reactive_ts$data_loaded)) > 0) {
        shinyalert(
          "Check the data file",
          "One or more of the required columns are missing from your dataset. Please make sure
        that your dataset contains all the following columns: facility, indicator, psnu,
        numeratordenom, standardizeddisaggregate, fiscal_year, qtr1, qtr2, qtr3, and qtr4.",
          type = "error"
        )
      }

      # make sure we have enough time periods
      if (n_distinct(input_reactive_ts$obs_to_keep[, c("fiscal_year", "qtr")]) < 12) {
        shinyalert(
          "Check the data file",
          "There are fewer than 12 quarters of data.",
          type = "error"
        )
      } else {
        shinyalert("Proceed", "Continue to run models.", type = "success")
      }
      
      # delete data from those intermediate tables to free up memory
      # only keep obs_to_keep, which is what we'll feed to the model
      input_reactive_ts$mer_data <- NULL
      input_reactive_ts$mer_data_long <- NULL
      gc()
      
    })
  })

  # now that data is processed, enable the run model button
  observe({
    if(input$tsdatacheck == 0){
      disable("tsrun")
    }
    else{
      enable("tsrun")
    }
  })

  # initialize list to store outputs of TS model
  forout_reactive_ts <- reactiveValues()

  # execute code chunk when run model button is pressed
  observeEvent(input$tsrun, {

    # runTimeSeries is defined in utils. returns a list of tables of results by ARIMA, STL, and ETS and 
    # summary tables by indicator, facility, and IP
    tsoutputs <- runTimeSeries(
      dat = input_reactive_ts$obs_to_keep,
      recent_year = input$tsyear,
      recent_qtr = input$tsquarter,
      MIN_THRESH = input$min_threshts,
      RETURN_ALL = TRUE,
      keys = keysts
    )

    print("end")
    # output$ts2 = DT::renderDT(tsoutputs$ARIMA,
    #                           filter = "top",
    #                           options = list(scrollX = TRUE))
    output$ts2 <- reactable::renderReactable({
      reactable::reactable(tsoutputs$ARIMA)
    })
    
    # output$ts3 = DT::renderDT(tsoutputs$ETS,
    #                           filter = "top",
    #                           options = list(scrollX = TRUE))
    output$ts3 <- reactable::renderReactable({
      reactable::reactable(tsoutputs$ETS)
    })
    
    # output$ts4 = DT::renderDT(tsoutputs$STL,
    #                           filter = "top",
    #                           options = list(scrollX = TRUE))
    output$ts4 <- reactable::renderReactable({
      reactable::reactable(tsoutputs$STL)
    })
    
    # output$ts5 = DT::renderDT(tsoutputs$Summary,
    #                           filter = "top",
    #                           options = list(scrollX = TRUE))
    output$ts5 <- reactable::renderReactable({
      reactable::reactable(tsoutputs$Summary)
    })
    
    # output$ts6 = DT::renderDT(
    #   tsoutputs$facility_scorecard,
    #   filter = "top",
    #   options = list(scrollX = TRUE)
    # )
    output$ts6 <- reactable::renderReactable({
      reactable::reactable(tsoutputs$facility_scorecard)
    })
    
    # output$ts7 = DT::renderDT(tsoutputs$ip_scorecard,
    #                           filter = "top",
    #                           options = list(scrollX = TRUE))
    output$ts7 <- reactable::renderReactable({
      reactable::reactable(tsoutputs$ip_scorecard)
    })
    
    forout_reactive_ts$ARIMA <- tsoutputs$ARIMA
    forout_reactive_ts$ETS <- tsoutputs$ETS
    forout_reactive_ts$STL <- tsoutputs$STL
    forout_reactive_ts$Summary <- tsoutputs$Summary
    forout_reactive_ts$FacilityScorecard <-
      tsoutputs$facility_scorecard
    forout_reactive_ts$IPScorecard <- tsoutputs$ip_scorecard

    # indicate to user that models runs are complete
    shinyalert("Proceed", "Completed Time series Models", type = "success")
    
  })


  # Download outputs -----------------

  # Download summary recommender outputs when button is pressed
  output$download_rec_sum <- downloadHandler(
    filename = function() {
      "recommender_summary.xlsx"
    },
    content = function(file) {
      # Get cover sheet
      wb <- loadWorkbook("RecommenderCoverSheet.xlsx")
      
      
      # Create header style
      headerStyle <- createStyle(fontSize = 14, textDecoration = "bold", fgFill = "#d3d3d3")
      textStyle <- createStyle(fontSize = 16, textDecoration = "bold", fgFill = "#add8e6")

      # Go through each table stored in list and if table exists, add it to the Excel workbook and format
      if(length(forout_reactive$scorecard)>0){
        addWorksheet(wb, 'Facility Scorecard', tabColour = "blue")
        writeData(wb, sheet = 'Facility Scorecard', forout_reactive$scorecard, startRow = 3)
        writeData(wb, sheet = 'Facility Scorecard', "This tab shows the indicators most commonly flagged by facility.")
        addStyle(wb, sheet = 'Facility Scorecard', headerStyle, rows = 1, cols = 1:ncol(forout_reactive$scorecard))
        addStyle(wb, sheet = 'Facility Scorecard', headerStyle, rows = 3, cols = 1:ncol(forout_reactive$scorecard))
        setColWidths(wb, sheet = 'Facility Scorecard', 1:20, width = "auto")
      }
      
      if(length(forout_reactive$ipcover)>0){
        addWorksheet(wb, 'IP Scorecard', tabColour = "blue")
        writeData(wb, sheet = 'IP Scorecard', forout_reactive$ipcover, startRow = 3)
        writeData(wb, sheet = 'IP Scorecard', "This tab shows the indicators most commonly flagged by IP.")
        addStyle(wb, sheet = 'IP Scorecard', headerStyle, rows = 1, cols = 1:ncol(forout_reactive$ipcover))
        addStyle(wb, sheet = 'IP Scorecard', headerStyle, rows = 3, cols = 1:ncol(forout_reactive$ipcover))
        setColWidths(wb, sheet = 'IP Scorecard', 1:20, width = "auto")
      }
      
      if(length(forout_reactive$facility_summary)>0){
        addWorksheet(wb, 'Summary By Facility', tabColour = "orange")
        writeData(wb, sheet = 'Summary By Facility', forout_reactive$facility_summary)
        addStyle(wb, sheet = 'Summary By Facility', headerStyle, rows = 1, cols = 1:ncol(forout_reactive$facility_summary))
        setColWidths(wb, sheet = 'Summary By Facility', 1:length(keys_facility), width = "auto")
        
      }
      
      if(length(forout_reactive$disags_summary)>0){
        addWorksheet(wb, 'Summary By Disag', tabColour = "orange")
        writeData(wb, sheet = 'Summary By Disag', forout_reactive$disags_summary)
        addStyle(wb, sheet = 'Summary By Disag', headerStyle, rows = 1, cols = 1:ncol(forout_reactive$disags_summary))
        setColWidths(wb, sheet = 'Summary By Disag', 1:length(keys_disag), width = "auto")
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
    }
  )

  # When user presses button to download all recommender outputs, prep summary and observation-level tables
  output$download_rec_all <- downloadHandler(
    filename = function() {
      "recommender_all.xlsx"
    },
    content = function(file) {
      
      # Get cover sheet
      wb <- loadWorkbook("RecommenderCoverSheet.xlsx")
      
      
      # Create header style
      headerStyle <- createStyle(fontSize = 14, textDecoration = "bold", fgFill = "#d3d3d3")
      textStyle <- createStyle(fontSize = 16, textDecoration = "bold", fgFill = "#add8e6")
      
      if(length(forout_reactive$scorecard)>0){
        addWorksheet(wb, 'Facility Scorecard', tabColour = "blue")
        writeData(wb, sheet = 'Facility Scorecard', forout_reactive$scorecard, startRow = 3)
        writeData(wb, sheet = 'Facility Scorecard', "This tab shows the indicators most commonly flagged by facility.")
        addStyle(wb, sheet = 'Facility Scorecard', headerStyle, rows = 1, cols = 1:ncol(forout_reactive$scorecard))
        addStyle(wb, sheet = 'Facility Scorecard', headerStyle, rows = 3, cols = 1:ncol(forout_reactive$scorecard))
        setColWidths(wb, sheet = 'Facility Scorecard', 1:20, width = "auto")
      }
      
      if(length(forout_reactive$ipcover)>0){
        addWorksheet(wb, 'IP Scorecard', tabColour = "blue")
        writeData(wb, sheet = 'IP Scorecard', forout_reactive$ipcover, startRow = 3)
        writeData(wb, sheet = 'IP Scorecard', "This tab shows the indicators most commonly flagged by IP.")
        addStyle(wb, sheet = 'IP Scorecard', headerStyle, rows = 1, cols = 1:ncol(forout_reactive$ipcover))
        addStyle(wb, sheet = 'IP Scorecard', headerStyle, rows = 3, cols = 1:ncol(forout_reactive$ipcover))
        setColWidths(wb, sheet = 'IP Scorecard', 1:20, width = "auto")
      }
      
      if(length(forout_reactive$facility_summary)>0){
        addWorksheet(wb, 'Summary By Facility', tabColour = "orange")
        writeData(wb, sheet = 'Summary By Facility', forout_reactive$facility_summary)
        addStyle(wb, sheet = 'Summary By Facility', headerStyle, rows = 1, cols = 1:ncol(forout_reactive$facility_summary))
        setColWidths(wb, sheet = 'Summary By Facility', 1:length(keys_facility), width = "auto")
        
      }
      
      if(length(forout_reactive$disags_summary)>0){
        addWorksheet(wb, 'Summary By Disag', tabColour = "orange")
        writeData(wb, sheet = 'Summary By Disag', forout_reactive$disags_summary)
        addStyle(wb, sheet = 'Summary By Disag', headerStyle, rows = 1, cols = 1:ncol(forout_reactive$disags_summary))
        setColWidths(wb, sheet = 'Summary By Disag', 1:length(keys_disag), width = "auto")
      }
      
      if(length(forout_reactive$all_outputs)>0){
        addWorksheet(wb, "Outliers All Disags", tabColour = "green")
        writeData(wb, "Outliers All Disags", forout_reactive$all_outputs)
      }
      
      if(length(forout_reactive$site_sex_outliers)>0){
        addWorksheet(wb, "Outliers Sex Disags", tabColour = "green")
        writeData(wb, "Outliers Sex Disags", forout_reactive$site_sex_outliers)
      }
      
      if(length(forout_reactive$site_age_outliers)>0){
        addWorksheet(wb, "Outliers Age Disags", tabColour = "green")
        writeData(wb, "Outliers Age Disags", forout_reactive$site_age_outliers)
      }
      
      if(length(forout_reactive$facility_outputs)>0){
        addWorksheet(wb, "Outliers Facility Level", tabColour = "green")
        writeData(wb, "Outliers Facility Level", forout_reactive$facility_outputs)
      }
      
      # Format individual runs - these are the tabs that do not contain scorecard or summary in the names
      sheets_to_format <- names(wb)[which(grepl("Outliers",names(wb)))]
      
      # Loop through sheets to format and run formatCells function to color code output
      for(i in sheets_to_format){
        print(paste("Formatting Excel sheet for:", i))
        formatCells(name = i,
                    disags = list("Outliers All Disags" = forout_reactive$all_outputs,
                                  "Outliers Sex Disags" = forout_reactive$site_sex_outliers,
                                  "Outliers Age Disags" = forout_reactive$site_age_outliers),
                    facilities = list("Outliers Facility Level" = forout_reactive$facility_outputs),
                    keys_disag = keys_disag,
                    keys_facility = keys_facility,
                    wb_format = wb)
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # Create same functions for time series. first for when user wants to download summary tables only, then for all
  output$download_ts_sum <- downloadHandler(
    filename = function() {
      "timeseries_summary.xlsx"
    },
    content = function(file) {
      wb <- loadWorkbook("TimeSeriesCoverSheet.xlsx")
      # Create styles
      headerStyle <- createStyle(fontSize = 14, textDecoration = "bold", fgFill = "#d3d3d3")
      textStyle <- createStyle(fontSize = 16, textDecoration = "bold", fgFill = "#add8e6")
      
      addWorksheet(wb, 'IP Scorecard', tabColour = "blue")
      writeData(wb, sheet = 'IP Scorecard', "This tab contains a summary of anomalies by IP by indicator.")
      writeData(wb, sheet = 'IP Scorecard', forout_reactive_ts$IPScorecard, startRow = 3)
      setColWidths(wb, sheet = 'IP Scorecard', 1:20, width = "auto")
      addStyle(wb, sheet = 'IP Scorecard', headerStyle, rows = 1, cols = 1)
      addStyle(wb, sheet = 'IP Scorecard', headerStyle, rows = 3, cols = 1:ncol(forout_reactive_ts$IPScorecard))
      
      addWorksheet(wb, 'Facility Scorecard', tabColour = "blue")
      writeData(wb, sheet = 'Facility Scorecard', "This tab contains a summary of anomalies by facility.")
      writeData(wb, sheet = 'Facility Scorecard', forout_reactive_ts$FacilityScorecard, startRow = 3)
      setColWidths(wb, sheet = 'Facility Scorecard', 1:20, width = "auto")
      addStyle(wb, sheet = 'Facility Scorecard', headerStyle, rows = 1, cols = 1)
      addStyle(wb, sheet = 'Facility Scorecard', headerStyle, rows = 3, cols = 1:ncol(forout_reactive_ts$FacilityScorecard))
      
      addWorksheet(wb, 'Summary', tabColour = "orange")
      writeData(wb, sheet = 'Summary', forout_reactive_ts$Summary)
      setColWidths(wb, sheet = 'Summary', 1:3, width = "auto")
      addStyle(wb, sheet = 'Summary', headerStyle, rows = 1, cols = 1:ncol(forout_reactive_ts$Summary))
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_ts_all <- downloadHandler(
    filename = function() {
      "timeseries_all.xlsx"
    },
    content = function(file) {
      wb <- loadWorkbook("TimeSeriesCoverSheet.xlsx")
      # Create styles
      headerStyle <- createStyle(fontSize = 14, textDecoration = "bold", fgFill = "#d3d3d3")
      textStyle <- createStyle(fontSize = 16, textDecoration = "bold", fgFill = "#add8e6")
      
      addWorksheet(wb, 'IP Scorecard', tabColour = "blue")
      writeData(wb, sheet = 'IP Scorecard', "This tab contains a summary of anomalies by IP by indicator.")
      writeData(wb, sheet = 'IP Scorecard', forout_reactive_ts$IPScorecard, startRow = 3)
      setColWidths(wb, sheet = 'IP Scorecard', 1:20, width = "auto")
      addStyle(wb, sheet = 'IP Scorecard', headerStyle, rows = 1, cols = 1)
      addStyle(wb, sheet = 'IP Scorecard', headerStyle, rows = 3, cols = 1:ncol(forout_reactive_ts$IPScorecard))
      
      addWorksheet(wb, 'Facility Scorecard', tabColour = "blue")
      writeData(wb, sheet = 'Facility Scorecard', "This tab contains a summary of anomalies by facility.")
      writeData(wb, sheet = 'Facility Scorecard', forout_reactive_ts$FacilityScorecard, startRow = 3)
      setColWidths(wb, sheet = 'Facility Scorecard', 1:20, width = "auto")
      addStyle(wb, sheet = 'Facility Scorecard', headerStyle, rows = 1, cols = 1)
      addStyle(wb, sheet = 'Facility Scorecard', headerStyle, rows = 3, cols = 1:ncol(forout_reactive_ts$FacilityScorecard))
      
      addWorksheet(wb, 'Summary', tabColour = "orange")
      writeData(wb, sheet = 'Summary', forout_reactive_ts$Summary)
      setColWidths(wb, sheet = 'Summary', 1:3, width = "auto")
      addStyle(wb, sheet = 'Summary', headerStyle, rows = 1, cols = 1:ncol(forout_reactive_ts$Summary))
      
      addWorksheet(wb, 'ARIMA', tabColour = "green")
      writeData(wb, sheet = 'ARIMA', forout_reactive_ts$ARIMA)
      setColWidths(wb, sheet = 'ARIMA', 1:3, width = "auto")
      addStyle(wb, sheet = 'ARIMA', headerStyle, rows = 1, cols = 1:ncol(forout_reactive_ts$ARIMA))
      
      addWorksheet(wb, 'ETS', tabColour = "green")
      writeData(wb, sheet = 'ETS', forout_reactive_ts$ETS)
      setColWidths(wb, sheet = 'ETS', 1:3, width = "auto")
      addStyle(wb, sheet = 'ETS', headerStyle, rows = 1, cols = 1:ncol(forout_reactive_ts$ETS))
      
      addWorksheet(wb, 'STL', tabColour = "green")
      writeData(wb, sheet = 'STL', forout_reactive_ts$STL)
      setColWidths(wb, sheet = 'STL', 1:3, width = "auto")
      addStyle(wb, sheet = 'STL', headerStyle, rows = 1, cols = 1:ncol(forout_reactive_ts$STL))
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
    }
  )
  
}
