# add read parquet
# qtr columns are automatically read in
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

  user_input  <-  reactiveValues(authenticated = TRUE,
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
      if (!user$type %in% c(USG_USERS, PARTNER_USERS)) {
        
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
  
  # Recommender Pipeline -------------------------------
  
  input_reactive <- reactiveValues()
  
  observeEvent(input$rec_upload, {
    input_reactive <- reactiveValues()
  })
  
  observeEvent(input$rec_upload, {
    
    cols_to_read <- c(
      "facility", "indicator", "psnu", "operatingunit", "country",
      "numeratordenom", "standardizeddisaggregate", "statushiv", "ageasentered",
      "prime_partner_name", "fiscal_year", "funding_agency",
      "sitename", "sex", "otherdisaggregate_sub"
    )
    
    withProgress(message = 'Loading Data', value = 0.5, {

    my_items <- s3_list_bucket_items(bucket = Sys.getenv("S3_READ"), filter_parquet = TRUE)

    my_data_recent <- my_items[grepl(input$country_selected, my_items$file_names) &
                                 grepl("Site", my_items$file_names) &
                                 grepl("Recent", my_items$file_names),]$path_names
    
    print(paste0("recommender uploading: ", my_data_recent))

    data_recent <- read_parquet_file(my_data_recent, columns_to_read = cols_to_read)
    
    # Conditionally rename columns if they exist
    column_renames <- c("prime_partner_name" = "primepartner",
                        "funding_agency" = "fundingagency",
                        "countryname" = "country")
    
    existing_columns <- intersect(names(data_recent), names(column_renames))
    data_recent <- data_recent %>%
      rename_at(vars(all_of(existing_columns)), ~ column_renames[existing_columns])
    
    
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
    print("e")
    if (input_reactive$data_loaded$operatingunit[1] == "Western Hemisphere Region") {
      input_reactive$data_loaded <- input_reactive$data_loaded %>% filter(country %in% input$westernhemishpherefilter)
    }
    
    
    print(nrow(input_reactive$data_loaded))
    updateNumericInput(session,
                       "year",
                       value = max(input_reactive$data_loaded$fiscal_year),
                       min = min(input_reactive$data_loaded$fiscal_year),
                       max = max(input_reactive$data_loaded$fiscal_year))
    
    updatePickerInput(session,
                       "recfunder",
                       choices = unique(input_reactive$data_loaded$fundingagency),
                      selected = unique(input_reactive$data_loaded$fundingagency),
                      options = list(`actions-box` = TRUE))
    
    most_recent_quarter <- input_reactive$data_loaded %>%
      filter(fiscal_year == max(input_reactive$data_loaded$fiscal_year)) %>%
      select(qtr2, qtr3, qtr4)
    
    if((sum(is.na(most_recent_quarter$qtr4))/nrow(most_recent_quarter))<.5){
      
      updateSelectInput(session,
                        "quarter",
                        choices = c('qtr1', 'qtr2', 'qtr3', 'qtr4'),
                        selected = 'qtr4')
      
    } else if((sum(is.na(most_recent_quarter$qtr3))/nrow(most_recent_quarter))<.5){
      
      updateSelectInput(session,
                        "quarter",
                        choices = c('qtr1', 'qtr2', 'qtr3', 'qtr4'),
                        selected = 'qtr3')
      
    }  else if((sum(is.na(most_recent_quarter$qtr2))/nrow(most_recent_quarter))<.5){
      
      updateSelectInput(session,
                        "quarter",
                        choices = c('qtr1', 'qtr2', 'qtr3', 'qtr4'),
                        selected = 'qtr2')
      
    } else {
      
      updateSelectInput(session,
                        "quarter",
                        choices = c('qtr1', 'qtr2', 'qtr3', 'qtr4'),
                        selected = 'qtr1')
      
    }
    
    shinyalert("Proceed",
               "Select Year/Quarter for Analysis and Run Data Checks.",
               type="success")
    
    })
  })

  values <- reactiveValues()
  values$num_country_selections <- 0
  values$num_year_selections <- 0
  values$num_quarter_selections <- 0
  
  observeEvent(input$country_selected, {
    values$num_country_selections <- values$num_country_selections + 1
    if(values$num_country_selections >= 2 & input$rec_upload >= 1){
      shinyalert("Reminder",
                 "Remember to reload data and rerun data checks after changing the country selected!",
                 type = "warning")
    }
  })
  
  observeEvent(input$rec_upload, {
    values$num_year_selections <- 0
    values$num_quarter_selections <- 0
  })
  
  observeEvent(input$year, {
    values$num_year_selections <- values$num_year_selections + 1
    if(values$num_year_selections >= 2 & input$recdatacheck >= 1){
      shinyalert("Reminder",
                 "Remember to rerun data checks after changing the year selected!",
                 type = "warning")
    }
  })
  
  observeEvent(input$quarter, {
    values$num_quarter_selections <- values$num_quarter_selections + 1
    if(values$num_quarter_selections >= 3 & input$recdatacheck >= 1){
      shinyalert("Reminder",
                 "Remember to rerun data checks after changing the quarter selected!",
                 type = "warning")
    }
  })
  
  observe({
    if(input$rec_upload == 0){
      disable("recdatacheck")
    }
    else{
      enable("recdatacheck")
    }
  })
  
  observeEvent(input$recdatacheck, {
    
    withProgress(message = 'Beginning Checks', value = 0.3, {
      
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
        
    # input_reactive$data_loaded <- NULL
    input_reactive$data_recent <- NULL
    input_reactive$data_disag_out1 <- NULL
    gc()
    
  })
  })
  
  observe({
    if(input$recdatacheck == 0){
      disable("recrun")
    }
    else{
      enable("recrun")
    }
  })
  
  #### Run Rec Model ####
  
  forout_reactive <- reactiveValues()
  
  observeEvent(input$recrun, {
    forout_reactive <- reactiveValues()
  })
  
  observeEvent(input$recrun, {
    gc()
    all_outputs <- NULL
    site_sex_outliers <- NULL
    site_age_outliers <- NULL
    facility_outputs <- NULL
    
    withProgress(message = 'Running Models', value = 0, {
      # Scenario can take on values set by user including "all", "sex", and "age"
      # if (input$obs) {
      incProgress(.2, detail = paste("Running Model with All Disaggregrates"))
      
      if(nrow(input_reactive$dat_disag_out)/ncol(input_reactive$dat_disag_out) < 3){
        shinyalert(title = "Warning",
                   text = "Insufficient Data to Run Analysis with All Disaggregates",
                   type = "warning")
      }
      
      if(nrow(input_reactive$dat_disag_out)/ncol(input_reactive$dat_disag_out) > 3){

        # Apply the runRecAnalysis function on entire dataset disaggregated by sex and age
        all_outputs <- tryCatch({
          runRecAnalysis(dat = input_reactive$dat_disag_out,
                         keys = keys_disag)
        }, error = function(cond){
          message("No Outliers Found with All Disags")
          #message(cond)
        })
        # Sort outputs by anomalous distance
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
        if(exists("all_outputs")){
          z <- all_outputs %>% mutate(outlier_sp = ifelse(outlier_sp == 1, "Yes", "No"))
          print(head(z))
          output$rec1 <- reactable::renderReactable({
            reactable::reactable(z)
          })
          
          # output$rec1 <- renderUI({
          #   flextable::flextable((z))
          # })
          
          # output$rec1 = DT::renderDT(
          #   datatable(
          #     z,
          #     filter = "top",
          #     options = list(scrollX = TRUE,
          #                    columnDefs = list(list(
          #                      visible = FALSE, targets = c(grep("^D_", colnames(
          #                        all_outputs
          #                      )),
          #                      grep("^E_", colnames(
          #                        all_outputs
          #                      )))
          #                    )))
           # ) #%>%
              # formatStyle(
              #   7:(6 + length(grep(
              #     "^D_", colnames(all_outputs)
              #   ))),
              #   grep("^D_", colnames(all_outputs)),
              #   backgroundColor = styleInterval(
              #     as.numeric(quantile(
              #       all_outputs[, grep("^D_", colnames(all_outputs))],
              #       probs = c(.8, .9, 1),
              #       na.rm = T
              #     )),
              #     c(
              #       "rgb(255,255,255)",
              #       "rgb(255,170,170)",
              #       "rgb(255,80,80)",
              #       "rgb(255,0,0)"
              #     )
              #   )
              # )
          #)
          forout_reactive$all_outputs <- all_outputs 
        }
      }
      # }
      
      # if (input$sex) {
      incProgress(.2, detail = paste("Running Model with Sex Disaggregrates"))
      
      dat <- input_reactive$dat_disag_out
      
      # Confirm sex is a string and not a factor
      dat$sex <- as.character(dat$sex)
      
      # Limit to observations that are male or female
      dat <- dat[dat$sex %in% c("Male", "Female"), ]
      
      # Split dataset by sex and run Recommender analysis on each subset
      site_split <- split(dat, dat$sex)
      site_out <- list()
      for (j in 1:length(site_split)) {
        
        if(nrow(site_split[[j]])/ncol(site_split[[j]]) < 3){
          shinyalert(title = "Warning",
                     text = paste("Insufficient Data to Run Analysis for Sex Subgroup"),
                     type = "warning")
        }
        
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
      # stack the outputs
      site_sex_outliers <- do.call(plyr::rbind.fill, site_out)
      
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
      if (exists("site_sex_outliers")) {
        Sys.sleep(2)
        output$rec2 = DT::renderDT(
          datatable(
            site_sex_outliers %>% mutate(outlier_sp = ifelse(outlier_sp == 1, "Yes", "No")),
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
          ) #%>%
            # formatStyle(
            #   7:(6 + length(grep(
            #     "^D_", colnames(site_sex_outliers)
            #   ))),
            #   grep("^D_", colnames(site_sex_outliers)),
            #   backgroundColor = styleInterval(
            #     as.numeric(quantile(
            #       site_sex_outliers[, grep("^D_", colnames(site_sex_outliers))],
            #       probs = c(.8, .9, 1),
            #       na.rm = T
            #     )),
            #     c(
            #       "rgb(255,255,255)",
            #       "rgb(255,170,170)",
            #       "rgb(255,80,80)",
            #       "rgb(255,0,0)"
            #     )
            #   )
            # )
        )
        
        forout_reactive$site_sex_outliers <- site_sex_outliers 
      } else {
        shinyalert("Proceed",
                   "Completed Sex Disag. No outliers found.",
                   type = "success")
      }
      # }
      
      # if (input$age) {
      incProgress(.2, detail = paste("Running Model with Age Disaggregrates"))
      
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
        
        # site_out <- site_out[sapply(site_out, length) != 0]
        
        # stack the outputs and drop the age group variable so that outputs from all runs can be appropriately stacked
        site_age_outliers <-
          do.call(plyr::rbind.fill, site_out) %>% select(-agegroup)
        
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
        
        if (exists("site_age_outliers")) {
          Sys.sleep(2)
          output$rec3 = DT::renderDT(
            datatable(
              site_age_outliers %>% mutate(outlier_sp = ifelse(outlier_sp == 1, "Yes", "No")),
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
            ) #%>%
              # formatStyle(
              #   7:(6 + length(grep(
              #     "^D_", colnames(site_age_outliers)
              #   ))),
              #   grep("^D_", colnames(site_age_outliers)),
              #   backgroundColor = styleInterval(
              #     as.numeric(quantile(
              #       site_age_outliers[, grep("^D_", colnames(site_age_outliers))],
              #       probs = c(.8, .9, 1),
              #       na.rm = T
              #     )),
              #     c(
              #       "rgb(255,255,255)",
              #       "rgb(255,170,170)",
              #       "rgb(255,80,80)",
              #       "rgb(255,0,0)"
              #     )
              #   )
              # )
          )
          forout_reactive$site_age_outliers <- site_age_outliers 
        } else {
          shinyalert("Proceed",
                     "Completed Age Disag. No outliers found.",
                     type = "success")
        }
      }
      # }
      
      # if (input$facility) {
      incProgress(.2, detail = paste("Running Model at Facility Level"))
      
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
        
        if (exists("facility_outputs")) {
          Sys.sleep(2)
          output$rec4 = DT::renderDT(
            datatable(
              facility_outputs %>% mutate(outlier_sp = ifelse(outlier_sp == 1, "Yes", "No")),
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
            ) #%>%
              # formatStyle(
              #   7:(6 + length(grep(
              #     "^D_", colnames(facility_outputs)
              #   ))),
              #   grep("^D_", colnames(facility_outputs)),
              #   backgroundColor = styleInterval(
              #     as.numeric(quantile(
              #       facility_outputs[, grep("^D_", colnames(facility_outputs))],
              #       probs = c(.8, .9, 1),
              #       na.rm = T
              #     )),
              #     c(
              #       "rgb(255,255,255)",
              #       "rgb(255,170,170)",
              #       "rgb(255,80,80)",
              #       "rgb(255,0,0)"
              #     )
              #   )
              # )
          )
          forout_reactive$facility_outputs <- facility_outputs 
        } else {
          shinyalert("Proceed",
                     "Completed Facility Run. No outliers found.",
                     type = "success")
        }
        
      }
      # }
      
      disags_list <-
        list(all_outputs, site_sex_outliers, site_age_outliers)
      disags_list <- disags_list[lengths(disags_list) != 0]
      
      facility_list <- list(facility_outputs)
      facility_list <- facility_list[lengths(facility_list) != 0]
      
      if (length(disags_list) > 0) {
        incProgress(.2, detail = paste("Creating Summary Disaggregate Tab"))
        disags_summary <-
          createSummaryTab(dat_summary_list = disags_list)
        
        # output$rec6 = DT::renderDT(
        #   disags_summary$summary,
        #   filter = "top",
        #   options = list(scrollX = TRUE)
        # )
        
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
        # output$rec7 = DT::renderDT(
        #   facility_summary$summary,
        #   filter = "top",
        #   options = list(scrollX = TRUE)
        # )
        
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
      # Generate scorecard
      incProgress(.2, detail = paste("Creating Scorecard"))
      scorecard <- createScoreCard(scorecard_in = dat_tmp)
      # output$rec8 = DT::renderDT(scorecard,
      #                            filter = "top",
      #                            options = list(scrollX = TRUE))
      
      output$rec8 <- reactable::renderReactable({
        reactable::reactable(scorecard)
      })
      forout_reactive$scorecard <- scorecard
      
      # Create IP scorecard sheet
      cover_ip <- dat_tmp %>%
        group_by(primepartner, Indicator) %>%
        summarize(Outliers = n(), .groups = "drop") %>% 
        mutate(primepartner = as.character(primepartner))
      
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
  
  input_reactive_ts <- reactiveValues()
  
  observeEvent(input$ts_upload, {
    input_reactive_ts <- reactiveValues()
  })
  
  observe({
    if(is.null(input$ts_vars)){
      disable("ts_upload")
    }
    else{
      enable("ts_upload")
    }
  })
  
  observeEvent(input$ts_upload, {
    
    cols_to_read <- c(
      "facility", "indicator", "psnu", "operatingunit", "country",
      "numeratordenom", "standardizeddisaggregate", "statushiv", "ageasentered",
      "prime_partner_name", "fiscal_year", "funding_agency"
    )
    
    
    withProgress(message = 'Loading Recent Data', value = 0.3, {
      
      my_items <- s3_list_bucket_items(bucket = Sys.getenv("S3_READ"), filter_parquet = TRUE)

      my_data_recent <- my_items[grepl(input$country_selected_ts, my_items$file_names) &
                                   grepl("Site", my_items$file_names) &
                                   grepl("Recent", my_items$file_names),]$path_names
      print(paste0("upload: ", my_data_recent))

      data_recent <- read_parquet_file(my_data_recent, columns_to_read = cols_to_read)
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
    
    withProgress(message = 'Loading Historical Data', value = 0.7, {
      
      my_data_historical <- my_items[grepl(input$country_selected_ts, my_items$file_names) &
                                   grepl("Site", my_items$file_names) &
                                   grepl("Historic", my_items$file_names),]$path_names
      print(paste0("historical: ", my_data_historical))

      data_historical <- read_parquet_file(my_data_historical, columns_to_read = cols_to_read)
      print(dim(data_historical))
      
      if("prime_partner_name" %in% names(data_historical)){
        data_historical$primepartner <- data_historical$prime_partner_name
      }
      if("funding_agency" %in% names(data_historical)){
        data_historical$fundingagency <- data_historical$funding_agency
      }
      if("countryname" %in% names(data_historical)){
        data_historical$country <- data_historical$countryname
      }
      
      data_all <- bind_rows(data_recent, data_historical)
      
      # input_reactive_ts$data_all <- data_all # keep this for second runs
      input_reactive_ts$data_loaded <- data_all
      
      if (input_reactive_ts$data_loaded$operatingunit[1] == "Asia Region") {
        input_reactive_ts$data_loaded <- input_reactive_ts$data_loaded %>% filter(country == input$asiafilter_ts)
      }
      
      if (input_reactive_ts$data_loaded$operatingunit[1] == "West Africa Region") {
        input_reactive_ts$data_loaded <- input_reactive_ts$data_loaded %>% filter(country == input$westafricafilter_ts)
      }
      
      if (input_reactive_ts$data_loaded$operatingunit[1] == "Western Hemisphere Region") {
        input_reactive_ts$data_loaded <- input_reactive_ts$data_loaded %>% filter(country == input$westernhemishpherefilter_ts)
      }
      
      # input_reactive_ts$data_loaded <- input_reactive_ts$data_loaded[input_reactive_ts$data_loaded$indicator %in% input$ts_vars, ]
      
      print(dim(input_reactive_ts$data_loaded))
      
      # Logic to calculate what years and quarters are valid
      # Get unique years and quarters, and then it's rows 13 and on
      
      yrmt_check <- input_reactive_ts$data_loaded %>%
        filter(indicator %in% input$ts_vars) %>%
        select(fiscal_year, qtr1, qtr2, qtr3, qtr4) %>%
        pivot_longer(cols = !fiscal_year,
                     names_to = "qtr",
                     values_to = "value",
                     values_drop_na = TRUE) %>%
        mutate(qtr = factor(qtr, levels = c("qtr1", "qtr2", "qtr3", "qtr4"))) %>%
        group_by(fiscal_year, qtr) %>%
        summarize(count = n()) %>%
        arrange(desc(fiscal_year), desc(qtr)) 
      
      yrmt_keep <- yrmt_check[1:(nrow(yrmt_check)-12), ]
      
      updateNumericInput(session,
                         "tsyear",
                         value = max(yrmt_keep$fiscal_year),
                         min = min(yrmt_keep$fiscal_year),
                         max = max(yrmt_keep$fiscal_year))
      
      # Reactive function to update selectInput choices based on numericInput value
      updateSelectInputChoices <- reactive({
        numeric_value <- input$tsyear
        print(input$tsyear)
        # Define your logic to generate choices based on the numeric value
        choices <- if(input$tsyear == max(yrmt_keep$fiscal_year)){
          as.character(unique(yrmt_keep$qtr[yrmt_keep$fiscal_year==max(yrmt_keep$fiscal_year)]))
        } else if(input$tsyear == min(yrmt_keep$fiscal_year)){
          as.character(unique(yrmt_keep$qtr[yrmt_keep$fiscal_year==min(yrmt_keep$fiscal_year)])) 
        } else {c('qtr1', 'qtr2', 'qtr3', 'qtr4')}
        
        return(choices)
      })
      
      # Update selectInput choices whenever the numericInput changes
      observe({
        choices <- updateSelectInputChoices()
        updateSelectInput(session, "tsquarter", choices = choices)
      })
      
      updatePickerInput(session,
                        "tsfunder",
                        choices = unique(input_reactive_ts$data_loaded$fundingagency),
                        selected = unique(input_reactive_ts$data_loaded$fundingagency),
                        options = list(`actions-box` = TRUE))
      
      
      shinyalert("Proceed",
                 "Select Year/Quarter for Analysis and Run Data Checks.",
                 type="success")
      
    })
  })
  
  
  #### Time Series DATA CHECK FUNCTION####
  values$num_country_selections_ts <- 0
  values$num_year_selections_ts <- 0
  values$num_quarter_selections_ts <- 0
  values$num_indicator_selections_ts <- 0
  
  observeEvent(input$country_selected_ts, {
    values$num_country_selections_ts <- values$num_country_selections_ts + 1
    if(values$num_country_selections_ts >= 2 & input$ts_upload >= 1){
      shinyalert("Reminder",
                 "Remember to reload data and rerun data checks after changing the country selected!",
                 type = "warning")
    }
  })
  
  observeEvent(input$ts_upload, {
    values$num_year_selections_ts <- 0
    values$num_quarter_selections_ts <- 0
    values$num_indicator_selections_ts <- 0
  })
  
  observeEvent(input$tsyear, {
    values$num_year_selections_ts <- values$num_year_selections_ts + 1
    if(values$num_year_selections_ts >= 2 & input$tsdatacheck >= 1){
      shinyalert("Reminder",
                 "Remember to rerun data checks after changing the year selected!",
                 type = "warning")
    }
  })
  
  observeEvent(input$tsquarter, {
    values$num_quarter_selections_ts <- values$num_quarter_selections_ts + 1
    if(values$num_quarter_selections_ts >= 2 & input$tsdatacheck >= 1){
      shinyalert("Reminder",
                 "Remember to rerun data checks after changing the quarter selected!",
                 type = "warning")
    }
  })
  
  observeEvent(input$ts_vars, {
    values$num_indicator_selections_ts <- values$num_indicator_selections_ts + 1
    if(values$num_indicator_selections_ts >= 1 & input$tsdatacheck >= 1){
      shinyalert("Reminder",
                 "Remember to rerun data checks after changing the variables selected!",
                 type = "warning")
    }
  })
  
  
  observe({
    if(input$ts_upload == 0){
      disable("tsdatacheck")
    }
    else{
      enable("tsdatacheck")
    }
  })
  
  observeEvent(input$tsdatacheck, {
    
    withProgress(message = 'Running Checks', value = 0.5, {
      
    input_reactive_ts$mer_data <- input_reactive_ts$data_loaded
    
    recent_year <- input$tsyear
    recent_qtr <- input$tsquarter
    
    input_reactive_ts$mer_data <- input_reactive_ts$mer_data[input_reactive_ts$mer_data$fundingagency %in% input$tsfunder,]
    input_reactive_ts$mer_data <- input_reactive_ts$mer_data[input_reactive_ts$mer_data$indicator %in% input$ts_vars, ]
    
    input_reactive_ts$mer_data$indicator <- as.character(input_reactive_ts$mer_data$indicator)
    input_reactive_ts$mer_data <- input_reactive_ts$mer_data %>%
      filter(indicator %in% quarterly_indicators)
    
    input_reactive_ts$mer_data$indicator <-
      paste0(input_reactive_ts$mer_data$indicator, "_", input_reactive_ts$mer_data$numeratordenom)
    
    # remove the rows that report on Total Numerator or Total Denominator data
    input_reactive_ts$mer_data <-
      input_reactive_ts$mer_data %>% filter(!standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"))
    
    input_reactive_ts$mer_data <-
      input_reactive_ts$mer_data %>% filter(tolower(facility) != "data reported above facility level")
    
    # remove rows that are aggregates of age groups (e.g. 15+) but keep 50+
    input_reactive_ts$mer_data <-
      rbind(input_reactive_ts$mer_data[-grep("\\+", input_reactive_ts$mer_data$ageasentered), ],
            input_reactive_ts$mer_data[grep("50+", input_reactive_ts$mer_data$ageasentered), ])
    
    input_reactive_ts$mer_data <- input_reactive_ts$mer_data[-grep("<+[0-9]", input_reactive_ts$mer_data$ageasentered), ]
    
    # For HTS_INDEX, keep only those rows where statushiv is positive or negative
    input_reactive_ts$mer_data <- rbind(input_reactive_ts$mer_data[input_reactive_ts$mer_data$indicator != 'HTS_INDEX', ],
                                        input_reactive_ts$mer_data[input_reactive_ts$mer_data$indicator == 'HTS_INDEX' &
                                 (input_reactive_ts$mer_data$statushiv %in% c('Negative', 'Positive')), ])
    
    # Drop deduplication rows from prime partner
    input_reactive_ts$mer_data <-
      input_reactive_ts$mer_data[!input_reactive_ts$mer_data$primepartner %in% c("Dedup", "TBD"),]
    
    input_reactive_ts$mer_data <-
      input_reactive_ts$mer_data[,!names(input_reactive_ts$mer_data) %in% c("numeratordenom",
                                        "standardizeddisaggregate",
                                        "ageasentered",
                                        "statushiv")]
    
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
    
    if (length(indicators_to_keep) == 0) {
      shinyalert(
        "No data found in target year and quarter.",
        "Please confirm that the dataset includes the year and quarter selected.",
        type = "error"
      )
    }
    
    facilities_to_keep <- input_reactive_ts$mer_data_long %>%
      filter(fiscal_year == recent_year) %>%
      filter(qtr == recent_qtr) %>%
      filter(!is.na(value)) %>%
      .$facility %>%
      unique() %>%
      as.character()
    # Only keep these indicators
    input_reactive_ts$mer_data_long <-
      input_reactive_ts$mer_data_long[input_reactive_ts$mer_data_long$indicator %in% indicators_to_keep,]
    
    input_reactive_ts$mer_data_long <-
      input_reactive_ts$mer_data_long[as.character(input_reactive_ts$mer_data_long$facility) %in% facilities_to_keep,]
    
    # Turn quarter into a number for sorting
    input_reactive_ts$mer_data_long$qtr <-
      as.numeric(gsub(".*?([0-9]+).*", "\\1", input_reactive_ts$mer_data_long$qtr))
    
    # Take primepartner from most recent quarter in case it changed
    input_reactive_ts$mer_data_long <- input_reactive_ts$mer_data_long %>% arrange(desc(fiscal_year))
    input_reactive_ts$mer_data_long <- input_reactive_ts$mer_data_long %>%
      group_by(facility, indicator) %>%
      mutate(primepartner = primepartner[1])

    # summarize by facility/indicator/fiscal_year/qtr
    input_reactive_ts$mer_data_long <- input_reactive_ts$mer_data_long %>%
      group_by(psnu, primepartner, facility, indicator, fiscal_year, qtr) %>%
      summarize(value = sum(value, na.rm = TRUE), .groups = "drop")
    
    earliest_year <- min(input_reactive_ts$mer_data_long$fiscal_year)
    shell <-
      expand.grid(fiscal_year = earliest_year:recent_year, qtr = 1:4) %>%
      filter(!(fiscal_year >= recent_year &
                 qtr > as.numeric(
                   gsub(".*?([0-9]+).*", "\\1", recent_qtr)
                 ))) %>%
      arrange(desc(fiscal_year), desc(qtr)) %>%
      # mutate(rownum = row_number()) %>% filter(rownum <= 12) %>% select(-rownum) %>%
      mutate(keep = paste0(fiscal_year, qtr))
    
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
      
      
      if (sum(!cols_to_keep %in% names(input_reactive_ts$data_loaded)) > 0) {
        shinyalert(
          "Check the data file",
          "One or more of the required columns are missing from your dataset. Please make sure
        that your dataset contains all the following columns: facility, indicator, psnu,
        numeratordenom, standardizeddisaggregate, fiscal_year, qtr1, qtr2, qtr3, and qtr4.",
          type = "error"
        )
      }
      
      if (n_distinct(input_reactive_ts$obs_to_keep[, c("fiscal_year", "qtr")]) < 12) {
        shinyalert(
          "Check the data file",
          "There are fewer than 12 quarters of data.",
          type = "error"
        )
      } else {
        shinyalert("Proceed", "Continue to run models.", type = "success")
      }
      
      # input_reactive_ts$data_loaded <- NULL
      input_reactive_ts$mer_data <- NULL
      input_reactive_ts$mer_data_long <- NULL
      gc()
      
    })
  })

  observe({
    if(input$tsdatacheck == 0){
      disable("tsrun")
    }
    else{
      enable("tsrun")
    }
  })
  
  forout_reactive_ts <- reactiveValues()
  
  observeEvent(input$tsrun, {
    tsoutputs <- runTimeSeries(
      dat = input_reactive_ts$obs_to_keep,
      recent_year = input$tsyear,
      recent_qtr = input$tsquarter,
      MIN_THRESH = input$min_threshts,
      RETURN_ALL = TRUE,
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