# This script contains all the functions to run the Data.FI Anomaly Detection
# Recommender system solution that is called in the accompanying script, main.R. Users  
# should not make changes to this script without care. 



# Set Java parameters prior to loading xlsx package
options(java.parameters = "-Xmx16000m") 

# Check if packages are installed; if not, install
list_packages <- c("readr", "htmltools", "dplyr", "tidyr", "modi", "htmltools",
                      "magrittr", "reshape2", "xlsx", "data.table")
new_packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
library(readr)
library(htmltools)
library(dplyr)
library(tidyr)
library(modi)
library(htmltools)
library(magrittr)
library(reshape2)
library(xlsx)
library(data.table)

# Specify which variables identify a unique observation
keys_disag <- c('facility','ageasentered', 'sex', 'kp', 'psnu', 'primepartner')
keys_facility <- c('facility', 'psnu','primepartner')

#' runRecommenderSolution
#'
#' Main function to run solution, a wrapper for all subsequent functions created. 
#' Called in main.R
#'
#' @return saves Excel file to directory specificied by user in main.R
#' @export
#'
#' @examples
#' runRecommenderSolution()
runRecommenderSolution <- function(){
  
  # Get list of scenarios to run as set by user in main.R
  scenarios_to_run_disag <- which(list("all" = all,"age" = age, "sex" = sex)==TRUE)
  scenarios_to_run_facility <- which(list("facility" = facility,"type" = type, "psnu" = psnu)==TRUE)
  
  # Run check function to validate solution can be run against input data
  runChecks()
  
  # Take raw MER data and produce two dataframes at disag and facility level, ready for Recommender analysis
  dat_prepped <- datPrep()
  dat_disag <- dat_prepped$dat_disag_out
  dat_facility <- dat_prepped$dat_facility_out

  # Loop through disag scenarios and run anaysis for each, adding output to disags_list
  disags_list = list()
  for (i in names(scenarios_to_run_disag)) {
    print(paste("Running analysis for dataset including disaggregates by:", i))
    disags_list[[i]] <- runRecAnalysisDisag(dat_disag_wrapper=dat_disag, scenario=i)
  }
  
  # Loop through facility scenarios and run anaysis for each, adding output to facility_list
  facility_list = list()
  for (i in names(scenarios_to_run_facility)) {
    print(paste("Running analysis for dataset by:", i))
    facility_list[[i]] <- runRecAnalysisFacility(dat_facility_wrapper=dat_facility,scenario=i)
  }
  
  # If output lists are empty, then no anomalies found. End script and notify user.
  if(length(disags_list) == 0 & length(facility_list) == 0){
    stop("No anomalies found. Ending script.")
  }
  
  # Create summary tab for anomalies found using data disaggregated by sex and age
  # If no anomalies found, then the length of disags_list will be zero
  # TODO: if RETURN_ALL = TRUE and no anomalies found, length of disags_list will not be zero
  if(length(disags_list) > 0){
    print("Creating Disag Summary Tab")
    disags_summary <- createSummaryTab(dat_summary_list = disags_list)
  }
  
  # Create summary tab for anomalies found using data aggregated at facility level
  if(length(facility_list) > 0){
    print("Creating Facility Summary Tab")
    facility_summary <- createSummaryTab(dat_summary_list = facility_list,disag = FALSE)
  }
  
  # Combine summary tabs into scorecard of anomalies tabulated by facilities and indicator
  print("Creating Scorecard")
  
  # Prior to creating scorecard, pull the summary tabs that were created in previous step
  dat_tmp <- list()
  if(exists("disags_summary")){
    dat_tmp[['disag']] <- disags_summary$scorecard
  }
  if(exists("facility_summary")){
    dat_tmp[['facility']] <- facility_summary$scorecard
  }
  
  # Combine lists into dataframe
  dat_tmp <- rbindlist(dat_tmp)
  
  # Generate scorecard
  scorecard <- createScoreCard(scorecard_in = dat_tmp)
  
  # Save output to Excel
  # Set filename
  file_out <- paste0(OU, "-", Sys.Date(), ".xlsx")
  
  print("Creating Excel Workbook - This may take a while if returning non-anomalies as well.")
  
  # Write scorecard
  write.xlsx(scorecard, file_out, sheetName="Scorecard", row.names = FALSE)
  # Write summary tabs that exist (they exist if anomalies were found at disag or facility level)
  if(exists("disags_summary")){
    write.xlsx(disags_summary$summary, file_out, sheetName="Summary_Disaggregates", row.names = FALSE, append = TRUE)
  }
  if(exists("facility_summary")){
    write.xlsx(facility_summary$summary, file_out, sheetName="Summary_Facility", row.names = FALSE, append = TRUE)
  }
  # Loop through individual runs and write outputs to Excel
  if(exists("disags_summary")){
    for(i in 1:length(disags_list)){
      write.xlsx(disags_list[[i]], file_out, sheetName=paste0(names(disags_list)[[i]],"_Disaggregates"), row.names=FALSE, append = TRUE)
    }
  }
  
  if(exists("facility_summary")){
    for(i in 1:length(facility_list)){
      write.xlsx(facility_list[[i]], file_out, sheetName=paste0(names(facility_list)[[i]],"_Facility"), row.names=FALSE, append = TRUE)
    }
  }
  
  ## Format outputs
  # First, load workbook and get sheets
  wb <- loadWorkbook(file_out)  
  sheets <- getSheets(wb) 
  # Format individual runs - these are the tabs that do not contain scorecard or summary in the names
  sheets_to_format <- which(!grepl("Scorecard|Summary",names(sheets)))
  
  # Loop through sheets to format and run formatCells function to color code output
  for(i in sheets_to_format){
    print(paste("Formatting Excel sheet for:", names(sheets)[[i]]))
    formatCells(sheet = sheets[[i]],
                name = names(sheets)[[i]],
                disags = disags_list,
                facilities = facility_list,
                keys_disag = keys_disag,
                keys_facility = keys_facility,
                wb_format = wb)
  }
  
  # Save workbook
  saveWorkbook(wb, file_out)
  
  print("Process Complete. File saved.")
  
}

#' runChecks
#' 
#' Runs validation checks to confirm that variables, year/quarter, and facility type
#' descriptions that are needed to run solution are included in the input data
#'
#' @param dat dataframe containing mer_data uploaded by user
#' @param year_for_analysis numeric set by user in main.R
#' @param qtr_for_analysis  string set by user in main.R
#' @param type_check logical, whether to run analysis by facility type, set by user in main.R
#'
#' @return stops code with error messages if input dataset is not compatible with user inputs
#' @export
#'
#' @examples
runChecks <- function(dat=mer_data,
                      year_for_analysis=year,
                      qtr_for_analysis = qtr,
                      type_check = type){
  
  # Check to confirm if fiscal year selected by user for analysis exists in the dataset
  if(!year_for_analysis %in% unique(dat$fiscal_year)){
    stop("Please confirm the fiscal year selected is included in the file uploaded.")
  }
  
  # Check to confirm if quarter selected by user for analysis exists in the dataset
  if(!qtr_for_analysis %in% names(dat)){
    stop("Please confirm the quarter selected is included in the file uploaded.")
  }
  
  # Check to confirm if other required variables exist in the dataset
  if(any(!c("sitename","psnu","facility","indicator","numeratordenom",
            "disaggregate","ageasentered","sex","primepartner") %in% names(dat))){
    stop("Please confirm the file selected contains the required columns: 
         sitename,psnu,facility,indicator,numeratordenom,disaggregate,ageasentered,sex,primepartner")
  }
  
  # # If user chooses to run analysis by facility type, check to confirm if facility type
  # # descriptions exist in the dataset
  # if(type_check == TRUE & any(!facility_strings %in% unique(tolower(dat$facility)))){
  #   stop("Please confirm that facility types exist among facility names. All types should be lowercase.")
  # }
  
}

# dat_Prep for disagg level
#' MER Data Preparation
#'
#' @param dat - data frame containing raw MER data
#' @param year_for_analysis - numeric user-defined
#' @param qtr_for_analysis - string user-defined 
#'
#' @return list of dataframes
#' dat_disag_out observations are disaggregated at sex/age, indicators in wide format
#' dat_facility_out observations are at facility level, indicators in wide format
#' @export
#'
#' @examples
datPrep <- function(dat=mer_data,
                    year_for_analysis=year,
                    qtr_for_analysis = qtr) {

  
  # keep only the columns we need
  cols_to_keep <- c("sitename","psnu","facility","indicator","numeratordenom",
                    "disaggregate","ageasentered","sex", "fiscal_year","primepartner", qtr_for_analysis)
  dat <- dat[, cols_to_keep]
  
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
  
  
  # filter to the fiscal year entered by the user
  dat <- dat %>% filter(fiscal_year == year_for_analysis)
  
  # remove the rows that report on Total Numerator or Total Denominator
  dat <- dat %>% filter(!disaggregate %in% c("Total Numerator", "Total Denominator"))
  dat <- dat %>% filter(tolower(facility) != "data reported above facility level")
  
  # remove rows that are aggregates of age groups (e.g. 15+ and 50+)
  dat <- dat[-grep("\\+", dat$ageasentered),]
  
  # label indicators with N and D for those for which both numerators and denominators are reported
  dat$indicator <- paste0(dat$indicator, "_", dat$numeratordenom)
  
  # create the facility data frame which we will need later in this function for data prep of the facility level file 
  dat_facility <- dat
  
  # for disaggregate output - create a column for the key population disaggregate
  dat$kp <- ifelse(grepl("KeyPop", dat$disaggregate), "Yes", "No")
  
  # for disaggregate output - drop disaggregate and numeratordenom columns
  cols_to_drop <- c("numeratordenom", "disaggregate")
  dat <- dat[,!(names(dat) %in% cols_to_drop)]
  
  # for disaggregate output - we'll need the qtr variable - drop the 1/2/3/4 from quarter name 
  # so that we can reference the variable regardless of the quarter selected
  names(dat) <- gsub("[0-9]","", names(dat))
  dat <- dat %>% 
    filter(!is.na(qtr))
  
  # group by facility, age, sex, and indicator, kp, and psnu, and then summarize qtr (before pivot)
  dat_grouped <- dat %>% group_by(facility, ageasentered, sex, indicator, kp, psnu, primepartner) %>% 
    summarise(qtr_sum = sum(qtr, na.rm = TRUE)) 
  
  # for disaggregate output - pivot wider to get MER indicators in wide format
  dat_out <- dat_grouped %>%
    pivot_wider(names_from = "indicator", values_from = "qtr_sum") %>%
    as.data.frame()
  
  # facility level file - filter for just the quarter of interest
  names(dat_facility) <- gsub("[0-9]","", names(dat_facility))
  dat_facility <- dat_facility %>% 
    filter(!is.na(qtr))
  
  # facility level file - group by facility, psnu and indicator, and then summarize qtr 2 (before pivot)
  dat_facility <- dat_facility %>% group_by(facility, indicator, psnu, primepartner) %>% 
    summarise(qtr_sum = sum(qtr, na.rm = TRUE)) 
  
  # facility level file - pivot wider to get indicators in wide format
  dat_facility_out <- dat_facility %>%
    pivot_wider(names_from = "indicator", values_from = "qtr_sum") %>%
    as.data.frame()
  
  # return data frames
  return(list(
    "dat_disag_out" = dat_out,
    "dat_facility_out" = dat_facility_out))
  
}


#' runRecAnalysisDisag
#' 
#' Wrapper function to run Recommender System analysis for each of all, sex, age disaggregates.
#' This function will call the main runRecAnalysis function directly on all disaggregates, and 
#' calls separate wrapper functions for sex and disaggregates
#'
#' @param dat_disag_wrapper dataframe returned by datPrep function
#' @param keys character vector defined above
#' @param scenario list of strings of all, sex, age scenarios to run, defined by user
#'
#' @return 
#' @export
#'
#' @examples
runRecAnalysisDisag <- function(dat_disag_wrapper,
                                keys=keys_disag,
                                scenario) {
  
  # Scenario can take on values set by user including "all", "sex", and "age"
  if (scenario == "all") {

    # Apply the runRecAnalysis function on entire dataset disaggregated by sex and age
    all_outputs <- runRecAnalysis(dat=dat_disag_wrapper,keys)
    # Sort outputs by anomalous distance
    all_outputs <- tryCatch({
      sortOutputs(all_outputs, keys = keys,scenario_tmp = scenario)
    }, error = function(cond){
      message("No Outliers Found with All Disags")
      message(cond)})
    return(all_outputs)
    
  } else if (scenario == "age") {
    
    # Apply age wrapper function to run Recommender analysis on each age disaggregate
    ageWrapper(dat=dat_disag_wrapper,keys,scenario_wrapper=scenario, age_groups)
    
  } else if (scenario == "sex") {
    
    # Apply sex wrapper function to run Recommender analysis on each sex disaggregate
    sexWrapper(dat=dat_disag_wrapper,keys,scenario_wrapper=scenario)
    
  }
}

#' ageWrapper
#' 
#' Wrapper function to run Recommender analysis by age disaggregate.
#' Contains flag to run by raw five-year age disaggregates or grouped by over/under 15 years of age
#'
#' @param dat dataframe returned by datPrep function
#' @param keys character vector defined above
#' @param scenario_wrapper character of scenario to run
#' @param age_groups string of value "Five Year" or "Over/Under 15"
#'
#' @return dataframe containing disaggregates with estimates, deviations, distance and outlier flags
#' @export
#'
#' @examples
ageWrapper <- function(dat,keys, scenario_wrapper, age_groups) {
  
  # Define five-year age categories and filter to rows containing only these categories
  age_categories <- c("01-04",  "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                      "45-49", "05-09", "10-14")
  dat$ageasentered <- as.character(dat$ageasentered)
  dat <- dat[dat$ageasentered %in% age_categories, ]
  
  # If grouping observations for Over/Under 15 (sensible for smaller datasets)
  if(age_groups == "Over/Under 15"){
    
    # Create "agegroup" variable which takes value of Under 15 of Over 15 based on "ageasentered"
    dat <- cbind("agegroup" = ifelse(dat$ageasentered %in% c("01-04", "05-09", "10-14"), "Under 15", "Over 15"),
                            dat, stringsAsFactors = FALSE) 
    
    # Split dataset by age group
    site_split <- split(dat, factor(dat$agegroup))
    
    # Loop through list and run Recommender analysis on each
    site_out <- list()
    
    for (j in 1:length(site_split)) {
      
      site_out[[j]] <- tryCatch({
        # append "agegroup" to vector of keys
        runRecAnalysis(site_split[[j]],keys = c(keys, "agegroup"))
      }, error = function(cond){
        message(paste("Insufficient Data to Run Disag for Age Group:", names(site_split)[j]))
        message(cond)})
    }
    
    # stack the outputs and drop the age group variable so that outputs from all runs can be appropriately stacked
    site_age_outliers <- do.call(plyr::rbind.fill, site_out) %>% select(-agegroup)

  }
  
  # If grouping observations by five-year disaggregates (sensible for larger datasets)
  if(age_groups == "Five Year"){
    
    # Split dataset by age group
    site_split <- split(dat, factor(dat$ageasentered))
  
    # Loop through list and run Recommender analysis on each
    site_out <- list()
    
    for (j in 1:length(site_split)) {
      
      site_out[[j]] <- tryCatch({
        runRecAnalysis(site_split[[j]],keys)
      }, error = function(cond){
        message(paste("Insufficient Data to Run Disag for Age Group:", names(site_split)[j]))
        message(cond)})
    }
    
    # stack the outputs
    site_age_outliers <- do.call(plyr::rbind.fill, site_out)
    
  }
  
  # If output is empty, then analysis cannot be run as is. If run with five-year age diags, instruct
  # user to try over/under 15. If over/under 15 does not work, then insufficient data to run any age disag.
  if(is.null(site_age_outliers)){
    stop("Insufficient data to run age disaggregates. Consider grouping at over/under 15 or not running age disags.")
  }
  
  # Sort outputs by anomalous distance
  site_age_outliers <- tryCatch({
    sortOutputs(site_age_outliers, keys = keys,scenario_tmp = scenario_wrapper)
  }, error = function(cond){
    message("No Outliers Found for Disag for Age Group:")
    message(cond)})
  
  
  return(site_age_outliers)
  
}

#' sexWrapper
#'
#' Wrapper function to run Recommender analysis by sex disaggregate.
#'
#' @param dat dataframe returned by datPrep function
#' @param keys character vector defined above
#' @param scenario_wrapper character of scenario to run
#'
#' @return dataframe containing recommender outputs stacked for all age disags
#' @export
#'
#' @examples
sexWrapper <- function(dat,keys, scenario_wrapper) {
  
  # Confirm sex is a string and not a factor
  dat$sex <- as.character(dat$sex)
  
  # Limit to observations that are male or female
  dat <- dat[dat$sex %in% c("Male", "Female"), ]
  
  # Split dataset by sex and run Recommender analysis on each subset
  site_split <- split(dat, dat$sex)
  site_out <- list()
  for (j in 1:length(site_split)) {
    site_out[[j]] <- runRecAnalysis(site_split[[j]],keys)
  }
  # stack the outputs
  site_sex_outliers <- do.call(plyr::rbind.fill, site_out)
  
  site_sex_outliers <- tryCatch({
    sortOutputs(site_sex_outliers, keys = keys,scenario_tmp = scenario_wrapper)
  }, error = function(cond){
    message("No Outliers Found for Sex Disag")
    message(cond)})
  return(site_sex_outliers)
  
}

#' sortOutputs
#' 
#' Sort outputs by outlier flag and then by Mahalanobis distance.
#' Order columns by keys, DATIM data, estimated values, normalized deviations, and then outlier flags
#' Function will return all observatios or only anomalous observations based on user-set parameter of RETURN_ALL
#'
#' @param dat dataframe returned by runRecAnalysis function
#' @param keys character vector defined above
#' @param scenario_tmp character of scenario ran
#'
#' @return dataframe containing recommender outputs stacked for all sex disags
#' @export
#'
#' @examples
sortOutputs <- function(dat,keys,scenario_tmp) {
  
  # order columns by keys, DATIM values, estimates (prepended with "E_"),
  # normalize deviations (prepended with "D_"), Mahalanobis distance, and outlier flag
  site_out_total <- dat %>%
    select(names(dat)[!grepl("^E_|^D_|^MD|outlier_sp", names(dat))],
           names(dat)[grepl("^E_", names(dat))],
           names(dat)[grepl("^D_", names(dat))],
           names(dat)[grepl("^MD", names(dat))],
           names(dat)[grepl("outlier_sp", names(dat))])
  # Sort by outlier flag, and then by Mahalanobis distance
  site_out_total <- site_out_total %>% 
    arrange(desc(outlier_sp), desc(MD)) %>%
    mutate(Indicator = NA)
  
  # If user wants to return anomalies only, then filter to anomalies
  if(RETURN_ALL == FALSE){
    site_out_total <- site_out_total %>% filter(outlier_sp == 1)
  }
  
  # Get number of columns and number of keys
  n_columns = sum(grepl("^E_", names(site_out_total)))
  n_keys = length(keys)
  
  # For each anomaly, identify the indicator with the greatest normalized deviation for which the 
  # original value is above the user-set threshold
  # These classifications will be summarized on the Scorecard tab
  if(nrow(site_out_total)>0){
    # Loop through all rows in output
    for(m in 1:nrow(site_out_total)){
      # Get row
      dat_tmp <- site_out_total[m, ]
      # If row is not an outlier, then skip
      if(dat_tmp$outlier_sp == 0){next}
      # Keep columns with deviations
      cols_to_keep <- which(dat_tmp[, (n_keys+1):(n_keys+n_columns)] > MIN_THRESH) + n_keys + n_columns*2
      # If there are fewer than 2 columns present, skip
      if(length(cols_to_keep)<2){next}
      # Get the indicator with the maximum normalized deviation
      site_out_total[m, "Indicator"] <- colnames(dat_tmp[, cols_to_keep])[which.max(dat_tmp[, cols_to_keep])]
    }
    
    # Create a column to contain the scenario name
    site_out_total$scenario <- paste0("outlier_", scenario_tmp)
    
    return(site_out_total)
  }
}

#' runRecAnalysis
#'
#' Workhorse function to calculate Mahalanobis distance, create outlier flags, estimate values using
#' Recommender Systems, and calculate normalized deviations
#'
#' @param dat dataframe returned by datPrep function or run through scenario wrapper functions
#' @param keys character vector of columns that identify an observation
#'
#' @return dataframe containing disaggregates with estimates, deviations, distance and outlier flags
#' @export
#'
#' @examples
runRecAnalysis <- function(dat,keys) {
  
  # Create copy of input data
  site_spread <- dat
  # Subset input data to keys columns
  keys <- site_spread[, 1:length(keys)] 
  
  ## Drop columns that are too sparse (present less than 10%)
  # calculate how often MER indicators are present
  count_present <- apply(site_spread[, (ncol(keys)+1):ncol(site_spread)], 2, function(x) length(which(!is.na(x)))) 
  # keep variables present at least 10% of the time
  cols_to_keep <- which(count_present > (nrow(site_spread)*.10))+ncol(keys) 
  # Horizontally stack keys and MER indicators
  site_keep <- cbind.data.frame(site_spread[, names(keys)], site_spread[, cols_to_keep])
  
  ## Drop variables that have no variance
  # Calculate variance
  var_inds <- apply(site_keep[,(ncol(keys)+1):ncol(site_keep)], 2, FUN = var, na.rm = TRUE)
  # Identify indicators with zero variance to drop
  cols_to_drop <- names(site_keep[,(ncol(keys)+1):ncol(site_keep)])[var_inds == 0] 
  site_keep <- site_keep[,!(names(site_keep) %in% cols_to_drop)]
  
  ## Drop variables that are colinear
  # Create pairwise correlation matrix
  # Convert dataframe to matrix
  dat_df <- site_keep
  dat_matrix <- data.matrix(site_keep[,(ncol(keys)+1):ncol(site_keep)], rownames.force = NA)
  # Calculate pairwise correlations
  cormat <- suppressWarnings(cor(dat_matrix, use = "pairwise.complete.obs"))
  # Set lower triangle to zero since we don't want to double count; diag to zero as these are the same variable
  cormat[lower.tri(cormat)] <- 0
  diag(cormat) <- 0
  # Get correlation matrix in long format
  cormat_long <- reshape2::melt(cormat)
  
  # While there is a correlation between two MER indicators of greater than 0.95:
  while(max(cormat_long$value, na.rm = TRUE) > 0.95) {
    # Get relevant indicators that are collinear
    cormat_perf <- cormat_long %>% filter(value > .95)
    # Select the first variable to drop
    col_to_drop <- cormat_perf$Var1[1]
    col_to_drop <- toString(col_to_drop)
    # Drop the variable from dat_df
    dat_df <- dat_df[, !names(dat_df) %in% col_to_drop]
    # Follow steps above to recompute pairwise correlation matrix
    dat_matrix <- data.matrix(dat_df[,(ncol(keys)+1):ncol(dat_df)], rownames.force = NA)
    cormat <- suppressWarnings(cor(dat_matrix, use = "pairwise.complete.obs"))
    cormat[lower.tri(cormat)] <- 0
    diag(cormat) <- 0
    cormat_long <- reshape2::melt(cormat)
  }

  # assign output, with collinear variables dropped, to site_keep
  site_keep <- dat_df
  
  ## Drop rows with three or fewer MER indicators present 
  # calculate number of present indicators for each observation
  obs_count <- apply(site_keep[, (ncol(keys)+1):ncol(site_keep)], 1, function(x) length(which(!is.na(x))))
  # keep observations with at least 4 present values
  obs_to_keep <- which(obs_count > 3) 
  site_keep <- site_keep[obs_to_keep,]
  

  # sum of present values by variable; use na.rm so we remove NAs
  sum_sparse <- colSums(site_keep[, (ncol(keys)+1):ncol(site_keep)], na.rm = TRUE) 
  # get number of present values by indicator
  count_present_keep <- apply(site_keep[, (ncol(keys)+1):ncol(site_keep)], 2, function(x) length(which(!is.na(x))))
  # get sparse mu (vector of means)
  mu <- sum_sparse / count_present_keep 
  # number of variables (indicators)
  k <- length(mu) 
  
  # set up k by k matrix; the diagonal is the number of observations
  N <- matrix(0, k, k) 
  # diagonal initiated with count of present values
  diag(N) <- count_present_keep 
  
  # set up identity matrix
  i_mat <- matrix(0, k, k) 
  diag(i_mat) <- 1
  
  # N is the counts, s is where we store the covariances, and I is useful for some calculations
  S <- matrix(0, k, k) 
  
  # Loop through each observations
  for (i in 1:nrow(site_keep)){
    
    dat <- site_keep[i, ]
    inds <- which(!is.na(dat[(ncol(keys)+1):ncol(site_keep)])) # inds returns the index of the columns that have non NA values
    yt <- dat[(ncol(keys)+1):ncol(site_keep)][inds] # yt are the actual values that are associated with the indices in inds
    yt_mu <- as.matrix(yt - mu[inds]) # yt_mu subtracts the mu for each indicator from yt for each indicator
    
    Hyt <- as.matrix(i_mat[inds, ]) # hyt is a matrix that has the number of present values as rows and the numbers of all variables as columns
    if(dim(Hyt)[2] == 1){Hyt <- t(Hyt)} #this relevant if we set the threshold too high and we only have 1 indicator column coming through
    
    S <- S + (t(Hyt) %*% (t(yt_mu) %*% yt_mu) %*% Hyt)
  }
  
  # Compute sparse correlation matrix
  N_sqrt <- sqrt(N)
  diag(N_sqrt) <- 1/(diag(N_sqrt))
  R <- (N_sqrt %*% S %*% N_sqrt)
  
  # Calculate Mahalanobis distance
  site_sparse <- site_keep
  site_sparse$MD <- suppressWarnings(MDmiss(site_sparse[, (ncol(keys)+1):ncol(site_sparse)], center = mu, cov = R))
  # Use quantile function to generate cutoff for anomaly flag
  cv<-qchisq(.95,df=ncol(site_sparse)-1)
  site_sparse$outlier_sp <- ifelse(site_sparse$MD>cv, 1, 0)
  
  # Estimate present values - xt is the value to predict, yt are the other present values
  # value will be Rxtyt %*% Ryt_inv %*% yt-uyt + uxt
  preds <- matrix(data = NA, nrow = nrow(site_keep), ncol = k) # set up matrix to hold estimates
  
  # Loop through each row
  for (i in 1:nrow(site_keep)){
    
    # Get present values and index of present values
    dat <- site_keep[i, ]
    inds <- which(!is.na(dat[(ncol(keys)+1):ncol(site_keep)]))
    
    # loop through index of present values
    for (j in inds){
      
      # Get Rxtyt - covariance of other present values with selected present values
      Rxtyt <- R[j, inds[!(inds %in% j)]]
      # Get Ryt_inv
      Ryt <- R[inds[!(inds %in% j)], inds[!(inds %in% j)]]
      if (length(Ryt) > 1){
        Ryt_inv <- matlib::inv(Ryt)  
      } else {Ryt_inv <- 1/Ryt} # if Ryt is scalar, take the inverse of Ryt
      
      #Get yt-uyt
      yt <- dat[(ncol(keys)+1):ncol(site_keep)][inds[!(inds %in% j)]]
      yt_mu <- as.matrix(yt - mu[inds[!(inds %in% j)]])
      # uxt
      uxt <- mu[j]
      
      # Get estimated value
      preds[i,j] <- Rxtyt %*% Ryt_inv %*% t(yt_mu) + uxt
      
    }
    
    
  }
  
  # Convert matrix to dataframe
  preds_df <- data.frame(preds)
  # Column names will be original indicators with an "E_" prepended to indicate estimated value
  names(preds_df) <- paste0("E_", names(site_sparse)[(ncol(keys)+1):(ncol(site_sparse)-2)])
  # Stack estimates with values as reported
  site_all <- cbind(site_sparse, preds_df)
  
  # Take the difference between estimate and actual and normalize by dividing by sample variance
  deviation <- abs(site_all[, (ncol(site_sparse)+1):(ncol(site_all))] - site_all[, (ncol(keys)+1):(ncol(site_sparse)-2)])
  deviation <- mapply('/', deviation, diag(R))
  deviation <- data.frame(deviation)
  names(deviation) <- paste0("D_", names(site_sparse)[(ncol(keys)+1):(ncol(site_sparse)-2)])
  site_out_total <- cbind(site_all, deviation)
  return(site_out_total)
  
  
}

#' runRecAnalysisFacility
#' 
#' Wrapper function to run Recommender System analysis for each of facility, psnu, and facility type.
#' This function will call the main runRecAnalysis function directly at facility level, and 
#' calls separate wrapper functions for facility type and for facilities separately by PSNU
#'
#' @param dat_facility_wrapper dataframe returned by datPrep function
#' @param keys character vector defined above
#' @param scenario list of strings of facility, type, psnu scenarios to run, defined by user
#'
#' @return dataframe containing facilities with estimates, deviations, distance and outlier flags
#' @export
#'
#' @examples
runRecAnalysisFacility <- function(dat_facility_wrapper,keys=keys_facility,scenario) {
  
  if (scenario == "facility") {
    facility_outputs <- runRecAnalysis(dat=dat_facility_wrapper,keys)
    facility_outputs <- tryCatch({
      sortOutputs(facility_outputs, keys = keys, scenario_tmp = scenario)
    }, error = function(cond){
      message("No Outliers Found at Facility Level")
      message(cond)})
    return(facility_outputs)
  } else if (scenario == "type") {
    facilityTypeWrapper(dat=dat_facility_wrapper,keys,facility_strings,scenario_wrapper = scenario)
  } else if (scenario == "psnu") {
    psnuWrapper(dat=dat_facility_wrapper,keys, scenario_wrapper = scenario)
  }
}

#' facilityTypeWrapper
#'
#' Wrapper function to split dataset by user-defined facility types and run analysis on each
#'
#' @param dat dataframe returned by datPrep function
#' @param keys character vector defined above
#' @param facility_strings user-defined vector of facility types
#' @param scenario_wrapper string defining scenario to run
#' 
#' @return dataframe containing facilities with estimates, deviations, distance and outlier flags
#' @export
#'
#' @examples
facilityTypeWrapper <- function(dat,keys,facility_strings, scenario_wrapper) {
  
  # Initiate empty list
  site_split = list()
  
  # Create copy of facility level dataframe
  site_tmp = dat
  
  # For each facility type defined by the user, loop through dat and extract subset of data 
  # for which facility names contain the facility type
  for (k in 1:length(facility_strings)) {
    # get facility type defined by user
    facility_sub = facility_strings[k]
    # Create a subset data to facilities defined by user
    data_tmp = site_tmp[grepl(facility_sub, tolower(site_tmp$facility)),]
    # Drop these rows from overall dataset
    site_tmp = site_tmp[!grepl(facility_sub, tolower(site_tmp$facility)),]
    # Add subset to list for subsequent Recommender analysis
    site_split[[k]] = data_tmp
  }
  
  # Now, iterate through list and run Recommender analysis on each
  site_out <- list()
  for (j in 1:length(site_split)) {
    site_out[[j]] <- runRecAnalysis(site_split[[j]],keys)
  }
  # stack the outputs
  facility_type_outliers <- do.call(plyr::rbind.fill, site_out)
  
  # Sort and order outputs
  facility_type_outliers <- tryCatch({
    sortOutputs(facility_type_outliers, keys = keys,scenario_tmp = scenario_wrapper)
  }, error = function(cond){
    message("No Outliers Found at Level of Facility Type")
    message(cond)})
  return(facility_type_outliers)
}

#' psnuWrapper
#' 
#' Wrapper function to split dataset by PSNU and run analysis on each
#'
#' @param dat dataframe returned by datPrep function
#' @param keys character vector defined above
#' @param scenario_wrapper string defining scenario to run
#'
#' @return dataframe containing facilities with estimates, deviations, distance and outlier flags
#' @export
#'
#' @examples
psnuWrapper <- function(dat,keys,scenario_wrapper) {
  
  dat$psnu <- factor(as.character(dat$psnu))
  site_split <- split(dat, dat$psnu)
  site_out <- list()
  for (j in 1:length(site_split)) {
    
    site_out[[j]] <- tryCatch({
      runRecAnalysis(site_split[[j]],keys)
    }, error = function(cond){
      message(paste("Insufficient Data to Run Disag for PSNU:", names(site_split)[j]))
      message(cond)})
  }
  # stack the outputs
  facility_psnu_outliers <- do.call(plyr::rbind.fill, site_out)
  
  facility_psnu_outliers <- tryCatch({
    sortOutputs(facility_psnu_outliers, keys = keys,scenario_tmp = scenario_wrapper)
  }, error = function(cond){
    message("No Outliers Found at Level of Facility by PSNU")
    message(cond)})
  return(facility_psnu_outliers)
  
}

#' createSummaryTab
#'
#' @param dat_summary_list list of dataframes of anomaly outputs
#' @param disag Logical flag to indicate if summary tab is for facility level or disaggregate level
#'
#' @return dataframe in which each row is an observation flagged as anomalous and each column
#' represents whether it was flagged by a particular run
#' @export
#'
#' @examples
createSummaryTab <- function(dat_summary_list,
                             disag = TRUE){
  
  # If summary tab to be created is for disags:
  if(disag == TRUE){
  cols_to_keep <- c("psnu", "facility", "primepartner", "ageasentered",  "sex",
                    "kp", "scenario", "Indicator", "outlier_sp") 
  dat <- lapply(dat_summary_list, function(x) x[, cols_to_keep])
  } else {
    cols_to_keep <- c("psnu", "facility", "primepartner", "scenario", "Indicator", "outlier_sp") 
    dat <- lapply(dat_summary_list, function(x) x[, cols_to_keep])
  }
 
  # Bind list of dataframes into a single dataframe
  dat <- rbindlist(dat)
  # Underlying tabs may contain all results, but summary tabs should present only outliers
  dat <- dat[dat$outlier_sp == 1, ]
  # Drop the actual outlier flag as everything remaining is an outlier
  dat <- dat %>% select(-outlier_sp)
  
  # PSNU, Facility, Primepartner are keys for the scorecard - returned separately
  dat_for_scorecard <- dat[, c("psnu", "facility", "primepartner", "Indicator")]
  
  dat$outlier <- 1
  
  # Summarize data - create wide dataframe so that each scenario run becomes a column
  # Values will be 1 if an outlier, NA otherwise
  dat_summary <- dat %>%
    select(-Indicator) %>%
    pivot_wider(names_from = "scenario",
                values_from = "outlier") %>%
    as.data.frame()
  
  # Replace NAs with zeros 
  dat_summary[is.na(dat_summary)] <- 0
  # Create column to summarize the number of times each observations was flagged as anomalous
  dat_summary$Outliers <- apply(dat_summary[, grepl("outlier", names(dat_summary))],
                                     1,
                                     function(x){sum(x, na.rm = T)})
  
  # Sort summary tabs by summary of outliers, and then within each, by the facility most commonly flagged
  dat_summary <- dat_summary %>%
    group_by(Outliers, facility) %>%
    mutate(count = n()) %>%
    arrange(desc(Outliers), desc(count)) %>%
    select(-count) %>%
    as.data.frame()

  outlist <- list("summary" = dat_summary,
                  "scorecard" = dat_for_scorecard)
  
}

#' createScoreCard
#'
#' Create scorecard tab of facilities and indicators most commonly flagged
#'
#' @param scorecard_in dataframe of summaries of outliers returned by createSummaryTab
#'
#' @return dataframe in which rows are facilities, columns are indicators,
#'  and cells are number of anomalies
#' @export
#'
#' @examples
createScoreCard <- function(scorecard_in){
  
  # Summarize the number of anomalies by facility and indicator primarily responsible 
  scorecard <- scorecard_in %>%
    filter(!is.na(Indicator)) %>%
    group_by(psnu,facility,primepartner, Indicator) %>%
    summarize(count = n()) %>%
    mutate(facility = as.character(facility),
           psnu = as.character(psnu),
           primepartner = as.character(primepartner)) %>%
    pivot_wider(., names_from = "Indicator", values_from = "count") %>%
    as.data.frame()
  
  # Replace NAs with zeros (for facility-indicator combinations with no anomalies)
  scorecard[is.na(scorecard)] <- 0
  
  # Create total row to sum number of anomalies by indicator
  scorecard$Total <- rowSums(scorecard[, 4:ncol(scorecard)])
  indicator_sums <- c(rep(0, 3), colSums(scorecard[, 4:ncol(scorecard)]))
  scorecard <- rbind(scorecard, indicator_sums)
  
  # Create total column to sum number of anomalies by facility
  scorecard[nrow(scorecard),1:3] <- "Total"
  return(scorecard)
  
}

#' formatCells
#' 
#' Function to color code cells by deciles of normalized deviation and to contatenate
#' values as reported with estimated values
#'
#' @param sheet numeric position of Excel worksheet to format
#' @param name name of Excel worksheet to format
#' @param disags list of dataframes returned by runRecAnalysisDisag
#' @param facilities list of dataframes returned by runRecAnalysisFacility
#' @param keys_disag character vector of variables describing unique observation for disags
#' @param keys_facility character vector of variables describing unique observation for facility 
#' @param wb_format Excel workbook containing output
#'
#' @return formatted Excel workbook to save
#' @export
#'
#' @examples
formatCells <- function(sheet, name, disags, facilities, keys_disag, keys_facility, wb_format){
  
  name_sheet <- sub("_.*", "", name)
  if(name_sheet %in% c("sex", "all", "age")){
    dat <- disags
    n_keys <- length(keys_disag)
  } else if(name_sheet %in% c("facility", "type", "psnu")){
    dat <- facilities
    n_keys <- length(keys_facility)
  }
  
  dat_tmp <- dat[[name_sheet]]
  n_columns <- sum(grepl("^E_", names(dat_tmp)))
  
  rows <- getRows(sheet, rowIndex=2:(nrow(dat_tmp)+1))     # 1st row is headers
  
  # https://www.w3schools.com/colors/colors_picker.asp?colorhex=8B0000
  fo1 <- Fill(foregroundColor="#FF0000")   # create fill object # 1
  cs1 <- CellStyle(wb_format, fill=fo1)        # create cell style # 1
  fo2 <- Fill(foregroundColor="#FF8080")    # create fill object # 2
  cs2 <- CellStyle(wb_format, fill=fo2)        # create cell style # 2 
  
  # loop through columns
  for(j in 1:n_columns){
  
    cells <- getCells(rows, colIndex = j+n_keys)         # get original value cell indexes
    cells_deviation <- getCells(rows, colIndex = (n_keys + j + (2*n_columns))) # get deviation cell indexes
    
    # Get the values for each
    values <- lapply(cells, getCellValue)
    # values_estimates <- lapply(cells_estimates, getCellValue)
    values_deviation <- lapply(cells_deviation, getCellValue)
    
    # Setting ten quantiles (number of buckets should align with number of color fill objects)
    if(RETURN_ALL == TRUE){
      quants <- quantile(as.numeric(values_deviation), c(.98, .99), na.rm = TRUE)
    } else {
      quants <- quantile(as.numeric(values_deviation), c(.8, .9), na.rm = TRUE)
    }

    # find cells meeting conditional criteria: deviations in top quintile
    # add these cells to a list
    highlightred1 <- NULL
    for (i in 1:length(values)) {
      name_val <- names(values)[i]
      x <- as.numeric(values_deviation[i])
      if (x > as.numeric(quants[2]) & !is.na(x)) {
        highlightred1 <- c(highlightred1, name_val)
      }    
    }
    
    # repeat for four other quintiles
    highlightred2 <- NULL
    for (i in 1:length(values)) {
      name_val <- names(values)[i]
      x <- as.numeric(values_deviation[i])
      if (x > as.numeric(quants[1]) & x <= as.numeric(quants[2]) & !is.na(x)) {
        highlightred2 <- c(highlightred2, name_val)
      }
    }
    
    
    # Go through each list and set the cell style to the associated color
    lapply(names(cells[highlightred1]),
           function(ii) setCellStyle(cells[[ii]], cs1))
    
    lapply(names(cells[highlightred2]),
           function(ii) setCellStyle(cells[[ii]], cs2))
    
    gc()
    
    cells_estimates <- getCells(rows, colIndex = (n_keys + j + n_columns)) # get estimate indexes
    values_estimates <- lapply(cells_estimates, getCellValue)
    
    # Append the estimate in parentheses to the original values so they appear in same cell
    for(i in 1:length(cells)){
      setCellValue(cells[[i]], paste0(values[[i]], " (", round(values_estimates[[i]], 1), ")"))
    }
    
    # Clean up to restore memory space
    gc()
  
  }
  
  setColumnWidth(sheet, (n_keys+n_columns+1):ncol(dat_tmp), 0)

}

