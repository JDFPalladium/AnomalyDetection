# This script contains all the functions to run the Data.FI Anomaly Detection
# Time Series solution that is called in the accompanying script, main.R. Users  
# should not make changes to this script without care. 


# Check if packages are installed; if not, install
list_packages <- c("dplyr", "tidyr", "forecast", "imputeTS", "zoo", "data.table", "writexl")
new_packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
library(dplyr)
library(tidyr)
library(forecast)
library(imputeTS)
library(zoo)
library(data.table)
library(openxlsx)

# run TimeSeriesSolution function
runTimeSeriesSolution  <- function() {
  # dat prep
  print("Preparing Data")
  dat_prepped <- datPrep(mer_list = mer_data, recent_year = year, recent_qtr = qtr)
  # model
  print("Running Time Series Models")
  output <- runTimeSeries(dat = dat_prepped, recent_year = year, recent_qtr = qtr)
  # write output
  print("Writing Output")
  file_out <- paste0(OU, "-TS-", Sys.Date(), ".xlsx")
  write.xlsx(output, file = file_out)
  print("Code Completed.")
}

datPrep <- function(mer_list, recent_year, recent_qtr) {
  
  cols_to_keep <- c("facility", "indicator", "psnu", "numeratordenom", "disaggregate",
                    "ageasentered", "fiscal_year", "qtr1", "qtr2", "qtr3", "qtr4")
  
  for(i in 1:length(mer_list)){
    if(sum(!cols_to_keep %in% names(mer_list[[i]])) > 0){
      stop("One or more of the required columns are missing from your dataset. Please make sure 
          that your dataset contains all the following columns: facility, indicator, psnu, 
          frequency, numeratordenom, disaggregate, fiscal_year, qtr1, qtr2, qtr3, and qtr4.")
    }
  }
  
  mer_list <- lapply(mer_list, function(x) x[, names(x) %in% cols_to_keep])
  mer_data <- rbindlist(mer_list) %>% as.data.frame()
  
  mer_data$indicator <- as.character(mer_data$indicator)
  mer_data <- mer_data %>%
    filter(indicator %in% quarterly_indicators)
  
  mer_data$indicator <- paste0(mer_data$indicator, "_", mer_data$numeratordenom)
  
  # remove the rows that report on Total Numerator or Total Denominator data
  mer_data <- mer_data %>% filter(!disaggregate %in% c("Total Numerator", "Total Denominator"))
  
  mer_data <- mer_data %>% filter(tolower(facility) != "data reported above facility level")
  
  # remove rows that are aggregates of age groups (e.g. 15+ and 50+)
  mer_data <- mer_data[-grep("\\+", mer_data$ageasentered),]
  
  mer_data <- mer_data[, !names(mer_data) %in% c("numeratordenom", "disaggregate", "ageasentered")]
  
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
  
  facilities_to_keep <- mer_data_long %>%
    filter(fiscal_year == recent_year) %>%
    filter(qtr == recent_qtr) %>%
    filter(!is.na(value)) %>%
    .$facility %>%
    unique() %>%
    as.character()
  
  # Only keep these indicators
  mer_data_long <- mer_data_long[mer_data_long$indicator %in% indicators_to_keep, ]
  mer_data_long <- mer_data_long[as.character(mer_data_long$facility) %in% facilities_to_keep, ]
  
  # Turn quarter into a number for sorting
  mer_data_long$qtr <- as.numeric(gsub(".*?([0-9]+).*", "\\1", mer_data_long$qtr))
  
  # summarize by facility/indicator/fiscal_year/qtr
  mer_data_long <- mer_data_long %>%
    group_by(psnu, facility, indicator, fiscal_year, qtr) %>%
    summarize(value = sum(value, na.rm = TRUE))
  
  # # drop empty quarters
  # input_qtr <- as.numeric(gsub(".*?([0-9]+).*", "\\1", qtr))
  # qtrs <- c(1,2,3,4)
  # empty_qtrs <- qtrs[qtrs > input_qtr]
  # all <- all %>%
  #   filter(!(fiscal_year == year & qtr %in% empty_qtrs))
  
  mer_data_long <- mer_data_long %>%
    group_by(psnu, facility, indicator) %>%
    mutate(count = n()) %>%
    filter(count >= 12) %>%
    ungroup() %>%
    select(-count)
  
  # convert facility to character string
  mer_data_long$facility <- as.character(mer_data_long$facility)
  
  return(mer_data_long)
}

runTimeSeries <- function(dat, recent_year, recent_qtr) {

  # split by facility and indicator
  dat_split <- split(dat, dat$facility)
  
  # Create shell
  earliest_year <- min(dat$fiscal_year)
  shell <- expand.grid(fiscal_year = earliest_year:recent_year, qtr = 1:4) %>%
    filter(!(fiscal_year >= recent_year & qtr > as.numeric(gsub(".*?([0-9]+).*", "\\1", recent_qtr)))) %>%
    arrange(fiscal_year, qtr)
  
  outlist_arima <- list()
  outlist_ets <- list()
  outlist_stl_arima <- list()
  
  # for each facility
  # for(i in 1:20){
  # for(i in 1:length(facility_split)){
    for(i in 1:20){
    print(i)
    dat_tmp <- dat_split[[i]]
    ind_split <- split(dat_tmp, dat_tmp$indicator)
    
    
    for(j in 1:length(ind_split)){
      
      dat_ind <- ind_split[[j]]
      
      shell_tmp <- merge(shell, dat_ind, by = c("fiscal_year", "qtr"), all.x = TRUE) 
      
      # set up as time series
      dat_ts <- ts(shell_tmp[1:(nrow(shell_tmp)-1), 'value'], start = c(earliest_year, 1), frequency = 4) %>%
        na.trim(sides = "left") %>%
        na_interpolation()
      
      # Fit STL model and forecast last present period
      stlf_arima_forecast <- stlf(dat_ts,
                                  method = "arima",
                                  level = c(99),
                                  h = 1)
      
      upper99 <- stlf_arima_forecast$upper[1]
      lower99 <- stlf_arima_forecast$lower[1]
      pred <- stlf_arima_forecast$mean[1]
      
      shell_tmp$upper99 <- upper99
      shell_tmp$lower99 <- lower99
      actual <- tail(shell_tmp$value,1)
      
      if(!is.na(actual) & (actual < lower99 | actual > upper99) & (abs(actual - pred) > MIN_THRESH)){
        outlist_stl_arima[[length(outlist_stl_arima)+1]] <- shell_tmp
      }
      
      # Fit STL model and forecast last present period
      stlf_arima_forecast <- stlf(dat_ts,
                                  method = "ets",
                                  level = c(99),
                                  h = 1)
      
      upper99 <- stlf_arima_forecast$upper[1]
      lower99 <- stlf_arima_forecast$lower[1]
      pred <- stlf_arima_forecast$mean[1]
      
      shell_tmp$upper99 <- upper99
      shell_tmp$lower99 <- lower99
      actual <- tail(shell_tmp$value,1)
      
      if(!is.na(actual) & (actual < lower99 | actual > upper99) & (abs(actual - pred) > MIN_THRESH)){
        outlist_ets[[length(outlist_ets)+1]] <- shell_tmp
      }
      
      arima_mod <- suppressWarnings(auto.arima(dat_ts,
                              seasonal=TRUE,
                              approximation = TRUE,
                              max.p = 2,
                              max.q = 2,
                              max.order = 3,
                              stepwise=TRUE))
      arima_mod_forecast <- forecast(arima_mod, h=1, level = c(99))
      upper99 <- arima_mod_forecast$upper[1]
      lower99 <- arima_mod_forecast$lower[1]
      pred <- stlf_arima_forecast$mean[1]
      
      shell_tmp$upper99 <- upper99
      shell_tmp$lower99 <- lower99
      actual <- tail(shell_tmp$value,1)
      
      if(!is.na(actual) & (actual < lower99 | actual > upper99) & (abs(actual - pred) > MIN_THRESH)){
        outlist_arima[[length(outlist_arima)+1]] <- shell_tmp
      }
      
    }
    
  }
  
  out_stl_arima <- rbindlist(outlist_stl_arima)
  out_ets <- rbindlist(outlist_ets)
  out_arima <- rbindlist(outlist_arima)
  
  out_arima <- out_arima %>%
    arrange(desc(fiscal_year), desc(qtr)) %>%
    filter(!is.na(value))
  out_arima_wide <- pivot_wider(out_arima,
                                id_cols = c("psnu", "facility", "indicator", "lower99", "upper99"),
                                names_from = c("fiscal_year", "qtr"),
                                values_from = c("value"))
  # Let's calculate by how much the differ (difference / range of interval)
  arima_out <- out_arima_wide %>%
    mutate(gap = ifelse(`2021_2` > upper99, `2021_2` - upper99, lower99 - `2021_2`),
           deviation = gap / (upper99 - lower99)) %>%
    arrange(desc(deviation)) %>%
    mutate(`2021_2` = paste0(`2021_2`, " (", round(lower99, digits=1), " - ", round(upper99,digits=1), ")")) %>%
    select(-gap, -deviation, -upper99, -lower99) %>%
    as.data.frame()
  
  out_ets <- out_ets %>%
    arrange(desc(fiscal_year), desc(qtr)) %>%
    filter(!is.na(value))
  out_ets_wide <- pivot_wider(out_ets,
                              id_cols = c("psnu","facility", "indicator", "lower99", "upper99"),
                              names_from = c("fiscal_year", "qtr"),
                              values_from = c("value"))
  ets_out <- out_ets_wide %>%
    mutate(gap = ifelse(`2021_2` > upper99, `2021_2` - upper99, lower99 - `2021_2`),
           deviation = gap / (upper99 - lower99)) %>%
    arrange(desc(deviation)) %>%
    mutate(`2021_2` = paste0(`2021_2`, " (", round(lower99, digits=1), " - ", round(upper99,digits=1), ")")) %>%
    select(-gap, -deviation, -upper99, -lower99) %>%
    as.data.frame()
  
  
  out_stl_arima <- out_stl_arima %>%
    arrange(desc(fiscal_year), desc(qtr)) %>%
    filter(!is.na(value))
  out_stl_arima_wide <- pivot_wider(out_stl_arima,
                                    id_cols = c("psnu","facility", "indicator", "lower99", "upper99"),
                                    names_from = c("fiscal_year", "qtr"),
                                    values_from = c("value"))
  stl_out <- out_stl_arima_wide %>%
    mutate(gap = ifelse(`2021_2` > upper99, `2021_2` - upper99, lower99 - `2021_2`),
           deviation = gap / (upper99 - lower99)) %>%
    arrange(desc(deviation)) %>%
    mutate(`2021_2` = paste0(`2021_2`, " (", round(lower99, digits=1), " - ", round(upper99,digits=1), ")")) %>%
    select(-gap, -deviation, -upper99, -lower99) %>%
    as.data.frame()
  
  # Create Summary Tab
  summary <- merge(out_arima_wide[, c("psnu", "facility", "indicator", "upper99")],
                   out_stl_arima_wide[, c("psnu","facility", "indicator", "upper99")],
                   by = c("psnu","facility", "indicator"), all = TRUE) %>%
    rename("Outlier_Arima" = upper99.x,
           "Outlier_STL" = upper99.y) %>%
    merge(., out_ets_wide[, c("psnu","facility", "indicator", "upper99")],
          by = c("psnu","facility", "indicator"), all = TRUE) %>%
    rename("Outlier_ETS" = upper99) %>%
    mutate(Outlier_Arima = ifelse(is.na(Outlier_Arima), 0, 1),
           Outlier_STL = ifelse(is.na(Outlier_STL), 0, 1),
           Outlier_ETS = ifelse(is.na(Outlier_ETS), 0, 1))
  summary$Outliers <- apply(summary[, c("Outlier_Arima", "Outlier_STL", "Outlier_ETS")],
                            1,
                            function(x){sum(x, na.rm = T)})
  summary <- summary %>%
    arrange(desc(Outliers))
  
  # Create cover sheet
  cover <- summary %>%
    group_by(facility, indicator) %>%
    summarize(Outliers = sum(Outliers, na.rm = TRUE)) %>% 
    mutate(facility = as.character(facility)) %>%
    pivot_wider(., id_cols = "facility", names_from = "indicator", values_from = "Outliers") %>%
    as.data.frame()
  cover[is.na(cover)] <- 0
  cover$Total <- rowSums(cover[, 2:ncol(cover)])
  indicator_sums <- c(0, colSums(cover[, 2:ncol(cover)]))
  cover <- rbind(cover, indicator_sums)
  cover[nrow(cover),1] <- "Total"


  outlist <- list("Scorecard" = cover,
                  "Summary" = summary,
                  "ARIMA" = arima_out,
                  "ETS" = ets_out,
                  "STL" = stl_out)
  
  return(outlist)
  
}


