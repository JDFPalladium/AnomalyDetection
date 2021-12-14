# This script contains all the functions to run the Data.FI Anomaly Detection
# Time Series solution that is called in the accompanying script, main.R. Users  
# should not make changes to this script without care. 

list_packages <- c("dplyr", "tidyr", "forecast", "imputeTS", "zoo", "openxlsx", "data.table")
new_packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
library(dplyr)
library(tidyr)
library(forecast)
library(imputeTS)
library(data.table)
library(openxlsx)
library(zoo)

keys <- c("psnu", "facility", "primepartner", "indicator", "lower99", "upper99", "outlier")

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
  file_out <- paste0("TimeSeries/Outputs/", OU, "-TS-", Sys.Date(), ".xlsx")
  # Get cover sheet
  wb <- loadWorkbook("TimeSeries/TimeSeriesCoverSheet.xlsx")
  # Create styles
  headerStyle <- createStyle(fontSize = 14, textDecoration = "bold", fgFill = "#d3d3d3")
  textStyle <- createStyle(fontSize = 16, textDecoration = "bold", fgFill = "#add8e6")
  
  if(nrow(output$facility_scorecard) > 50){
    dat <- output$facility_scorecard[1:10, 1:min(12, ncol(output$facility_scorecard))]
    facilities_flagged <- data.frame(Facilities = dat$Facility[1:10],
                                     PrimePartner = dat$PrimePartner[1:10])
    indicators_flagged <- names(dat)[3:min(11, ncol(output$facility_scorecard))]
    indicators_flagged <- indicators_flagged[indicators_flagged != "Total"]
    if(length(indicators_flagged)<10){
      indicators_flagged <- c(indicators_flagged, rep(NA, 10-length(indicators_flagged)))
    }
    indicators_flagged <- data.frame(Indicators = indicators_flagged)
    # takeaway <- data.frame(Facilities = facilities_flagged,
    #                        Indicators = indicators_flagged)
    addWorksheet(wb, 'Takeaway', tabColour = "blue")
    writeData(wb, sheet = 'Takeaway', facilities_flagged, startRow = 3, rowNames = FALSE)
    writeData(wb, sheet = 'Takeaway', indicators_flagged, startRow = 15, rowNames = FALSE)
    setColWidths(wb, sheet = 'Takeaway', 1:2, width = "auto")
    writeData(wb, sheet = 'Takeaway', "This tab shows the facilities and indicators with the most anomalies.")
    addStyle(wb, sheet = 'Takeaway', textStyle, rows = 1, cols = 1)
    addStyle(wb, sheet = 'Takeaway', headerStyle, rows = 3, cols = 1:2)
    addStyle(wb, sheet = 'Takeaway', headerStyle, rows = 15, cols = 1)
  }
  
  addWorksheet(wb, 'IP Scorecard', tabColour = "blue")
  writeData(wb, sheet = 'IP Scorecard', "This tab contains a summary of anomalies by IP by indicator.")
  writeData(wb, sheet = 'IP Scorecard', output$ip_scorecard, startRow = 3)
  setColWidths(wb, sheet = 'IP Scorecard', 1:20, width = "auto")
  addStyle(wb, sheet = 'IP Scorecard', headerStyle, rows = 1, cols = 1)
  addStyle(wb, sheet = 'IP Scorecard', headerStyle, rows = 3, cols = 1:ncol(output$ip_scorecard))
  
  addWorksheet(wb, 'Facility Scorecard', tabColour = "blue")
  writeData(wb, sheet = 'Facility Scorecard', "This tab contains a summary of anomalies by facility.")
  writeData(wb, sheet = 'Facility Scorecard', output$facility_scorecard, startRow = 3)
  setColWidths(wb, sheet = 'Facility Scorecard', 1:20, width = "auto")
  addStyle(wb, sheet = 'Facility Scorecard', headerStyle, rows = 1, cols = 1)
  addStyle(wb, sheet = 'Facility Scorecard', headerStyle, rows = 3, cols = 1:ncol(output$facility_scorecard))
  
  addWorksheet(wb, 'Summary', tabColour = "orange")
  writeData(wb, sheet = 'Summary', output$Summary)
  setColWidths(wb, sheet = 'Summary', 1:3, width = "auto")
  addStyle(wb, sheet = 'Summary', headerStyle, rows = 1, cols = 1:ncol(output$Summary))
  
  addWorksheet(wb, 'ARIMA', tabColour = "green")
  writeData(wb, sheet = 'ARIMA', output$ARIMA)
  setColWidths(wb, sheet = 'ARIMA', 1:3, width = "auto")
  addStyle(wb, sheet = 'ARIMA', headerStyle, rows = 1, cols = 1:ncol(output$ARIMA))
  
  addWorksheet(wb, 'ETS', tabColour = "green")
  writeData(wb, sheet = 'ETS', output$ETS)
  setColWidths(wb, sheet = 'ETS', 1:3, width = "auto")
  addStyle(wb, sheet = 'ETS', headerStyle, rows = 1, cols = 1:ncol(output$ETS))
  
  addWorksheet(wb, 'STL', tabColour = "green")
  writeData(wb, sheet = 'STL', output$STL)
  setColWidths(wb, sheet = 'STL', 1:3, width = "auto")
  addStyle(wb, sheet = 'STL', headerStyle, rows = 1, cols = 1:ncol(output$STL))
  
  saveWorkbook(wb, file = file_out, overwrite = TRUE)
  print("Code Completed.")
}

datPrep <- function(mer_list, recent_year, recent_qtr) {
  
  cols_to_keep <- c("facility", "indicator", "psnu", "numeratordenom", "disaggregate", "statushiv",
                    "ageasentered","primepartner", "fiscal_year", "qtr1", "qtr2", "qtr3", "qtr4")
  
  for(i in 1:length(mer_list)){
    if(sum(!cols_to_keep %in% names(mer_list[[i]])) > 0){
      stop("One or more of the required columns are missing from your dataset. Please make sure 
          that your dataset contains all the following columns: facility, indicator, psnu, 
          frequency, numeratordenom, disaggregate, fiscal_year, qtr1, qtr2, qtr3, and qtr4.")
    }
  }
  
  mer_list <- lapply(mer_list, function(x) x[, names(x) %in% cols_to_keep])
  mer_data <- rbindlist(mer_list) %>% as.data.frame()
  
  # Keep only USAID sites
  mer_data <- mer_data %>% filter(fundingagency == "USAID")
  
  mer_data$indicator <- as.character(mer_data$indicator)
  mer_data <- mer_data %>%
    filter(indicator %in% quarterly_indicators)
  
  mer_data$indicator <- paste0(mer_data$indicator, "_", mer_data$numeratordenom)
  
  # remove the rows that report on Total Numerator or Total Denominator data
  mer_data <- mer_data %>% filter(!disaggregate %in% c("Total Numerator", "Total Denominator"))
  
  mer_data <- mer_data %>% filter(tolower(facility) != "data reported above facility level")
  
  # remove rows that are aggregates of age groups (e.g. 15+) but keep 50+
  mer_data <- rbind(mer_data[-grep("\\+", mer_data$ageasentered),],
                    mer_data[grep("50+", mer_data$ageasentered),])
  
  mer_data <- mer_data[-grep("<+[0-9]", mer_data$ageasentered),]
  
  # For HTS_INDEX, keep only those rows where statushiv is positive or negative
  mer_data <- rbind(mer_data[mer_data$indicator!='HTS_INDEX',],
                    mer_data[mer_data$indicator=='HTS_INDEX'&(mer_data$statushiv %in% c('Negative', 'Positive')),])
  
  # Drop deduplication rows from prime partner
  mer_data <- mer_data[!mer_data$primepartner %in% c("Dedup", "TBD"), ]
  
  mer_data <- mer_data[, !names(mer_data) %in% c("numeratordenom", "disaggregate", "ageasentered", "statushiv")]
  
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
  
  if(length(indicators_to_keep)==0){
    stop("No data found in target year and quarter. Please confirm
         that the dataset includes the year and quarter selected.")
  }
  
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
  shell <- expand.grid(fiscal_year = earliest_year:recent_year, qtr = 1:4) %>%
    filter(!(fiscal_year >= recent_year & qtr > as.numeric(gsub(".*?([0-9]+).*", "\\1", recent_qtr)))) %>%
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
    select(-count, -yrqtr, - keep)
  
  # convert facility to character string
  obs_to_keep$facility <- as.character(obs_to_keep$facility)
  
  return(obs_to_keep)
}

runTimeSeries <- function(dat, recent_year, recent_qtr) {

  # split by facility and indicator
  dat_split <- split(dat, dat$facility)
  
  # Create shell
  earliest_year <- as.numeric(min(dat$fiscal_year))
  shell <- expand.grid(fiscal_year = earliest_year:recent_year, qtr = 1:4) %>%
    filter(!(fiscal_year >= recent_year & qtr > as.numeric(gsub(".*?([0-9]+).*", "\\1", recent_qtr)))) %>%
    arrange(fiscal_year, qtr)
  
  outlist_arima <- list()
  outlist_ets <- list()
  outlist_stl_arima <- list()
  
  # for each facility
  # for(i in 1:100){
  for(i in 1:length(dat_split)){
    
    if(i %% 25 ==0 ){
      cat(paste0("Running facility: ", i, " of ", length(dat_split), "\n"))
    }
    
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
        shell_tmp$outlier <- 1
      } else {
        shell_tmp$outlier <- 0
      }
      
      outlist_stl_arima[[length(outlist_stl_arima)+1]] <- shell_tmp
      
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
        shell_tmp$outlier <- 1
      } else {
        shell_tmp$outlier <- 0
      }
      
      outlist_ets[[length(outlist_ets)+1]] <- shell_tmp
      
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
        shell_tmp$outlier <- 1
      } else {
        shell_tmp$outlier <- 0
      }
      
      outlist_arima[[length(outlist_arima)+1]] <- shell_tmp
      
    }
    
  }
  
  out_stl_arima <- rbindlist(outlist_stl_arima)
  out_ets <- rbindlist(outlist_ets)
  out_arima <- rbindlist(outlist_arima)
  
  out_arima <- out_arima %>%
    arrange(desc(fiscal_year), desc(qtr)) %>%
    filter(!is.na(value))
  out_arima_wide <- pivot_wider(out_arima,
                                id_cols = c("psnu", "facility", "primepartner", "indicator", "lower99", "upper99", "outlier"),
                                names_from = c("fiscal_year", "qtr"),
                                values_from = c("value"))
  if(RETURN_ALL == FALSE){
    out_arima_wide <- out_arima_wide %>% filter(outlier == 1)
  }
  
  # Let's calculate by how much the differ (difference / range of interval)
  arima_out <- out_arima_wide %>%
    mutate(gap = ifelse(.[[length(keys)+1]] > upper99, .[[length(keys)+1]] - upper99, lower99 - .[[length(keys)+1]]),
           deviation = gap / (upper99 - lower99)) %>%
    arrange(desc(outlier), desc(deviation)) %>%
    mutate(most_recent = paste0(.[[length(keys)+1]], " (", round(lower99, digits=1), " - ", round(upper99,digits=1), ")"))
  arima_out[, length(keys)+1] <- arima_out$most_recent 
  arima_out <- arima_out %>% select(-most_recent, -gap, -deviation, -upper99, -lower99) %>% as.data.frame()
  
  out_arima_wide <- out_arima_wide %>% filter(outlier == 1)
  
  
  out_ets <- out_ets %>%
    arrange(desc(fiscal_year), desc(qtr)) %>%
    filter(!is.na(value))
  out_ets_wide <- pivot_wider(out_ets,
                              id_cols = c("psnu","facility", "primepartner", "indicator", "lower99", "upper99", "outlier"),
                              names_from = c("fiscal_year", "qtr"),
                              values_from = c("value"))
  if(RETURN_ALL == FALSE){
    out_ets_wide <- out_ets_wide %>% filter(outlier == 1)
  }
  ets_out <- out_ets_wide %>%
    mutate(gap = ifelse(.[[length(keys)+1]] > upper99, .[[length(keys)+1]] - upper99, lower99 - .[[length(keys)+1]]),
           deviation = gap / (upper99 - lower99)) %>%
    arrange(desc(outlier), desc(deviation)) %>%
    mutate(most_recent = paste0(.[[length(keys)+1]], " (", round(lower99, digits=1), " - ", round(upper99,digits=1), ")"))
  ets_out[, length(keys)+1] <- ets_out$most_recent 
  ets_out <- ets_out %>% select(-most_recent, -gap, -deviation, -upper99, -lower99) %>% as.data.frame()
  out_ets_wide <- out_ets_wide %>% filter(outlier == 1)
  
  out_stl_arima <- out_stl_arima %>%
    arrange(desc(fiscal_year), desc(qtr)) %>%
    filter(!is.na(value))
  out_stl_arima_wide <- pivot_wider(out_stl_arima,
                                    id_cols = c("psnu","facility", "primepartner","indicator", "lower99", "upper99", "outlier"),
                                    names_from = c("fiscal_year", "qtr"),
                                    values_from = c("value"))
  if(RETURN_ALL == FALSE){
    out_stl_arima_wide <- out_stl_arima_wide %>% filter(outlier == 1)
  }
  stl_out <- out_stl_arima_wide %>%
    mutate(gap = ifelse(.[[length(keys)+1]] > upper99, .[[length(keys)+1]] - upper99, lower99 - .[[length(keys)+1]]),
           deviation = gap / (upper99 - lower99)) %>%
    arrange(desc(outlier), desc(deviation)) %>%
    mutate(most_recent = paste0(.[[length(keys)+1]], " (", round(lower99, digits=1), " - ", round(upper99,digits=1), ")"))
  stl_out[, length(keys)+1] <- stl_out$most_recent 
  stl_out <- stl_out %>% select(-most_recent, -gap, -deviation, -upper99, -lower99) %>% as.data.frame()
  out_stl_arima_wide <- out_stl_arima_wide %>% filter(outlier == 1)
  
  # Create Summary Tab
  summary <- merge(out_arima_wide[, c("psnu", "facility", "primepartner",  "indicator", "upper99")],
                   out_stl_arima_wide[, c("psnu","facility", "primepartner", "indicator", "upper99")],
                   by = c("psnu","facility", "primepartner",  "indicator"), all = TRUE) %>%
    rename("Outlier_Arima" = upper99.x,
           "Outlier_STL" = upper99.y) %>%
    merge(., out_ets_wide[, c("psnu","facility", "primepartner", "indicator", "upper99")],
          by = c("psnu","facility", "primepartner",  "indicator"), all = TRUE) %>%
    rename("Outlier_ETS" = upper99) %>%
    mutate(Outlier_Arima = ifelse(is.na(Outlier_Arima), 0, 1),
           Outlier_STL = ifelse(is.na(Outlier_STL), 0, 1),
           Outlier_ETS = ifelse(is.na(Outlier_ETS), 0, 1))
  summary$Outliers <- apply(summary[, c("Outlier_Arima", "Outlier_STL", "Outlier_ETS")],
                            1,
                            function(x){sum(x, na.rm = T)})
  summary <- summary %>%
    arrange(desc(Outliers))
  
  # Create IP scorecard sheet
  cover_ip <- summary %>%
    group_by(primepartner, indicator) %>%
    summarize(Outliers = sum(Outliers, na.rm = TRUE), .groups = "drop") %>% 
    mutate(primepartner = as.character(primepartner))
  
  ips <- unique(cover_ip$primepartner)
  ip_cover <- data.frame()
  for(i in 1:length(ips)){
    dat_tmp <- cover_ip %>% filter(primepartner == ips[i]) %>%
      arrange(desc(Outliers)) %>%
      mutate(rownum = row_number()) %>%
      filter(rownum <= 5)
    
    ip_cover[1:(min(length(dat_tmp$indicator), 5)), i] <- dat_tmp$indicator
    names(ip_cover)[i] <- ips[i]
  }
  
  # Create facility scorecard sheet
  cover <- summary %>%
    group_by(facility, primepartner, indicator) %>%
    summarize(Outliers = sum(Outliers, na.rm = TRUE), .groups = "drop") %>% 
    mutate(facility = as.character(facility)) %>%
    pivot_wider(., id_cols = c("facility", "primepartner"), names_from = "indicator", values_from = "Outliers") %>%
    as.data.frame()
  cover[is.na(cover)] <- 0
  # sort columns by number of outliers
  cover <- cbind.data.frame(Facility = cover$facility, PrimePartner = cover$primepartner,
    cover[, 3:ncol(cover)][order(colSums(cover[, 3:ncol(cover)]), decreasing = T)],
    stringsAsFactors = FALSE)
  cover$Total <- rowSums(cover[, 3:ncol(cover)])
  cover <- cover %>% arrange(desc(Total))
  indicator_sums <- c(0,0, colSums(cover[, 3:ncol(cover)]))
  cover <- rbind(cover, indicator_sums)
  cover[nrow(cover),1:2] <- "Total"

  outlist <- list("facility_scorecard" = cover,
                  "ip_scorecard" = ip_cover,
                  "Summary" = summary,
                  "ARIMA" = arima_out,
                  "ETS" = ets_out,
                  "STL" = stl_out)
  
  return(outlist)
  
}


