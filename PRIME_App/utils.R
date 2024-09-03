#' sortOutputs
#' 
#' Sort recommender model outputs by outlier flag and then by Mahalanobis distance.
#' Order columns by keys, DATIM data, estimated values, normalized deviations, and then outlier flags
#'
#' @param dat dataframe returned by runRecAnalysis function
#' @param keys character vector defined above
#' @param scenario_tmp character of scenario ran
#' @param return_all logical, indicates to return anomalies only or all observations - this is now defunct, always returning anomalies only
#' @param min_thresh numeric, minimum value to consider as important deviation from expected value
#' @param fund character of funding agencies to include in output
#'
#' @return dataframe containing recommender outputs stacked for all sex disags
#' @export
#'
#' @examples
sortOutputs <- function(dat,keys,scenario_tmp, return_all, min_thresh, fund) {
  
  # remove funding scenario from keys
  keys <- keys[!keys == "fundingagency"]

  # filter model outputs for funding agencies users are interested in
  dat <- dat[dat$fundingagency %in% fund, ]

  # remove fundingagency variable which is no longer needed
  dat <- dat %>% select(-fundingagency)

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
      # Keep columns with deviations
      cols_to_keep <- which(dat_tmp[, (n_keys+1):(n_keys+n_columns)] > min_thresh) + n_keys + n_columns*2
      # If row is not an outlier, then skip
      if(dat_tmp$outlier_sp == 0){next}
      # If there are fewer than 2 columns present, skip
      if(length(cols_to_keep)<2){next}
      # Get the indicator with the maximum normalized deviation
      site_out_total[m, "Indicator"] <- colnames(dat_tmp[, cols_to_keep])[which.max(dat_tmp[, cols_to_keep])]
      # For values below min_thresh, set corresponding deviations to zero, so they are not colored as red later
      site_out_total[m, (n_keys+1+n_columns*2):(n_keys+n_columns*3)] <- ifelse(
        site_out_total[m, (n_keys+1):(n_keys+n_columns)] <= min_thresh,
        0,
        site_out_total[m, (n_keys+1+n_columns*2):(n_keys+n_columns*3)]
      )
    }
    
    # Create a column to contain the scenario name
    site_out_total$scenario <- paste0("outlier_", scenario_tmp)
    
    site_out_total <- site_out_total %>%
      mutate(across(where(is.numeric), function(x) round(x, 2)))
  }
  
  # Concatenate reported values with estimates for easier display in app
  for(p in 1:n_columns){
    site_out_total[, n_keys+p] <- paste0(site_out_total[, n_keys+p],
                                         " (",
                                         round(site_out_total[, n_keys+p+n_columns], 1),
                                         ")")
  }
  
  return(site_out_total)
  
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
  dat_df <- site_keep
  # Convert dataframe to matrix
  dat_matrix <- data.matrix(site_keep[,(ncol(keys)+1):ncol(site_keep)], rownames.force = NA)
  # Calculate pairwise correlations
  cormat <- suppressWarnings(cor(dat_matrix, use = "pairwise.complete.obs"))
  # Set lower triangle to zero since we don't want to double count; diag to zero as these are the same variable
  cormat[lower.tri(cormat)] <- 0
  diag(cormat) <- 0
  # Get correlation matrix in long format
  cormat_long <- reshape2::melt(cormat)

  # # While there is a correlation between two MER indicators of greater than 0.95:
  # while(max(cormat_long$value, na.rm = TRUE) > 0.95) {
  #   # Get relevant indicators that are collinear
  #   cormat_perf <- cormat_long %>% filter(value > .95)
  #   # Select the first variable to drop
  #   col_to_drop <- cormat_perf$Var1[1]
  #   col_to_drop <- toString(col_to_drop)
  #   # Drop the variable from dat_df
  #   dat_df <- dat_df[, !names(dat_df) %in% col_to_drop]
  #   # Follow steps above to recompute pairwise correlation matrix
  #   dat_matrix <- data.matrix(dat_df[,(ncol(keys)+1):ncol(dat_df)], rownames.force = NA)
  #   cormat <- suppressWarnings(cor(dat_matrix, use = "pairwise.complete.obs"))
  #   cormat[lower.tri(cormat)] <- 0
  #   diag(cormat) <- 0
  #   cormat_long <- reshape2::melt(cormat)
  # }
  
  # assign output, with collinear variables dropped, to site_keep
  site_keep <- dat_df
 
  ## Drop rows/observations with three or fewer MER indicators present 
  # calculate number of present indicators for each observation
  obs_count <- apply(site_keep[, (ncol(keys)+1):ncol(site_keep)], 1, function(x) length(which(!is.na(x))))
  # keep observations with at least 4 present values
  obs_to_keep <- which(obs_count > 3) 
  site_keep <- site_keep[obs_to_keep,]

  ## Now, create sparse covariance matrix - refer to paper for further explanation
  # Get sparse sum of each indicator - sum of present values by variable; use na.rm so we remove NAs
  sum_sparse <- colSums(site_keep[, (ncol(keys)+1):ncol(site_keep)], na.rm = TRUE) 
  # get number of present values by indicator - this is denominator of sparse average
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
  
  # Loop through each observation
  for (i in 1:nrow(site_keep)){

    dat <- site_keep[i, ] # get corresponding row
    inds <- which(!is.na(dat[(ncol(keys)+1):ncol(site_keep)])) # inds returns the index of the columns that have non NA values
    yt <- dat[(ncol(keys)+1):ncol(site_keep)][inds] # yt are the actual values that are associated with the indices in inds
    yt_mu <- as.matrix(yt - mu[inds]) # yt_mu subtracts the mu for each indicator from yt for each indicator
    
    Hyt <- as.matrix(i_mat[inds, ]) # hyt is a matrix that has the number of present values as rows and the numbers of all variables as columns
    if(dim(Hyt)[2] == 1){Hyt <- t(Hyt)} #this relevant if we set the threshold too high and we only have 1 indicator column coming through
    
    S <- S + (t(Hyt) %*% (t(yt_mu) %*% yt_mu) %*% Hyt) # update covariance matrix
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
  
  # Generating estimates is computationally expensive, so only generate for outliers
  site_outliers <- site_sparse[site_sparse$outlier_sp == 1, ]

  # Estimate present values - xt is the value to predict, yt are the other present values
  # value will be Rxtyt %*% Ryt_inv %*% yt-uyt + uxt
  # refer to paper for explanation
  preds <- matrix(data = NA, nrow = nrow(site_outliers), ncol = k) # set up matrix to hold estimates

  # Loop through each row
  for (i in 1:nrow(site_outliers)){
    
    # Get present values and index of present values
    dat <- site_outliers[i, ]
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
  site_all <- cbind(site_outliers, preds_df)

  # Take the difference between estimate and actual and normalize by dividing by sample variance
  # First, get the deviation
  deviation <- abs(site_all[, (ncol(site_sparse)+1):(ncol(site_all))] - site_all[, (ncol(keys)+1):(ncol(site_sparse)-2)])
  # Then normalize by dividing by sample variance
  deviation <- mapply('/', deviation, diag(R))
  deviation <- data.frame(deviation)
                              
  # Prepend "D" so it's clear it refers to deviation
  names(deviation) <- paste0("D_", names(site_sparse)[(ncol(keys)+1):(ncol(site_sparse)-2)])
                              
  # Horizontally stack deviations to table with original values and estimates
  site_out_total <- cbind(site_all, deviation)
  return(site_out_total)

  
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
  # If summary tab to be created is for facility table
    cols_to_keep <- c("psnu", "facility", "primepartner", "scenario", "Indicator", "outlier_sp") 
    dat <- lapply(dat_summary_list, function(x) x[, cols_to_keep])
  }
  
  # Bind list of dataframes into a single dataframe (only relevant with disags)
  dat <- rbindlist(dat)
  # Underlying tabs may contain all results, but summary tabs should present only outliers
  # this is now redundant since sortOutputs was updated to always only return outliers
  dat <- dat[dat$outlier_sp == 1, ]
  # Drop the actual outlier flag as everything remaining is an outlier
  dat <- dat %>% select(-outlier_sp)
  
  # PSNU, Facility, Primepartner are keys for the scorecard - returned separately
  dat_for_scorecard <- dat[, c("psnu", "facility", "primepartner", "Indicator")]

  # everything remaining is an outlier - creating a flag so that we can use numeric operations later to summarize
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
  
  # If there are multiple scenarios run that found outliers, then create a summary column
  if(n_distinct(dat$scenario) > 1){
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
    
  }
  
  
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
  scorecard <- scorecard %>% arrange(desc(Total))
  indicator_sums <- c(rep(0, 3), colSums(scorecard[, 4:ncol(scorecard)]))
  scorecard <- rbind(scorecard, indicator_sums)
  
  # Create total column to sum number of anomalies by facility
  scorecard[nrow(scorecard),1:3] <- "Total"
  return(scorecard)
  
}


#' runTimeSeries
#' 
#' Function to run time series anomaly detection using ARIMA, STL, ETS, and to create summary tables of anomalies found
#'
#' @param dat dataframe of values by facility and indicator organized chronologically
#' @param recent_year numeric the year selected by the user for classification
#' @param recent_qtr character the quarter selected by the user for classification
#' @param MIN_THRESH numeric value below which to not consider as anomalous
#' @param RETURN_ALL logical whether to return anomalies only or all values
#' @param keys character vector of variables describing unique observation 
#'
#' @return list of dataframes
#' @export
#'
#' @examples
                  
runTimeSeries <- function(dat, recent_year, recent_qtr, MIN_THRESH, RETURN_ALL, keys) {

  # provide progress updates to users
  withProgress(message = 'Running Models', value = 0, {
  
  incProgress(.2, detail = paste("Preparing Data"))
    
  # split by facility so we can loop through
  dat_split <- split(dat, dat$facility)
  
  # Create shell so we retain placeholders for missing values
  earliest_year <- as.numeric(min(dat$fiscal_year))
  shell <- expand.grid(fiscal_year = earliest_year:recent_year, qtr = 1:4) %>%
    filter(!(fiscal_year >= recent_year & qtr > as.numeric(gsub(".*?([0-9]+).*", "\\1", recent_qtr)))) %>%
    arrange(fiscal_year, qtr)

  # initialize list to hold model outputs as we loop through facilities and indicators
  outlist_arima <- list()
  outlist_ets <- list()
  outlist_stl_arima <- list()
  
  # loop through each facility
  for(i in 1:length(dat_split)){

    # tell user what number facility we're up to and how many are remaining
    incProgress(1/length(dat_split), detail = paste("Running facility:", i, "of", length(dat_split)))

    # subset to get facility data
    dat_tmp <- dat_split[[i]]

    # now split by indicator, since model is run per indicator
    ind_split <- split(dat_tmp, dat_tmp$indicator)

    # loop through indicators
    for(j in 1:length(ind_split)){

      # subset to get data per indicator
      dat_ind <- ind_split[[j]]

      # join on shell, left join, so we retain placeholders for missing values
      shell_tmp <- merge(shell, dat_ind, by = c("fiscal_year", "qtr"), all.x = TRUE) 
 
      # set up as time series
      dat_ts <- ts(shell_tmp[1:(nrow(shell_tmp)-1), 'value'], start = c(earliest_year, 1), frequency = 4) %>%
        na.trim(sides = "left") %>% # removing leading zeros from earliest periods, if there are any
        na_interpolation() # interpolate missing values with last observed value
      
      # Fit STL model and forecast last present period
      stlf_arima_forecast <- stlf(dat_ts,
                                  method = "arima",
                                  level = c(99),
                                  h = 1)

      # get 99% forecast interval and point forecast
      upper99 <- stlf_arima_forecast$upper[1]
      lower99 <- stlf_arima_forecast$lower[1]
      pred <- stlf_arima_forecast$mean[1]
      

      # add forecast range to shell which we'll carry through
      shell_tmp$upper99 <- upper99
      shell_tmp$lower99 <- lower99

      # get the actual value we observed for comparison with forecast interval
      actual <- tail(shell_tmp$value,1)

      # if the observed value is outside the forecast interval and is above min_thresh, consider it an outlier
      if(!is.na(actual) & (actual < lower99 | actual > upper99) & (abs(actual - pred) > MIN_THRESH)){
        shell_tmp$outlier <- 1
      } else {
        shell_tmp$outlier <- 0
      }

      # add these results to the list initialized above
      outlist_stl_arima[[length(outlist_stl_arima)+1]] <- shell_tmp
      
      # Repeat for ETS model
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

      # Repeat for ARIMA model
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


  # With loops complete, now stack results for each time series model
  out_stl_arima <- rbindlist(outlist_stl_arima)
  out_ets <- rbindlist(outlist_ets)
  out_arima <- rbindlist(outlist_arima)

  # First, let's proces ARIMA outputs, then we'll process STL and ETS the same way
  # sort outputs in reverse chronological order and drop missing values
  out_arima <- out_arima %>%
    arrange(desc(fiscal_year), desc(qtr)) %>%
    filter(!is.na(value))
  # pivot wider so that each facility-indicator combination becomes a row, with values sorted from
  # left to right, most recent to least (so that the most recent value, ie the one the user is interested in,
  # is the first that appears)
  out_arima_wide <- pivot_wider(out_arima,
                                id_cols = c("psnu", "facility", "primepartner", "indicator", "lower99", "upper99", "outlier"),
                                names_from = c("fiscal_year", "qtr"),
                                values_from = c("value"))
  # this is now defunct - we always return all values for time series. used to give user the option to select.
  if(RETURN_ALL == FALSE){
    out_arima_wide <- out_arima_wide %>% filter(outlier == 1)
  }

  # we want to not only push outliers to the top, but also to sort by degree of deviation from forecast range,
  # meaning to get the most anomalous at the top. 
  # Let's calculate by how much they differ (difference / range of interval)
  arima_out <- out_arima_wide %>%
    filter(!is.na(.[[length(keysts)+1]])) %>%
    mutate(gap = ifelse(.[[length(keys)]] > upper99, .[[length(keys)]] - upper99, lower99 - .[[length(keys)]]),
           deviation = gap / (upper99 - lower99)) %>%
    arrange(desc(outlier), desc(deviation)) %>%
    mutate(most_recent = paste0(.[[length(keys)]], " (", round(lower99), " - ", round(upper99), ")"))
  arima_out[, length(keys)] <- arima_out$most_recent 
  arima_out <- arima_out %>% select(-most_recent, -gap, -deviation, -upper99, -lower99) %>% as.data.frame()
  
  out_arima_wide <- out_arima_wide %>% filter(outlier == 1)
  
  # Now, repeat process for ETS
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
    filter(!is.na(.[[length(keysts)+1]])) %>%
    mutate(gap = ifelse(.[[length(keys)]] > upper99, .[[length(keys)]] - upper99, lower99 - .[[length(keys)]]),
           deviation = gap / (upper99 - lower99)) %>%
    arrange(desc(outlier), desc(deviation)) %>%
    mutate(most_recent = paste0(.[[length(keys)]], " (", round(lower99), " - ", round(upper99), ")"))
  ets_out[, length(keys)] <- ets_out$most_recent 
  ets_out <- ets_out %>% select(-most_recent, -gap, -deviation, -upper99, -lower99) %>% as.data.frame()
  out_ets_wide <- out_ets_wide %>% filter(outlier == 1)

  # And repeat process for STL
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
    filter(!is.na(.[[length(keysts)+1]])) %>%
    mutate(gap = ifelse(.[[length(keys)]] > upper99, .[[length(keys)]] - upper99, lower99 - .[[length(keys)]]),
           deviation = gap / (upper99 - lower99)) %>%
    arrange(desc(outlier), desc(deviation)) %>%
    mutate(most_recent = paste0(.[[length(keys)]], " (", round(lower99), " - ", round(upper99), ")"))
  stl_out[, length(keys)] <- stl_out$most_recent 
  stl_out <- stl_out %>% select(-most_recent, -gap, -deviation, -upper99, -lower99) %>% as.data.frame()
  out_stl_arima_wide <- out_stl_arima_wide %>% filter(outlier == 1)
  
  # Create Summary Tab - merge the three outputs together and sum the number of outliers for each
  # facility-indicator combination
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
  
  # Create IP scorecard sheet - identify the five indicators most commonly flagged as outliers per IP - as with recommender
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
  
  # Create facility scorecard sheet - sum number of anomalies by indicator and by facility
  cover <- summary %>%
    group_by(facility, psnu, primepartner, indicator) %>%
    summarize(Outliers = sum(Outliers, na.rm = TRUE), .groups = "drop") %>% 
    mutate(facility = as.character(facility)) %>%
    pivot_wider(., id_cols = c("facility", "psnu", "primepartner"), names_from = "indicator", values_from = "Outliers") %>%
    as.data.frame()
  cover[is.na(cover)] <- 0

  # if multiple indicators selected, use rowsums and colsums to aggregate
  # cover's first three columns are keys - psnu, primepartner, facility, so column four is the number of outliers
  # this will be the case if more than one indicator is selected by user for analysis 
  if(ncol(cover)>4){
    cover <- cbind.data.frame(PSNU = cover$psnu,
                              Facility = cover$facility,
                              PrimePartner = cover$primepartner,
                              cover[, 4:ncol(cover)],
                              stringsAsFactors = FALSE)
    cover$Total <- rowSums(cover[, 4:ncol(cover)])
    cover <- cover %>% arrange(desc(Total))
    indicator_sums <- c(0,0,0, colSums(cover[, 4:ncol(cover)]))
    cover <- rbind(cover, indicator_sums)
    cover[nrow(cover),1:3] <- "Total"
  }
  
  # If only one indicator selected, don't use rowsums and colsums which will break 
  if(ncol(cover) == 4){
    cover <- cbind.data.frame(PSNU = cover$psnu,
                              Facility = cover$facility,
                              PrimePartner = cover$primepartner,
                              cover[, 4],
                              stringsAsFactors = FALSE)
    cover$Total <- cover[, 4]
    # Get name of indicator for column name
    names(cover)[4] <- summary$indicator[1]
    cover <- cover %>% arrange(desc(Total))
    indicator_sums <- c(0,0,0, sum(cover[, 4]))
    cover <- rbind(cover, indicator_sums)
    cover[nrow(cover),1:3] <- "Total"
  }

  })

  # return list of dataframes to then send back to UI and for download
  outlist <- list("facility_scorecard" = cover,
                  "ip_scorecard" = ip_cover,
                  "Summary" = summary,
                  "ARIMA" = arima_out,
                  "ETS" = ets_out,
                  "STL" = stl_out)
  
  return(outlist)
  
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
formatCells <- function(name, disags, facilities, keys_disag, keys_facility, wb_format){

  # get the name of the sheet to create so we can know what output table to pull
  name_sheet <- sub("_.*", "", name)

  # gather if sheet name indicates information is from disag or facility
  if(grepl("Sex|All|Age", name_sheet)){
    dat <- disags
    n_keys <- length(keys_disag)
  } else {
    dat <- facilities
    n_keys <- length(keys_facility)
  }
  
  dat_tmp <- dat[[name_sheet]] # get corresponding output table
  n_columns <- sum(grepl("^E_", names(dat_tmp))) # number of indicators
  nrows <- nrow(dat_tmp)+1
  
  # https://www.w3schools.com/colors/colors_picker.asp?colorhex=8B0000
  cs1 <- createStyle(bgFill = "#FF0000")
  cs2 <- createStyle(bgFill = "#FF8080")
  cs3 <- createStyle(bgFill = "#FFFFFF")

  # get the columns that correspond to the deviation values
  deviations <- dat_tmp[, (n_keys + 1 + (2*n_columns)):(n_keys + n_columns + (2*n_columns))]
  # create cutoffs for 80th and 90th percentile of deviation value range
  quants <- suppressWarnings(quantile(as.numeric(reshape2::melt(deviations)$value), c(.8, .9), na.rm = TRUE))
  
  # loop through columns
  for(j in 1:n_columns){
    
    # Get Excel column position of deviation and deviation
    # since this is Excel, 27th column is AA, so code identifies which block of 26 a column is in
    deviation_col <- n_keys + j + (2*n_columns)
    estimation_col <- n_keys + j + n_columns
    deviation_col <- if(deviation_col <= 26){
      LETTERS[deviation_col]
    } else if(deviation_col > 26 & deviation_col < 52){
      paste0("A", LETTERS[deviation_col %% length(LETTERS)])
    } else if(deviation_col == 52){
      "AZ"
    } else if(deviation_col > 52 & deviation_col < 78){
      paste0("B", LETTERS[deviation_col %% length(LETTERS)])
    } else if(deviation_col == 78){
      "BZ"
    } else if(deviation_col > 78 & deviation_col < 104){
      paste0("C", LETTERS[deviation_col %% length(LETTERS)])
    } else if(deviation_col == 104){
      "CZ"
    } else if(deviation_col > 104 & deviation_col <= 130){
      paste0("D", LETTERS[deviation_col %% length(LETTERS)])
    }
    estimation_col <- if(estimation_col <= 26){
      LETTERS[estimation_col]
    } else if(estimation_col > 26 & estimation_col < 52){
      paste0("A", LETTERS[estimation_col %% length(LETTERS)])
    } else if(estimation_col == 52){
      "AZ"
    } else if(estimation_col > 52 & estimation_col < 78){
      paste0("B", LETTERS[estimation_col %% length(LETTERS)])
    } else if(estimation_col == 78){
      "BZ"
    }

    # apply conditional formatting - if deviation is above 80th percentile, color reported value with cs2
    conditionalFormatting(wb_format, name,
                          cols = n_keys + j,
                          rows = 2:nrows,
                          rule = paste0(deviation_col, 2, ">", quants[1]), 
                          style = cs2)
    # apply conditional formatting - if deviation is above 90th percentile, color reported value with cs1
    conditionalFormatting(wb_format, name,
                          cols = n_keys + j,
                          rows = 2:nrows,
                          rule = paste0(deviation_col, 2, ">", quants[2]), 
                          style = cs1)
    
    # If value is less than 10, set to no fill
    conditionalFormatting(wb_format, name,
                          cols = n_keys + j,
                          rows = 2:nrows,
                          rule = paste0(estimation_col, 2, "< 10"), 
                          style = cs3)

    
  }

  # shrink column width for estimated values and deviation columns
  setColWidths(wb_format, name, (n_keys+n_columns+1):(ncol(dat_tmp)+1), 0)
  
}
