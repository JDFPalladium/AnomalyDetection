
# function 1: dat_Prep for disagg level
#' MER Data Preparation
#'
#' @param dat - data frame containing raw MER data
#' @param year_for_analysis - numeric user-defined
#' @param qtr_for_analysis - string user-defined 
#'
#' @return - data frame suitable for recommender analysis function
#' @export
#'
#' @examples
datPrep <- function(dat=mer_data,
                    year_for_analysis=year,
                    qtr_for_analysis = qtr) {
  
  # Add check to confirm year exists in the dataset
  if(!year_for_analysis %in% unique(dat$fiscal_year)){
    stop("Please confirm the fiscal year selected is included in the file uploaded.")
  }
  
  # Add check to confirm year exists in the dataset
  if(!qtr_for_analysis %in% names(dat)){
    stop("Please confirm the quarter selected is included in the file uploaded.")
  }
  
  if(any(!c("sitename","psnu","facility","indicator","numeratordenom",
            "disaggregate","ageasentered","sex","primepartner") %in% names(dat))){
    stop("Please confirm the file selected contains the required columns: 
         sitename,psnu,facility,indicator,numeratordenom,disaggregate,ageasentered,sex,primepartner")
  }
  
  # remove the columns we don't need
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
  
  # label indicators with N and D
  dat$indicator <- paste0(dat$indicator, "_", dat$numeratordenom)
  
  dat <- dat %>% 
    mutate(ageasentered = ifelse(ageasentered == "01-04", "01-04",
                             ifelse(ageasentered == "05-09", "05-09", 
                                    ifelse(ageasentered == "10-14", "10-14", 
                                           ifelse(ageasentered == "15-19", "15-19",
                                                  ifelse(ageasentered == "20-24", "20-24",
                                                         ifelse(ageasentered == "25-29", "25-29",
                                                                ifelse(ageasentered == "30-34", "30-34",
                                                                       ifelse(ageasentered == "35-39", "35-39",
                                                                              ifelse(ageasentered == "40-44", "40-49",
                                                                                     ifelse(ageasentered == "45-49", "40-49",
                                                                                            ifelse(ageasentered == "Unknown Age", "Unknown","Other"))))))))))))
  
  # create the facility data frame which we will need later in this function for data prep of the facility level file 
  dat_facility <- dat
  
  # disaggregate level file - create a column for the key population disaggregate
  dat$kp <- ifelse(grepl("KeyPop", dat$disaggregate), "Yes", "No")
  
  # disaggregate level file - drop disaggregate and numeratordenom columns
  cols_to_drop <- c("numeratordenom", "disaggregate")
  dat <- dat[,!(names(dat) %in% cols_to_drop)]
  
  # disaggregate level file - group by facility, age, sex, and indicator, kp, and psnu, and then summarize qtr (before pivot)
  names(dat) <- gsub("[0-9]","", names(dat))
  dat <- dat %>% 
    filter(!is.na(qtr))
  dat_grouped <- dat %>% group_by(facility, ageasentered, sex, indicator, kp, psnu, primepartner) %>% 
    summarise(qtr_sum = sum(qtr, na.rm = TRUE)) 
  
  # disaggregate level file - pivot wider
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
  
  # facility level file - pivot wider
  dat_facility_out <- dat_facility %>%
    pivot_wider(names_from = "indicator", values_from = "qtr_sum") %>%
    as.data.frame()
  
  # return data frames
  return(list(
    "dat_disag_out" = dat_out,
    "dat_facility_out" = dat_facility_out))
  
}


runRecAnalysisDisag <- function(dat=dat_disag_prepped$dat_disag_out,
                                keys=keys_disag,
                                scenario) {
  if (scenario == "all") {
    
    all_outputs <- runRecAnalysis(dat,keys)
    all_outputs <- tryCatch({
      sortOutputs(all_outputs, keys = keys,scenario_tmp = scenario)
    }, error = function(cond){
      message("No Outliers Found with All Disags")
      message(cond)})
    return(all_outputs)
    
  } else if (scenario == "age") {
    
    ageWrapper(dat,keys,scenario_wrapper=scenario, age_groups)
    
  } else if (scenario == "sex") {
    
    sexWrapper(dat,keys,scenario_wrapper=scenario)
    
  }
}

#dat_test2 <- dat_disag_prepped$dat_disag_out
#dat_test2 <- cbind("agegroup" = ifelse(dat_test2$ageasentered %in% c("01-04", "05-09", "10-14"), "Under 15", "Over 15"),
#             dat_test2, stringsAsFactors = FALSE) 

#dat_test1 <- dat_disag_prepped$dat_disag_out
#dat_test1 <- dat_test1 %>% 
#  mutate(agegroup = ifelse(ageasentered == "01-04", "01-04",
#                           ifelse(ageasentered == "05-09", "05-09", 
#                                  ifelse(ageasentered == "10-14", "10-14", 
#                                         ifelse(ageasentered == "15-19", "15-19",
#                                                ifelse(ageasentered == "20-24", "20-24",
#                                                       ifelse(ageasentered == "25-29", "25-29",
#                                                              ifelse(ageasentered == "30-34", "30-34",
#                                                                     ifelse(ageasentered == "35-39", "35-39",
#                                                                            ifelse(ageasentered == "40-44", "40-49",
#                                                                                   ifelse(ageasentered == "45-49", "40-49",
#                                                                                          ifelse(ageasentered == "Unknown Age", "Unknown","Other"))))))))))))
#dat_test1 <- dat_test1 %>%
#  select(agegroup, everything())

ageWrapper <- function(dat,keys, scenario_wrapper, age_groups) {
  
  age_categories <- c("01-04",  "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                      "45-49", "05-09", "10-14")
  dat$ageasentered <- as.character(dat$ageasentered)
  dat <- dat[dat$ageasentered %in% age_categories, ]
  
  if(age_groups == "Over/Under 15"){
    
    dat <- cbind("agegroup" = ifelse(dat$ageasentered %in% c("01-04", "05-09", "10-14"), "Under 15", "Over 15"),
                 dat, stringsAsFactors = FALSE) 
    
    site_split <- split(dat, factor(dat$agegroup))
    
    site_out <- list()
    
    for (j in 1:length(site_split)) {
      
      site_out[[j]] <- tryCatch({
        runRecAnalysis(site_split[[j]],keys = c(keys, "agegroup"))
      }, error = function(cond){
        message(paste("Insufficient Data to Run Disag for Age Group:", names(site_split)[j]))
        message(cond)})
    }
    
    # stack the outputs
    site_age_outliers <- do.call(plyr::rbind.fill, site_out) %>% select(-agegroup)
    
  }
  
  if(age_groups == "DQA"){
    
    dat <- dat %>% 
      mutate(agegroup = ifelse(ageasentered == "01-04", "01-04",
                               ifelse(ageasentered == "05-09", "05-09", 
                               ifelse(ageasentered == "10-14", "10-14", 
                               ifelse(ageasentered == "15-19", "15-19",
                               ifelse(ageasentered == "20-24", "20-24",
                               ifelse(ageasentered == "25-29", "25-29",
                               ifelse(ageasentered == "30-34", "30-34",
                               ifelse(ageasentered == "35-39", "35-39",
                               ifelse(ageasentered == "40-44", "40-49",
                               ifelse(ageasentered == "45-49", "40-49",
                               ifelse(ageasentered == "Unknown Age", "Unknown","Other"))))))))))))
    
    dat <- dat %>%
      select(agegroup, everything())
    
    site_split <- split(dat, factor(dat$agegroup))
    
    site_out <- list()
    
    for (j in 1:length(site_split)) {
      
      site_out[[j]] <- tryCatch({
        runRecAnalysis(site_split[[j]],keys = c(keys, "agegroup"))
      }, error = function(cond){
        message(paste("Insufficient Data to Run Disag for Age Group:", names(site_split)[j]))
        message(cond)})
    }
    
    # stack the outputs
    site_age_outliers <- do.call(plyr::rbind.fill, site_out) %>% select(-agegroup)
    
  }
  
  if(age_groups == "Five Year"){
    
    site_split <- split(dat, factor(dat$ageasentered))
    
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
  
  if(is.null(site_age_outliers)){
    stop("Insufficient data to run age disaggregates. Consider grouping at over/under 15 or not running age disags.")
  }
  
  site_age_outliers <- tryCatch({
    sortOutputs(site_age_outliers, keys = keys,scenario_tmp = scenario_wrapper)
  }, error = function(cond){
    message("No Outliers Found for Disag for Age Group:")
    message(cond)})
  
  
  return(site_age_outliers)
  
}

sexWrapper <- function(dat,keys, scenario_wrapper) {
  dat$sex <- as.character(dat$sex)
  dat <- dat[dat$sex %in% c("Male", "Female"), ]
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

sortOutputs <- function(dat,keys,scenario_tmp) {
  # stack the outputs
  site_out_total <- dat
  site_out_total <- site_out_total %>%
    select(names(site_out_total)[!grepl("^E_|^D_|^MD|outlier_sp", names(site_out_total))],
           names(site_out_total)[grepl("^E_", names(site_out_total))],
           names(site_out_total)[grepl("^D_", names(site_out_total))],
           names(site_out_total)[grepl("^MD", names(site_out_total))],
           names(site_out_total)[grepl("outlier_sp", names(site_out_total))])
  site_out_total <- site_out_total %>% 
    arrange(desc(MD)) %>%
    mutate(Indicator = NA)
  
  if(RETURN_ALL == FALSE){
    site_out_total <- site_out_total %>% filter(outlier_sp == 1)
  }
  
  n_columns = sum(grepl("^E_", names(site_out_total)))
  n_keys = length(keys)
  
  if(nrow(site_out_total)>0){
    for(m in 1:nrow(site_out_total)){
      dat_tmp <- site_out_total[m, ]
      if(dat_tmp$outlier_sp == 0){next}
      cols_to_keep <- which(dat_tmp[, (n_keys+1):(n_keys+n_columns)] > MIN_THRESH) + n_keys + n_columns*2
      if(length(cols_to_keep)<2){next}
      site_out_total[m, "Indicator"] <- colnames(dat_tmp[, cols_to_keep])[which.max(dat_tmp[, cols_to_keep])]
    }
    site_out_total$scenario <- paste0("outlier_", scenario_tmp)
    return(site_out_total)
  }
}

#dat <- dat_disag_prepped$dat_disag_out
#keys <- c('facility','ageasentered', 'sex', 'kp', 'psnu', 'primepartner')

runRecAnalysis <- function(dat,keys) {
  site_spread <- dat
  keys <- site_spread[, 1:length(keys)] 
  count_present <- apply(site_spread[, (ncol(keys)+1):ncol(site_spread)], 2, function(x) length(which(!is.na(x)))) # tells us how many rows have each indicator reported
  cols_to_keep <- which(count_present > (nrow(site_spread)*.05))+ncol(keys) # keep variables present at least 10% of the time
  site_keep <- cbind.data.frame(site_spread[, names(keys)], site_spread[, cols_to_keep])
  
  # drop variables that have no variance
  var_inds <- apply(site_keep[,(ncol(keys)+1):ncol(site_keep)], 2, FUN = var, na.rm = TRUE)
  cols_to_drop <- names(site_keep[,(ncol(keys)+1):ncol(site_keep)])[var_inds == 0] 
  site_keep <- site_keep[,!(names(site_keep) %in% cols_to_drop)]
  
  # drop variables that are colinear. Filter for greater than 0.98. Drop the variable that is present the most times across all of the pairs that are greater than 0.98. Regenerate the correlation matrix and repeat. Keep looping until there are no correlations left greater than 0.98.
  dat_df <- site_keep
  dat_matrix <- data.matrix(site_keep[,(ncol(keys)+1):ncol(site_keep)], rownames.force = NA)
  cormat <- suppressWarnings(cor(dat_matrix, use = "pairwise.complete.obs"))
  cormat[lower.tri(cormat)] <- 0
  diag(cormat) <- 0
  # melt cormat
  cormat_long <- reshape2::melt(cormat)
  
  while(max(cormat_long$value, na.rm = TRUE) > 0.95) {
    cormat_perf <- cormat_long %>% filter(value > .95) 
    col_to_drop <- cormat_perf$Var2[1]
    col_to_drop <- toString(col_to_drop)
    dat_df <- dat_df[, !names(dat_df) %in% col_to_drop]
    dat_matrix <- data.matrix(dat_df[,(ncol(keys)+1):ncol(dat_df)], rownames.force = NA)
    cormat <- suppressWarnings(cor(dat_matrix, use = "pairwise.complete.obs"))
    cormat[lower.tri(cormat)] <- 0
    diag(cormat) <- 0
    # melt cormat
    cormat_long <- reshape2::melt(cormat)
  }
  
  # assign dat_df to site_keep
  site_keep <- dat_df
  
  # sparsity drops
  obs_count <- apply(site_keep[, (ncol(keys)+1):ncol(site_keep)], 1, function(x) length(which(!is.na(x)))) # drops the facilities for which all values are missing
  obs_to_keep <- which(obs_count > 3) # keep observations with at least 4 present values
  site_keep <- site_keep[obs_to_keep,]
  
  # get sparse mu (vector of means)
  # alternatively use g sub to replace commas with blanks, the convert using as numeric
  sum_sparse <- colSums(site_keep[, (ncol(keys)+1):ncol(site_keep)], na.rm = TRUE) # sum of present values by variable; use na.rm so we remove NAs
  count_present_keep <- apply(site_keep[, (ncol(keys)+1):ncol(site_keep)], 2, function(x) length(which(!is.na(x))))
  # count_present_keep <- count_present[cols_to_keep-5] # count of present values by variable
  mu <- sum_sparse / count_present_keep # means
  k <- length(mu) # number of variables (indicators)
  
  N <- matrix(0, k, k) # set up k by k matrix; the diagonal is the number of observations; look at the paper
  diag(N) <- count_present_keep # diagonal initiated with count of present values
  
  i_mat <- matrix(0, k, k) # set up identity matrix
  diag(i_mat) <- 1
  
  S <- matrix(0, k, k) # N is the counts, s is where we store the covariances, and I is useful for some calculations
  
  for (i in 1:nrow(site_keep)){
    dat <- site_keep[i, ]
    inds <- which(!is.na(dat[(ncol(keys)+1):ncol(site_keep)])) # inds returns the index of the columns that have non NA values
    yt <- dat[(ncol(keys)+1):ncol(site_keep)][inds] # yt are the actual values that are associated with the indices in inds
    yt_mu <- as.matrix(yt - mu[inds]) # yt_mu subtracts the mu for each indicator from yt for each indicator
    
    Hyt <- as.matrix(i_mat[inds, ]) # hyt is a matrix that has the number of present values as rows and the numbers of all variables as columns
    if(dim(Hyt)[2] == 1){Hyt <- t(Hyt)} #this relevant if we set the threshold too high and we only have 1 indicator column coming through
    
    S <- S + (t(Hyt) %*% (t(yt_mu) %*% yt_mu) %*% Hyt)
  }
  
  N_sqrt <- sqrt(N)
  diag(N_sqrt) <- 1/(diag(N_sqrt))
  R <- (N_sqrt %*% S %*% N_sqrt)
  
  # Calculate Mahalanobis distance
  site_sparse <- site_keep
  site_sparse$MD <- suppressWarnings(MDmiss(site_sparse[, (ncol(keys)+1):ncol(site_sparse)], center = mu, cov = R))
  cv<-qchisq(.95,df=ncol(site_sparse)-1)
  site_sparse$outlier_sp <- ifelse(site_sparse$MD>cv, 1, 0)
  
  # Predict present values - xt is the value to predict, yt are the other present values
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
  
  
  preds_df <- data.frame(preds)
  names(preds_df) <- paste0("E_", names(site_sparse)[(ncol(keys)+1):(ncol(site_sparse)-2)])
  site_all <- cbind(site_sparse, preds_df)
  
  # Take the difference between estimate and actual and normalize by dividing by sample variance
  deviation <- abs(site_all[, (ncol(site_sparse)+1):(ncol(site_all))] - site_all[, (ncol(keys)+1):(ncol(site_sparse)-2)])
  deviation <- mapply('/', deviation, diag(R))
  deviation <- data.frame(deviation)
  names(deviation) <- paste0("D_", names(site_sparse)[(ncol(keys)+1):(ncol(site_sparse)-2)])
  site_out_total <- cbind(site_all, deviation)
  return(site_out_total)
  
  
}

runRecAnalysisFacility <- function(dat=dat_disag_prepped$dat_facility_out,keys=keys_facility,scenario) {
  if (scenario == "facility") {
    facility_outputs <- runRecAnalysis(dat,keys)
    facility_outputs <- tryCatch({
      sortOutputs(facility_outputs, keys = keys, scenario_tmp = scenario)
    }, error = function(cond){
      message("No Outliers Found at Facility Level")
      message(cond)})
    return(facility_outputs)
  } else if (scenario == "type") {
    facility_typeWrapper(dat,keys,facility_strings,scenario_wrapper = scenario)
  } else if (scenario == "psnu") {
    psnuWrapper(dat,keys, scenario_wrapper = scenario)
  }
}

facility_typeWrapper <- function(dat,keys,facility_strings, scenario_wrapper) {
  site_split = list()
  site_tmp = dat
  for (k in 1:length(facility_strings)) {
    facility_sub = facility_strings[k]
    data_tmp = site_tmp[grepl(facility_sub, tolower(site_tmp$facility)),]
    site_tmp = site_tmp[!grepl(facility_sub, tolower(site_tmp$facility)),]
    site_split[[k]] = data_tmp
  }
  site_out <- list()
  for (j in 1:length(site_split)) {
    site_out[[j]] <- runRecAnalysis(site_split[[j]],keys)
  }
  # stack the outputs
  facility_type_outliers <- do.call(plyr::rbind.fill, site_out)
  
  facility_type_outliers <- tryCatch({
    sortOutputs(facility_type_outliers, keys = keys,scenario_tmp = scenario_wrapper)
  }, error = function(cond){
    message("No Outliers Found at Level of Facility Type")
    message(cond)})
  return(facility_type_outliers)
}

psnuWrapper <- function(dat,keys,scenario_wrapper) {
  psnu_to_keep <- dat %>% group_by(psnu) %>% summarize(count = n()) %>% filter(count > 20) %>% .$psnu %>% unique() %>% as.character()
  dat <- dat %>% filter(psnu %in% psnu_to_keep)
  dat$psnu <- factor(as.character(dat$psnu))
  site_split <- split(dat, dat$psnu)
  site_out <- list()
  for (j in 1:length(site_split)) {
    
    site_out[[j]] <- runRecAnalysis(site_split[[j]],keys)
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

createSummaryTab <- function(disag = TRUE){
  
  if(disag == TRUE){
    cols_to_keep <- c("psnu", "facility", "primepartner", "ageasentered",  "sex",
                      "kp", "scenario", "Indicator", "outlier_sp") 
    dat <- lapply(disags_list, function(x) x[, cols_to_keep])
  } else {
    cols_to_keep <- c("psnu", "facility", "primepartner", "scenario", "Indicator", "outlier_sp") 
    dat <- lapply(facility_list, function(x) x[, cols_to_keep])
  }
  
  
  dat <- rbindlist(dat)
  dat <- dat %>% filter(outlier_sp == 1) %>% select(-outlier_sp)
  
  dat_for_scorecard <- dat[, c("psnu", "facility", "primepartner", "Indicator")]
  
  dat$outlier <- 1
  
  dat_summary <- dat %>%
    select(-Indicator) %>%
    pivot_wider(names_from = "scenario",
                values_from = "outlier") %>%
    as.data.frame()
  
  dat_summary[is.na(dat_summary)] <- 0
  dat_summary$Outliers <- apply(dat_summary[, grepl("outlier", names(dat_summary))],
                                1,
                                function(x){sum(x, na.rm = T)})
  dat_summary <- dat_summary %>%
    group_by(Outliers, facility) %>%
    mutate(count = n()) %>%
    arrange(desc(Outliers), desc(count)) %>%
    select(-count) %>%
    as.data.frame()
  
  outlist <- list("summary" = dat_summary,
                  "scorecard" = dat_for_scorecard)
  
}

createScoreCard <- function(){
  
  dat_tmp <- list()
  if(exists("disags_summary")){
    dat_tmp[['disag']] <- disags_summary$scorecard
  }
  if(exists("facility_summary")){
    dat_tmp[['facility']] <- facility_summary$scorecard
  }
  
  scorecard <- rbindlist(dat_tmp)
  
  scorecard <- scorecard %>%
    filter(!is.na(Indicator)) %>%
    group_by(psnu,facility,primepartner, Indicator) %>%
    summarize(count = n()) %>%
    mutate(facility = as.character(facility),
           psnu = as.character(psnu),
           primepartner = as.character(primepartner)) %>%
    pivot_wider(., names_from = "Indicator", values_from = "count") %>%
    as.data.frame()
  scorecard[is.na(scorecard)] <- 0
  scorecard$Total <- rowSums(scorecard[, 4:ncol(scorecard)])
  indicator_sums <- c(rep(0, 3), colSums(scorecard[, 4:ncol(scorecard)]))
  scorecard <- rbind(scorecard, indicator_sums)
  scorecard[nrow(scorecard),1:3] <- "Total"
  return(scorecard)
  
}

createExcel <- function(file=file_out){
  
  write.xlsx(scorecard, file, sheetName="Scorecard", row.names = FALSE)
  if(exists("disags_summary")){
    write.xlsx(disags_summary$summary, file, sheetName="Summary_Disaggregates", row.names = FALSE, append = TRUE)
  }
  if(exists("facility_summary")){
    write.xlsx(facility_summary$summary, file, sheetName="Facility_Disaggregates", row.names = FALSE, append = TRUE)
  }
  
  if(exists("disags_summary")){
    for(i in 1:length(disags_list)){
      write.xlsx(disags_list[[i]], file, sheetName=paste0(names(disags_list)[[i]],"_Disaggregates"), row.names=FALSE, append = TRUE)
    }
  }
  
  if(exists("facility_summary")){
    for(i in 1:length(facility_list)){
      write.xlsx(facility_list[[i]], file, sheetName=paste0(names(facility_list)[[i]],"_Facility"), row.names=FALSE, append = TRUE)
    }
  }
  
}

formatCells <- function(sheet, name, disags, facilities, keys_disag, keys_facility){
  
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
  cs1 <- CellStyle(wb, fill=fo1)        # create cell style # 1
  fo2 <- Fill(foregroundColor="#FF8080")    # create fill object # 2
  cs2 <- CellStyle(wb, fill=fo2)        # create cell style # 2 
  
  # loop through columns
  for(j in 1:n_columns){
    
    cells <- getCells(rows, colIndex = j+n_keys)         # get original value cell indexes
    cells_deviation <- getCells(rows, colIndex = (n_keys + j + (2*n_columns))) # get deviation cell indexes
    
    # Get the values for each
    values <- lapply(cells, getCellValue)
    # values_estimates <- lapply(cells_estimates, getCellValue)
    values_deviation <- lapply(cells_deviation, getCellValue)
    
    # Setting five quantiles (number of buckets should align with number of color fill objects)
    quants <- quantile(as.numeric(values_deviation), c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE)
    
    # find cells meeting conditional criteria: deviations in top quintile
    # add these cells to a list
    highlightred1 <- NULL
    for (i in 1:length(values)) {
      name_val <- names(values)[i]
      x <- as.numeric(values_deviation[i])
      if (x > as.numeric(quants[9]) & !is.na(x)) {
        highlightred1 <- c(highlightred1, name_val)
      }    
    }
    
    # repeat for four other quintiles
    highlightred2 <- NULL
    for (i in 1:length(values)) {
      name_val <- names(values)[i]
      x <- as.numeric(values_deviation[i])
      if (x > as.numeric(quants[8]) & x <= as.numeric(quants[9]) & !is.na(x)) {
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
