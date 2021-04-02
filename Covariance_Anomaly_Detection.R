library(magrittr)
library(dplyr)
library(reshape2)
library(caret)
library(modi)
library(tidyverse)
library(expm)

# Read in data
setwd("~/Data.FI/SouthAfrica")
site_spread <- read.csv('./Data/historic_mer.csv', stringsAsFactors = FALSE)

# All ---------------------------------------------------------
# drop observations where most variables are missing and variables where most observations are missing
obs_count <- apply(site_spread[, 5:ncol(site_spread)], 1, function(x) length(which(!is.na(x))))
count_present <- apply(site_spread[, 5:ncol(site_spread)], 2, function(x) length(which(!is.na(x))))
cols_to_keep <- which(count_present > (nrow(site_spread)*.1))+4 # keep variables present at least 10% of the time
obs_to_keep <- which(obs_count > 4) # keep observations with at least four present values
site_keep <- cbind(site_spread[obs_to_keep, 1:4], site_spread[obs_to_keep, cols_to_keep])

# get sparse mu (vector of means)
sum_sparse <- colSums(site_keep[, 5:ncol(site_keep)], na.rm = T) # sum of present values by variable
count_present_keep <- count_present[cols_to_keep-4] # count of present values by variable
mu <- sum_sparse / count_present_keep # means
k <- length(mu) # number of variables

N <- matrix(0, k, k) # set up k by k matrix
diag(N) <- count_present_keep # diagonal initiated with count of present values

i_mat <- matrix(0, k, k) # set up identity matrix
diag(i_mat) <- 1

S <- matrix(0, k, k)

for (i in 1:nrow(site_keep)){
  dat <- site_keep[i, ]
  inds <- which(!is.na(dat[5:ncol(site_keep)]))
  yt <- dat[5:ncol(site_keep)][inds]
  yt_mu <- as.matrix(yt - mu[inds])
  
  Hyt <- as.matrix(i_mat[inds, ])
  if(dim(Hyt)[2] == 1){Hyt <- t(Hyt)}
  
  S <- S + (t(Hyt) %*% (t(yt_mu) %*% yt_mu) %*% Hyt)
}

N_sqrt <- sqrt(N)
diag(N_sqrt) <- 1/(diag(N_sqrt))
R <- (N_sqrt %*% S %*% N_sqrt)


# Calculate Mahalanobis distance
site_sparse <- site_keep
site_sparse$MD_sp <- MDmiss(site_sparse[, 5:ncol(site_sparse)], center = mu, cov = R)
cv<-qchisq(.95,df=ncol(site_sparse)-1)
site_sparse$outlier_sp <- ifelse(site_sparse$MD_sp>cv, 1, 0)

# Predict present values - xt is the value to predict, yt are the other present values
# value will be Rxtyt %*% Ryt_inv %*% yt-uyt + uxt
preds <- matrix(data = NA, nrow = nrow(site_keep), ncol = k) # set up matrix to hold estimates

# Loop through each row
for (i in 1:nrow(site_keep)){
 
  # Get present values and index of present values
  dat <- site_keep[i, ]
  inds <- which(!is.na(dat[5:ncol(site_keep)]))
  
  # loop through index of present values
  for (j in inds){
    
    # Get Rxtyt - covariance of other present values with selected present values
    Rxtyt <- R[j, inds[!(inds %in% j)]]
    # Get Ryt_inv
    Ryt <- R[inds[!(inds %in% j)], inds[!(inds %in% j)]]
    Ryt_inv <- matlib::inv(Ryt)
    #Get yt-uyt
    yt <- dat[5:ncol(site_keep)][inds[!(inds %in% j)]]
    yt_mu <- as.matrix(yt - mu[inds[!(inds %in% j)]])
    # uxt
    uxt <- mu[j]
    
    # Get estimated value
    preds[i,j] <- Rxtyt %*% Ryt_inv %*% t(yt_mu) + uxt
    
  }
   
  
}
preds_df <- data.frame(preds)
names(preds_df) <- paste0("E_", names(site_sparse)[5:14])
site_all <- cbind(site_sparse, preds_df)

# Take the difference between estimate and actual and normalize by dividing by sample variance
deviation <- abs(site_all[, 17:26] - site_all[, 5:14])
deviation <- mapply('/', deviation, diag(R))
deviation <- data.frame(deviation)
names(deviation) <- paste0("D_", names(site_sparse)[5:14])
site_out <- cbind(site_all, deviation)

write_xlsx(site_out, './anomaly_recommender.xlsx')

# Let's split the data by sex and gender and run on each sub-group ------------------------
site_split <- split(site_spread, paste(site_spread$AgeGroup, site_spread$Sex))
site_sexage <- lapply(site_split, function(x){
  output <- tryCatch({
  # drop observations where most variables are missing and variables where most observations
  obs_count <- apply(x[, 5:ncol(x)], 1, function(y) length(which(!is.na(y))))
  count_present <- apply(x[, 5:ncol(x)], 2, function(y) length(which(!is.na(y))))
  cols_to_keep <- which(count_present > (nrow(x)*.1))+4
  obs_to_keep <- which(obs_count > 4)
  site_keep <- cbind(x[obs_to_keep, 1:4], x[obs_to_keep, cols_to_keep])
  # if(ncol(site_keep) > 5){

    # get sparse mu
    sum_sparse <- colSums(site_keep[, 5:ncol(site_keep)], na.rm = T)
    count_present_keep <- count_present[cols_to_keep-4]
    mu <- sum_sparse / count_present_keep
    k <- length(mu)
    
    N <- matrix(0, k, k)
    diag(N) <- count_present_keep
    
    i_mat <- matrix(0, k, k)
    diag(i_mat) <- 1
    
    S <- matrix(0, k, k)
    
    for (i in 1:nrow(site_keep)){
      dat <- x[i, ]
      inds <- which(!is.na(dat[5:ncol(site_keep)]))
      yt <- dat[5:ncol(site_keep)][inds]
      yt_mu <- as.matrix(yt - mu[inds])
      
      Hyt <- as.matrix(i_mat[inds, ])
      if(dim(Hyt)[2] == 1){Hyt <- t(Hyt)}
      
      S <- S + (t(Hyt) %*% (t(yt_mu) %*% yt_mu) %*% Hyt)
    }
    
    N_sqrt <- sqrt(N)
    diag(N_sqrt) <- 1/(diag(N_sqrt))
    R <- (N_sqrt %*% S %*% N_sqrt)
    
    
    # Try modi package
    site_sexage_out <- site_keep
    site_sexage_out$MD_sp <- MDmiss(site_sexage_out[, 5:ncol(site_sexage_out)], center = mu, cov = R)
    cv<-qchisq(.95,df=ncol(site_sexage_out)-1)
    site_sexage_out$outlier_sexage <- ifelse(site_sexage_out$MD_sp>cv, 1, 0)
    # site_sexage_out
    
    preds_sexage <- matrix(data = NA, nrow = nrow(site_keep), ncol = k)
    
    for (i in 1:nrow(site_keep)){
      
      dat <- site_keep[i, ]
      inds <- which(!is.na(dat[5:ncol(site_keep)]))
      # yt <- dat[5:ncol(site_keep)][inds]
      
      # loop through inds
      for (j in inds){
        
        # Get Rxtyt
        Rxtyt <- R[j, inds[!(inds %in% j)]]
        # Get Ryt_inv
        Ryt <- R[inds[!(inds %in% j)], inds[!(inds %in% j)]]
        Ryt_inv <- matlib::inv(Ryt)
        #Get yt-uyt
        yt <- dat[5:ncol(site_keep)][inds[!(inds %in% j)]]
        yt_mu <- as.matrix(yt - mu[inds[!(inds %in% j)]])
        # uxt
        uxt <- mu[j]
        
        # Get estimated value
        preds_sexage[i,j] <- Rxtyt %*% Ryt_inv %*% t(yt_mu) + uxt
        
      }
      
      
    }
    preds_df_sexage <- data.frame(preds_sexage)
    names(preds_df_sexage) <- paste0("E_", names(site_keep)[5:ncol(site_keep)])
    site_all_sexage <- cbind(site_sexage_out, preds_df_sexage)
    
    # Take the difference between estimate and actual and normalize by dividing by sample variance
    deviation <- abs(site_all_sexage[, (ncol(site_keep)+3):ncol(site_all_sexage)] - site_all_sexage[, 5:ncol(site_keep)])
    deviation <- mapply('/', deviation, diag(R))
    deviation <- data.frame(deviation)
    names(deviation) <- paste0("D_", names(site_keep)[5:ncol(site_keep)])
    site_out_sexage <- cbind(site_all_sexage, deviation)
    site_out_sexage
    }, error = function(cond){
    message("Insufficient Data or Singular Matrix")
    message(cond)})
  output
  
})

# stack the outputs
site_sexage_outliers <- do.call(plyr::rbind.fill, site_sexage)
site_sexage_outliers <- site_sexage_outliers %>%
  select("Facility", "AnalysisDate", "AgeGroup", "Sex", "TX_CURR", "TX_NEW", "TX_PVLS_D", "TX_PVLS_N", "TX_ML",
         "TX_RTT", "PMTCT_ART", "HTS_TST", "TB_STAT_D", "TB_ART", "TB_STAT_N", "MD_sp", "outlier_sexage",
         "E_TX_CURR", "E_TX_NEW", "E_TX_PVLS_D", "E_TX_PVLS_N", "E_TX_ML",
         "E_TX_RTT", "E_PMTCT_ART", "E_HTS_TST", "E_TB_STAT_D", "E_TB_ART", "E_TB_STAT_N",
         "D_TX_CURR", "D_TX_NEW", "D_TX_PVLS_D", "D_TX_PVLS_N", "D_TX_ML",
         "D_TX_RTT", "D_PMTCT_ART", "D_HTS_TST", "D_TB_STAT_D", "D_TB_ART", "D_TB_STAT_N")
write_xlsx(site_sexage_outliers, './anomaly_recommender_sexage.xlsx')

# Let's group by time period -----------------------------------
site_split <- split(site_spread, site_spread$AnalysisDate)
site_date <- lapply(site_split, function(x){
  
  # drop observations where most variables are missing and variables where most observations
  obs_count <- apply(x[, 5:ncol(x)], 1, function(y) length(which(!is.na(y))))
  count_present <- apply(x[, 5:ncol(x)], 2, function(y) length(which(!is.na(y))))
  cols_to_keep <- which(count_present > (nrow(x)*.1))+4
  obs_to_keep <- which(obs_count > 4)
  site_keep <- cbind(x[obs_to_keep, 1:4], x[obs_to_keep, cols_to_keep])
  
  # get sparse mu
  sum_sparse <- colSums(site_keep[, 5:ncol(site_keep)], na.rm = T)
  count_present_keep <- count_present[cols_to_keep-4]
  mu <- sum_sparse / count_present_keep
  k <- length(mu)
  
  N <- matrix(0, k, k)
  diag(N) <- count_present_keep
  
  i_mat <- matrix(0, k, k)
  diag(i_mat) <- 1
  
  S <- matrix(0, k, k)
  
  for (i in 1:nrow(site_keep)){
    dat <- x[i, ]
    inds <- which(!is.na(dat[5:ncol(site_keep)]))
    yt <- dat[5:ncol(site_keep)][inds]
    yt_mu <- as.matrix(yt - mu[inds])
    
    Hyt <- as.matrix(i_mat[inds, ])
    if(dim(Hyt)[2] == 1){Hyt <- t(Hyt)}
    
    S <- S + (t(Hyt) %*% (t(yt_mu) %*% yt_mu) %*% Hyt)
  }
  
  N_sqrt <- sqrt(N)
  diag(N_sqrt) <- 1/(diag(N_sqrt))
  R <- (N_sqrt %*% S %*% N_sqrt)
  
  
  # Try modi package
  site_date_out <- site_keep
  site_date_out$MD_sp <- MDmiss(site_date_out[, 5:ncol(site_date_out)], center = mu, cov = R)
  cv<-qchisq(.95,df=ncol(site_date_out)-1)
  site_date_out$outlier_date <- ifelse(site_date_out$MD_sp>cv, 1, 0)
  
  preds_date <- matrix(data = NA, nrow = nrow(site_keep), ncol = k)
  
  for (i in 1:nrow(site_keep)){
    
    dat <- site_keep[i, ]
    inds <- which(!is.na(dat[5:ncol(site_keep)]))
    # yt <- dat[5:ncol(site_keep)][inds]
    
    # loop through inds
    for (j in inds){
      
      # Get Rxtyt
      Rxtyt <- R[j, inds[!(inds %in% j)]]
      # Get Ryt_inv
      Ryt <- R[inds[!(inds %in% j)], inds[!(inds %in% j)]]
      Ryt_inv <- matlib::inv(Ryt)
      #Get yt-uyt
      yt <- dat[5:ncol(site_keep)][inds[!(inds %in% j)]]
      yt_mu <- as.matrix(yt - mu[inds[!(inds %in% j)]])
      # uxt
      uxt <- mu[j]
      
      # Get estimated value
      preds_date[i,j] <- Rxtyt %*% Ryt_inv %*% t(yt_mu) + uxt
      
    }
    
    
  }
  
  preds_df_date <- data.frame(preds_date)
  names(preds_df_date) <- paste0("E_", names(site_keep)[5:ncol(site_keep)])
  site_all_date <- cbind(site_date_out, preds_df_date)
  
  # Take the difference between estimate and actual and normalize by dividing by sample variance
  deviation <- abs(site_all_date[, (ncol(site_keep)+3):ncol(site_all_date)] - site_all_date[, 5:ncol(site_keep)])
  deviation <- mapply('/', deviation, diag(R))
  deviation <- data.frame(deviation)
  names(deviation) <- paste0("D_", names(site_keep)[5:ncol(site_keep)])
  site_out_date <- cbind(site_all_date, deviation)
  site_out_date
  
})

# stack the outputs
site_date_outliers <- do.call(plyr::rbind.fill, site_date)
site_date_outliers <- site_date_outliers %>%
  select("Facility", "AnalysisDate", "AgeGroup", "Sex", "TX_CURR", "TX_NEW", "TX_PVLS_D", "TX_PVLS_N", "TX_ML",
         "TX_RTT", "PMTCT_ART", "HTS_TST", "TB_STAT_D", "TB_ART", "TB_STAT_N", "MD_sp", "outlier_date",
         "E_TX_CURR", "E_TX_NEW", "E_TX_PVLS_D", "E_TX_PVLS_N", "E_TX_ML",
         "E_TX_RTT", "E_PMTCT_ART", "E_HTS_TST", "E_TB_STAT_D", "E_TB_ART", "E_TB_STAT_N",
         "D_TX_CURR", "D_TX_NEW", "D_TX_PVLS_D", "D_TX_PVLS_N", "D_TX_ML",
         "D_TX_RTT", "D_PMTCT_ART", "D_HTS_TST", "D_TB_STAT_D", "D_TB_ART", "D_TB_STAT_N")
write_xlsx(site_date_outliers, './anomaly_recommender_date.xlsx')

# clinic vs hospital ------------------------------------------
site_spread$facility_type <- ifelse(grepl("Hospital", site_spread$Facility), "Hospital", "Clinic")
site_split <- split(site_spread, site_spread$facility_type)
site_facility <- lapply(site_split, function(x){
  x <- x[, 1:(ncol(x)-1)] # drop facility type variable
  # drop observations where most variables are missing and variables where most observations
  obs_count <- apply(x[, 5:ncol(x)], 1, function(y) length(which(!is.na(y))))
  count_present <- apply(x[, 5:ncol(x)], 2, function(y) length(which(!is.na(y))))
  cols_to_keep <- which(count_present > (nrow(x)*.1))+4
  obs_to_keep <- which(obs_count > 4)
  site_keep <- cbind(x[obs_to_keep, 1:4], x[obs_to_keep, cols_to_keep])
  
  # get sparse mu
  sum_sparse <- colSums(site_keep[, 5:ncol(site_keep)], na.rm = T)
  count_present_keep <- count_present[cols_to_keep-4]
  mu <- sum_sparse / count_present_keep
  k <- length(mu)
  
  N <- matrix(0, k, k)
  diag(N) <- count_present_keep
  
  i_mat <- matrix(0, k, k)
  diag(i_mat) <- 1
  
  S <- matrix(0, k, k)
  
  for (i in 1:nrow(site_keep)){
    dat <- x[i, ]
    inds <- which(!is.na(dat[5:ncol(site_keep)]))
    yt <- dat[5:ncol(site_keep)][inds]
    yt_mu <- as.matrix(yt - mu[inds])
    
    Hyt <- as.matrix(i_mat[inds, ])
    if(dim(Hyt)[2] == 1){Hyt <- t(Hyt)}
    
    S <- S + (t(Hyt) %*% (t(yt_mu) %*% yt_mu) %*% Hyt)
  }
  
  N_sqrt <- sqrt(N)
  diag(N_sqrt) <- 1/(diag(N_sqrt))
  R <- (N_sqrt %*% S %*% N_sqrt)
  
  
  # Try modi package
  site_facility_out <- site_keep
  site_facility_out$MD_sp <- MDmiss(site_facility_out[, 5:ncol(site_facility_out)], center = mu, cov = R)
  cv<-qchisq(.95,df=ncol(site_facility_out)-1)
  site_facility_out$outlier_facility <- ifelse(site_facility_out$MD_sp>cv, 1, 0)
  
  preds_facility <- matrix(data = NA, nrow = nrow(site_keep), ncol = k)
  
  for (i in 1:nrow(site_keep)){
    
    dat <- site_keep[i, ]
    inds <- which(!is.na(dat[5:ncol(site_keep)]))
    # yt <- dat[5:ncol(site_keep)][inds]
    
    # loop through inds
    for (j in inds){
      
      # Get Rxtyt
      Rxtyt <- R[j, inds[!(inds %in% j)]]
      # Get Ryt_inv
      Ryt <- R[inds[!(inds %in% j)], inds[!(inds %in% j)]]
      Ryt_inv <- matlib::inv(Ryt)
      #Get yt-uyt
      yt <- dat[5:ncol(site_keep)][inds[!(inds %in% j)]]
      yt_mu <- as.matrix(yt - mu[inds[!(inds %in% j)]])
      # uxt
      uxt <- mu[j]
      
      # Get estimated value
      preds_facility[i,j] <- Rxtyt %*% Ryt_inv %*% t(yt_mu) + uxt
      
    }
    
    
  }
  
  preds_df_facility <- data.frame(preds_facility)
  names(preds_df_facility) <- paste0("E_", names(site_keep)[5:ncol(site_keep)])
  site_all_facility <- cbind(site_facility_out, preds_df_facility)
  
  # Take the difference between estimate and actual and normalize by dividing by sample variance
  deviation <- abs(site_all_facility[, (ncol(site_keep)+3):ncol(site_all_facility)] - site_all_facility[, 5:ncol(site_keep)])
  deviation <- mapply('/', deviation, diag(R))
  deviation <- data.frame(deviation)
  names(deviation) <- paste0("D_", names(site_keep)[5:ncol(site_keep)])
  site_out_facility <- cbind(site_all_facility, deviation)
  site_out_facility
  
})

# stack the outputs
site_facility_outliers <- do.call(plyr::rbind.fill, site_facility)
site_facility_outliers <- site_facility_outliers %>%
  select("Facility", "AnalysisDate", "AgeGroup", "Sex", "TX_CURR", "TX_NEW", "TX_PVLS_D", "TX_PVLS_N", "TX_ML",
         "TX_RTT", "PMTCT_ART", "HTS_TST", "TB_STAT_D", "TB_ART", "TB_STAT_N", "MD_sp", "outlier_facility",
         "E_TX_CURR", "E_TX_NEW", "E_TX_PVLS_D", "E_TX_PVLS_N", "E_TX_ML",
         "E_TX_RTT", "E_PMTCT_ART", "E_HTS_TST", "E_TB_STAT_D", "E_TB_ART", "E_TB_STAT_N",
         "D_TX_CURR", "D_TX_NEW", "D_TX_PVLS_D", "D_TX_PVLS_N", "D_TX_ML",
         "D_TX_RTT", "D_PMTCT_ART", "D_HTS_TST", "D_TB_STAT_D", "D_TB_ART", "D_TB_STAT_N")
write_xlsx(site_facility_outliers, './anomaly_recommender_facility.xlsx')

# site_all <- merge(site_sparse, site_sexage_outliers[, c("Facility", "AnalysisDate", "AgeGroup", "Sex", "outlier_sexage")],
#                   by = c("Facility", "AnalysisDate", "AgeGroup", "Sex"), all.x = TRUE) %>%
#   merge(., site_date_outliers[, c("Facility", "AnalysisDate", "AgeGroup", "Sex", "outlier_date")],
#         by = c("Facility", "AnalysisDate", "AgeGroup", "Sex"), all.x = TRUE) %>%
#   merge(., site_facility_outliers[, c("Facility", "AnalysisDate", "AgeGroup", "Sex", "outlier_facility")],
#         by = c("Facility", "AnalysisDate", "AgeGroup", "Sex"), all.x = TRUE)
# site_all$sum_outlier <- apply(site_all[, 16:19], 1, function(x){sum(x, na.rm = TRUE)})
# saveRDS(site_all, './historic_mer_outliers.rds')
