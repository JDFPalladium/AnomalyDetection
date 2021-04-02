library(magrittr)
library(dplyr)
library(reshape2)
library(caret)
library(spcov)
library(matlib)
library(rrecsys)
library(modi)
library(tidyverse)
library(coindeskr)
library(anomalize)
library(zoo)

#### South Africa MER ####
setwd("~/Data.FI/SouthAfrica")
mer <- read.csv('./MER_20200928.csv', stringsAsFactors = FALSE)

#### Data Prep ####

# Read in Site-level Data
filepath <- "C:/Users/jonathan.friedman/OneDrive - Palladium International, LLC/Documents/PEPFAR Data/"
site <- read.csv(paste0(filepath, '7_Site Performance.csv'), stringsAsFactors = FALSE)

# select numeric columns of interest
cols_to_keep <- c(2,3,4,5,7:25)
site <- site[, cols_to_keep]

# filter out entries where site is N/A
site <- site %>% filter(Site != "N/A")

# set "" to N/A
site[site == ""] <- NA

# convert counts to numeric values
for (i in 5:ncol(site)){
  site[, i] <- as.numeric(gsub(",", "", site[, i]))
}

#### Forecast-based Methods ####
site_sa <- site %>% 
  filter(Operating.Unit == 'South Africa') %>%
  rename('Q12016' = X2016Q1.R,
         'Q22016' = X2016Q2.R,
         'Q32016' = X2016Q3.R,
         'Q42016' = X2016Q4.R,
         'Q12017' = X2017Q1.R,
         'Q22017' = X2017Q2.R,
         'Q32017' = X2017Q3.R,
         'Q42017' = X2017Q4.R,
         'Q12018' = X2018Q1.R,
         'Q22018' = X2018Q2.R,
         'Q32018' = X2018Q3.R,
         'Q42018' = X2018Q4.R) %>%
  dplyr::select(Site, Indicator, Q12016, Q22016, Q32016, Q42016, Q12017, Q22017, Q32017, Q42017, Q12018, Q22018, Q32018, Q42018) 

# For now, only use complete cases - no imputation of missing values - only a few variables exist in each quarter anywhere
site_complete <- site_sa[complete.cases(site_sa), ]

# Get in long format to get date variable
site_melted <- melt(site_complete, id.vars = c('Site', 'Indicator'), measure.vars = 3:ncol(site_complete))
site_melted$date <- paste0(substr(as.character(site_melted$variable), 1,2), '/', substr(as.character(site_melted$variable),3, 6))
site_melted$date <- as.Date(as.yearqtr(site_melted$date, format = "Q%q/%Y"))

anomalies <- site_melted %>%
  filter(Site == 'Facility 070e79' & Indicator == 'TX_NEW') %>%
  as_tibble() %>%
  # group_by(Site, Indicator) %>%
  time_decompose(value, method = 'stl', frequency = 'auto', trend = 'auto') %>%
  anomalize(remainder) %>%
  dplyr::ungroup()

anomalies %>% #filter(Site == 'Facility 070e79' & Indicator == 'TX_NEW') %>% dplyr::ungroup() %>%
  # plot_anomalies()
   plot_anomaly_decomposition(strip.position = "top")



#### Point in Time ####

# Filter to a particular country and period: let's do South Africa in 2018Q4
site_sa <- site %>% filter(Operating.Unit == 'South Africa') %>% rename(value = X2018Q4.R) %>% dplyr::select(Site, Indicator, value) 

# Six site/Indicator combinations are repeated (for now, drop one of the observations)
site_dedup <- site_sa %>%
  group_by(Site, Indicator) %>%
  dplyr::mutate(rownum = row_number()) %>%
  dplyr::filter(rownum == 1) %>%
  dplyr::select(-rownum)

# reshape data into m x n (site by indicator) format
site_spread <- site_dedup %>% spread(Indicator, value)

# drop observations where most variables are missing and variables where most observations
obs_count <- apply(site_spread[, -1], 1, function(x) length(which(!is.na(x))))
count_present <- apply(site_spread[, 2:ncol(site_spread)], 2, function(x) length(which(!is.na(x))))
cols_to_keep <- which(count_present > 100)
obs_to_keep <- which(obs_count > 4)
site_keep_raw <- cbind('site' = site_spread$Site[obs_to_keep], site_spread[obs_to_keep, cols_to_keep + 1])


# Convert skewed variables to Gaussian - all except for TX_NET_NEW
cols_to_log_transform <- names(site_keep_raw)
cols_to_log_transform <- cols_to_log_transform[!(cols_to_log_transform %in% c('site', 'TX_NET_NEW'))]
cols_to_log_transform <- which(names(site_keep_raw) %in% cols_to_log_transform)
cols_transformed <- apply(site_keep_raw[, cols_to_log_transform], 2, function(x) log(x+1)) # adding 1 to handle zero values
site_keep <- cbind('site' = as.character(site_keep_raw$site), 'TX_NET_NEW' = site_keep_raw$TX_NET_NEW, as.data.frame(cols_transformed))

#### Non-Sparse Gaussian Anomaly Detection ####

# fill in missing values using recsys
site_m <- as.matrix(site_keep[,-1])
site_m[is.na(site_m)] <- 0
site_prep <- defineData(site_m, maximum = 1000000)
# r <- rrecsys(site_prep, alg = "BPR")
r <- rrecsys(site_prep, alg = "IBKNN")
site_full <- predict(r)
site_full[is.na(site_full)] <- 0

# Get mu and sigma
mu <- colMeans(site_full)
sigma <- cov(site_full, site_full)

site_full <- data.frame(site_full)
site_full$MD <- mahalanobis(site_full, mu, sigma)
cv<-qchisq(.999,df=ncol(site_full)-1)
site_full$outlier <- ifelse(site_full$MD>cv, 1, 0)

#### Sparse Anomaly Detection ####

# get sparse mu
sum_sparse <- colSums(site_keep[, -1], na.rm = T)
count_present_keep <- count_present[cols_to_keep]
mu <- sum_sparse / count_present_keep
k <- length(mu)

N <- matrix(0, k, k)
diag(N) <- count_present_keep

i_mat <- matrix(0, k, k)
diag(i_mat) <- 1

S <- matrix(0, k, k)

for (i in 1:nrow(site_keep)){
    dat <- site_keep[i, ]
    inds <- which(!is.na(dat[2:ncol(site_keep)]))
    yt <- dat[2:ncol(site_keep)][inds]
    yt_mu <- as.matrix(yt - mu[inds])
    
    Hyt <- as.matrix(i_mat[inds, ])
    if(dim(Hyt)[2] == 1){Hyt <- t(Hyt)}
    
    S <- S + (t(Hyt) %*% (t(yt_mu) %*% yt_mu) %*% Hyt)
}

N_sqrt <- sqrt(N)
diag(N_sqrt) <- 1/(diag(N_sqrt))
R <- (N_sqrt %*% S %*% N_sqrt)


# Try modi package
site_sparse <- site_keep
site_sparse$MD_sp <- MDmiss(site_sparse[, -1], center = mu, cov = R)
cv<-qchisq(.9,df=ncol(site_sparse)-1)
site_sparse$outlier_sp <- ifelse(site_sparse$MD_sp>cv, 1, 0)

site_all <- cbind(site_keep_raw, 'MD' = site_full$MD, 'outlier' = site_full$outlier, 'MD_sp' = site_sparse$MD_sp, 'outlier_sp' = site_sparse$outlier_sp)

img_prep <- site_all %>% mutate(color = ifelse(outlier_sp == 1, "red", "black"))
with(img_prep, plot(TX_NET_NEW, TX_CURR, main = "TX_CURR and TX_NET_NEW \n South Africa PEPFAR Facilities 2018Q4", col = color))

#### KNN ####

data(mtcars)
data <- as.matrix(mtcars)
outl <- do_knno(data, 3, 2)
outl
data[outl,]

#### Clustering  K-means or GMM ####


#### SVM ####



# # Now, get probability of each observation
# preds <- list()
# for (i in 1:nrow(site_keep)){
#     dat <- site_keep[i, ]
#     inds <- which(!is.na(dat[2:ncol(site_keep)]))
#     kt <- length(inds)
#     yt <- dat[2:ncol(site_keep)][inds]
#     yt_mu <- as.matrix(yt - mu[inds])
# 
#     mah <- yt_mu %*% inv(R[inds, inds]) %*% t(yt_mu)
# 
#     # prob <- c(prob, exp(-(1/2)*mah) / sqrt(((2*pi)^kt)*det(R[inds, inds])))
# }

#### Time-Based ####
