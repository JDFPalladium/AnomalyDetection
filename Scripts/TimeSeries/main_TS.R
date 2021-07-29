# This script runs the Data.FI Anomaly Detection Time Series solution on PEPFAR MER data. 
# The accompanying script, utils_TS.R, loads all the functions needed to run the solution. Users 
# should save utils.R in a folder that is then set as the working directory, which is set in the 
# script below.
# 
# To run the solution, there are a few user-adjustable fields at the start of the script. Once
# the user is satisfied with the settings, it is recommended to save the script by clicking 
# on the disk icon above or pressing CTRL+S, and then pressing the Source button at the 
# top right of this window. 
# 
# Please follow the five steps below:

# Step One - set parameters -----------------------------------------------------------------
# Set the OU, and the most recent fiscal year and quarter for which there is data present
OU <- "Nigeria" # in quotes
year <- 2021 # NOT in quotes
qtr <- "qtr2" # in quotes

# Specify how many files you are uploading (can be any number)
num_files <- 2 # must be a number

# In determining which indicator drives the determination that an observation is anomalous,
# you can discount indicators with very low values. This will NOT affect whether an observation
# is determined to be anomalous, but WILL affect which indicator is responsible for the 
# determination on the SCORECARD tab of the Excel output.
MIN_THRESH <- 10

# If you want the Excel to include both anomalous and non-anomalous observations, set to TRUE. 
# If interested only in anomalous observations, set to FALSE. For OUs with many facilities, 
# setting to TRUE will materially impact the time needed to run the solution.
RETURN_ALL <- TRUE

# Step Two - choose folder that contains utils_TS.R (also where Excel file will be saved) -------
setwd(choose.dir())
if(!"utils_TS.R" %in% list.files()){
  print("utils_TS.R is not in the selected folder. Please select the folder that contains utils_TS.R")}
source("utils_TS.R")

# Step Three - load MER data in xlsx (MER data should be on first sheet), csv, or txt format -----
mer_data <- list()
for (i in 1:num_files) {
  file_path <- file.choose()
  if(sub('.*\\.', '', file_path) == "xlsx") {
    mer_data[[i]] <- read.xlsx(file_path, sheet = 1)
  } else if(sub('.*\\.', '', file_path) == "csv"){
    mer_data[[i]] <- read.csv(file_path, stringsAsFactors = FALSE)
  } else if(sub('.*\\.', '', file_path) == "txt"){
    mer_data[[i]] <- read.delim(file_path)
  } else {
    print("Please select a file with an xlsx, csv, or txt extension.")
  }
}


# Step Four - defining quarterly indicators ------------------------------------------

quarterly_indicators <- c("HTS_INDEX", "HTS_RECENT", "HTS_TST", "HTS_TST_NEG",
                          "HTS_TST_POS", "PMTCT_ART", "PMTCT_EID", "PMTCT_HEI_POS",
                          "PMTCT_HEI_POS_2MO", "PMTCT_HEI_POS_ART", "PMTCT_STAT",
                          "PMTCT_STAT_POS", "PrEP_CURR", "PrEP_NEW", "TB_ART", "TB_STAT",
                          "TB_STAT_POS", "TX_CURR", "TX_ML", "TX_NET_NEW", "TX_NEW", "TX_PVLS",
                          "TX_RTT", "VMMC_CIRC")

# Step Five - Run line of code below. Check console for updates and messages ------
# Runs the Time Series Solution. Will print progress messages throughout.
runTimeSeriesSolution()
