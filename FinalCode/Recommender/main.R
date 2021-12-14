# This script runs the Data.FI Anomaly Detection Recommender System solution on PEPFAR MER data. 
# The accompanying script, utils.R, loads all the functions needed to run the solution. Users 
# should save utils.R in a folder that is then set as the working directory, which is set in the 
# script below.
# 
# To run the solution, there are a few user-adjustable fields at the start of the script. Once
# the user is satisfied with the settings, it is recommended to save the script by clicking 
# on the disk icon above or pressing CTRL+S, and then pressing the Source button at the 
# top right of this window. 
# 
# Please follow the four steps below:

# Step One - set parameters -----------------------------------------------------------------
# Set the OU, fiscal year, and quarter for which to run the analysis
OU <- "Example" # in quotes
year <- 2020 # NOT in quotes
qtr <- "qtr1" # in quotes

# Select the analyses to run with MER data disaggregated by sex and age
# Set to TRUE if you want to run, FALSE if you do not
all <- TRUE # each observation compared against all observations
sex <- FALSE # each observation compared against all observations of the same sex
age <- FALSE # each observation compared against all observations of the same age group
age_groups <- "Five Year" # Either "Five Year" or "Over/Under 15"
age_under15 <- c("01-04", "05-09", "10-14") # Age groups under 15
age_over15 <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+") # Age groups over 15

# Select the analyses to run with MER data aggregated at the facility
# Set to TRUE if you want to run, FALSE if you do not
facility <- TRUE # each observation compared against all observations
psnu <- FALSE # each observation compared against all observations of the same psnu
type <- FALSE # each observation compared against all observations of the same facility type
# if running by facility type, please specify the words found in facility names that indicate
# the type of facility. examples may include maternity, hospital, clinic, and centre.
facility_strings <- c("hospital", "clinic", "maternity", "centre") # lower case  

# In determining which indicator drives the determination that an observation is anomalous,
# you can discount indicators with very low values. This will NOT affect whether an observation
# is determined to be anomalous, but WILL affect which indicator is responsible for the 
# determination on the SCORECARD tab of the Excel output.
MIN_THRESH <- 10

# If you want the Excel to include both anomalous and non-anomalous observations, set to TRUE. 
# If interested only in anomalous observations, set to FALSE. For OUs with many facilities, 
# setting to TRUE will materially impact the time needed to run the solution.
RETURN_ALL <- TRUE

# Step Two - choose folder that contains Utils.R (also where Excel file will be saved) -------
# setwd(choose.dir())
# if(!"utils.R" %in% list.files()){
#   print("utils.R is not in the selected folder. Please select the folder that contains utils.R")}
source("Recommender/utils.R")

# Step Three - load MER data in xlsx (MER data should be on firt sheet), csv, or txt format -----
file_path <- file.choose()
if(sub('.*\\.', '', file_path) == "xlsx") {
  mer_data <- read.xlsx(file_path, sheet = 1)
} else if(sub('.*\\.', '', file_path) == "csv"){
  mer_data <- read.csv(file_path, stringsAsFactors = FALSE)
} else if(sub('.*\\.', '', file_path) == "txt"){
  mer_data <- read.delim(file_path)
} else {
  print("Please select a file with an xlsx, csv, or txt extension.")
}

# Step Four - Run line of code below. Check console for updates and messages ------
# Runs the Recommender Solution. Will print progress messages throughout.
runRecommenderSolution()
