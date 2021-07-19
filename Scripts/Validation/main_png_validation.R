
#utils.R - contains all the functions
#main.R - contains source, user defined variables, and then will run utils.R

# load in necessary libraries
options(java.parameters = "-Xmx16000m") 
library(readr)
library(htmltools)
library(dplyr)
library(knitr)
library(tidyr)
library(modi)
library(htmltools)
library(magrittr)
library(reshape2)
library(xlsx)
library(data.table)
library(openxlsx)

source('utils.R')

# user defined variables
OU <- "PNG"
year <- 2020
qtr <- "qtr1"
all <- TRUE
age <- TRUE
age_groups <- "DQA" # Either "Five Year" or "Over/Under 15 or DQA
sex <- TRUE
facility <- FALSE
type <- FALSE
psnu <- FALSE
facility_strings <- c("maternity", "hospital", "clinic", "centre")
MIN_THRESH <- 10
RETURN_ALL <- TRUE

# read in raw data
file_path <- file.choose()
if(sub('.*\\.', '', file_path) == "xlsx") {
  mer_data <- read.xlsx(file_path, sheetIndex = 2)
} else if(sub('.*\\.', '', file_path) == "csv"){
  mer_data <- read.csv(file_path, stringsAsFactors = FALSE)
} else if(sub('.*\\.', '', file_path) == "txt"){
  mer_data <- read.delim(file_path)
} else {
  print("Please select a file with an xlsx, csv, or txt extension.")
}

# create lists for the scenarios to run
scenarios_to_run_disag <- which(list("all" = all,"age" = age, "sex" = sex)==TRUE)
scenarios_to_run_facility <- which(list("facility" = facility,"type" = type, "psnu" = psnu)==TRUE)
# create the keys variable for the disag level
keys_disag <- c('facility','ageasentered', 'sex', 'kp', 'psnu', 'primepartner')
keys_facility <- c('facility', 'psnu','primepartner')


# run datPrep on mer_data - this outputs both the disag level file and the facility level file
dat_disag_prepped <- datPrep()

# run runRecAnalysisDisag
disags_list = list()
for (i in names(scenarios_to_run_disag)) {
  print(paste("Running analysis for dataset including disaggregates by:", i))
  disags_list[[i]] <- runRecAnalysisDisag(scenario=i)
}

# run runRecAnalysisFacility 
facility_list = list()
for (i in names(scenarios_to_run_facility)) {
  print(paste("Running analysis for dataset by:", i))
  facility_list[[i]] <- runRecAnalysisFacility(scenario=i)
}


if(length(disags_list) == 0 & length(facility_list) == 0){
  stop("No anomalies found. Ending script.")
}

if(length(disags_list) > 0){
  disags_summary <- createSummaryTab()
}
if(length(facility_list) > 0){
  facility_summary <- createSummaryTab(disag = FALSE)
}

scorecard <- createScoreCard()

# Saving data frame as xlsx workbook -------------------------

file_out <- paste0(OU, "-", Sys.Date(), ".xlsx")

# code I added to write the runs to excel
write.xlsx(disags_list[["all"]], file = "PNG-2021-07-15_all.xlsx")
write.xlsx(disags_list[["age"]], file = "PNG-2021-07-15_age.xlsx")
write.xlsx(disags_list[["sex"]], file = "PNG-2021-07-15_sex.xlsx")

createExcel()

wb <- loadWorkbook(file_out)  
sheets <- getSheets(wb) 
sheets_to_run <- which(!grepl("Scorecard|Summary",names(sheets)))

for(i in sheets_to_run){
  formatCells(sheet = sheets[[i]],
              name = names(sheets)[[i]],
              disags = disags_list,
              facilities = facility_list,
              keys_disag = keys_disag,
              keys_facility = keys_facility)
}

# Save workbook -----------------------------------
saveWorkbook(wb, file_out)
