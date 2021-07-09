
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


# read in raw data
file_path <- file.choose()
mer_data <- read.delim(file_path)

# user defined variables
OU <- "Nigeria"
year_for_analysis <- 2021
qtr_for_analysis <- "qtr2"
all <- TRUE
age <- TRUE
sex <- TRUE
facility <- TRUE
type <- TRUE
psnu <- TRUE
facility_strings <- c("Maternity", "Hospital", "Clinic", "Centre")
MIN_THRESH <- 10

# create lists for the scenarios to run
scenarios_to_run_disag <- list("all" = all,"age" = age, "sex" = sex)
scenarios_to_run_facility <- list("facility" = facility,"type" = type, "psnu" = psnu)

# source the utils.R script which contains the functions
source('utils.R')

# run datPrep on mer_data - this outputs both the disag level file and the facility level file
dat_disag_prepped <- datPrep(mer_data, year_for_analysis, qtr_for_analysis)

# create the keys variable for the disag level
keys_disag <- c('facility','ageasentered', 'sex', 'kp', 'psnu', 'primepartner')

# run runRecAnalysisDisag on dat_disag_prepped[[1]] which is the disag level file
disags_list = list()
for (i in 1:length(scenarios_to_run_disag)) {
  if (scenarios_to_run_disag[[i]] == TRUE) {
    print(names(scenarios_to_run_disag)[[i]])
    disags_list[[names(scenarios_to_run_disag)[[i]]]] <- runRecAnalysisDisag(dat_disag_prepped$dat_disag_out,keys_disag,names(scenarios_to_run_disag)[[i]])
  
  }
}

# create the keys variable for the facility level
keys_facility <- c('facility', 'psnu','primepartner')

# run runRecAnalysisFacility on dat_disag_prepped[[2]] which is the facility level file
facility_list = list()
for (i in 1:length(scenarios_to_run_facility)) {
  if (scenarios_to_run_facility[[i]] == TRUE) {
    name_tmp <- names(scenarios_to_run_facility)[[i]]
    facility_list[[name_tmp]] <- runRecAnalysisFacility(dat_disag_prepped$dat_facility_out,keys_facility,name_tmp)
    
  }
}

saveRDS(disags_list, "disags_out_9jul2021.rds")
saveRDS(facility_list, "facility_out_9jul2021.rds")

# disags_list <- readRDS('./disags_out_9jul2021.rds')
# facility_list <- readRDS('./facility_out_9jul2021.rds')

disags_summary <- createSummaryTab(disags_list)
facility_summary <- createSummaryTab(facility_list, disag = FALSE)

scorecard <- createScoreCard(disags_summary$scorecard, facility_summary$scorecard)

# Saving data frame as xlsx workbook -------------------------
file <- paste0(OU, "-", Sys.Date(), ".xlsx")
write.xlsx(scorecard, file, sheetName="Scorecard", row.names = FALSE)
write.xlsx(disags_summary$summary, file, sheetName="Summary_Disaggregates", row.names = FALSE, append = TRUE)
write.xlsx(facility_summary$summary, file, sheetName="Facility_Disaggregates", row.names = FALSE, append = TRUE)

for(i in 1:length(disags_list)){
  write.xlsx(disags_list[[i]], file, sheetName=paste0(names(disags_list)[[i]],"_Disaggregates"), row.names=FALSE, append = TRUE)
}

for(i in 1:length(facility_list)){
  write.xlsx(facility_list[[i]], file, sheetName=paste0(names(facility_list)[[i]],"_Facility"), row.names=FALSE, append = TRUE)
}

wb <- loadWorkbook(file)  
sheets <- getSheets(wb)               # get all sheets

# https://www.w3schools.com/colors/colors_picker.asp?colorhex=8B0000
fo1 <- Fill(foregroundColor="#FF0000")   # create fill object # 1
cs1 <- CellStyle(wb, fill=fo1)        # create cell style # 1
fo2 <- Fill(foregroundColor="#FF8080")    # create fill object # 2
cs2 <- CellStyle(wb, fill=fo2)        # create cell style # 2 

for(i in 4:length(sheets)){
  formatCells(sheet = sheets[[i]],
              name = names(sheets)[[i]],
              disags = disags_list,
              facilities = facility_list,
              keys_disag = keys_disag,
              keys_facility = keys_facility)
}

# Save workbook -----------------------------------
saveWorkbook(wb, file)
