
#utils.R - contains all the functions
#main.R - contains source, user defined variables, and then will run utils.R

# load in necessary libraries
library(readr)
library(htmltools)
library(dplyr)
library(knitr)
library(tidyr)
library(modi)
library(htmltools)
library(magrittr)
library(reshape2)

# read in raw data
file_path <- file.choose()
mer_data <- read.delim(file_path)

# user defined variables
year_for_analysis <- 2021
qtr_for_analysis <- "qtr2"
all <- TRUE
age <- TRUE
sex <- TRUE
facility <- TRUE
facility_type <- TRUE
psnu <- TRUE
facility_strings <- c("Maternity", "Hospital", "Clinic", "Centre")

# create lists for the scenarios to run
scenarios_to_run_disag <- list("all" = all,"age" = age, "sex" = sex)
scenarios_to_run_facility <- list("facility" = facility,"facility_type" = facility_type, "psnu" = psnu)

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
    print(scenarios_to_run_disag[[i]])
    disags_list[[names(scenarios_to_run_disag)[[i]]]] <- runRecAnalysisDisag(dat_disag_prepped[[1]],keys_disag,names(scenarios_to_run_disag)[[i]])
  
  }
}

# create the keys variable for the facility level
keys_facility <- c('facility', 'psnu','primepartner')

# run runRecAnalysisFacility on dat_disag_prepped[[2]] which is the facility level file
facility_list = list()
for (i in 1:length(scenarios_to_run_facility)) {
  if (scenarios_to_run_facility[[i]] == TRUE) {
    print(names(scenarios_to_run_facility)[[i]])
    print(scenarios_to_run_facility[[i]])
    facility_list[[names(scenarios_to_run_facility)[[i]]]] <- runRecAnalysisFacility(dat_disag_prepped[[2]],keys_facility,names(scenarios_to_run_facility)[[i]])
    
  }
}

saveRDS(disags_list, "disags_out_8jul2021.rds")
saveRDS(facility_list, "facility_out_8jul2021.rds")
