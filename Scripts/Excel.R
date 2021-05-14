# Script to take R data frame and convert to color-coded Excel workbook

# Set parameters and load libraries -----------------------
# this is expanding memory limits (xlsx uses Java stuff and apparently this is needed)
options(java.parameters = "-Xmx8000m") 
library(xlsx)
library(dplyr)

# Set directory and load data --------------------------
setwd("~/Data.FI/SouthAfrica/AnomalyDetection")
ad_all <- read.csv('./anomaly_recommender_all.csv') %>% select(-X)
# subsetting data just to make it run faster
ad_all <- ad_all[1:1000, ]
ad_all <- ad_all %>% arrange(desc(MD_sp))

# Saving data frame as xlsx workbook -------------------------
sheetname <- "no_disag"
write.xlsx(ad_all, "test.xlsx", sheetName=sheetname, row.names = FALSE)
file <- "test.xlsx"

# Loading xlsx workbook ----------------------
wb <- loadWorkbook(file)              # load workbook

# Create color codes
# https://www.w3schools.com/colors/colors_picker.asp?colorhex=8B0000
fo1 <- Fill(foregroundColor="#FF0000")   # create fill object # 1
cs1 <- CellStyle(wb, fill=fo1)        # create cell style # 1
fo2 <- Fill(foregroundColor="#FF3333")    # create fill object # 2
cs2 <- CellStyle(wb, fill=fo2)        # create cell style # 2 
fo3 <- Fill(foregroundColor="#FF6666")    
cs3 <- CellStyle(wb, fill=fo3)
fo4 <- Fill(foregroundColor="#FF9999")   
cs4 <- CellStyle(wb, fill=fo4)
fo5 <- Fill(foregroundColor="#FFCCCC")    
cs5 <- CellStyle(wb, fill=fo5)

# XLSX sytax to get sheet from workbook and get index of rows to act on below
sheets <- getSheets(wb)               # get all sheets
sheet <- sheets[[sheetname]]          # get specific sheet
rows <- getRows(sheet, rowIndex=2:(nrow(ad_all)+1))     # 1st row is headers

# Create formatting function ----------------------
# Function that applies to column of original values and color codes based on quantiles of normalized deviations
formatCells <- function(data, columnIndex, offset_estimate, offset_deviation){

  cells <- getCells(data, colIndex = columnIndex)         # get original value cell indexes
  cells_estimates <- getCells(data, colIndex = (columnIndex + offset_estimate)) # get estimate indexes
  cells_deviation <- getCells(data, colIndex = (columnIndex + offset_deviation)) # get deviation cell indexes
  
  # Get the values for each
  values <- lapply(cells, getCellValue)
  values_estimates <- lapply(cells_estimates, getCellValue)
  values_deviation <- lapply(cells_deviation, getCellValue)
  
  # Setting five quantiles (number of buckets should align with number of color fill objects)
  quants <- quantile(as.numeric(values_deviation), c(.2, .4, .6, .8), na.rm = TRUE)
  
  # find cells meeting conditional criteria: deviations in top quintile
  # add these cells to a list
  highlightred5 <- NULL
  for (i in 1:length(values)) {
    name <- names(values)[i]
    x <- as.numeric(values_deviation[i])
    if (x > as.numeric(quants[4]) && !is.na(x)) {
      highlightred5 <- c(highlightred5, name)
    }    
  }
  
  # repeat for four other quintiles
  highlightred4 <- NULL
  for (i in 1:length(values)) {
    name <- names(values)[i]
    x <- as.numeric(values_deviation[i])
    if (x > as.numeric(quants[3]) & x <= as.numeric(quants[4]) && !is.na(x)) {
      highlightred4 <- c(highlightred4, name)
    }    
  }
  
  highlightred3 <- NULL
  for (i in 1:length(values)) {
    name <- names(values)[i]
    x <- as.numeric(values_deviation[i])
    if (x > as.numeric(quants[2]) & x <= as.numeric(quants[3]) && !is.na(x)) {
      highlightred3 <- c(highlightred3, name)
    }    
  }
  
  highlightred2 <- NULL
  for (i in 1:length(values)) {
    name <- names(values)[i]
    x <- as.numeric(values_deviation[i])
    if (x > as.numeric(quants[1]) & x <= as.numeric(quants[2]) && !is.na(x)) {
      highlightred2 <- c(highlightred2, name)
    }    
  }
  
  highlightred1 <- NULL
  for (i in 1:length(values)) {
    name <- names(values)[i]
    x <- as.numeric(values_deviation[i])
    if (x <= as.numeric(quants[1]) && !is.na(x)) {
      highlightred1 <- c(highlightred1, name)
    }    
  }
  
  
  # Go through each list and set the cell style to the associated color
  lapply(names(cells[highlightred5]),
         function(ii) setCellStyle(cells[[ii]], cs1))
  
  lapply(names(cells[highlightred4]),
         function(ii) setCellStyle(cells[[ii]], cs2))
  
  lapply(names(cells[highlightred3]),
         function(ii) setCellStyle(cells[[ii]], cs3))
  
  lapply(names(cells[highlightred2]),
         function(ii) setCellStyle(cells[[ii]], cs4))
  
  lapply(names(cells[highlightred1]),
         function(ii) setCellStyle(cells[[ii]], cs5))
  
  # Append the estimate in parentheses to the original values so they appear in same cell
  for(i in 1:length(cells)){
    setCellValue(cells[[i]], paste0(values[[i]], " (", round(values_estimates[[i]], 1), ")"))
  }
  
  # Clean up to restore memory space
  gc()

}

# Apply function for each column that contains an indicator -----------------------------
for(j in 5:14){
  formatCells(data = rows, columnIndex = j, offset_estimate = 12, offset_deviation, 22)
}

# Shrink column width for columns that we don't want to export ----------------------
# TO DO: figure out how to delete these rather than shrink
setColumnWidth(sheet, 15:ncol(ad_all), 0)

# Save workbook -----------------------------------
saveWorkbook(wb, file)
