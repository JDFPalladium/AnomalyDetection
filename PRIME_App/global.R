#Utils Packages#
library(dplyr)
library(tidyr)
library(modi)
library(reshape2)
library(openxlsx)
library(data.table)
library(forecast)
library(imputeTS)
library(zoo)
library(matlib)
library(futile.logger)

#App Packages#
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(rintrojs) #Tutorial package
library(htmltools)
library(tidyverse)
library(shinyalert) #Used in the data checks portion
library(shinycssloaders) #Used from the loading spinning symbols throughout
library(shinyBS)
library(writexl)
library(DT)
library(aws.s3)
library(pdaprules)
library(readr)
library(datimutils)
library(paws)

#Global Settings
source("utils.R")

# Specify which variables identify a unique observation
keys_disag <-
  c('facility', 'ageasentered', 'sex', 'kp', 'psnu', 'primepartner', 'fundingagency')
keys_facility <- c('facility', 'psnu', 'primepartner', 'fundingagency')
keysts <-
  c("psnu",
    "facility",
    "primepartner",
    "indicator",
    "lower99",
    "upper99",
    "outlier",
    "fundingagency")
quarterly_indicators <-
  c(
    "HTS_INDEX",
    "HTS_RECENT",
    "HTS_TST",
    "HTS_TST_NEG",
    "HTS_TST_POS",
    "PMTCT_ART",
    "PMTCT_EID",
    "PMTCT_HEI_POS",
    "PMTCT_HEI_POS_2MO",
    "PMTCT_HEI_POS_ART",
    "PMTCT_STAT",
    "PMTCT_STAT_POS",
    "PrEP_CURR",
    "PrEP_NEW",
    "TB_ART",
    "TB_STAT",
    "TB_STAT_POS",
    "TX_CURR",
    "TX_ML",
    "TX_NET_NEW",
    "TX_NEW",
    "TX_PVLS",
    "TX_RTT",
    "VMMC_CIRC"
  )
ASIA <- c("Burma", "Cambodia", "India", "Indonesia", "Kazakhstan",
          "Kyrgyzstan", "Laos", "Nepal", "Papua New Guinea",
          "Philippines", "Tajikistan", "Thailand")
WESTAFRICA <- c("Benin", "Burkina Faso", "Ghana", "Liberia",
                "Mali", "Senegal", "Sierra Leone", "Togo")
WESTERNHEMISPHERE <- c("Jamaica", "Trinidad and Tobago", "Guyana",
                 "Barbados", "Panama", "Guatemala",
                 "El Salvador", "Honduras", "Nicaragua",
                 "Brazil", "Colombia", "Peru")
COUNTRIES <- c("Malawi", "Botswana", "Eswatini", "Asia", "West Africa", "Western Hemisphere",
               "Zambia", "Zimbabwe", "Nigeria")
USG_USERS = c("Agency", "Interagency", "Global Agency", "Global")
PARTNER_USERS = c("Global Partner", "Partner")