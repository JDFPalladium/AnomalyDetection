setwd("~/Kenya/Forecasting")
library(forecast) # arima and stl models
library(vars) # VAR model
library(dplyr) # Essentially enables writing SQL in R
library(stringr) # For string processing

# Read in facility-level aggregate data
hts_1617 <- read.csv('./HTSOld2016_2017.csv', stringsAsFactors = FALSE) %>%
  rename('periodcode' = Periodcode) %>% # rename so that matches variable name from later years
  select(County, periodcode, Positive, Tested) # keep only variables needed later on

# We have two datasets covering 2018/2019 with some overlap - will merge and rationalize below
hts_1819_old <- read.csv('./HTSOld2018_2019.csv', stringsAsFactors = FALSE) %>%
  rename('periodcode' = Periodcode) %>%
  select(County, periodcode, Positive, Tested, FacilityName) # will merge on facilityname so keep as well
# Remove non-letters from facility name to facilitate merge 
hts_1819_old$FacilityName <- str_replace_all(hts_1819_old$FacilityName, regex("\\W+"), " ")

hts_1819_new <- read.csv('./HTS2018_2019.csv', stringsAsFactors = FALSE) %>%
  rename('FacilityName' = Facility.Name) %>%
  select(County, periodcode, Positive, Tested, FacilityName)
hts_1819_new$FacilityName <- str_replace_all(hts_1819_new$FacilityName, regex("\\W+"), " ")

hts_2021 <- read.csv('./HTS2020_2021.csv', stringsAsFactors = FALSE) %>%
  rename('FacilityName' = Facility.Name) %>%
  select(County, periodcode, Positive, Tested)


# 6,831 observations appear in both datasets, but the values are the same, so taking values from "new" dataset
hts_1819 <- merge(hts_1819_old, hts_1819_new, by = c("FacilityName", "periodcode"), all = TRUE) # outer join
hts_1819 <- hts_1819 %>%
  mutate(Positive = ifelse(!is.na(Positive.y), Positive.y, Positive.x),
         Tested = ifelse(!is.na(Tested.y), Tested.y, Tested.x),
         County = ifelse(!is.na(County.y), County.y, County.x)) %>%
  select(County, periodcode, Positive, Tested)


hts_all <- rbind(hts_1617, hts_1819, hts_2021) # vertically stack all years
hts_all$Month <- as.numeric(substr(hts_all$periodcode, 5, 6))
hts_all$Year <- substr(hts_all$periodcode, 1, 4)
hts_all$County <- gsub(" ", "", hts_all$County) # get rid of white space in county names
hts_all$County <- gsub("County", "", hts_all$County) # one dataset had "county" in the name - drop these

# Now aggregate at county level by year and month
hts_sum <- hts_all %>%
  group_by(County, Year, Month) %>%
  summarize(mon_pos = sum(Positive, na.rm = T),
            mon_test = sum(Tested, na.rm = T))

# Create Shell - will merge on shell to ensure we're not missing any months in our series
shell <- expand.grid(Year = 2016:2020,
                     Month = 1:12,
                     County = unique(hts_all$County),
                     stringsAsFactors = FALSE)

shell <- merge(shell, hts_sum, by = c("County", "Year", "Month"), all.x = TRUE) # left join on shell
shell[is.na(shell)] <- 0 # if no values for any month, then comes through as NA after join - set to zero

# Get vector of country names to loop through
counties <- unique(shell$County)
# Set up list to hold outputs
forecasts_out <- list()

# Loop through counties
for(i in counties){

  print(i)
  # Filter data for county and sort by year and month in chronological order
  dat <- shell %>% filter(County == i) %>% arrange(Year, Month)
  
  period_train <- nrow(dat) * .8
  period_validation <- nrow(dat) * .2
  
  # dat_ts <- apply(dat[, 4:5], 2, function(x) ts(x, start = c(2016, 1), frequency = 12))
  
  # Arima model
  training <- ts(dat[1:period_train, 'mon_pos'], start = c(2016, 1), frequency = 12)
  validation <- ts(dat[(period_train+1):nrow(dat), 'mon_pos'], start = c(2020, 1), frequency = 12)
  arima_optimal = auto.arima(training, approximation = FALSE, lambda = "auto")
  arima_forecast = forecast(arima_optimal, h = period_validation*2)
  arima_mape <- Metrics::mape(validation, arima_forecast$mean[1:period_validation])
  
  # STL Models
  stlf_naive_forecast <- stlf(training, method = "naive")
  stlf_naive_mape <- Metrics::mape(validation, stlf_naive_forecast$mean[1:period_validation])
  
  stlf_ets_forecast <- stlf(training, method = "ets")
  stlf_ets_mape <- Metrics::mape(validation, stlf_ets_forecast$mean[1:period_validation])
  
  stlf_arima_forecast <- stlf(training, method = "arima")
  stlf_arima_mape <- Metrics::mape(validation, stlf_arima_forecast$mean[1:period_validation])
  
  
  # VAR Model
  training <- ts(dat[1:period_train, 4:5], start = c(2016, 1), frequency = 12)
  validation <- ts(dat[(period_train+1):nrow(dat), 4], start = c(2020, 1), frequency = 12)
  var_optimal <- VAR(training, ic = "AIC")
  var_forecast <- predict(var_optimal, n.ahead=period_validation*2)$fcst$mon_pos
  var_mape <- Metrics::mape(validation, var_forecast[1:period_validation, 1])
  # Rename outputs so that they match models above (will be easier later if all dataframes share variable names)
  var_forecast <- data.frame(var_forecast) %>%
    rename('Hi.95' = upper,
           'Lo.95' = lower,
           'Point.Forecast' = fcst)
  
  # Add forecasts, actuals, and mape to out list
  forecasts_out[[i]] <- list('arima_forecast' = data.frame(arima_forecast), #convert to dataframe (from forecast object)
                             'arima_mape' = arima_mape,
                             'stlf_naive_forecast' = data.frame(stlf_naive_forecast),
                             'stlf_naive_mape' = stlf_naive_mape,
                             'stlf_ets_forecast' = data.frame(stlf_ets_forecast),
                             'stlf_ets_mape' = stlf_ets_mape,
                             'stlf_arima_forecast' = data.frame(stlf_arima_forecast),
                             'stlf_arima_mape' = stlf_arima_mape,
                             'var_forecast' = var_forecast,
                             'var_mape' = var_mape,
                             'num_pos' = dat$mon_pos,
                             'num_tests' = dat$mon_test)
}

# Save outputs
saveRDS(forecasts_out, './county_hts_forecasts.rds')

