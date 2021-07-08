setwd("~/Data.FI/AnomalyDetection/Data/Nigeria")
library(dplyr); library(tidyr); library(forecast); library(imputeTS); library(zoo); library(data.table)
library(xlsx)
# Read in indicator list
indicators <- read.csv('./Indicators.csv', stringsAsFactors = FALSE)
quarterly_indicators <- indicators %>% filter(Frequency == "Quaterly") %>% .$Indicator
semiannual_indicators <- indicators %>% filter(Frequency == "Semi-Annual") %>% .$Indicator


first <- read.delim("./MER_Structured_Datasets_Site_IM_FY19-21_20210514_v1_1_Nigeria.txt")

cols_to_keep <- c("facility", "indicator", "psnu", "frequency", "numeratordenom", "disaggregate",
                 "fiscal_year", "qtr1", "qtr2", "qtr3", "qtr4")
first <- first[, names(first) %in% cols_to_keep]

first$indicator <- as.character(first$indicator)
first <- first %>%
  mutate(frequency = ifelse(indicator %in% quarterly_indicators, "Quarterly",
                            ifelse(indicator %in% semiannual_indicators, "Semi-Annual", "Other")))
first$indicator <- paste0(first$indicator, "_", first$numeratordenom)

# remove the rows that report on Total Numerator or Total Denominator data
first <- first %>% filter(!disaggregate %in% c("Total Numerator", "Total Denominator"))

first <- first[, !names(first) %in% c("numeratordenom", "disaggregate")]

# Pivot longer to get all the values in one column
first_long <- pivot_longer(first, 
                           cols = names(first)[grepl("qtr", names(first))],
                           names_to = "qtr")

# Turn quarter into a number for sorting
first_long$qtr <- as.numeric(substr(first_long$qtr, nchar(first_long$qtr), nchar(first_long$qtr)))

# What are the indicators reported in the most recent quarter
indicators_to_keep <- first_long %>%
  filter(fiscal_year == max(first_long$fiscal_year)) %>%
  filter(qtr == 2) %>%
  filter(!is.na(value)) %>%
  .$indicator %>%
  unique()

facilities_to_keep <- first_long %>%
  filter(fiscal_year == max(first_long$fiscal_year)) %>%
  filter(qtr == 2) %>%
  filter(!is.na(value)) %>%
  .$facility %>%
  unique() %>%
  as.character()

# Only keep these indicators
first_long <- first_long[first_long$indicator %in% indicators_to_keep, ]
first_long <- first_long[as.character(first_long$facility) %in% facilities_to_keep, ]

# summarize by facility/indicator/fiscal_year/qtr
first_long <- first_long %>%
  group_by(psnu, facility, indicator, frequency, fiscal_year, qtr) %>%
  summarize(value = sum(value, na.rm = TRUE))

second <- read.delim("./MER_Structured_Datasets_Site_IM_FY15-18_20210514_v1_1_Nigeria.txt")

second <- second[, names(second) %in% cols_to_keep]

second$indicator <- as.character(second$indicator)
second <- second %>%
  mutate(frequency = ifelse(indicator %in% quarterly_indicators, "Quarterly",
                            ifelse(indicator %in% semiannual_indicators, "Semi-Annual", "Other")))
second$indicator <- paste0(second$indicator, "_", second$numeratordenom)
second <- second %>% filter(!disaggregate %in% c("Total Numerator", "Total Denominator"))
second <- second[as.character(second$facility) %in% facilities_to_keep, ]
second <- second[second$indicator %in% indicators_to_keep, ]

# Pivot longer to get all the values in one column
second_long <- pivot_longer(second, 
                           cols = names(first)[grepl("qtr", names(first))],
                           names_to = "qtr")

# Turn quarter into a number for sorting
second_long$qtr <- as.numeric(substr(second_long$qtr, nchar(second_long$qtr), nchar(second_long$qtr)))

# summarize by facility/indicator/fiscal_year/qtr
second_long <- second_long %>%
  group_by(psnu, facility, indicator, frequency, fiscal_year, qtr) %>%
  summarize(value = sum(value, na.rm = TRUE))

all <- rbind(first_long, second_long)
rm(first);rm(second);rm(first_long);rm(second_long);gc()

# drop last two quarters
all <- all %>%
  filter(!(fiscal_year == 2021 & qtr %in% c(3,4)))

all <- all %>%
  group_by(psnu, facility, indicator, frequency) %>%
  mutate(count = n()) %>%
  filter(count >= 12) %>%
  ungroup() %>%
  select(-count)

# convert facility to character string
all$facility <- as.character(all$facility)

# Quarterly Indicators ---------------------
quarterly <- filter(all, frequency == "Quarterly")
# split by facility and indicator
facility_split <- split(quarterly, quarterly$facility)

outlist_arima <- list()
outlist_ets <- list()
outlist_stl_arima <- list()

# for each facility
# for(i in 1:20){
for(i in 1:length(facility_split)){
# for(i in 1:20){
  print(i)
  dat <- facility_split[[i]]
  ind_split <- split(dat, dat$indicator)
  
  for(j in 1:length(ind_split)){
    
    dat_ind <- ind_split[[j]]

    
    shell <- expand.grid(fiscal_year = 2015:2021, qtr = 1:4) %>%
      filter(!(fiscal_year == 2021 & qtr %in% c(3,4)))
    shell <- merge(shell, dat_ind, by = c("fiscal_year", "qtr"), all.x = TRUE) %>%
      arrange(fiscal_year, qtr)
    
    # set up as time series
    dat_ts <- ts(shell[1:(nrow(shell)-1), 'value'], start = c(2015, 1), frequency = 4) %>%
      na.trim(sides = "left") %>%
      na_interpolation()
    
    # Fit STL model and forecast last present period
    stlf_arima_forecast <- stlf(dat_ts,
                                method = "arima",
                                level = c(99),
                                h = 1)

    upper99 <- stlf_arima_forecast$upper[1]
    lower99 <- stlf_arima_forecast$lower[1]
    pred <- stlf_arima_forecast$mean[1]
    
    shell$upper99 <- upper99
    shell$lower99 <- lower99
    actual <- tail(shell$value,1)

    if(!is.na(actual) & (actual < lower99 | actual > upper99) & (abs(actual - pred) > 10)){
      outlist_stl_arima[[length(outlist_stl_arima)+1]] <- shell
    }
    
    # Fit STL model and forecast last present period
    stlf_arima_forecast <- stlf(dat_ts,
                                method = "ets",
                                level = c(99),
                                h = 1)
    
    upper99 <- stlf_arima_forecast$upper[1]
    lower99 <- stlf_arima_forecast$lower[1]
    pred <- stlf_arima_forecast$mean[1]
    
    shell$upper99 <- upper99
    shell$lower99 <- lower99
    actual <- tail(shell$value,1)
    
    if(!is.na(actual) & (actual < lower99 | actual > upper99) & (abs(actual - pred) > 10)){
      outlist_ets[[length(outlist_ets)+1]] <- shell
    }
    
    arima_mod <- auto.arima(dat_ts,
                         seasonal=TRUE,
                         approximation = TRUE,
                         max.p = 2,
                         max.q = 2,
                         max.order = 3,
                         stepwise=TRUE)
    arima_mod_forecast <- forecast(arima_mod, h=1, level = c(99))
    upper99 <- arima_mod_forecast$upper[1]
    lower99 <- arima_mod_forecast$lower[1]
    pred <- stlf_arima_forecast$mean[1]
    
    shell$upper99 <- upper99
    shell$lower99 <- lower99
    actual <- tail(shell$value,1)
    
    if(!is.na(actual) & (actual < lower99 | actual > upper99) & (abs(actual - pred) > 10)){
      outlist_arima[[length(outlist_arima)+1]] <- shell
    }
    
  }
  
}

out_stl_arima <- rbindlist(outlist_stl_arima)
out_ets <- rbindlist(outlist_ets)
out_arima <- rbindlist(outlist_arima)

out_arima <- out_arima %>%
  arrange(desc(fiscal_year), desc(qtr)) %>%
  filter(!is.na(value))
out_arima_wide <- pivot_wider(out_arima,
                       id_cols = c("psnu", "facility", "indicator", "lower99", "upper99"),
                       names_from = c("fiscal_year", "qtr"),
                       values_from = c("value"))

out_ets <- out_ets %>%
  arrange(desc(fiscal_year), desc(qtr)) %>%
  filter(!is.na(value))
out_ets_wide <- pivot_wider(out_ets,
                              id_cols = c("psnu","facility", "indicator", "lower99", "upper99"),
                              names_from = c("fiscal_year", "qtr"),
                              values_from = c("value"))

out_stl_arima <- out_stl_arima %>%
  arrange(desc(fiscal_year), desc(qtr)) %>%
  filter(!is.na(value))
out_stl_arima_wide <- pivot_wider(out_stl_arima,
                              id_cols = c("psnu","facility", "indicator", "lower99", "upper99"),
                              names_from = c("fiscal_year", "qtr"),
                              values_from = c("value"))

# write.csv(out_arima_wide, './nigeria_arima_20210705.csv')
# write.csv(out_stl_arima_wide, './nigeria_stl_arima_20210705.csv')
# write.csv(out_ets_wide, './nigeria_ets_20210705.csv')
# 
# 
# out_arima_wide <- read.csv('./nigeria_arima_20210705.csv', check.names = FALSE) %>% select(-1) 
# out_stl_arima_wide <- read.csv('./nigeria_stl_arima_20210705.csv', check.names = FALSE) %>% select(-1)
# out_ets_wide <- read.csv('./nigeria_ets_20210705.csv', check.names = FALSE) %>% select(-1)

# Create cover sheet
summary <- merge(out_arima_wide[, c("psnu", "facility", "indicator", "upper99")],
               out_stl_arima_wide[, c("psnu","facility", "indicator", "upper99")],
               by = c("psnu","facility", "indicator"), all = TRUE) %>%
  rename("Outlier_Arima" = upper99.x,
         "Outlier_STL" = upper99.y) %>%
  merge(., out_ets_wide[, c("psnu","facility", "indicator", "upper99")],
        by = c("psnu","facility", "indicator"), all = TRUE) %>%
  rename("Outlier_ETS" = upper99) %>%
  mutate(Outlier_Arima = ifelse(is.na(Outlier_Arima), 0, 1),
         Outlier_STL = ifelse(is.na(Outlier_STL), 0, 1),
         Outlier_ETS = ifelse(is.na(Outlier_ETS), 0, 1))
summary$Outliers <- apply(summary[, c("Outlier_Arima", "Outlier_STL", "Outlier_ETS")],
                           1,
                           function(x){sum(x, na.rm = T)})
summary <- summary %>%
  arrange(desc(Outliers))

cover <- summary %>%
  group_by(facility, indicator) %>%
  summarize(Outliers = sum(Outliers, na.rm = TRUE)) %>% 
  mutate(facility = as.character(facility)) %>%
  pivot_wider(., id_cols = "facility", names_from = "indicator", values_from = "Outliers") %>%
  as.data.frame()
cover[is.na(cover)] <- 0
cover$Total <- rowSums(cover[, 2:ncol(cover)])
indicator_sums <- c(0, colSums(cover[, 2:ncol(cover)]))
cover <- rbind(cover, indicator_sums)
cover[nrow(cover),1] <- "Total"

# Let's calculate by how much the differ (difference / range of interval)
arima_wide <- out_arima_wide %>%
  mutate(gap = ifelse(`2021_2` > upper99, `2021_2` - upper99, lower99 - `2021_2`),
         deviation = gap / (upper99 - lower99)) %>%
  arrange(desc(deviation)) %>%
  mutate(`2021_2` = paste0(`2021_2`, " (", round(lower99, digits=1), " - ", round(upper99,digits=1), ")")) %>%
  select(-gap, -deviation, -upper99, -lower99) %>%
  as.data.frame()

stl_arima_wide <- out_stl_arima_wide %>%
  mutate(gap = ifelse(`2021_2` > upper99, `2021_2` - upper99, lower99 - `2021_2`),
         deviation = gap / (upper99 - lower99)) %>%
  arrange(desc(deviation)) %>%
  mutate(`2021_2` = paste0(`2021_2`, " (", round(lower99, digits=1), " - ", round(upper99,digits=1), ")")) %>%
  select(-gap, -deviation, -upper99, -lower99) %>%
  as.data.frame()

ets_wide <- out_ets_wide %>%
  mutate(gap = ifelse(`2021_2` > upper99, `2021_2` - upper99, lower99 - `2021_2`),
         deviation = gap / (upper99 - lower99)) %>%
  arrange(desc(deviation)) %>%
  mutate(`2021_2` = paste0(`2021_2`, " (", round(lower99, digits=1), " - ", round(upper99,digits=1), ")")) %>%
  select(-gap, -deviation, -upper99, -lower99) %>%
  as.data.frame()

write.xlsx(cover, "nigeria_ts_07062021.xlsx", sheetName="Scorecard", row.names = FALSE)
write.xlsx(summary, "nigeria_ts_07062021.xlsx", sheetName="Summary", row.names = FALSE, append = TRUE)
write.xlsx(arima_wide, "nigeria_ts_07062021.xlsx", sheetName="ARIMA", row.names = FALSE, append = TRUE)
write.xlsx(stl_arima_wide, "nigeria_ts_07062021.xlsx", sheetName="STL", row.names = FALSE, append = TRUE)
write.xlsx(ets_wide, "nigeria_ts_07062021.xlsx", sheetName="ETS", row.names = FALSE, append = TRUE)

