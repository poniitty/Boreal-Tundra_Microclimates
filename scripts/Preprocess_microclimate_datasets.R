####################################################################
# This code is used to filter and preprocess the microclimate 
# datasets used in the further analyses

library(tidyverse)
library(lubridate)
library(data.table)
library(zoo)

# Raw data filtering
# This file is several GB big and thus cannot be deposited in Github
d <- fread("C:/datacloud/biogeoclimate/microclimate/data/logger/all_data.csv") %>% 
  filter(!(area == "PIS" & site >= 100)) %>% # Esclude PISA sites in forest management areas
  filter(!(area %in% c("RAR","SAA","RAS"))) # Exclude Saana, Pekka's Kilpisjärvi sites and Rastigaisa
  
# Change the datetime to Finnish time
d %>% mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) -> d

# Filter wanted period
d %>% mutate(date = as_date(datetime)) %>% 
  filter(date >= "2019-11-01" & date < "2020-11-01") %>% 
  select(-date) -> d

# Change bad data to NA
d %>% mutate(T1 = ifelse(error_tomst %in% c(1,2,4), NA, T1),
             T2 = ifelse(error_tomst %in% c(1,2), NA, T2),
             T3 = ifelse(error_tomst > 0, NA, T3),
             T4 = ifelse(error_T4 > 0, NA, T4),
             moist = ifelse(error_tomst %in% c(1,2), NA, moist),
             moist = ifelse(T1 < 1, NA, moist)) %>% 
  select(-moist_count, -arh) -> d

# Thin HOBO measuring interval to match with used with HAXOs (2 hours)

d %>% mutate(hour = hour(datetime),
             mins = minute(datetime)) %>% 
  mutate(T4 = ifelse((hour %% 2 == 0), NA, T4),
         T4 = ifelse(mins != 0, NA, T4)) %>% 
  select(-hour,-mins) -> d

# Just to check the data
d %>% filter(id_code == "AIL101") %>% tail(20)

# write filtered data
fwrite(d %>% select(-starts_with("error_")), "C:/datacloud/biogeoclimate/microclimate/data/logger/data_article/all_data.csv")


##############################################################################
# DAILY DATA

# Add date column
d %>% mutate(date = as_date(datetime)) %>% 
  relocate(date, .after = datetime) -> d

d %>% mutate(hour = hour(datetime),
             mins = minute(datetime)) %>% 
  filter((hour %% 2 != 0) & (mins == 0)) %>% 
  select(-hour,-mins) -> T4


# Fill missing observations (max length of three consecutive NAs) with running average
# Linear interpolation
d %>% group_by(id_code) %>% 
  mutate(across(T1:moist, ~na.approx(.x, datetime, maxgap = 3, na.rm = F))) -> d

T4 %>% group_by(id_code) %>% 
  mutate(across(T4, ~na.approx(.x, datetime, maxgap = 1, na.rm = F))) -> T4

########################################################################
# AGGREGATE TO DAILY VALUES

notNA_prop <- function(x){ round(sum(is.finite(x)/length(x))*100,1) }

d %>% select(-T4) %>% 
  group_by(id_code, area, site, date) %>%
  summarise(across(T1:moist, ~notNA_prop(.x), 
                   na.rm = F, .names = "{.col}_prop"),
            across(T1:moist, list(mean = mean, min = min, max = max), 
                   na.rm = T, .names = "{.col}_{.fn}"),
            error_tomst = max(error_tomst, na.rm = T)) %>% 
  ungroup() -> daily

# Change Inf and -Inf to NA
infmutate <- function(x) ifelse(is.infinite(x),NA,x)
daily %>% mutate(across(T1_prop:error_tomst, infmutate)) -> daily

# T4 data
# Function to summarize column types
what_logger <- function(x){
  xx <- na.omit(unique(x))
  if(length(xx) > 1){
    return("b")
  } else {
    return(xx[1])
  }
}

T4 %>% group_by(id_code, area, site, date) %>%
  summarise(across(T4, ~notNA_prop(.x), 
                   na.rm = F, .names = "{.col}_prop"),
            across(T4, list(mean = mean, min = min, max = max), 
                   na.rm = T, .names = "{.col}_{.fn}"),
            error_T4 = max(error_T4, na.rm = T),
            logger_T4 = what_logger(logger_T4)) %>% 
  ungroup() -> dailyT4
dailyT4 %>% mutate(across(T4_prop:error_T4, infmutate)) -> dailyT4

# combine again
daily <- full_join(daily, dailyT4) %>% 
  arrange(id_code, date)

daily %>% mutate(across(T1_mean:T1_max, ~ifelse(T1_prop < 0.95 | error_tomst %in% c(1,2,4), NA, .x)),
                 across(T2_mean:T2_max, ~ifelse(T2_prop < 0.95 | error_tomst %in% c(1,2), NA, .x)),
                 across(T3_mean:T3_max, ~ifelse(T3_prop < 0.95 | error_tomst > 0, NA, .x)),
                 across(T4_mean:T4_max, ~ifelse(T4_prop < 0.95 | error_T4 > 0, NA, .x))) %>% 
  select(-ends_with("_prop"), -starts_with("error")) %>% 
  relocate(T4_mean:T4_max, .after = T3_max) %>% 
  relocate(logger_T4, .after = date) -> daily


# Write daily data
fwrite(daily, "C:/datacloud/biogeoclimate/microclimate/data/logger/data_article/all_data_daily.csv")
fwrite(daily, "data/all_data_daily.csv")

########################################################################
# MONTHLY DATA

# Create a matrix of number of days in each calender month
daycount <- data.frame(date = c(as_date(as_date("2018-01-01"):as_date("2021-12-31")))) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(year, month) %>% 
  summarise(ndaysmax = n())

# T1
daily %>% 
  select(id_code, area, site, date, starts_with("T1")) %>% 
  filter(is.finite(T1_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(id_code, area, site, year, month) %>% 
  summarise(ndays = n(),
            T1_mean = round(mean(T1_mean, na.rm = T),2),
            T1_absmax = round(max(T1_max, na.rm = T),2),
            T1_absmin = round(min(T1_min, na.rm = T),2),
            T1_meanmax = round(mean(T1_max, na.rm = T),2),
            T1_meanmin = round(mean(T1_min, na.rm = T),2)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_T1 = ndays/ndaysmax) %>% 
  relocate(day_frac_T1, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_T1

# T2
daily %>% 
  select(id_code, area, site, date, starts_with("T2")) %>% 
  filter(is.finite(T2_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(id_code, area, site, year, month) %>% 
  summarise(ndays = n(),
            T2_mean = round(mean(T2_mean, na.rm = T),2),
            T2_absmax = round(max(T2_max, na.rm = T),2),
            T2_absmin = round(min(T2_min, na.rm = T),2),
            T2_meanmax = round(mean(T2_max, na.rm = T),2),
            T2_meanmin = round(mean(T2_min, na.rm = T),2)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_T2 = ndays/ndaysmax) %>% 
  relocate(day_frac_T2, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_T2

# T3
daily %>% 
  select(id_code, area, site, date, starts_with("T3")) %>% 
  filter(is.finite(T3_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(id_code, area, site, year, month) %>% 
  summarise(ndays = n(),
            T3_mean = round(mean(T3_mean, na.rm = T),2),
            T3_absmax = round(max(T3_max, na.rm = T),2),
            T3_absmin = round(min(T3_min, na.rm = T),2),
            T3_meanmax = round(mean(T3_max, na.rm = T),2),
            T3_meanmin = round(mean(T3_min, na.rm = T),2)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_T3 = ndays/ndaysmax) %>% 
  relocate(day_frac_T3, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_T3

# T4
daily %>% 
  select(id_code, area, site, date, logger_T4, starts_with("T4")) %>% 
  filter(is.finite(T4_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(id_code, area, site, year, month) %>% 
  summarise(logger_T4 = what_logger(logger_T4),
            ndays = n(),
            T4_mean = round(mean(T4_mean, na.rm = T),2),
            T4_absmax = round(max(T4_max, na.rm = T),2),
            T4_absmin = round(min(T4_min, na.rm = T),2),
            T4_meanmax = round(mean(T4_max, na.rm = T),2),
            T4_meanmin = round(mean(T4_min, na.rm = T),2)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_T4 = ndays/ndaysmax) %>% 
  relocate(day_frac_T4, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_T4

# moist
daily %>% 
  select(id_code, area, site, date, starts_with("moist")) %>% 
  filter(is.finite(moist_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(id_code, area, site, year, month) %>% 
  summarise(ndays = n(),
            moist_sd = round(sd(moist_mean, na.rm = T),2),
            moist_cv = round(sd(moist_mean, na.rm = T)/mean(moist_mean, na.rm = T),2),
            moist_med = round(median(moist_mean, na.rm = T),1),
            moist_mean = round(mean(moist_mean, na.rm = T),1),
            moist_absmax = round(max(moist_max, na.rm = T),1),
            moist_absmin = round(min(moist_min, na.rm = T),1)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_moist = ndays/ndaysmax) %>% 
  relocate(day_frac_moist, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_moist

# Quantiles from hourly data 

d %>% mutate(month = month(date)) %>% 
  group_by(id_code, month) %>% 
  summarise(across(T1:T4, ~quantile(.x, c(0.25, 0.5, 0.75), na.rm = T), q = c(0.25, 0.5, 0.75)))

d %>% mutate(month = month(date)) %>% 
  group_by(id_code, month) %>% 
  summarise(T1 = quantile(T1, c(0.99, 0.98, 0.97, 0.96, 0.95), na.rm = T),
            T2 = quantile(T2, c(0.99, 0.98, 0.97, 0.96, 0.95), na.rm = T),
            T3 = quantile(T3, c(0.99, 0.98, 0.97, 0.96, 0.95), na.rm = T),
            T4 = quantile(T4, c(0.99, 0.98, 0.97, 0.96, 0.95), na.rm = T),
            q = c(99,98,97,96,95)) %>% 
  pivot_wider(id_cols = id_code:month, names_from = q, values_from = T1:T4) -> dm_q



full_join(dm_T1, dm_T2) %>% 
  full_join(., dm_T3) %>% 
  full_join(., dm_T4) %>% 
  full_join(., dm_moist) %>% 
  full_join(., dm_q) %>% 
  mutate(across(starts_with("day_f"), ~ifelse(!is.finite(.x), 0, .x))) %>% 
  relocate(logger_T4, .after = month) %>% 
  mutate(across(T1_mean:T4_95, ~ifelse(!is.finite(.x), NA, .x))) %>% 
  relocate(starts_with("day_f"), .after = month) -> dm
dm %>% as.data.table()

dm %>% mutate(across(starts_with("T1_"), ~ifelse(day_frac_T1 < 0.95, NA, .x)),
              across(starts_with("T2_"), ~ifelse(day_frac_T2 < 0.95, NA, .x)),
              across(starts_with("T3_"), ~ifelse(day_frac_T3 < 0.95, NA, .x)),
              across(starts_with("T4_"), ~ifelse(day_frac_T4 < 0.95, NA, .x))) %>% 
  arrange(id_code, year, month) -> dm

dm %>% rename(T1_prop = day_frac_T1,
              T2_prop = day_frac_T2,
              T3_prop = day_frac_T3,
              T4_prop = day_frac_T4,
              moist_prop = day_frac_moist) -> dm

fwrite(dm, "C:/datacloud/biogeoclimate/microclimate/data/logger/data_article/all_data_monthly.csv")
fwrite(dm, "data/all_data_monthly.csv")
