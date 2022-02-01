library(tidyverse)
library(lme4)
library(zoo)
library(broom)
library(vip)
library(pdp)
library(lubridate)
 
# The study areas modelled
areas_to_model <- c("MAL","AIL","VAR","TII","PIS","HYY","KAR")
# Predictors in the models
predictors <- c("altitude", "pisr", "wetland_prop_100m", "snow_days", "tpi100",
                "water_prop_500m", "canopy_cover2m_10m", "moist_med")

# Read data
d <- read_csv("data/all_data_monthly.csv") %>% 
  select(-ends_with("_prop"))
e <- read_csv("data/all_env_variables.csv") %>% 
  filter(logger == "Tomst") %>% 
  filter(area %in% areas_to_model) %>% 
  filter(!(area == "PIS" & site >= 100))
snow <- read_csv("data/snow_vars_all.csv") %>% 
  filter(area %in% areas_to_model) %>% 
  filter(!(area == "PIS" & site >= 100))


# we decidec to use 95% quantile instead of abs max
d %>% select(-ends_with("absmax")) %>% 
  rename(T1_absmax = T1_95,
         T2_absmax = T2_95,
         T3_absmax = T3_95,
         T4_absmax = T4_95) %>% 
  relocate(ends_with("_absmax"), .after = logger_T4) %>% 
  select(-c(T1_99:T4_96)) -> d

# Preprocess snow variables
# Model imputation missing values

snow_var <- data.frame()
for(i in areas_to_model){
  # i <- "VAR"
  snow %>% filter(area == i) -> temp
  
  mod <- glmer(snow_days ~ factor(hydro_year) + (1 | id_code), data = temp, family = ifelse(i == "VAR", "gaussian","poisson")) # This exception was needed because of a singular fit
  
  df <- data.frame(hydro_year = "2020",
                   id_code = unique(e %>% filter(area == i) %>% pull(id_code)),
                   area = i)
  
  df$snow_days <- round(predict(mod, df, type = "response", allow.new.levels = T))
  
  snow_var <- bind_rows(snow_var, df)
}

snow_var %>% filter(area == "TII", snow_days < 135)

snow_var %>% mutate(snow_days = ifelse(id_code == "TII045", 179, snow_days), # "2019-10-28","2020-04-24"
                    snow_days = ifelse(id_code == "TII020", 189, snow_days), # "2019-10-28","2020-05-04"
                    snow_days = ifelse(id_code == "TII006", 177, snow_days)) -> snow_var # "2019-10-28","2020-04-22"

# Snowy periods per area
# Used to select snow as a predictor only when relevant
snow %>% mutate(first_snow_date = as_date(ifelse(id_code == "TII045" & hydro_year == 2020, as_date("2019-10-28"), first_snow_date)), # "2019-10-28","2020-04-24"
                first_snow_date = as_date(ifelse(id_code == "TII020" & hydro_year == 2020, as_date("2019-10-28"), first_snow_date)), # "2019-10-28","2020-05-04"
                first_snow_date = as_date(ifelse(id_code == "TII006" & hydro_year == 2020, as_date("2019-10-28"), first_snow_date))) %>% 
  mutate(last_snow_date = as_date(ifelse(id_code == "TII045" & hydro_year == 2020, as_date("2020-04-24"), last_snow_date)), # "2019-10-28","2020-04-24"
         last_snow_date = as_date(ifelse(id_code == "TII020" & hydro_year == 2020, as_date("2020-05-04"), last_snow_date)), # "2019-10-28","2020-05-04"
         last_snow_date = as_date(ifelse(id_code == "TII006" & hydro_year == 2020, as_date("2020-04-22"), last_snow_date))) -> snow # "2019-10-28","2020-04-22"

snow %>% filter(hydro_year == 2020) %>% 
  group_by(area) %>% 
  summarise(first_snow_date = min(first_snow_date, na.rm = T),
            last_snow_date = max(last_snow_date, na.rm = T)) -> snowy_dates
  

# Preprocess soil moisture
# Impute missing values based on previous values (when soil frozen) or model when that is not possible

# Soil moisture from another repository
sm <- read_csv("https://raw.githubusercontent.com/poniitty/Fennoscandia_microclimates/main/data/all_data_monthly.csv") %>% 
  select(id_code:month, starts_with("moist"))

sm %>% select(id_code:month, moist_prop, moist_med) %>% 
  mutate(moist_med = ifelse(moist_prop < 0.1, NA, moist_med)) %>% 
  arrange(id_code, year, month) %>% 
  group_by(id_code) %>% 
  mutate(moist_med = na.locf(moist_med, na.rm = F)) -> sm

moist_var <- data.frame()
for(i in areas_to_model){
  
  sm %>% filter(area == i) -> temp
  
  mod <- lmer(moist_med ~ factor(year) + factor(month) + (1 | id_code), data = temp)
  
  expand_grid(ym = paste0(c(rep(2019, times = 2),rep(2020, times = 10)),
                          "_", c(11:12,1:10)),
              id_code = unique(e %>% filter(area == i) %>% pull(id_code)),
              area = i) %>% 
    mutate(year = as.numeric(unlist(lapply(ym, function(x) { str_split(x, "_")[[1]][1]}))),
           month = as.numeric(unlist(lapply(ym, function(x) { str_split(x, "_")[[1]][2]})))) %>% 
    arrange(id_code, year, month) %>% 
    select(-ym) -> df
  
  df$moist_med <- round(predict(mod, df, type = "response", allow.new.levels = T),2)
  
  moist_var <- bind_rows(moist_var, df)
}


# Monthly radiation values to long format
extr_month <- function(x){ as.numeric(gsub("m","",unlist(lapply(x, function(xx) strsplit(xx,"_")[[1]][2]))))}

e %>% dplyr::select(id_code, (starts_with("pisr_m") & ends_with("_50m"))) %>% 
  pivot_longer(cols = starts_with("pisr_m"), names_to = "month", values_to = "pisr") %>% 
  mutate(month = extr_month(month)) -> pisr_var

# Select and preprocess other env data

e %>% 
  mutate(altitude = altitude/100) %>% 
  mutate(across(starts_with("pgaps"), ~abs(.x-100))) %>% 
  select(id_code, names(.)[names(.) %in% predictors]) -> e

#####################################################################################
# Modelling

coef_df <- tibble()
summ_df <- tibble()
vi_df <- tibble()
coef_df_step <- tibble()
summ_df_step <- tibble()
vi_df_step <- tibble()
for(i in areas_to_model){
  print(i) # i <- "TII"
  
  for(ii in 1:12){
    print(ii) # ii <- 7
    
    d %>% filter(area == i,
                 month == ii) -> temp
    
    # Extract response variables
    responses <- names(temp)[grepl("^T[0-9]", names(temp))]
    
    for(iii in responses){
      # iii <- "T4_absmin"
      # selection of only relevant predictors
      env_vars <- predictors
      if(ii %in% c(1:2,11:12)){
        env_vars <- env_vars[-which(env_vars == "pisr")]
      }
      if(i == "KAR"){
        env_vars <- env_vars[-which(grepl("snow", env_vars))]
      }
      if(i %in% c("KAR","HYY","TII")){
        env_vars <- env_vars[-which(grepl("altitude", env_vars))]
      }
      if(i %in% c("VAR")){
        env_vars <- env_vars[-which(grepl("water", env_vars))]
      }
      if(!(ii %in% month(seq(snowy_dates$first_snow_date[snowy_dates$area == i],
                           snowy_dates$last_snow_date[snowy_dates$area == i],
                           by = "month")))){
        if(any(grepl("snow", env_vars))){
          env_vars <- env_vars[-which(grepl("snow", env_vars))]
        }
      }
      
      model_formula_lm <- formula(paste0(iii, " ~ ", paste(c(env_vars), collapse = " + ")))
      
      temp %>% select(id_code, year, month, iii) %>% 
        left_join(pisr_var) %>% 
        left_join(moist_var) %>% 
        left_join(snow_var) %>% 
        left_join(e) %>% 
        filter(complete.cases(.)) -> mod_data
      
      mod <- lm(model_formula_lm, data = mod_data)
      
      coef_df <- bind_rows(coef_df,
                           tidy(mod) %>% 
                             mutate(area = i,
                                    month = ii,
                                    resp = iii))
      summ_df <- bind_rows(summ_df,
                           glance(mod) %>% 
                             mutate(area = i,
                                    month = ii,
                                    resp = iii))
      
      vi_df <- bind_rows(vi_df,
                         vi_model(mod) %>% 
                           mutate(method = "tstat",
                                  area = i,
                                  month = ii,
                                  resp = iii),
                         vi_permute(mod, target = iii, train = mod_data %>% select(iii, env_vars),
                                    metric = "rsquared", pred_wrapper = predict, nsim = 10) %>% 
                           mutate(method = "perm_r2",
                                  area = i,
                                  month = ii,
                                  resp = iii),
                         vi_permute(mod, target = iii, train = mod_data %>% select(iii, env_vars),
                                    metric = "rmse", pred_wrapper = predict, nsim = 10) %>% 
                           mutate(method = "perm_rmse",
                                  area = i,
                                  month = ii,
                                  resp = iii),
                         vi_firm(mod, feature_names = env_vars) %>% 
                           mutate(method = "pdp",
                                  area = i,
                                  month = ii,
                                  resp = iii))
      
      # Stepwise selection
      
      mod <- step(mod, trace = 0, direction = "both")
      
      coef_df_step <- bind_rows(coef_df_step,
                                tidy(mod) %>% 
                                  mutate(area = i,
                                         month = ii,
                                         resp = iii))
      summ_df_step <- bind_rows(summ_df_step,
                                glance(mod) %>% 
                                  mutate(area = i,
                                         month = ii,
                                         resp = iii))
      
      if(ncol(mod$model) > 1){
        vi_df_step <- bind_rows(vi_df_step,
                                vi_model(mod) %>% 
                                  mutate(method = "tstat",
                                         area = i,
                                         month = ii,
                                         resp = iii),
                                vi_permute(mod, target = iii, train = if(ncol(mod$model) <= 2){mod$model %>% mutate(extra = 1, extra2 = 1)} else {mod$model},
                                           metric = "rsquared", pred_wrapper = predict, nsim = 10) %>% 
                                  mutate(method = "perm_r2",
                                         area = i,
                                         month = ii,
                                         resp = iii) %>% 
                                  filter(!grepl("^extra",Variable)),
                                vi_permute(mod, target = iii, train = if(ncol(mod$model) <= 2){mod$model %>% mutate(extra = 1)} else {mod$model},
                                           metric = "rmse", pred_wrapper = predict, nsim = 10) %>% 
                                  mutate(method = "perm_rmse",
                                         area = i,
                                         month = ii,
                                         resp = iii) %>% 
                                  filter(!grepl("^extra",Variable)),
                                vi_firm(mod, feature_names = names(mod$model)[-1]) %>% 
                                  mutate(method = "pdp",
                                         area = i,
                                         month = ii,
                                         resp = iii))
      }
    }
  }
}

coef_df %>% write_csv("output/model_outputs/coef_df.csv")
summ_df %>% write_csv("output/model_outputs/summ_df.csv")
vi_df %>% write_csv("output/model_outputs/vi_df.csv")
coef_df_step %>% write_csv("output/model_outputs/coef_df_step.csv")
summ_df_step %>% write_csv("output/model_outputs/summ_df_step.csv")
vi_df_step %>% write_csv("output/model_outputs/vi_df_step.csv")

# Write out modelling data for later use in plottings

d %>% select(id_code, area, year, month, starts_with("T")) %>% 
  left_join(pisr_var) %>% 
  left_join(moist_var) %>% 
  left_join(snow_var) %>% 
  left_join(e) -> mod_data

mod_data %>% write_csv("output/model_outputs/model_input_data.csv")

# correlations by area and month

mod_data %>%
  filter(month %in% c(1,7)) %>% 
  mutate(group = paste0(area, "_", month)) %>% 
  drop_na() %>% 
  select(group, predictors) -> df

by(df, INDICES = df$group, FUN = function(x) cor(x[, -1], method = "spearman"))

