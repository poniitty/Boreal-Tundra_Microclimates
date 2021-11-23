library(tidyverse)
library(lme4)
library(zoo)
library(lubridate)
library(MuMIn)
library(merTools)

areas_to_model <- c("MAL","AIL","VAR","TII","PIS","HYY","KAR")
month_to_model <- c(1,7)

# Predictors in the models
jan_predictors <- c("altitude", "pisr", "wetland_prop_100m", "snow_days", "tpi100",
                    "water_prop_500m", "canopy_cover2m_10m", "moist_med")

jul_predictors <- c("altitude", "pisr", "wetland_prop_100m", "tpi100",
                    "water_prop_500m", "canopy_cover2m_10m", "moist_med")

jan_random_factors <- c("(1 | area)")
jul_random_factors <- c("(1 | area)")

jan_model_formula <- formula(paste0("value ~ ", paste(c(jan_predictors, jan_random_factors), collapse = " + ")))
jul_model_formula <- formula(paste0("value ~ ", paste(c(jul_predictors, jul_random_factors), collapse = " + ")))


# Read data
d <- read_csv("data/all_data_monthly.csv") %>% 
  dplyr::select(-ends_with("_prop"))
e <- read_csv("data/all_env_variables.csv") %>% 
  filter(logger == "Tomst") %>% 
  filter(area %in% areas_to_model) %>% 
  filter(!(area == "PIS" & site >= 100))
snow <- read_csv("data/snow_vars_all.csv") %>% 
  filter(area %in% areas_to_model) %>% 
  filter(!(area == "PIS" & site >= 100))

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

# Snowy periods per area
# Used to select snow as a predictor only when relevant

snow %>% filter(hydro_year == 2020) %>% 
  group_by(area) %>% 
  summarise(first_snow_date = min(first_snow_date, na.rm = T),
            last_snow_date = max(last_snow_date, na.rm = T)) -> snowy_dates


# Preprocess soil moisture
# Impute missing values based on previous values (when soil frozen) or model when that is not possible

# Soil moisture from another repository
sm <- read_csv("https://raw.githubusercontent.com/poniitty/Fennoscandia_microclimates/main/data/all_data_monthly.csv") %>% 
  dplyr::select(id_code:month, starts_with("moist"))

sm %>% dplyr::select(id_code:month, moist_prop, moist_med) %>% 
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
    dplyr::select(-ym) -> df
  
  df$moist_med <- round(predict(mod, df, type = "response", allow.new.levels = T),2)
  
  moist_var <- bind_rows(moist_var, df)
}


# Monthly radiation values to long format
extr_month <- function(x){ as.numeric(gsub("m","",unlist(lapply(x, function(xx) strsplit(xx,"_")[[1]][2]))))}

e %>% dplyr::select(id_code, (starts_with("pisr_m") & ends_with("_50m"))) %>% 
  pivot_longer(cols = starts_with("pisr_m"), names_to = "month", values_to = "pisr") %>% 
  mutate(month = extr_month(month)) %>% 
  mutate(across(pisr, ~as.numeric(scale(.x)))) -> pisr_var

# Select and preprocess other env data

e %>% 
  mutate(altitude = altitude/100) %>% 
  mutate(across(starts_with("pgaps"), ~abs(.x-100))) %>% 
  dplyr::select(id_code, names(.)[names(.) %in% unique(c(jan_predictors, jul_predictors))]) %>% 
  mutate(across(names(.)[names(.) %in% unique(c(jan_predictors, jul_predictors))], ~as.numeric(scale(.x)))) -> e

coef_df <- tibble()
summ_df <- tibble()
for(ii in month_to_model){
  print(ii) # ii <- 1
  
  d %>% filter(month == ii) -> temp
  
  # Extract response variables
  responses <- names(temp)[grepl("^T[0-9]", names(temp))]
  
  for(iii in responses){
    # iii <- "T4_absmin"
    
    temp %>% dplyr::select(id_code, area, year, month, iii) %>% 
      left_join(pisr_var) %>% 
      left_join(moist_var) %>% 
      left_join(snow_var) %>% 
      left_join(e) %>% 
      filter(complete.cases(.)) %>% 
      rename(value = iii) %>% 
      mutate(across(c("moist_med","snow_days"), ~as.numeric(scale(.x)))) -> mod_data
    
    if(ii == 1){
      mod <- lmer(jan_model_formula, data = mod_data)
    }
    if(ii == 7){
      mod <- lmer(jul_model_formula, data = mod_data)
    }
    
    s <- summary(mod)
    
    coef_df <- bind_rows(coef_df,
                         tibble(term = names(fixef(mod)),
                                estimate = fixef(mod),
                                std.error = s$coefficients[,"Std. Error"],
                                statistic = s$coefficients[,"t value"]) %>% 
                           mutate(month = ii,
                                  resp = iii))
    
    summ_df <- bind_rows(summ_df,
                         tibble(r2m = r.squaredGLMM(mod)[,"R2m"],
                                r2c = r.squaredGLMM(mod)[,"R2c"]) %>% 
                           mutate(month = ii,
                                  resp = iii))
    
  }
}

coef_df %>% write_csv("output/model_outputs/coef_df_lmer.csv")
summ_df %>% write_csv("output/model_outputs/summ_df_lmer.csv")



