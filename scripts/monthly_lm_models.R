library(tidyverse)
library(lme4)
library(zoo)
library(broom)

# The study areas modelled
areas_to_model <- c("MAL","AIL","VAR","TII","PIS","HYY","KAR")
# Predictors in the models
predictors <- c("altitude", "pisr", "moist_med", "snow_days", "tpi100",
                "allwet_prop_100m", "pgaps2m_metsakeskus", "canopy_portion_decid", "forest_prop_100m")

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

# Preprocess snow variables

snow_var <- data.frame()
for(i in areas_to_model){
  # i <- "VAR"
  snow %>% filter(area == i) -> temp
  
  mod <- glmer(snow_days ~ factor(hydro_year) + (1 | id_code), data = temp, family = ifelse(i == "VAR", "gaussian","poisson"))
  
  df <- data.frame(hydro_year = "2020",
                   id_code = unique(e %>% filter(area == i) %>% pull(id_code)),
                   area = i)
  
  df$snow_days <- round(predict(mod, df, type = "response", allow.new.levels = T))
  
  snow_var <- bind_rows(snow_var, df)
}

# Preprocess soil moisture

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
                          "_",
                          c(11:12,1:10)),
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

e %>% dplyr::select(id_code, starts_with("pisr_m")) %>% 
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
coef_df_step <- tibble()
summ_df_step <- tibble()
for(i in areas_to_model){
  print(i)
  
  for(ii in 1:12){
    print(ii)
    
    d %>% filter(area == i,
                 month == ii) -> temp
    
    responses <- names(temp)[grepl("^T[0-9]", names(temp))]
    
    for(iii in responses){
      
      env_vars <- predictors
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
      
      mod <- step(mod, trace = -1)
      
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
      
      
    }
    
  }
  
}

coef_df %>% write_csv("output/model_outputs/coef_df.csv")
summ_df %>% write_csv("output/model_outputs/summ_df.csv")
coef_df_step %>% write_csv("output/model_outputs/coef_df_step.csv")
summ_df_step %>% write_csv("output/model_outputs/summ_df_step.csv")



