library(tidyverse)
library(lubridate)
library(formattable)
library(zoo)
library(htmltools)
library(webshot)

webshot::install_phantomjs()

# Function to format table colors
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

# Order the focus areas and months
plotting_order <- c("MAL","AIL","VAR","TII","PIS","HYY","KAR")
month_order <- as.character(as.yearmon(seq(as_date("2019-11-01"), as_date("2020-10-31"), by = "month"), format = "%b/%Y"))

# COlor palette
var_colors <- c("darkorange","gold","gray90","olivedrab3","lightgoldenrod3","#0080FF","#0019FF","darkorchid4")

# Environmental variables
d <- read_csv("output/model_outputs/model_input_data.csv")

d %>% mutate(pisr = pisr/3600) %>% 
  filter(!is.na(area)) -> d

d %>% select(-(T1_mean:T4_meanmin),-hydro_year, -year) %>% 
  filter(month %in% c(1,7)) %>% 
  mutate(month = recode_factor(month, 
                               "1" = "Jan",
                               "7" = "Jul")) %>% 
  relocate(pisr, .after = water_prop_500m) %>% 
  pivot_wider(id_code:wetland_prop_100m, names_from = month, values_from = pisr, names_prefix = "pisr_") %>% 
  group_by(id_code, area) %>% 
  summarise(across(where(is.double), ~mean(.x, na.rm = T))) %>% 
  pivot_longer(cols = moist_med:pisr_Jul) %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  mutate(name = recode_factor(name, altitude = "Elevation (m)",
                              pisr_Jan = "Solar radiation, January (kWh / m²)",
                              pisr_Jul = "Solar radiation, July (kWh / m²)",
                              canopy_cover2m_10m = "Canopy cover (%)",
                              snow_days = "Snow cover duration (days)",
                              tpi100 = "Topographic position index",
                              moist_med = "Soil moisture (VWC%)",
                              wetland_prop_100m = "Wetland cover (%)",
                              water_prop_500m = "Water cover (%)")) -> d

d %>% group_by(area, name) %>% 
  summarise(mean = mean(value, na.rm = T),
            median = median(value, na.rm = T),
            min = min(value, na.rm = T),
            max = max(value, na.rm = T),
            'range length' = max-min) %>% 
  pivot_longer(cols = mean:`range length`, names_to = "stat") %>% 
  pivot_wider(id_cols = c(area, stat), names_from = name, values_from = value) %>% 
  arrange(area) %>% 
  mutate(`Elevation (m)` = round(`Elevation (m)`, 1),
         `Solar radiation, January (kWh / m²)` = round(`Solar radiation, January (kWh / m²)`, 2),
         `Solar radiation, July (kWh / m²)` = round(`Solar radiation, July (kWh / m²)`, 2),
         `Canopy cover (%)` = round(`Canopy cover (%)`, 0),
         `Snow cover duration (days)` = round(`Snow cover duration (days)`, 0),
         `Topographic position index` = round(`Topographic position index`, 1),
         `Soil moisture (VWC%)` = round(`Soil moisture (VWC%)`, 1),
         `Wetland cover (%)` = round(`Wetland cover (%)`, 0),
         `Water cover (%)` = round(`Water cover (%)`, 0)) -> d

fd <- formattable(d, align =c("l","l",rep("c", times = 9)), list(
  `area` = formatter("span", style = ~ style(color = "black",font.weight = "bold")), 
  `stat` = formatter("span", style = ~ style(color = "black",font.weight = "bold"))
))

export_formattable(fd, "visuals/env_var_table.png")


# Read all results
coef_df <- read_csv("output/model_outputs/coef_df.csv") %>% 
  mutate(tsen = unlist(lapply(resp, function(x) str_split(x, "_")[[1]][1] )),
         tvar = unlist(lapply(resp, function(x) str_split(x, "_")[[1]][2] ))) %>% 
  mutate(year = ifelse(month %in% c(11,12), 2019, 2020)) %>% 
  mutate(ym = ym(paste0(year, "-", month))) %>% 
  mutate(area = factor(area, levels = plotting_order))
summ_df <- read_csv("output/model_outputs/summ_df.csv") %>% 
  mutate(tsen = unlist(lapply(resp, function(x) str_split(x, "_")[[1]][1] )),
         tvar = unlist(lapply(resp, function(x) str_split(x, "_")[[1]][2] ))) %>% 
  mutate(year = ifelse(month %in% c(11,12), 2019, 2020)) %>% 
  mutate(ym = ym(paste0(year, "-", month))) %>% 
  mutate(area = factor(area, levels = plotting_order))
vi_df <- read_csv("output/model_outputs/vi_df.csv") %>% 
  mutate(tsen = unlist(lapply(resp, function(x) str_split(x, "_")[[1]][1] )),
         tvar = unlist(lapply(resp, function(x) str_split(x, "_")[[1]][2] ))) %>% 
  mutate(year = ifelse(month %in% c(11,12), 2019, 2020)) %>% 
  mutate(ym = ym(paste0(year, "-", month))) %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  filter(!grepl("^extra",Variable))
coef_df_step <- read_csv("output/model_outputs/coef_df_step.csv") %>% 
  mutate(tsen = unlist(lapply(resp, function(x) str_split(x, "_")[[1]][1] )),
         tvar = unlist(lapply(resp, function(x) str_split(x, "_")[[1]][2] ))) %>% 
  mutate(year = ifelse(month %in% c(11,12), 2019, 2020)) %>% 
  mutate(ym = ym(paste0(year, "-", month))) %>% 
  mutate(area = factor(area, levels = plotting_order))
summ_df_step <- read_csv("output/model_outputs/summ_df_step.csv") %>% 
  mutate(tsen = unlist(lapply(resp, function(x) str_split(x, "_")[[1]][1] )),
         tvar = unlist(lapply(resp, function(x) str_split(x, "_")[[1]][2] ))) %>% 
  mutate(year = ifelse(month %in% c(11,12), 2019, 2020)) %>% 
  mutate(ym = ym(paste0(year, "-", month))) %>% 
  mutate(area = factor(area, levels = plotting_order))
vi_df_step <- read_csv("output/model_outputs/vi_df_step.csv") %>% 
  mutate(tsen = unlist(lapply(resp, function(x) str_split(x, "_")[[1]][1] )),
         tvar = unlist(lapply(resp, function(x) str_split(x, "_")[[1]][2] ))) %>% 
  mutate(year = ifelse(month %in% c(11,12), 2019, 2020)) %>% 
  mutate(ym = ym(paste0(year, "-", month))) %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  filter(!grepl("^extra",Variable))


# R2 table

summ_df_step %>% 
  filter(tsen != "T2") %>% 
  filter(tvar %in% c("absmin", "mean", "absmax")) %>% 
  mutate(tvar = recode_factor(tvar, absmin = "Tmin",
                              mean = "Tavg",
                              absmax = "Tmax")) %>% 
  mutate(month = month.abb[month]) %>% 
  pivot_wider(id_cols = c(tsen,tvar,area), names_from = month, values_from = r.squared) %>% 
  relocate(Nov:Dec, .after = area) %>% 
  relocate(area, tvar, tsen) %>% 
  group_by(tvar, tsen) %>%
  summarise(across(Nov:Oct, mean)) %>%
  as.data.frame() %>% 
  mutate(across(Nov:Oct, ~round(.x, 2))) %>% 
  rename('T variable' = tvar,
         'T height' = tsen) -> d

formattable(d)

fd <- formattable(d, align =c("l","l",rep("c", times = 12)), list(
  `T variable` = formatter("span", style = ~ style(color = "black",font.weight = "bold")), 
  `T height` = formatter("span", style = ~ style(color = "black",font.weight = "bold")), 
  area(col = 3:14) ~ color_tile("white", "red")
))


install.packages('htmltools')
install.packages('webshot')
library(htmltools)
library(webshot)
webshot::install_phantomjs()

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

export_formattable(fd, "visuals/R2_table.png")

# VI table
improvement_formatter <- formatter("span", style = x ~ style(font.weight = "bold", 
                                                             color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))), 
                                   x ~ icontext(ifelse(x == max(x), "thumbs-up", ""), x)
)

color_text2 <- function (...) 
{
  formatter("span", style = function(x) style(color = csscolor(gradient(abs(as.numeric(x)), 
                                                                        ...))))
}

# T1
vi_df_step %>% 
  mutate(Variable = recode_factor(Variable, altitude = "Elevation",
                                  pisr = "Solar radiation",
                                  snow_days = "Snow cover",
                                  canopy_cover2m_10m = "Canopy cover",
                                  tpi100 = "TPI",
                                  moist_med = "Soil moisture",
                                  wetland_prop_100m = "Wetland cover",
                                  water_prop_500m = "Water cover")) %>% 
  filter(tsen == "T1") %>% 
  filter(tvar %in% c("absmin", "mean", "absmax")) %>% 
  mutate(tvar = recode_factor(tvar, absmin = "Tmin",
                              mean = "Tavg",
                              absmax = "Tmax")) %>%
  filter(method %in% c("tstat", "perm_r2")) %>% 
  pivot_wider(id_cols = c(Variable, tsen, tvar, month, area), names_from = method, values_from = Importance:Sign) %>% 
  select(-Importance_tstat, -Sign_perm_r2) %>% 
  mutate(Importance_perm_r2 = ifelse(Importance_perm_r2 < 0, 0, Importance_perm_r2)) %>% 
  mutate(Sign_tstat =ifelse(Sign_tstat == "POS", 1, -1)) %>% 
  mutate(Importance_perm_r2 = Importance_perm_r2*Sign_tstat) %>% 
  bind_rows(.,   anti_join(x = expand_grid(Variable = unique(.$Variable),
                                          tsen = unique(.$tsen),
                                          tvar = unique(.$tvar),
                                          month = unique(.$month),
                                          area = unique(.$area)), y = .)) %>% 
  mutate(Importance_perm_r2 = ifelse(is.na(Importance_perm_r2), 0, Importance_perm_r2),
         Sign_tstat = ifelse(is.na(Sign_tstat), 0, Sign_tstat)) %>% 
  arrange(area, month, tsen, tvar) %>% 
  group_by(Variable, month, tvar) %>% 
  summarise(Sign_tstat = weighted.mean(Sign_tstat, w = abs(Importance_perm_r2)),
            Importance_perm_r2 = mean(Importance_perm_r2)) %>% 
  mutate(month = month.abb[month]) %>% 
  pivot_wider(id_cols = c(Variable, tvar), names_from = month, values_from = Importance_perm_r2) %>% 
  relocate(Nov:Dec, .after = tvar) %>% 
  mutate(across(Nov:Oct, ~round(.x, 2))) %>% 
  rename('Predictor' = Variable,
         'T_var' = tvar) -> d

d2 <- bind_rows(d, d %>% ungroup() %>% slice(n = 1) %>% mutate(Nov = 0.28))

color_tile3 <- function (...) {
  formatter("span", style = function(x) {
    style(display = "block",
          padding = "0 4px", 
          `border-radius` = "4px", 
          `background-color` = csscolor(matrix(as.integer(colorRamp(...)(normalize(as.numeric(x)))), 
                                               byrow=TRUE, dimnames=list(c("red","green","blue"), NULL), nrow=3)))
  })}

fd <- formattable(d2, align =c("l","l",rep("c", times = 12)), list(
  `Predictor` = formatter("span", style = ~ style(color = "black",font.weight = "bold")), 
  `T_var` = formatter("span", style = ~ style(color = "black",font.weight = "bold")), 
  area(col = 3:14) ~ color_tile3(c("blue","white", "red"))
))

export_formattable(fd, "visuals/VI_table_T1.png")

# T3
vi_df_step %>% 
  mutate(Variable = recode_factor(Variable, altitude = "Elevation",
                                  pisr = "Solar radiation",
                                  snow_days = "Snow cover",
                                  canopy_cover2m_10m = "Canopy cover",
                                  tpi100 = "TPI",
                                  moist_med = "Soil moisture",
                                  wetland_prop_100m = "Wetland cover",
                                  water_prop_500m = "Water cover")) %>% 
  filter(tsen == "T3") %>% 
  filter(tvar %in% c("absmin", "mean", "absmax")) %>% 
  mutate(tvar = recode_factor(tvar, absmin = "Tmin",
                              mean = "Tavg",
                              absmax = "Tmax")) %>%
  filter(method %in% c("tstat", "perm_r2")) %>% 
  pivot_wider(id_cols = c(Variable, tsen, tvar, month, area), names_from = method, values_from = Importance:Sign) %>% 
  select(-Importance_tstat, -Sign_perm_r2) %>% 
  mutate(Importance_perm_r2 = ifelse(Importance_perm_r2 < 0, 0, Importance_perm_r2)) %>% 
  mutate(Sign_tstat =ifelse(Sign_tstat == "POS", 1, -1)) %>% 
  mutate(Importance_perm_r2 = Importance_perm_r2*Sign_tstat) %>% 
  bind_rows(.,   anti_join(x = expand_grid(Variable = unique(.$Variable),
                                           tsen = unique(.$tsen),
                                           tvar = unique(.$tvar),
                                           month = unique(.$month),
                                           area = unique(.$area)), y = .)) %>% 
  mutate(Importance_perm_r2 = ifelse(is.na(Importance_perm_r2), 0, Importance_perm_r2),
         Sign_tstat = ifelse(is.na(Sign_tstat), 0, Sign_tstat)) %>% 
  arrange(area, month, tsen, tvar) %>% 
  group_by(Variable, month, tvar) %>% 
  summarise(Sign_tstat = weighted.mean(Sign_tstat, w = abs(Importance_perm_r2)),
            Importance_perm_r2 = mean(Importance_perm_r2)) %>% 
  mutate(month = month.abb[month]) %>% 
  pivot_wider(id_cols = c(Variable, tvar), names_from = month, values_from = Importance_perm_r2) %>% 
  relocate(Nov:Dec, .after = tvar) %>% 
  mutate(across(Nov:Oct, ~round(.x, 2))) %>% 
  rename('Predictor' = Variable,
         'T_var' = tvar) -> d

d2 <- bind_rows(d, d %>% ungroup() %>% slice(n = 1) %>% mutate(Nov = 0.48))

color_tile3 <- function (...) {
  formatter("span", style = function(x) {
    style(display = "block",
          padding = "0 4px", 
          `border-radius` = "4px", 
          `background-color` = csscolor(matrix(as.integer(colorRamp(...)(normalize(as.numeric(x)))), 
                                               byrow=TRUE, dimnames=list(c("red","green","blue"), NULL), nrow=3)))
  })}

fd <- formattable(d2, align =c("l","l",rep("c", times = 12)), list(
  `Predictor` = formatter("span", style = ~ style(color = "black",font.weight = "bold")), 
  `T_var` = formatter("span", style = ~ style(color = "black",font.weight = "bold")), 
  area(col = 3:14) ~ color_tile3(c("blue","white", "red"))
))

export_formattable(fd, "visuals/VI_table_T3.png")

# T4
vi_df_step %>% 
  mutate(Variable = recode_factor(Variable, altitude = "Elevation",
                                  pisr = "Solar radiation",
                                  snow_days = "Snow cover",
                                  canopy_cover2m_10m = "Canopy cover",
                                  tpi100 = "TPI",
                                  moist_med = "Soil moisture",
                                  wetland_prop_100m = "Wetland cover",
                                  water_prop_500m = "Water cover")) %>% 
  filter(tsen == "T4") %>% 
  filter(tvar %in% c("absmin", "mean", "absmax")) %>% 
  mutate(tvar = recode_factor(tvar, absmin = "Tmin",
                              mean = "Tavg",
                              absmax = "Tmax")) %>%
  filter(method %in% c("tstat", "perm_r2")) %>% 
  pivot_wider(id_cols = c(Variable, tsen, tvar, month, area), names_from = method, values_from = Importance:Sign) %>% 
  select(-Importance_tstat, -Sign_perm_r2) %>% 
  mutate(Importance_perm_r2 = ifelse(Importance_perm_r2 < 0, 0, Importance_perm_r2)) %>% 
  mutate(Sign_tstat =ifelse(Sign_tstat == "POS", 1, -1)) %>% 
  mutate(Importance_perm_r2 = Importance_perm_r2*Sign_tstat) %>% 
  bind_rows(.,   anti_join(x = expand_grid(Variable = unique(.$Variable),
                                           tsen = unique(.$tsen),
                                           tvar = unique(.$tvar),
                                           month = unique(.$month),
                                           area = unique(.$area)), y = .)) %>% 
  mutate(Importance_perm_r2 = ifelse(is.na(Importance_perm_r2), 0, Importance_perm_r2),
         Sign_tstat = ifelse(is.na(Sign_tstat), 0, Sign_tstat)) %>% 
  arrange(area, month, tsen, tvar) %>% 
  group_by(Variable, month, tvar) %>% 
  summarise(Sign_tstat = weighted.mean(Sign_tstat, w = abs(Importance_perm_r2)),
            Importance_perm_r2 = mean(Importance_perm_r2)) %>% 
  mutate(month = month.abb[month]) %>% 
  pivot_wider(id_cols = c(Variable, tvar), names_from = month, values_from = Importance_perm_r2) %>% 
  relocate(Nov:Dec, .after = tvar) %>% 
  mutate(across(Nov:Oct, ~round(.x, 2))) %>% 
  rename('Predictor' = Variable,
         'T_var' = tvar) -> d

d2 <- bind_rows(d, d %>% ungroup() %>% slice(n = 1) %>% mutate(Nov = 0.48))

color_tile3 <- function (...) {
  formatter("span", style = function(x) {
    style(display = "block",
          padding = "0 4px", 
          `border-radius` = "4px", 
          `background-color` = csscolor(matrix(as.integer(colorRamp(...)(normalize(as.numeric(x)))), 
                                               byrow=TRUE, dimnames=list(c("red","green","blue"), NULL), nrow=3)))
  })}

fd <- formattable(d2, align =c("l","l",rep("c", times = 12)), list(
  `Predictor` = formatter("span", style = ~ style(color = "black",font.weight = "bold")), 
  `T_var` = formatter("span", style = ~ style(color = "black",font.weight = "bold")), 
  area(col = 3:14) ~ color_tile3(c("blue","white", "red"))
))

export_formattable(fd, "visuals/VI_table_T4.png")

