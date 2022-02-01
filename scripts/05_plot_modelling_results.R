library(tidyverse)
library(viridis)
library(lubridate)
library(ggthemes)
library(zoo)
library(cowplot)
library(lemon)

# This is needed to plot the dates in English if your system language is something else
Sys.setlocale("LC_TIME", "english")

# Set plotting order for focus area and months
plotting_order <- c("MAL","AIL","VAR","TII","PIS","HYY","KAR")
month_order <- as.character(as.yearmon(seq(as_date("2019-11-01"), as_date("2020-10-31"), by = "month"), format = "%b/%Y"))

# Color palette
var_colors <- c("darkorange","gold","gray90","olivedrab3","lightgoldenrod3","#0080FF","#0019FF","darkorchid4")

# Read all modelling results
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


#####################################################################3
# Rsquared summarised

summ_df_step %>% 
  filter(tvar %in% c("absmax", "mean", "absmin")) %>% 
  group_by(tsen) %>% 
  summarise(mean = mean(r.squared),
            min = min(r.squared),
            max = max(r.squared))

summ_df_step %>% 
  filter(tvar %in% c("absmax", "mean", "absmin")) %>% 
  group_by(tvar) %>% 
  summarise(mean = mean(r.squared),
            min = min(r.squared),
            max = max(r.squared))

summ_df_step %>% 
  filter(tvar %in% c("absmax", "mean", "absmin")) %>% 
  group_by(area) %>% 
  summarise(mean = mean(r.squared),
            min = min(r.squared),
            max = max(r.squared)) %>% 
  arrange(mean)

summ_df_step %>% 
  filter(tvar %in% c("absmax", "mean", "absmin")) %>% 
  group_by(month) %>% 
  summarise(r.squared = mean(adj.r.squared)) %>% 
  arrange(r.squared)


# Plot Rsquared results

summ_df_step %>% 
  filter(tsen != "T2") %>% 
  filter(tvar %in% c("absmin", "mean", "absmax")) %>% 
  mutate(tvar = recode_factor(tvar, absmin = "Tmin",
                              mean = "Tavg",
                              absmax = "Tmax")) %>% 
  ggplot(aes(x = ym, y = r.squared, color = tvar))+
  geom_smooth(aes(linetype = tvar), span = 0.8, size = 1, se = F) +
  facet_grid(rows = vars(area), cols =vars(tsen)) +
  scale_color_economist()+
  theme_minimal_grid() + theme(legend.position = "bottom") +
  scale_x_date(breaks = as_date(c("2019-11-01","2020-02-01","2020-05-01","2020-08-01")),
               date_labels =  "%b") +
  ylab(bquote(R^2)) + xlab(NULL) +
  guides(color=guide_legend(title=NULL), linetype=guide_legend(title=NULL)) -> gg1

gg1

ggsave("visuals/R2_lines.pdf",
       gg1,
       width = 17, height = 24, units = "cm")

##############################################################################
# Variable importance scores

# Prepare predictor names and filter wanted response variables
vi_df_step %>% 
  mutate(Variable = recode_factor(Variable, altitude = "Elevation",
                                  pisr = "Solar radiation",
                                  snow_days = "Snow cover",
                                  canopy_cover2m_10m = "Canopy cover",
                                  tpi100 = "TPI",
                                  moist_med = "Soil moisture",
                                  wetland_prop_100m = "Wetland cover",
                                  water_prop_500m = "Water cover")) %>% 
  filter(tsen != "T2") %>% 
  filter(tvar %in% c("absmin", "mean", "absmax")) %>% 
  mutate(tvar = recode_factor(tvar, absmin = "Tmin",
                              mean = "Tavg",
                              absmax = "Tmax")) %>%
  filter(method %in% c("perm_r2")) %>% 
  pivot_wider(id_cols = c(Variable, tsen, tvar, month, area), names_from = method, values_from = Importance) %>% 
  bind_rows(.,   anti_join(x = expand_grid(Variable = unique(.$Variable),
                                           tsen = unique(.$tsen),
                                           tvar = unique(.$tvar),
                                           month = unique(.$month),
                                           area = unique(.$area)), y = .)) %>% 
  mutate(perm_r2 = ifelse(is.na(perm_r2), 0, perm_r2)) %>% 
  arrange(area, month, tsen, tvar) -> impsum

# Summarise
impsum %>% 
  group_by(Variable, tsen) %>% 
  summarise(perm_r2 = mean(perm_r2)) %>% 
  mutate(perm_r2 = round(perm_r2, 2)) %>% 
  arrange(tsen, desc(perm_r2)) %>% as.data.frame()

impsum %>% 
  group_by(Variable, tvar) %>% 
  summarise(perm_r2 = mean(perm_r2)) %>% 
  mutate(perm_r2 = round(perm_r2, 2)) %>% 
  arrange(tvar, desc(perm_r2)) %>% as.data.frame()

impsum %>% 
  group_by(Variable) %>% 
  summarise(perm_r2 = mean(perm_r2)) %>% 
  mutate(perm_r2 = round(perm_r2, 2)) %>% 
  arrange(desc(perm_r2)) %>% as.data.frame()

impsum %>% 
  group_by(Variable, month) %>% 
  summarise(perm_r2 = mean(perm_r2)) %>% 
  mutate(perm_r2 = round(perm_r2, 2)) %>% 
  arrange(month, desc(perm_r2)) %>% as.data.frame()


# Barplots
# T1, T2 and T3

vi_df_step %>% 
  mutate(Variable = recode_factor(Variable, altitude = "Elevation",
                                  pisr = "Solar radiation",
                                  snow_days = "Snow cover",
                                  canopy_cover2m_10m = "Canopy cover",
                                  tpi100 = "TPI",
                                  moist_med = "Soil moisture",
                                  wetland_prop_100m = "Wetland cover",
                                  water_prop_500m = "Water cover")) %>% 
  mutate(Importance = ifelse(Importance < 0, 0, Importance)) %>% 
  filter(method == "perm_r2") %>% 
  filter(tvar %in% c("absmin", "mean", "absmax")) %>% 
  filter(tsen == "T1") %>% 
  mutate(tvar = recode_factor(tvar, absmin = "Tmin",
                              mean = "Tavg",
                              absmax = "Tmax")) %>% 
  ggplot(aes(y = Importance, x = ym, fill = Variable)) +
  geom_bar(position="stack", stat="identity", width=29) +
  facet_grid(rows = vars(area), cols = vars(tvar)) +
  scale_fill_manual(values = var_colors)+
  theme_clean() + theme(legend.position = "bottom") +
  scale_x_date(breaks = as_date(c("2019-11-01","2020-02-01","2020-05-01","2020-08-01")),
               date_labels =  "%b") +
  ylim(0,1.5)+
  ylab(paste0("Stacked variable importance")) + xlab(NULL) +
  theme(axis.text.x = element_text(size = 7.5))+
  theme(axis.text.y = element_text(size = 7.5))+
  theme(legend.text=element_text(size=7.5))+
  guides(fill=guide_legend(title=NULL)) -> gg1

vi_df_step %>% 
  mutate(Variable = recode_factor(Variable, altitude = "Elevation",
                                  pisr = "Solar radiation",
                                  snow_days = "Snow cover",
                                  canopy_cover2m_10m = "Canopy cover",
                                  tpi100 = "TPI",
                                  moist_med = "Soil moisture",
                                  wetland_prop_100m = "Wetland cover",
                                  water_prop_500m = "Water cover")) %>% 
  mutate(Importance = ifelse(Importance < 0, 0, Importance)) %>% 
  filter(method == "perm_r2") %>% 
  filter(tvar %in% c("absmin", "mean", "absmax")) %>% 
  filter(tsen == "T3") %>% 
  mutate(tvar = recode_factor(tvar, absmin = "Tmin",
                              mean = "Tavg",
                              absmax = "Tmax")) %>% 
  ggplot(aes(y = Importance, x = ym, fill = Variable)) +
  geom_bar(position="stack", stat="identity", width=29) +
  facet_grid(rows = vars(area), cols = vars(tvar)) +
  scale_fill_manual(values = var_colors)+
  theme_clean() + theme(legend.position = "bottom") +
  scale_x_date(breaks = as_date(c("2019-11-01","2020-02-01","2020-05-01","2020-08-01")),
               date_labels =  "%b") +
  ylab(paste0("Stacked variable importance")) + xlab(NULL) +
  ylim(0,1.5)+
  theme(axis.text.x = element_text(size = 7.5))+
  theme(axis.text.y = element_text(size = 7.5))+
  theme(legend.text=element_text(size=7.5))+
  guides(fill=guide_legend(title=NULL)) -> gg2

vi_df_step %>% 
  mutate(Variable = recode_factor(Variable, altitude = "Elevation",
                                  pisr = "Solar radiation",
                                  snow_days = "Snow cover",
                                  canopy_cover2m_10m = "Canopy cover",
                                  tpi100 = "TPI",
                                  moist_med = "Soil moisture",
                                  wetland_prop_100m = "Wetland cover",
                                  water_prop_500m = "Water cover")) %>% 
  mutate(Importance = ifelse(Importance < 0, 0, Importance)) %>% 
  filter(method == "perm_r2") %>% 
  filter(tvar %in% c("absmin", "mean", "absmax")) %>% 
  filter(tsen == "T4") %>% 
  mutate(tvar = recode_factor(tvar, absmin = "Tmin",
                              mean = "Tavg",
                              absmax = "Tmax")) %>% 
  ggplot(aes(y = Importance, x = ym, fill = Variable)) +
  geom_bar(position="stack", stat="identity", width=29) +
  facet_grid(rows = vars(area), cols = vars(tvar)) +
  scale_fill_manual(values = var_colors)+
  theme_clean() + theme(legend.position = "bottom") +
  scale_x_date(breaks = as_date(c("2019-11-01","2020-02-01","2020-05-01","2020-08-01")),
               date_labels =  "%b") +
  ylab(paste0("Stacked variable importance")) + xlab(NULL) +
  ylim(0,1.5)+
  theme(axis.text.x = element_text(size = 7.5))+
  theme(axis.text.y = element_text(size = 7.5))+
  theme(legend.text=element_text(size=7.5))+
  guides(fill=guide_legend(title=NULL)) -> gg3

ggsave("visuals/variable_importance_step_T1_T3_T4.pdf",
       plot_grid(gg1, gg2, gg3, nrow = 1, labels = "auto"),
       width = 37, height = 20, units = "cm")

##############################################################################
# Effect sizes and slopes

unique(coef_df_step$term)

predictor <- "canopy_cover2m_10m"

coef_df %>% 
  filter(term == predictor | term == "(Intercept)") %>% 
  mutate(term = ifelse(term == predictor, "predictor", term)) %>% 
  pivot_wider(id_cols = area:ym, names_from = term, values_from = c(estimate, std.error, p.value)) %>% 
  select(-ends_with("(Intercept)")) %>% 
  mutate(p.value_predictor_cl = cut(p.value_predictor,
                                 breaks = c(Inf, 0.5, 0.1, 0.05, 0.01, 0.001, 0),
                                 labels = rev(c("1 - 0.5","0.5 - 0.1","0.1 - 0.05","0.05 - 0.01","0.01 - 0.001","< 0.001")))) %>% 
  mutate(lower = estimate_predictor-std.error_predictor,
         upper = estimate_predictor+std.error_predictor) -> slopes

slopes %>% filter(tsen == "T1") %>% 
  filter(tvar %in% c("absmin", "mean", "absmax")) %>% 
  mutate(tvar = recode_factor(tvar, absmin = "T_absmin",
                              mean = "T_avg",
                              absmax = "T_absmax")) %>% 
  rename(p.value = p.value_predictor_cl) %>% 
  ggplot(aes(x = ym, y = estimate_predictor, color = p.value))+
  geom_point()+
  geom_abline(slope = 0, linetype = 2)+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  facet_grid(rows = vars(area), cols = vars(tvar))+
  scale_color_manual(values = c("firebrick2","darkorange","goldenrod1","gray60","gray75","gray90"), 
                     na.translate = F, drop = FALSE)+
  theme_hc() +
  scale_x_date(breaks = as_date(c("2019-11-01","2020-02-01","2020-05-01","2020-08-01")),
               date_labels =  "%b")+
  ylab(paste0("b(canopy_cover)")) + xlab(NULL) +
  ylim(-0.279, 0.12)+
  theme(axis.text.x = element_text(size = 9))+
  ggtitle("T1 (-6cm soil temperature)") -> gg1


slopes %>% filter(tsen == "T3") %>% 
  filter(tvar %in% c("absmin", "mean", "absmax")) %>% 
  mutate(tvar = recode_factor(tvar, absmin = "T_absmin",
                              mean = "T_avg",
                              absmax = "T_absmax")) %>% 
  rename(p.value = p.value_predictor_cl) %>% 
  ggplot(aes(x = ym, y = estimate_predictor, color = p.value))+
  geom_point()+
  geom_abline(slope = 0, linetype = 2)+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  facet_grid(rows = vars(area), cols = vars(tvar))+
  scale_color_manual(values = c("firebrick2","darkorange","goldenrod1","gray60","gray75","gray90"), 
                     na.translate = F, drop = FALSE)+
  theme_hc() +
  scale_x_date(breaks = as_date(c("2019-11-01","2020-02-01","2020-05-01","2020-08-01")),
               date_labels =  "%b")+
  ylab(paste0("b(canopy_cover)")) + xlab(NULL) +
  ylim(-0.279, 0.12)+
  theme(axis.text.x = element_text(size = 9))+
  ggtitle("T3 (15cm near surface temperature)") -> gg2

ggsave("visuals/canopy_cover_slopes.pdf",
       plot_grid(gg1, gg2, labels = "auto"),
       width = 30, height = 25, units = "cm")
