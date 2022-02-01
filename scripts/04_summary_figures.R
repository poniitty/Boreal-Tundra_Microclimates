###############################################################################
# This code plots some summary figures
#

library(tidyverse)
library(lubridate)
library(cowplot)
library(viridis)
library(zoo)
library(data.table)

# This is needed to plot the dates in English if your system language is something else
Sys.setlocale("LC_TIME", "english")

d <- read_csv("data/all_data_monthly.csv")

plotting_order <- c("MAL","AIL","VAR","TII","PIS","HYY","KAR")

# check if study areas are as planned
unique(d$area)

# How many different sites
length(unique(d$id_code))


#######################################################################
# Plot the  number of functional sensors per month per area

d %>% select(id_code:moist_prop) %>% 
  pivot_longer(cols = T1_prop:moist_prop, names_to = "var", values_to = "prop") %>% 
  mutate(var = gsub("_prop","",var)) %>% 
  filter(prop > 0.95) %>% 
  group_by(area, year, month, var) %>% 
  count() -> aggr

aggr %>% full_join(., aggr %>% 
                     group_by(area, var) %>% 
                     summarise(max_n = max(n))) %>% 
  mutate(prop = round(n/max_n*100)) %>% 
  mutate(area = factor(area, levels = plotting_order)) -> aggr


aggr %>% filter(var != "moist") %>%
  mutate(ym = ym(paste0(year, "-", month))) %>% 
  ggplot(aes(x = ym, y = var, color = prop))+
  geom_point(size = 5) +
  scale_color_continuous(type = "viridis")+
  facet_grid(rows = vars(area))+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  xlab(NULL) + ylab("Temperature sensor") + labs(color = "%") +
  theme_bw() -> gg1

aggr %>% filter(var != "moist") %>%
  mutate(ym = ym(paste0(year, "-", month))) %>% 
  ggplot(aes(x = ym, y = var, color = n))+
  geom_point(size = 5) +
  scale_color_continuous(type = "viridis")+
  facet_grid(rows = vars(area))+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  xlab(NULL) + ylab("Temperature sensor") + labs(color = "n") +
  theme_bw() -> gg2

ggsave("visuals/logger_numbers_SUPPL.pdf",
       plot_grid(gg2, gg1, labels = "auto"),
       width = 20, height = 16, units = "cm")


########################################################################
# Environmental gradients

d <- read_csv("output/model_outputs/model_input_data.csv")
plotting_order <- c("MAL","AIL","VAR","TII","PIS","HYY","KAR")

your_palette <- colorRampPalette(c("#000099", "#0000FF", "#5000FF", "#9F0FF0", "#EF42BD", "#FF758A", "#FFA857", "#FFDB24"))


# kj to kwh
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

d %>% 
  ggplot(aes(x = area, y = value, fill = area, color = area)) +
  geom_violin(scale = "width")+
  stat_summary(fun=median, geom='point', color = "gray80", size = 1.5, show.legend = F) +
  facet_wrap(vars(name), scales = "free")+
  scale_fill_manual(values = your_palette(7)) +
  scale_color_manual(values = your_palette(7)) +
  theme_bw()+
  xlab(NULL)+ylab(NULL) -> gg1

ggsave("visuals/Env_gradients_violin.pdf",
       gg1,
       width = 25, height = 18, units = "cm")

##########################################################################
# THE REST ARE ONLY PRELIMINARY PLOTTINGS

##############################################################################
# SD time series
# This is needed to plot the dates in English if your system language is something else
Sys.setlocale("LC_TIME", "english")

plotting_order <- c("MAL","AIL","VAR","TII","PIS","HYY","KAR")

d <- fread("C:/datacloud/biogeoclimate/microclimate/data/logger/data_article/all_data.csv")

d %>% mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) -> d

d %>% group_by(area, datetime) %>% 
  summarise(T1 = sd(T1, na.rm = T),
            T2 = sd(T2, na.rm = T),
            T3 = sd(T3, na.rm = T),
            T4 = sd(T4, na.rm = T)) %>% 
  filter(!is.na(T4)) -> d

d %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  pivot_longer(cols = T1:T4, names_to = "tsen", values_to = "Temperature") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = Temperature), color = "orange")+
  geom_smooth(aes(y = Temperature), span = 0.3, method = "loess", se = T, color = "black")+
  facet_grid(rows = vars(area), cols = vars(tsen)) +
  scale_x_datetime(breaks = as_datetime(c("2019-11-01 01:00:00","2020-02-01 01:00:00",
                                      "2020-05-01 01:00:00","2020-08-01 01:00:00"), tz = "Etc/GMT-2"),
               date_labels =  "%b") +
  ylab("Standard deviation (Temperature)") + xlab(NULL)+
  theme_bw() -> gg1

d %>% 
  filter(month(datetime) == 7) %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  pivot_longer(cols = T1:T4, names_to = "tsen", values_to = "Temperature") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = Temperature), color = "orange")+
  geom_smooth(aes(y = Temperature), span = 0.3, method = "loess", se = T, color = "black")+
  facet_grid(rows = vars(area), cols = vars(tsen)) +
  scale_x_datetime(breaks = as_datetime(c("2020-07-01 01:00:00","2020-07-11 01:00:00",
                                          "2020-07-21 01:00:00"), tz = "Etc/GMT-2"),
                   date_labels =  "%b %d") +
  ylab("Standard deviation (Temperature)") + xlab(NULL)+
  theme_bw() -> gg2

ggsave("visuals/sd_across_areas_year_SUPPL.pdf",
       gg1,
       width = 20, height = 25, units = "cm")

ggsave("visuals/sd_across_areas_July_SUPPL.pdf",
       gg2,
       width = 20, height = 25, units = "cm")


#####################################################################
#  Scatter plots

library(pals) # for cyclic color palettes

d <- read_csv("data/all_data_monthly.csv")

plotting_order <- c("MAL","AIL","VAR","TII","PIS","HYY","KAR")
month_order <- as.character(as.yearmon(seq(as_date("2019-11-01"), as_date("2020-10-31"), by = "month"), format = "%b/%Y"))

d %>% 
  ggplot(aes(y = T1_absmin, x = T1_absmax, color = month, group = month)) +
  geom_point() +
  # geom_smooth(method = "lm")+
  scale_color_gradientn(colours = kovesi.cyclic_mrybm_35_75_c68_s25(12))

d %>% 
  ggplot(aes(y = T2_absmin, x = T2_absmax, color = month, group = month)) +
  geom_point() +
  geom_smooth(method = "lm")+
  scale_color_gradientn(colours = kovesi.cyclic_mrybm_35_75_c68_s25(12))

d %>% 
  ggplot(aes(y = T3_absmin, x = T3_absmax, color = month, group = month)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_gradientn(colours = kovesi.cyclic_mrybm_35_75_c68_s25(12))

d %>% 
  ggplot(aes(y = T4_absmin, x = T4_absmax, color = month, group = month)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_gradientn(colours = kovesi.cyclic_mrybm_35_75_c68_s25(12))

d %>% 
  ggplot(aes(y = T1_mean, x = T3_mean, color = month, group = month)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_gradientn(colours = kovesi.cyclic_mrybm_35_75_c68_s25(12))

d %>% 
  ggplot(aes(y = T1_absmin, x = T4_absmin, color = area, group = area)) +
  geom_point(size = 0.7) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = kovesi.cyclic_mrybm_35_75_c68_s25(7))

d %>% 
  ggplot(aes(y = T1_absmax, x = T4_absmax, color = area, group = area)) +
  geom_point(size = 0.7) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = kovesi.cyclic_mrybm_35_75_c68_s25(7))

d %>% mutate(area = factor(area, levels = plotting_order)) %>% 
  ggplot(aes(y = T1_mean, x = T4_mean)) +
  geom_point(aes(color = area),size = 0.7) +
  geom_smooth(method = "lm", color = "black") +
  geom_smooth(aes(color = area, group = area), method = "lm") +
  facet_wrap(vars(month), scales = "free")+
  scale_color_manual(values = kovesi.rainbow_bgyr_35_85_c72(7))

d %>% mutate(area = factor(area, levels = plotting_order)) %>% 
  ggplot(aes(y = T1_absmin, x = T1_absmax)) +
  geom_point(aes(color = area),size = 0.7) +
  geom_smooth(method = "lm", color = "black") +
  geom_smooth(aes(color = area, group = area), method = "lm") +
  facet_wrap(vars(month), scales = "free")+
  scale_color_manual(values = kovesi.rainbow_bgyr_35_85_c72(7))
