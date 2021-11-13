library(tidyverse)
library(viridis)
library(lubridate)

# Study-period average temperatures from the closest weather station to the study areas

d <- read_csv("data/fmi_daily.csv")

d %>% group_by(FMISID, area) %>% 
  summarise(n_days = n(),
            Tavg = mean(tday))


########################################################
# Plot station time series

# This is needed to plot the dates in English if your system language is something else
Sys.setlocale("LC_TIME", "english")

plotting_order <- c("MAL","AIL","VAR","TII","PIS","HYY","KAR")

d %>% mutate(area = factor(area, levels = plotting_order)) %>% 
  ggplot(aes(y = tday, x = time, color = area)) +
  geom_line(size = 0.2)+
  geom_smooth(span = 0.15, size = 1.2)+
  scale_color_manual(values = viridis(7))


# Plot against 30-year daily averages

d <- read_csv("data/fmi_daily.csv")
avgs <- read_csv("data/fmi_30years_daily_averages.csv")

plotting_order <- c("MAL & AIL","VAR","TII","PIS","HYY","KAR")


d %>% mutate(yday = yday(time)) %>% 
  left_join(avgs) %>% 
  mutate(area = ifelse(area == "MAL","MAL & AIL",area)) %>% 
  filter(area != "AIL") -> d

d %>% mutate(area = factor(area, levels = plotting_order)) %>% 
  ggplot(aes(x = time))+
  geom_abline(slope = 0, intercept = 0, linetype = 2, size = 0.5)+
  geom_line(aes(y = tday), color = "gray70", size = 0.2)+
  geom_smooth(aes(y = Tavg30),span = 0.2, size = 1, color = "black")+
  geom_smooth(aes(y = tday),span = 0.2, size = 1, color = "red")+
  facet_wrap(vars(area))+
  theme_bw() + 
  ylab("Daily mean temperature") + xlab(NULL) -> GG_st

ggsave("visuals/Weather_station_timeseries_SUPPL.pdf",
       GG_st,
       width = 20, height = 16, units = "cm")

# ########################################################################
# # PREPROCESS THE FMI DATA
# # This has to be done only once
# 
# dd <- read_csv("C:/datacloud/biogeoclimate/microclimate/data/weather/fmi_daily.csv")
# dh <- read_csv("C:/datacloud/biogeoclimate/microclimate/data/weather/fmi_hourly.csv")
# st <- read_csv("C:/datacloud/biogeoclimate/microclimate/data/weather/fmi_stations.csv")
# 
# areas <- c("MAL","AIL","VAR","TII","PIS","HYY","KAR")
# 
# st$area <- NA
# for(i in areas){
#   st %>% select(FMISID, first_date, last_date, i) %>%
#     filter(first_date <= "2019-11-01",
#            last_date >= "2020-10-31") -> temp
# 
#   st$area[st$FMISID == temp$FMISID[which.min(as_vector(temp[i]))]] <- i
# 
# }
# 
# st %>% filter(!is.na(area)) %>%
#  select(FMISID, area) -> st2
# 
# # Daily 30y averages
# dd %>% left_join(st2) %>%
#   filter(!is.na(area)) %>%
#   filter(time >= "1991-01-01",
#          time <= "2020-12-31") %>%
#   filter(is.finite(tday)) %>% 
#   mutate(yday = yday(time)) %>%
#   group_by(FMISID, area, yday) %>%
#   summarise(n_days = n(),
#             Tavg30 = mean(tday)) -> Tavg30
# 
# # Check for missing data
# Tavg30 %>% filter(is.na(Tavg30))
# Tavg30 %>% filter(n_days < 30) %>% arrange(n_days) # This seems ok
# 
# write_csv(Tavg30, "data/fmi_30years_daily_averages.csv")
# 
# 
# # Filter the study period
# dd %>% left_join(st2) %>%
#   filter(!is.na(area)) %>%
#   filter(time >= "2019-11-01",
#          time <= "2020-10-31") -> dd
# 
# dh %>% left_join(st2) %>%
#   filter(!is.na(area)) %>%
#   filter(time >= "2019-11-01",
#          time <= "2020-10-31") -> dh
# 
# 
# # Check the number of observations
# dd %>% group_by(area) %>%
#   count() # Great!
# 
# dh %>% group_by(area) %>%
#   count() # Very small differences. This is fine!
# 
# # Write daily data
# write_csv(dd, "C:/datacloud/biogeoclimate/microclimate/data/logger/data_article/fmi_daily.csv")
# write_csv(dd, "data/fmi_daily.csv")
# 
# write_csv(dh, "C:/datacloud/biogeoclimate/microclimate/data/logger/data_article/fmi_hourly.csv")
# write_csv(dh, "data/fmi_hourly.csv")
# 
# write_csv(st %>% filter(!is.na(area)), "C:/datacloud/biogeoclimate/microclimate/data/logger/data_article/fmi_stations.csv")
# write_csv(st %>% filter(!is.na(area)), "data/fmi_stations.csv")
# 
# 
