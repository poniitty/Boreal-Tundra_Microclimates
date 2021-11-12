library(tidyverse)
library(lubridate)
library(cowplot)

# This is needed to plot the dates in English if your system language is something else
Sys.setlocale("LC_TIME", "english")

d <- read_csv("data/all_data_monthly.csv")

plotting_order <- c("MAL","AIL","VAR","TII","PIS","HYY","KAR")

# check if study areas are as planned
unique(d$area)


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

