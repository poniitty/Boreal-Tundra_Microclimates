###############################################################################
# This code plots summary figures
#
#
#

library(tidyverse)
library(lubridate)
library(cowplot)
library(viridis)
library(zoo)

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


##############################################################################
# Stacked SD barplots

d <- read_csv("data/all_data_monthly.csv")

plotting_order <- c("MAL","AIL","VAR","TII","PIS","HYY","KAR")
month_order <- as.character(as.yearmon(seq(as_date("2019-11-01"), as_date("2020-10-31"), by = "month"), format = "%b/%Y"))

str_var <- function(x,no){ unlist(lapply(x, function(x){ strsplit(x,"_")[[1]][no]}))}

d %>% select(-starts_with("moist"),-ends_with("_meanmax"),
             -ends_with("_meanmin"),-ends_with("_prop")) %>% 
  pivot_longer(cols = T1_mean:T4_absmin, names_to = "var") %>% 
  mutate(var2 = str_var(var, 2),
         var = str_var(var, 1)) %>%
  filter(is.finite(value)) -> d

d %>% group_by(area, year, month, var, var2) %>% 
  summarise(tsd = sd(value)) -> dsd

# Within-area SD stacked
dsd %>% mutate(area = factor(area, levels = plotting_order)) %>% 
  ggplot(aes(fill=area, y=tsd, x=var)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = viridis(7))+
  facet_grid(cols = vars(month), rows = vars(var2)) + 
  theme_minimal()+
  theme(panel.spacing = unit(0.1, "lines")) +
  ylab("Stacked monthly within-area standard deviation") +
  xlab("Temperature variable")


# How the within-area  variability compares between areas
dsd %>% mutate(area = factor(area, levels = plotting_order)) %>% 
  mutate(ym = ym(paste0(year, "-", month))) %>% 
  mutate(ym = factor(as.yearmon(ym, format = "%b/%Y"), levels = month_order)) %>% 
  ggplot(aes(fill=area, y=tsd, x=var)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = viridis(7))+
  facet_grid(cols = vars(ym), rows = vars(var2)) + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 9))+
  theme(panel.spacing = unit(0.1, "lines")) +
  ylab(NULL) + xlab("Temperature sensor")


dsd %>% mutate(area = factor(area, levels = plotting_order)) %>% 
  mutate(ym = ym(paste0(year, "-", month))) %>% 
  ggplot(aes(fill=area, y=tsd, x=ym)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = viridis(7))+
  facet_grid(cols = vars(var), rows = vars(var2)) + 
  theme_cowplot()+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 10))+
  ylab("Stacked monthly within-area standard deviation") +
  xlab(NULL)

dsd %>% mutate(area = factor(area, levels = plotting_order)) %>% 
  mutate(ym = ym(paste0(year, "-", month))) %>% 
  ggplot(aes(fill=area, y=tsd, x=ym)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = viridis(7))+
  facet_grid(cols = vars(var), rows = vars(var2)) + 
  theme_cowplot()+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 10))+
  xlab(NULL) + ylab(NULL)


#####################################################################
#  Scatter plots

library(pals) # for cyclic color palettes

d <- read_csv("data/all_data_monthly.csv")

plotting_order <- c("MAL","AIL","VAR","TII","PIS","HYY","KAR")
month_order <- as.character(as.yearmon(seq(as_date("2019-11-01"), as_date("2020-10-31"), by = "month"), format = "%b/%Y"))

d %>% 
  ggplot(aes(y = T1_absmin, x = T1_absmax, color = month)) +
  geom_point() +
  scale_color_gradientn(colours = kovesi.cyclic_mrybm_35_75_c68_s25(12))

d %>% 
  ggplot(aes(y = T2_absmin, x = T2_absmax, color = month)) +
  geom_point() +
  scale_color_gradientn(colours = kovesi.cyclic_mrybm_35_75_c68_s25(12))

d %>% 
  ggplot(aes(y = T3_absmin, x = T3_absmax, color = month)) +
  geom_point() +
  scale_color_gradientn(colours = kovesi.cyclic_mrybm_35_75_c68_s25(12))

d %>% 
  ggplot(aes(y = T4_absmin, x = T4_absmax, color = month)) +
  geom_point() +
  scale_color_gradientn(colours = kovesi.cyclic_mrybm_35_75_c68_s25(12))

d %>% filter(T4_absmin < -30) %>% data.table::as.data.table()
