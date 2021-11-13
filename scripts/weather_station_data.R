library(tidyverse)



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
