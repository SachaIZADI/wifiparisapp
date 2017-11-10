library(dplyr)
library(wifiparis)
filter_DB <- as.data.frame(Data_Viz_1) %>% select(- start_time) %>%  select(-duration) %>% unique()
save(filter_DB, file = "filter_DB",compress = TRUE)

load("filter_DB")
filter_DB %>% View()
