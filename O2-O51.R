library(eurostat)
library(restatapi)
library(tidyverse)

# Find data på forbruget
search_EU <- search_eurostat("consumption")

# namq_10_fcs

forbrug_EU_values <- get_eurostat_dsd("namq_10_fcs") # overview of values
forbrug_EU_meta <- get_eurostat_data("namq_10_fcs") # get the data

# filter the raw data
forbrug_EU <- forbrug_EU_meta %>% 
  filter(str_detect(geo, "DK|BE|NL|SE|AT|DE|FR|IT|ES") &
         str_detect(na_item, "P31_S14") &
         str_detect(unit, "CLV20_MEUR") &
         str_detect(s_adj, "SCA"))

forbrug_EU <- forbrug_EU[,4:6] # remove unnecessary columns

forbrug_EU <- forbrug_EU %>% pivot_wider(names_from = geo, values_from = values) # pivot to per country

# arrange after the year
# As time is as factors -> order the levels
# Convert time to an ordered factor
forbrug_EU$time <- factor(forbrug_EU$time, levels = sort(unique(forbrug_EU$time)), ordered = TRUE)
forbrug_EU <- arrange(forbrug_EU, time)
forbrug_EU <- forbrug_EU %>%
  filter(time >= "1999-Q1" & time <= "2024-Q2")


#### calculate the y-o-y growth
kolonner <- c("Østrig","Belgium","Tyskland","Danmark","Spanien","Frankrig","Italien","Holland","Sverige")

# Initialize the new dataframe with the 'time' column
forbrug_vækst <- data.frame(time = forbrug_EU$time)

# Loop through each column and calculate realvækst
for (kolonne in kolonner) {
  realvækst <- c(rep(NA, 4), diff(log(as.numeric(forbrug_EU[[kolonne]])), lag = 4) * 100)
  forbrug_vækst <- cbind(forbrug_vækst, setNames(data.frame(realvækst), paste0("realvækst_", kolonne)))
}

forbrug_vækst <- forbrug_vækst[-c(1:4),]
row.names(forbrug_vækst) <- NULL
