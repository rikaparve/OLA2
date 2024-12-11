library(tidyverse)

### Merge bystørrelses og boligsidens dataframer
# Get the boligsiden file
boligsiden_renset <- readRDS("data/Boligsiden_renset.rds")

# Check that the postal code column name and type are the same
colnames(boligsiden_renset)
colnames(city_summary)
str(boligsiden_renset)
str(city_summary)

# Perform the left join
merged_data <- boligsiden_renset %>%
  left_join(city_summary, by = "postnr")


