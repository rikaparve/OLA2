library(dkstat)
library(tidyverse)

##### find data på befolkningen i byer
indbyggersearch <- dst_search(string="befolkningen", field="text")
indbygger_meta <- dst_meta(table="POSTNR1", lang="da")

indbygger_meta$values
indbygger_meta$variables
query_indbygger <- list(PNR20 = "*",
                        Tid = "2024")

indbygger_df <- dst_get_data(table= "POSTNR1", query= query_indbygger, lang= "da")

indbygger_df <- indbygger_df[-1,-2]

pattern <- "^(\\d{4})\\s+(.*?)\\s*\\((.*?)\\)$"
location <- str_match(indbygger_df$PNR20, pattern)

postnummer <- location[,2]
By <- location[,3]
kommune <- location[,4]

indbygger_df_clean <- data.frame(postnr=as.integer(postnummer), By, kommune, Indbyggertal=indbygger_df$value)
unique(indbygger_df_clean$By)

# Aggregate all the cities with many postcodes, like CPH, FRB gives extra problems as it has one extra where there are two kommunes on
indbygger_df_clean <- indbygger_df_clean %>%
  mutate(
    kommune = ifelse(str_detect(kommune, "Frederiksberg"), "Frederiksberg Kommune", kommune),
    Byer = case_when(
      str_detect(By, "^København") ~ "København",
      str_detect(By, "^Frederiksberg") ~ "Frederiksberg",
      str_detect(By, "^Aarhus") ~ "Aarhus",
      str_detect(By, "^Aalborg") ~ "Aalborg",
      str_detect(By, "^Esbjerg") ~ "Esbjerg",
      str_detect(By, "^Odense") ~ "Odense",
      str_detect(By, "^Randers") ~ "Randers",
      TRUE ~ By
    )
  )

##### Group by consolidated city and sum the residents
city_summary <- indbygger_df_clean %>%
  group_by(Byer, kommune) %>%
  summarise(
    postnr = list(postnr),
    Indbyggere = sum(Indbyggertal),
    .groups = 'drop'
  )

##### Unnest postcodes in city_summary to create one row per postcode (so the type of the data in postnr column can be matched to boligsiden)
city_summary <- city_summary %>%
  unnest(postnr)

