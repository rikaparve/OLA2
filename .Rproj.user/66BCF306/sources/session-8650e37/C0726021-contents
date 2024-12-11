library(skimr)
library(RColorBrewer)


############## I hvilke land faldt gns kv.år. realvækst mest?

forbrug_EU_allelande <- forbrug_EU_meta %>% 
  filter(str_detect(na_item, "P31_S14") & 
           str_detect(unit, "CP_MEUR") & 
           str_detect(s_adj, "SCA") &
           str_detect(time, "^(2019|2020|2021|2022|2023|2024)-(Q[1-2]|Q[3]|Q[4])$"))

# remove the unnecessary columns
forbrug_EU_allelande <- forbrug_EU_allelande[,4:6]

# Pivot to wider
forbrug_EU_allelande <- pivot_wider(
  data = forbrug_EU_allelande, 
  names_from = geo, 
  values_from = values
)

# look for NAs
skim(forbrug_EU_allelande)

# remove UK and remove Q3 2024 because of NAs
forbrug_EU_allelande <- forbrug_EU_allelande[-23,-c(10:13,17,34)]

# Define columns for the loop
kolonner_EU <- c(colnames(forbrug_EU_allelande[2:31]))

## Initiate a new data frame
forbrug_allelande_vækst <- forbrug_EU_allelande[,1]

# Loop over each column in kolonner_EU and add the realvækst data
for (kolonne in kolonner_EU) {
  forbrug_allelande_vækst <- forbrug_allelande_vækst %>%
    mutate(!!paste0("realvækst_", kolonne) := {
      realvækst_EU <- diff(log(as.numeric(forbrug_EU_allelande[[kolonne]])), lag= 4) * 100
      realvækstna_EU <- c(rep(NA, 4), realvækst_EU)
      realvækstna_EU
    })
}

# remove NAs
forbrug_allelande_vækst <- forbrug_allelande_vækst[-c(1:4),]


# Find the minimum value in the dataframe
max_downturn <- min(forbrug_allelande_vækst[2:31])

# Find the row and column index of the minimum value
md_location <- which(forbrug_allelande_vækst == max_downturn, arr.ind = TRUE)

# [3,23]









################## Extra
# calculate the averages
gns_vækst_alle <- data.frame(Land=kolonner_EU, GNS=colMeans(forbrug_allelande_vækst[,2:31]))
gns_vækst_alle$Land <- factor(gns_vækst_alle$Land, levels = gns_vækst_alle$Land[order(-gns_vækst_alle$GNS)])

# Create a color pallette that has 32 options
green_palette <- brewer.pal(9, "Greens")
green_palette <- colorRampPalette(green_palette)(30)


# plot the averages
ggplot(gns_vækst_alle, aes(x=Land, y=GNS, fill=Land)) +
  geom_bar(stat="identity") +
  labs(title="...",
       x="Land", y="Gennemsnitlig realvækst") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_manual(values = green_palette)

