### Fjerne koronakrisen og regne den nye gennemsnit

forbrug_vækst_uden_korona <- forbrug_vækst[!(forbrug_vækst$time >= "2020-Q1" & forbrug_vækst$time <= "2021-Q1"), ]

gns_vækst_uden_korona <- data.frame(Land=kolonner, GNS=colMeans(forbrug_vækst_uden_korona[,2:10]))
gns_vækst_uden_korona$Land <- factor(gns_vækst_uden_korona$Land, levels = gns_vækst_uden_korona$Land[order(-gns_vækst_uden_korona$GNS)])

# plot the averages
ggplot(gns_vækst_uden_korona, aes(x=Land, y=GNS, fill=Land)) +
  geom_bar(stat="identity") +
  labs(title="Sverige har den højeste gennemsnitlige realvækst i forbrug for perioden 2000-Q1 til 2024-Q2 uden koronatiden",
       x="Land", y="Gennemsnitlig realvækst") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_brewer(palette="Greens")



#################### Put the two charts together

# Combine the data
gns_vækst$Type <- "Med Corona-kvartaler"
gns_vækst_uden_korona$Type <- "Uden Corona-kvartaler"

combined_gns <- rbind(gns_vækst, gns_vækst_uden_korona)

# Create the bar chart
ggplot(combined_gns, aes(x=Land, y=GNS, fill=Type)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Gennemsnitlig realvækst i forbrug: Med og uden koronatiden",
       x="Land", y="Gennemsnitlig realvækst") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

