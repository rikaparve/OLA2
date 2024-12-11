library(ggplot2)
library(RColorBrewer)

##### Calculate the historic average

gns_vækst <- data.frame(Land=kolonner, GNS=colMeans(forbrug_vækst[,2:10]))
gns_vækst$Land <- factor(gns_vækst$Land, levels = gns_vækst$Land[order(-gns_vækst$GNS)])

# plot the averages
ggplot(gns_vækst, aes(x=Land, y=GNS, fill=Land)) +
  geom_bar(stat="identity") +
  labs(title="Sverige har den højeste gennemsnitlige kvartalvis årlige realvækst i forbrug for perioden 2000-K1 til 2024-K3",
       x="Land", y="Gennemsnitlig realvækst") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_brewer(palette="Greens")
  