library(dkstat)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)

####### Hent data på de forskellige forbrugsgrupper
forbrugsgrupper <- dst_meta(table="NAHC021", lang="da")
forbrugsgrupper$values

query_forbrugsgrupper <- list(
  FORMAAAL= c("Fødevarer mv.", "Drikkevarer og tobak mv.",
             "Beklædning og fodtøj", "Boligbenyttelse",
             "Elektricitet, fjernvarme og andet brændsel",
             "Boligudstyr, husholdningstjenester mv.",
             "Medicin, lægeudgifter o.l.",
             "Køb af køretøjer", "Drift af køretøjer og transporttjenester",
             "Information og kommunikation",
             "Fritid, sport og kultur", "Undervisning",
             "Restauranter og hoteller",
             "Forsikring og finansielle tjenester",
             "Andre varer og tjenester"),
  PRISENHED="2020-priser, kædede værdier",
  Tid="*"
)

forbrugsgrupper <- dst_get_data(table="NAHC021", query=query_forbrugsgrupper, lang="da")

# Fjern unødvendig pris kolonne
forbrugsgrupper <- forbrugsgrupper[,-2]

# Justèr fra lang til brede
forbrugsgrupper_wide <- forbrugsgrupper %>% pivot_wider(names_from=FORMAAAL, values_from=value)

# Dan en dataframe for 2020-2023
forbrugsgrupper_period <- forbrugsgrupper_wide[forbrugsgrupper_wide$TID >= "2020-01-01" & 
                                                 forbrugsgrupper_wide$TID <= "2023-01-01", ]


# Pivot til lange format og plot forbruget for 2023
forbrug_2023 <- as.data.frame(as.matrix(forbrugsgrupper_period[4,c(1:16)]))
forbrug_2023 <- forbrug_2023 %>%  pivot_longer(cols = -TID, names_to = "Vare_Service", values_to = "Forbrug")
forbrug_2023$Forbrug <- as.numeric(forbrug_2023$Forbrug)


forbrug_2023$Forbrug <- forbrug_2023$Forbrug/1000 # to remove scientific e
forbrug_2023 <- forbrug_2023 %>%
  mutate(Vare_Service = factor(Vare_Service, levels = Vare_Service[order(Forbrug)]))

# Dan flere grønne farver til at bruge i plotten
green_palette <- colorRampPalette(brewer.pal(9, "Greens"))(15)

ggplot(forbrug_2023, aes(x = Vare_Service, y = Forbrug, fill=Vare_Service)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Centrér og gør titlen fed
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="Forbrugsgrupper", y="Forbrug i  mia. DKK", title="Danske husholdninger brugte fleste penge på boligbenyttelse i 2023") +
  scale_fill_manual(values = green_palette)


# Pivot data fra 2020-2023 fra brede til lange
forbrugsgrupper_period_long <- forbrugsgrupper_period %>% pivot_longer(cols = -TID, names_to = "Vare_Service", values_to = "Forbrug")

# Dan en ny dataframe med kun 2020 og 2023
forbrugsgrupper_difference[,1] <- forbrugsgrupper_period_long[1:15,2]
forbrugsgrupper_difference[,2] <- forbrugsgrupper_period_long[1:15,3]
forbrugsgrupper_difference[,3] <- forbrugsgrupper_period_long[46:60,3]

# Regn forskellerne i %
colnames(forbrugsgrupper_difference) <- c("Vare_Service", "F2020", "F2023")
forbrugsgrupper_difference$percdif <- c((forbrugsgrupper_difference$F2023-forbrugsgrupper_difference$F2020)/forbrugsgrupper_difference$F2020*100)


# Plot forskellerne mellem 2020 og 2023
forbrugsgrupper_difference <- forbrugsgrupper_difference %>%
  mutate(Vare_Service = factor(Vare_Service, levels = Vare_Service[order(percdif)]))

ggplot(forbrugsgrupper_difference, aes(x = Vare_Service,y = percdif, fill=Vare_Service)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # Roter x-aksens labels
  labs(x="Grupper for husholdningernes forbrugsudgifter", y="Forskellerne mellem forbruget fra 2020 til 2023",
       title="Forbrugsudgifter på bekældning og fodtøj og restauranter og hoteller steg mest fra 2020 til 2023") +
  scale_fill_manual(values = green_palette)





