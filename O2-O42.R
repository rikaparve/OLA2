# Lav en dataframe for større forbrugsgoder
Forbrugsgoder <- ftillid_K[,c(1,7)]

# Skær til den specifik tidsperiod
lv <- Forbrugsgoder$TID >= as.Date("2000-01-01") & Forbrugsgoder$TID <= as.Date("2024-12-31")
Forbrugsgoder <- Forbrugsgoder[lv,]

# Regn gennemsnittet
forbrugsgoder_mean = mean(Forbrugsgoder$Q5_Forbrugsgoder_nu)

# Plot resultaten
library(ggplot2)
ggplot(Forbrugsgoder, aes(x = TID, y = Q5_Forbrugsgoder_nu)) +
  geom_line(aes(color = "Forbrugsgoder", linetype = "Forbrugsgoder"), size = 0.5) +
  geom_point(aes(color = "Forbrugsgoder"), size = 1) +
  geom_hline(aes(yintercept = forbrugsgoder_mean, color = "Gennemsnit", linetype = "Gennemsnit"), size = 1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    x = "Kvartaler",
    y = "DSTs indikator for at anskaffe større forbrugsgoder",
    title = "Udvikling af indikatoren for køb af større forbrugsgoder",
    color = "Linjer",
    linetype = "Linjer"
  ) +
  scale_color_manual(values = c("Forbrugsgoder" = "black", "Gennemsnit" = "orange")) +
  scale_linetype_manual(values = c("Forbrugsgoder" = "solid", "Gennemsnit" = "solid")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2000-03-01", "2024-09-01"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Svarmuligheder

#Fordelagtigt at købe nu 100,
#Hverken fordelagtigt eller ufordelagtigt at købe nu 0,
#Ufordelagtigt at købe nu, bedre at vente -100
