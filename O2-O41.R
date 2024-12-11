# Find min og max
max_x <- ftillid_K$TID[which.max(ftillid_K$FTI)]
min_x <- ftillid_K$TID[which.min(ftillid_K$FTI)]
max_forbruger <- max(ftillid_K$FTI)
min_forbruger <- min(ftillid_K$FTI)

# Plot dataen
library(ggplot2)
ggplot(ftillid_K, aes(x=TID, y=FTI), size=2) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_point(aes(x = max_x, y = max_forbruger, color="Højeste 2006 K1"), size=5) +
  geom_point(aes(x = min_x, y = min_forbruger, color="Laveste 2022 K4"), size=5) +
  scale_color_manual(values = c("Højeste 2006 K1" = "green", "Laveste 2022 K4" = "red")) +
  labs(x="Kvartaler", y= "DSTs forbrugertillidsindikator", title="Udviklingen af forbrugertillidsindikatoren fra Danmarks Statistik") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("1996-03-01", "2024-09-01"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))













       