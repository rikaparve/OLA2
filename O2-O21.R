library(corrplot)
library(ggplot2)

# Gather the time, realvækst and FTIs in one dataframe
FTIs <- data.frame(
  År = ftillid_K$TID[1:115],
  Realvaekst = forbrug_meta$Vaekst[21:135],
  DST_FTI = ftillid_K$FTI[1:115],
  DI_FTI = rowMeans(ftillid_K[1:115,c(3,5,7,11)])
)

# Adjust to the period 2000 Q1 - Q3 2024 (because that's in the article)
FTIs <- FTIs[-c(1:16),]
row.names(FTIs) <- NULL

# Find the R^2 for DI FTI
DI_FTI_model <- lm(Realvaekst ~ DI_FTI, data=FTIs)
summary(DI_FTI_model)
# R2 <- 35,8%

# Find the R^2 for DST FTI
DST_FTI_model <- lm(Realvaekst ~ DST_FTI, data=FTIs)
summary(DST_FTI_model)
# R2 <- 30,2%

# Correlationmatrix
cor_matrix_DI <- cor(y=FTIs$Realvaekst, x=FTIs$DI_FTI)
cor_matrix_DST <- cor(y=FTIs$Realvaekst, x=FTIs$DST_FTI)

# Historic averages
DI_gns <- mean(FTIs$DI_FTI)
DST_gns <- mean(FTIs$DST_FTI)

# Plot
ggplot(FTIs, aes(x = År)) +
  geom_bar(aes(y = Realvaekst), stat = "identity", fill = "black", alpha = 0.5) +
  geom_line(aes(y = DI_FTI, color = "Dansk Industri"), size = 0.8) +
  geom_hline(aes(yintercept = DI_gns, color = "Dansk Industri"), linetype = "dotted", size = 1) +
  geom_line(aes(y = DST_FTI, color = "Danmarks Statistik"), size = 0.8) +
  geom_hline(aes(yintercept = DST_gns, color = "Danmarks Statistik"), linetype = "dotted", size = 1) +
  labs(
    title = "Forbrugertillidsindikatorerne følger udviklingen i realvæksten af forbruget",
    x = "Tidsperiode 2000 K1 - 2024 K3",
    y = "Den årlige realvækst af forbruget pr. kvartal",
    color = "Forbrugertillidsindikator",  
    linetype = "Historisk gennemsnit" 
  )


### With the incorrect historic average legend
ggplot(FTIs, aes(x = År)) +
  geom_bar(aes(y = Realvaekst), stat = "identity", fill = "black", alpha = 0.5) +
  geom_line(aes(y = DI_FTI, color = "Dansk Industri"), size = 0.8) +
  geom_hline(aes(yintercept = DI_gns, color = "Dansk Industri", linetype = "Historisk gennemsnit"), size = 1) +
  geom_line(aes(y = DST_FTI, color = "Danmarks Statistik"), size = 0.8) +
  geom_hline(aes(yintercept = DST_gns, color = "Danmarks Statistik", linetype = "Historisk gennemsnit"), size = 1) +
  labs(
    title = "Forbrugertillidsindikatorerne følger udviklingen i realvæksten af forbruget",
    x = "Tidsperiode 2000 K1 - 2024 K3",
    y = "Den årlige realvækst af forbruget pr. kvartal",
    color = "Forbrugertillidsindikator",
    linetype = "Historisk gennemsnit"
  ) +
  scale_linetype_manual(values = c("Historisk gennemsnit" = "dotted"))


#Realvækst
(0.6958+(-0.5114)+0.8485+0.2323)/4
