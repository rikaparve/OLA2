library(tidyverse)
library(dkstat)

# Hent dataen for husholdningernes forbrugsudgifter i kvartaler
forbrugsgrupper_K <- dst_meta(table = "NKHC021", lang = "da")

forbrugsgrupper_K$values

query_forbrugsgrupper_K <- list(
  FORMAAAL=c("Fødevarer mv.", "Drikkevarer og tobak mv.", "Beklædning og fodtøj",
             "Boligbenyttelse","Elektricitet, fjernvarme og andet brændsel",
             "Boligudstyr, husholdningstjenester mv.", "Medicin, lægeudgifter o.l.",
             "Køb af køretøjer", "Drift af køretøjer og transporttjenester",
             "Information og kommunikation", "Fritid, sport og kultur",
             "Undervisning", "Restauranter og hoteller", "Forsikring og finansielle tjenester",
             "Andre varer og tjenester"),
  PRISENHED=c("2020-priser, kædede værdier"),
  SÆSON=c("Sæsonkorrigeret"),
  Tid="*"
)


forbrugsgrupper_K <- dst_get_data(table = "NKHC021", query = query_forbrugsgrupper_K, lang = "da")
forbrugsgrupper_K <- forbrugsgrupper_K[,-c(2,3)]

# Sæt indikatorerne i forskellige kolonner
forbrugsgrupper_K <- forbrugsgrupper_K %>% pivot_wider(names_from = FORMAAAL, values_from = value)
forbrugsgrupper_K <- forbrugsgrupper_K[-c(1:4),]

#Skær dataframen til ønsket tidsramme
forbrugsgrupper_K <- forbrugsgrupper_K[-c(1:36),]

# Tilføj DST FTI og DI FTI til dataframen
forbrugsgrupper_K$DST_FTI <- FTIs$DST_FTI
forbrugsgrupper_K$DI_FTI <- FTIs$DI_FTI

# Lav simple linear regressioner
# Justér kolonnenavner for SLR modellerne
colnames(forbrugsgrupper_K) <- c("Tid","Fødevarer", "Drikkevarer_tobak",
                                  "Beklædning_fodtøj","Boligbenyttelse",
                                  "Elektricitet_fjernvarme_andet_brændsel",
                                  "Boligudstyr_husholdningstjenester","Medicin_lægeudgifter",
                                  "Køb_af_køretøjer","Drift_af_køretøjer_transporttjenester",
                                  "Information_kommunikation","Fritid_sport_kultur",
                                  "Undervisning","Restauranter_hoteller",
                                  "Forsikring_finansielletjenester","Andrevarer_tjenester",
                                  "DST_FTI", "DI_FTI")

# Vælg variabler
independent_vars <- c(colnames(forbrugsgrupper_K[2:16]))

dependent_vars <- c("DI_FTI", "DST_FTI")

# Dan en ny liste og løb loop'et
regres_liste <- list()

for (dependent in dependent_vars) {
  regres_liste[[dependent]] <- list()
  for (independent in independent_vars) {
    model <- lm(as.formula(paste(dependent,"~",independent)), data = forbrugsgrupper_K)
    regres_liste[[dependent]][[independent]] <- summary(model)
  }
}

# Pull an example
regres_liste[["DI_FTI"]][["Fødevarer"]]
  
