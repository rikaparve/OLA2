# Hent data på hvad danske husholdninger bruger deres penge på fra DST
library(dkstat)
forbrug_meta <- dst_meta(table = "NKH1", lang = "da")
forbrug_meta$values

myquery_forbrug <- list(
  TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

forbrug_meta <- dst_get_data(table = "NKH1", query = myquery_forbrug, lang = "da")
forbrug_meta <- forbrug_meta[-c(1:4),-c(1:3)]
Vaekst <- diff(log(as.numeric(forbrug_meta$value)), lag = 4)*100
forbrug_meta$Vaekst <- c(rep(NA,4),Vaekst)

row.names(forbrug_meta) <- NULL
