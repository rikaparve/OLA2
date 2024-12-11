####katergorier

#definerer kategorierne baseseret på info givet i opgaven

bystørrelse <- function(Indbyggertal) {
  if (Indbyggertal < 250) {
    return("ikke en by")
  } else if (Indbyggertal < 1000) {
    return("landsby")
  } else if (Indbyggertal < 2500) {
    return("lille by")
  } else if (Indbyggertal < 10000) {
    return("almindelig by")
  } else if (Indbyggertal < 50000) {
    return("større by")
  } else {
    return("storby")
  }
}

city_summary$Størrelse <- sapply(city_summary$Indbyggere, FUN=bystørrelse)
