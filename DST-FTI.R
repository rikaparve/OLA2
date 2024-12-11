library("devtools")
library(dkstat)

# Retrieve data on forbrugertillid
ftillid_meta <- dst_meta(table = "FORV1", lang = "da")
str(ftillid_meta)

myquery <- list(
  INDIKATOR = "*",
  Tid = "*"
)

ftillid_meta2 <- dst_get_data(table = "FORV1", query = myquery, lang = "da")

# Put the indicators in different columns
library(reshape)
ftillid_wide <- reshape(ftillid_meta2, 
                     idvar = "TID", 
                     timevar = "INDIKATOR", 
                     direction = "wide")

colnames(ftillid_wide)= c("TID", "FTI", "Famnu", "Fametår", "Dannu", "Danetår", "Forbrugsgodernu", "Prisernu", "Priseretår", "AL", "Forbrugsgoderetår", "Sparopnu", "Sparopetår", "Famøknu")
                          

# Famnu - Familiens økonomiske situation i dag, sammenlignet med for et år siden"                      
# Fametår - Familiens økonomiske  situation om et år, sammenlignet med i dag"                            
# Dannu - Danmarks økonomiske situation i dag, sammenlignet med for et år siden"                       
# Danetår - Danmarks økonomiske situation om et år, sammenlignet med i dag"                              
# Forbrugsgodernu - Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"                            
# Prisernu - Priser i dag, sammenlignet med for et år siden"                                              
# Priseretår - Priser om et år, sammenlignet med i dag"                                                     
# AL - Arbejdsløsheden om et år, sammenlignet med i dag"                                            
# Forbrugsgoderetår - Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."                             
# Sparopnu - Anser det som fornuftigt at spare op i den nuværende økonomiske situation"                   
# Sparopetår - Regner med at kunne spare op i de kommende 12 måneder"                                       
# Famøknu - Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener"

# Convert the data into quarters
ftillid_wide <- ftillid_wide[-c(1:255),]

ftillid_K <- as.data.frame(matrix(data=NA, ncol=8, nrow=197))
colnames(ftillid_K)= c("TID", "FTI", "Famnu", "Fametår", "Dannu", "Danetår", "Forbrugsgodernu", "Forbrugsgoderetår")

# FTI
rowindextilFTI=1

for (i in seq(1,nrow(ftillid_wide), by=3)) {
  if (i + 2 <= nrow(ftillid_wide)) {
      FTI1 = ftillid_wide$FTI[i]
      FTI2 = ftillid_wide$FTI[i+1]
      FTI3 = ftillid_wide$FTI[i+2]
      meanFTI = mean(c(FTI1,FTI2,FTI3))
      rowindextilFTI = rowindextilFTI+1
      ftillid_K$FTI[rowindextilFTI] = meanFTI
    }
  }

#Famnu
rowindextilFTI=1

for (i in seq(1,nrow(ftillid_wide), by=3)) {
  if (i + 2 <= nrow(ftillid_wide)) {
    FTI1 = ftillid_wide$Famnu[i]
    FTI2 = ftillid_wide$Famnu[i+1]
    FTI3 = ftillid_wide$Famnu[i+2]
    meanFTI = mean(c(FTI1,FTI2,FTI3))
    rowindextilFTI = rowindextilFTI+1
    ftillid_K$Famnu[rowindextilFTI] = meanFTI
  }
}

#Fametår
rowindextilFTI=1

for (i in seq(1,nrow(ftillid_wide), by=3)) {
  if (i + 2 <= nrow(ftillid_wide)) {
    FTI1 = ftillid_wide$Fametår[i]
    FTI2 = ftillid_wide$Fametår[i+1]
    FTI3 = ftillid_wide$Fametår[i+2]
    meanFTI = mean(c(FTI1,FTI2,FTI3))
    rowindextilFTI = rowindextilFTI+1
    ftillid_K$Fametår[rowindextilFTI] = meanFTI
  }
}

#Dannu
rowindextilFTI=1

for (i in seq(1,nrow(ftillid_wide), by=3)) {
  if (i + 2 <= nrow(ftillid_wide)) {
    FTI1 = ftillid_wide$Dannu[i]
    FTI2 = ftillid_wide$Dannu[i+1]
    FTI3 = ftillid_wide$Dannu[i+2]
    meanFTI = mean(c(FTI1,FTI2,FTI3))
    rowindextilFTI = rowindextilFTI+1
    ftillid_K$Dannu[rowindextilFTI] = meanFTI
  }
}

#Danetår
rowindextilFTI=1

for (i in seq(1,nrow(ftillid_wide), by=3)) {
  if (i + 2 <= nrow(ftillid_wide)) {
    FTI1 = ftillid_wide$Danetår[i]
    FTI2 = ftillid_wide$Danetår[i+1]
    FTI3 = ftillid_wide$Danetår[i+2]
    meanFTI = mean(c(FTI1,FTI2,FTI3))
    rowindextilFTI = rowindextilFTI+1
    ftillid_K$Danetår[rowindextilFTI] = meanFTI
  }
}

#Forbrugsgodernu
rowindextilFTI=1

for (i in seq(1,nrow(ftillid_wide), by=3)) {
  if (i + 2 <= nrow(ftillid_wide)) {
    FTI1 = ftillid_wide$Forbrugsgodernu[i]
    FTI2 = ftillid_wide$Forbrugsgodernu[i+1]
    FTI3 = ftillid_wide$Forbrugsgodernu[i+2]
    meanFTI = mean(c(FTI1,FTI2,FTI3))
    rowindextilFTI = rowindextilFTI+1
    ftillid_K$Forbrugsgodernu[rowindextilFTI] = meanFTI
  }
}

#Forbrugsgoderetår
rowindextilFTI=1

for (i in seq(1,nrow(ftillid_wide), by=3)) {
  if (i + 2 <= nrow(ftillid_wide)) {
    FTI1 = ftillid_wide$Forbrugsgoderetår[i]
    FTI2 = ftillid_wide$Forbrugsgoderetår[i+1]
    FTI3 = ftillid_wide$Forbrugsgoderetår[i+2]
    meanFTI = mean(c(FTI1,FTI2,FTI3))
    rowindextilFTI = rowindextilFTI+1
    ftillid_K$Forbrugsgoderetår[rowindextilFTI] = meanFTI
  }
}

#Quarters
rowindextilFTI=1
for (i in seq(1,nrow(ftillid_wide), by=3)) {
  if (i + 2 <= nrow(ftillid_wide)) {
    X=ftillid_wide$TID[i]  
    rowindextilFTI = rowindextilFTI+1
    ftillid_K$TID[rowindextilFTI]=X
  }
}

ftillid_K$TID <- NA
TID <- c(rep(1996:2024, each=4))
ftillid_K$TID[2:117] <- TID

lv2 <- ftillid_K$TID %in% c(1996:2024)
ftillid_K <- ftillid_K[lv2,]
ftillid_K <- ftillid_K[-116,]

library(ggplot2)
ggplot(ftillid_K, aes(x=TID, y=FTI)) +
  geom_line() +
  geom_point()

       