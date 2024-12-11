library(dkstat)
library(reshape)

# Hent data om forbrugertillid fra DST
ftillid_meta <- dst_meta(table = "FORV1", lang = "da")
str(ftillid_meta)

query_FTI <- list(
  INDIKATOR = "*",
  Tid = "*"
)

ftillid_meta <- dst_get_data(table = "FORV1", query = query_FTI, lang = "da")

# Lav dataframen om så alle indikatorer er i forskellige kolonner
ftillid_wide <- reshape(ftillid_meta, 
                        idvar = "TID", 
                        timevar = "INDIKATOR", 
                        direction = "wide")

colnames(ftillid_wide)= c("TID", "FTI", "Q1_Fam_nu", "Q2_Fam_etår", "Q3_Dan_nu", "Q4_Dan_etår", 
                          "Q5_Forbrugsgoder_nu", "Q6_Priser_nu", "Q7_Priser_etår", 
                          "Q8_Arbejdsløshed", "Q9_Forbrugsgoder_etår", "Q10_Sparop_nu", 
                          "Q11_Sparop_etår", "Q12_Famøk_nu")


# Q1_Fam_nu - Familiens økonomiske situation i dag, sammenlignet med for et år siden"                      
# Q2_Fam_etår - Familiens økonomiske  situation om et år, sammenlignet med i dag"                            
# Q3_Dan_nu - Danmarks økonomiske situation i dag, sammenlignet med for et år siden"                       
# Q4_Dan_etår - Danmarks økonomiske situation om et år, sammenlignet med i dag"                              
# Q5_Forbrugsgoder_nu - Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"                            
# Q6_Priser_nu - Priser i dag, sammenlignet med for et år siden"                                              
# Q7_Priser_etår - Priser om et år, sammenlignet med i dag"                                                     
# Q8_Arbejdsløshed - Arbejdsløsheden om et år, sammenlignet med i dag"                                            
# Q9_Forbrugsgoder_etår - Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."                             
# Q10_Sparop_nu - Anser det som fornuftigt at spare op i den nuværende økonomiske situation"                   
# Q11_Sparop_etår - Regner med at kunne spare op i de kommende 12 måneder"                                       
# Q12_Famøk_nu - Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener"

## Lav dataen om til kvartaler
ftillid_wide <- ftillid_wide[-c(1:255),]
row.names(ftillid_wide) <- NULL

ftillid_K <- as.data.frame(matrix(data=NA, ncol=14, nrow=0))
colnames(ftillid_K)= c("TID", "FTI", "Q1_Fam_nu", "Q2_Fam_etår", "Q3_Dan_nu", "Q4_Dan_etår", 
                       "Q5_Forbrugsgoder_nu", "Q6_Priser_nu", "Q7_Priser_etår", 
                       "Q8_Arbejdsløshed", "Q9_Forbrugsgoder_etår", "Q10_Sparop_nu", 
                       "Q11_Sparop_etår", "Q12_Famøk_nu")

# Lav indikatorerne om til kvartaler med loop
for (i in seq(3, nrow(ftillid_wide), by = 3)) {
  
  # Tidsperioden
  År <- ftillid_wide$TID[i]
  
  # Regn kvartal-baseret gennemsniter for indikatorer
  FTI <- mean(ftillid_wide$FTI[(i-2):i], na.rm = TRUE)
  Q1_Fam_nu <- mean(ftillid_wide$Q1_Fam_nu[(i-2):i], na.rm = TRUE)
  Q2_Fam_etår <- mean(ftillid_wide$Q2_Fam_etår[(i-2):i], na.rm = TRUE)
  Q3_Dan_nu <- mean(ftillid_wide$Q3_Dan_nu[(i-2):i], na.rm = TRUE)
  Q4_Dan_etår <- mean(ftillid_wide$Q4_Dan_etår[(i-2):i], na.rm = TRUE)
  Q5_Forbrugsgoder_nu <- mean(ftillid_wide$Q5_Forbrugsgoder_nu[(i-2):i], na.rm = TRUE)
  Q6_Priser_nu <- mean(ftillid_wide$Q6_Priser_nu[(i-2):i], na.rm = TRUE)
  Q7_Priser_etår <- mean(ftillid_wide$Q7_Priser_etår[(i-2):i], na.rm = TRUE)
  Q8_Arbejdsløshed <- mean(ftillid_wide$Q8_Arbejdsløshed[(i-2):i], na.rm = TRUE)
  Q9_Forbrugsgoder_etår <- mean(ftillid_wide$Q9_Forbrugsgoder_etår[(i-2):i], na.rm = TRUE)
  Q10_Sparop_nu <- mean(ftillid_wide$Q10_Sparop_nu[(i-2):i], na.rm = TRUE)
  Q11_Sparop_etår <- mean(ftillid_wide$Q11_Sparop_etår[(i-2):i], na.rm = TRUE)
  Q12_Famøk_nu <- mean(ftillid_wide$Q12_Famøk_nu[(i-2):i], na.rm = TRUE)
  
  
  # Sample gemmensnitter i en ny dataframe
  ftillid_K <- rbind(ftillid_K, 
                     data.frame(TID = År,
                                FTI = FTI,
                                Q1_Fam_nu = Q1_Fam_nu,
                                Q2_Fam_etår = Q2_Fam_etår,
                                Q3_Dan_nu = Q3_Dan_nu,
                                Q4_Dan_etår = Q4_Dan_etår,
                                Q5_Forbrugsgoder_nu = Q5_Forbrugsgoder_nu,
                                Q6_Priser_nu = Q6_Priser_nu,
                                Q7_Priser_etår = Q7_Priser_etår,
                                Q8_Arbejdsløshed = Q8_Arbejdsløshed,
                                Q9_Forbrugsgoder_etår = Q9_Forbrugsgoder_etår,
                                Q10_Sparop_nu = Q10_Sparop_nu,
                                Q11_Sparop_etår = Q11_Sparop_etår,
                                Q12_Famøk_nu = Q12_Famøk_nu
                     ))
}

row.names(ftillid_K) <- NULL
