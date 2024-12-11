# Make predictions for DI and DST
DI_K4 <- mean(c(
  ftillid_wide$Q1_Fam_nu[346:347],
  ftillid_wide$Q3_Dan_nu[346:347],
  ftillid_wide$Q5_Forbrugsgoder_nu[346:347],
  ftillid_wide$Q9_Forbrugsgoder_etår[346:347]))

DI_ind <- data.frame(DI_FTI = DI_K4)

DI_predict <- predict(DI_FTI_model, DI_ind, type="response")
DI_predict

DST_K4 <- mean(c(ftillid_wide$FTI[346],
               ftillid_wide$FTI[347]))

DST_ind <- data.frame(DST_FTI = DST_K4)

DST_predict <- predict(DST_FTI_model, DST_ind, type="response")
DST_predict

