### Lav en konfusionmatrix

# Dan en ny dataframe for indikatorerne
predict_forbrug_all <- data.frame(
  Q1_Fam_nu = FTIs_1998$Q1_Fam_nu,
  Q3_Dan_nu = FTIs_1998$Q3_Dan_nu,
  Q5_Forbrugsgoder_nu = FTIs_1998$Q5_Forbrugsgoder_nu,
  Q9_Forbrugsgoder_etår = FTIs_1998$Q9_Forbrugsgoder_etår
)


# Forudsig sandsynlighed for, at forbruget går op (1)
prediction_probabilities <- predict(DI_logistic_model, predict_forbrug_all, type = "response")

# Lav en vector for sandsynligheder
predictions_all <- c()

# Loop  for fortolkning af sandsynligheder
for (i in 1:length(prediction_probabilities)) {
  
  # Check if the current value is NA
  if (is.na(prediction_probabilities[i])) {
    predictions_all[i] <- "unknown"  # Or some other label for missing values
  } else if (prediction_probabilities[i] > 0.5) {
    predictions_all[i] <- "1"
  } else {
    predictions_all[i] <- "0"
  }
}

# Lav en konfusionmatrix
library(caret)

Vækst_factor <- as.factor(FTIs_1998$Dummy_variabel)
prediction_factor <- as.factor(predictions_all)

confusion_matrix <- confusionMatrix(prediction_factor, Vækst_factor)

print(confusion_matrix)
#Reference 0  1
#0         11  4
#1         14 78

# Accurancy: 83,2%

# Lav en ROC kurve for den oprindlige forudsigelse
library(pROC)

roc_original <- roc(Vækst_factor, prediction_probabilities)

plot(roc_original, col="blue", main="ROC kurve for den oprindelige forudsigelse")
