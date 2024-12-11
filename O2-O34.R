library(caret)
library(pROC)


######## Scenario 1: optimal threshold
#Extract sensitivity, specificity, and thresholds
sensitivities <- roc_original$sensitivities
specificities <- roc_original$specificities
thresholds <- roc_original$thresholds

#Calculate Youden's J statistic
youden_j <- sensitivities + specificities - 1

#Find the index of the maximum J statistic
optimal_index <- which.max(youden_j)

#Get the optimal threshold
optimal_threshold <- thresholds[optimal_index]


#### Convert the predictions
prediction_sce1 <- c()

for (i in 1:length(prediction_probabilities)) {
  
  # Check if the current value is NA
  if (is.na(prediction_probabilities[i])) {
    prediction_sce1[i] <- "unknown"  # Or some other label for missing values
  } else if (prediction_probabilities[i] > 0.7252824) {
    prediction_sce1[i] <- "1"
  } else {
    prediction_sce1[i] <- "0"
  }
}

prediction_sce1 <- as.factor(prediction_sce1)

confusion_matrix_sce1 <- confusionMatrix(prediction_sce1, Vækst_factor)

print(confusion_matrix_sce1)
#Reference  0  1
#0          17 18
#1          8 64

# accurancy: 75,7%


##### Scenario 2: remove insignificant variables and run the model again
# Remove Fam_nu
logistic_model_sce21 <- glm(Dummy_variabel ~ Q3_Dan_nu + Q5_Forbrugsgoder_nu + Q9_Forbrugsgoder_etår,
                            data = FTIs_1998, family = binomial)
summary(logistic_model_sce21)

#Remove Forbrugsgoder_nu
logistic_model_sce22 <- glm(Dummy_variabel ~ Q3_Dan_nu + Q9_Forbrugsgoder_etår,
                         data = FTIs_1998, family = binomial)
summary(logistic_model_sce22)

#Remove Forbrugsgoder_etår
logistic_model_sce23 <- glm(Dummy_variabel ~ Q3_Dan_nu,
                            data = FTIs_1998, family = binomial)
summary(logistic_model_sce23)

# Dan en ny dataframe for indikatoren
predict_forbrug_sce23 <- data.frame(
  Q3_Dan_nu = FTIs_1998$Q3_Dan_nu)

# Forudsig sandsynlighed for, at forbruget går op (1)
prediction_probabilities_sce23 <- predict(logistic_model_sce23, predict_forbrug_sce23, type = "response")

# Lav en vector for sandsynligheder
prediction_sce23 <- c()

# Loop  for fortolkning af sandsynligheder
for (i in 1:length(prediction_probabilities_sce23)) {
  
  # Check if the current value is NA
  if (is.na(prediction_probabilities_sce23[i])) {
    prediction_sce23[i] <- "unknown"  # Or some other label for missing values
  } else if (prediction_probabilities_sce23[i] > 0.5) {
    prediction_sce23[i] <- "1"
  } else {
    prediction_sce23[i] <- "0"
  }
}

# Lav en konfusionmatrix
prediction_sce23 <- as.factor(prediction_sce23)

confusion_matrix_sce23 <- confusionMatrix(prediction_sce23, Vækst_factor)
print(confusion_matrix_sce23)
#Reference 0  1
#0       12  3
#1       13 79

# accurancy: 85,05%

# Lav en ROC kurve
roc_sce23 <- roc(Vækst_factor , prediction_probabilities_sce23)
plot(roc_sce23, col="blue", main="ROC kurve for scenario med kun signifikate variable")



#####NOT UPDATED ############ Scenario x: Fjern 2022+ og dan en ny model for sandsynligheder

# Skær dataen ved slutningen af 2021
forbrug__sce2 <- as.data.frame(FTIs_1998[1:88,])

# Løb en ny logistisk regression model
logistic_model_sce2 <- glm(Dummy_variabel ~ Q1_Fam_nu + Q3_Dan_nu + Q5_Forbrugsgoder_nu + Q9_Forbrugsgoder_etår,
                           data = forbrug__sce2, family = binomial)

summary(logistic_model_sce2)

# Forudsig sandsynlighed for, at forbruget går op (1)
# Brug predict_forbrug_all fra opgave 3.3
prediction_probabilities_sce2 <- predict(logistic_model_sce2, predict_forbrug_all, type = "response")

# Dan en vector for fortolkninger og løb loopen
prediction_sce2 <- NULL

for (i in 1:length(prediction_probabilities_sce2)) {
  # Check if the current value is NA
  if (is.na(prediction_probabilities_sce2[i])) {
    prediction_sce2[i] <- "unknown"  # Or some other label for missing values
  } else if (prediction_probabilities_sce2[i] > 0.5) {
    prediction_sce2[i] <- "1"
  } else {
    prediction_sce2[i] <- "0"
  }
}

# Lav en konfusionsmatrix
prediction_sce2 <- as.factor(prediction_sce2)

confusion_matrix_sce2 <- confusionMatrix(prediction_sce2, Vækst_factor)

print(confusion_matrix_sce2)
#         Reference
#Prediction  0  1
#         0 10  7
#         1 12 70

# accurancy: 80,8%

# Lav en ROC kurve
roc_sce2 <- roc(Vækst_factor , prediction_probabilities_sce2)

plot(roc_sce2, col="blue", main="ROC kurve for Scenario 2")
