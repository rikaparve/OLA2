# info comes from opgave 4
### Lav en logistisk regression model
# Sikrer variabler er klar til modellen
str(FTIs_1998)
FTIs_1998$Dummy_variabel <- as.factor(FTIs_1998$Dummy_variabel)

# Lav modellen
DI_logistic_model <- glm(Dummy_variabel ~ Q1_Fam_nu + Q3_Dan_nu + Q5_Forbrugsgoder_nu + Q9_Forbrugsgoder_etår,
                      data = FTIs_1998, family = binomial)
summary(DI_logistic_model)

### Forudse forbruget i 4.kvartal i 2024 (vi har 3. kvartal)
# Dan en ny dataframe for indikatorerne
predict_forbrug <- data.frame(
  Q1_Fam_nu = mean(c(ftillid_wide$Q1_Fam_nu[346:347])),
  Q3_Dan_nu = mean(c(ftillid_wide$Q3_Dan_nu[346:347])),
  Q5_Forbrugsgoder_nu = mean(c(ftillid_wide$Q5_Forbrugsgoder_nu[346:347])),
  Q9_Forbrugsgoder_etår = mean(c(ftillid_wide$Q9_Forbrugsgoder_etår[346:347]))
)

# Forudsig sandsynlighed for, at forbruget går op (1)
prediction_prob <- predict(DI_logistic_model, predict_forbrug, type = "response")

# Fortolk forudsigelsen: hvis den forudsagte sandsynlighed > 0,5, forudsiger den »op«, ellers »ned«
if (prediction_prob > 0.5) {
  prediction <- "op"
} else {
  prediction <- "ned"
}

# Svar
print(paste("Den årlige vækst i husholdningernes forbrugudgift i 4. kvartal 2024 går", prediction))


##### Mathematical solution
2.44271 + (0.01555 * -9.5) + (0.05393 * -11.15) + (-0.06195 * -14.6) + (0.24717 * -5.85)
2.44271 + (-0.147725) + (-0.6013195) + (0.90447) + (-1.445944)
2.71828^1.152191
3.165118/4.165118
