library(ggplot2)

### Adjust for the timeperiod
FTIs_1998 <- data.frame(
  År = ftillid_K$TID[9:115],
  Realvaekst = forbrug_meta$Vaekst[29:135],
  Q1_Fam_nu = ftillid_K$Q1_Fam_nu[9:115],
  Q3_Dan_nu = ftillid_K$Q3_Dan_nu[9:115],
  Q5_Forbrugsgoder_nu = ftillid_K$Q5_Forbrugsgoder_nu[9:115],
  Q9_Forbrugsgoder_etår = ftillid_K$Q9_Forbrugsgoder_etår[9:115],
  DI_FTI = rowMeans(ftillid_K[9:115,c(3,5,7,11)])
)

# Dan en dummy variabel: op == "1", ned == "0"
FTIs_1998$Dummy_variabel <- ifelse(FTIs_1998$Realvaekst >= 0, "1", "0")

# Lav en frekvens barplot
freq_vaekst <- as.data.frame(table(FTIs_1998$Dummy_variabel))

# Make the chart
ggplot(freq_vaekst, aes(x=Var1, y=Freq, fill=Var1)) +
         geom_bar(stat="identity", position="dodge") +
         theme_minimal() +
  labs(x="Udvikling af kvartalvis årelige realvækst af husholdningernes forbrugsudgifter,
       Op = 1, Ned = 0", y="Antal af kvartaler", title="Husholdningernes forbrugsudgift går op 77% af kvartalerne")
       


