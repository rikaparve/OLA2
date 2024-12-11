###### EU FTI

EU_FTI <- ftillid_K[,c(1,3,4,7,8)]

EU_FTI$FTI <- (EU_FTI[,2]+EU_FTI[,3]+EU_FTI[,4]+EU_FTI[,5])/4

FTIs <- forbrug_tillid_slm[,c(1,17,18)]
FTIs$EU_FTI <- EU_FTI$FTI[17:113]

privatforbrug <- read.csv("data/Privatforbrug.csv", header=F, sep=",", skip=2)

privatforbrug <- privatforbrug[-2,-1]

forbrug <- data.frame(t(privatforbrug))
row.names(forbrug) <- NULL
forbrug[1,1] <- "Years"
colnames(forbrug) <- forbrug[1,]
forbrug <- forbrug[-1,]
colnames(forbrug) <- c("Years","Udgifter til individuelt forbrug","Faktisk individuelt forbrug",
                       "Udgifter til forbrug (hele okonomien)","Disponibel bruttoindkomst",
                       "Korrigeret disponibel bruttoindkomst"," for aendringer i pensionsrettigheder",
                       "Bruttoopsparing", "Forbrug af fast realkapital",
                       "Disponibel nettoindkomst","Korrigeret disponibel nettoindkomst","Nettoopsparing")

forbrug <- forbrug[,c(1,2)]
Vaekst <- diff(log(as.numeric(forbrug$`Udgifter til individuelt forbrug`)), lag = 4)*100
forbrug$Vaekst[5:nrow(forbrug)] <- Vaekst

forbrug <- forbrug[-c(1:4),]
FTIs$Forbrug <- Vaekst

row.names(FTIs) <- NULL
FTIs <- FTIs[-98,]

DI_lm <- lm(Forbrug ~ DI_FTI, data = FTIs)
summary(DI_lm)

DS_lm <- lm(Forbrug ~ DS_FTI, data = FTIs)
summary(DS_lm)

EU_lm <- lm(Forbrug ~ EU_FTI, data = FTIs)
summary(EU_lm)

ftillid_DI2 <- ftillid_DI
ftillid_DI2$DI_DTI <- (ftillid_DI2$Famnu + ftillid_DI2$Dannu + ftillid_DI2$Forbrugsgodernu + ftillid_DI2$Forbrugsgoderetår)/4

FTIs$DI_FTI <- ftillid_DI2$DI_DTI[17:113]

FTIs$DS_FTI <- ftillid_K$FTI[17:113]
