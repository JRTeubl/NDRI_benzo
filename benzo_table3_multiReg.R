dfAll <- read.csv("/Users/jrteubl/Desktop/heroin_data/database_final_464.csv", header=T, sep=",")

BenzoUse <- dfAll$LifetimeSU_Note1.LifetimeSU_11.10
BenzoRegUseMonths <-dfAll$LifetimeSU_Note1.LifetimeSU_21
BenzoRegUseMonthsGrp <- rep(NA, nrow(dfAll))
BenzoRegUseMonthsGrp[BenzoUse == 1 & BenzoRegUseMonths > 0] <- 1
BenzoRegUseMonthsGrp[BenzoUse == 1 & BenzoRegUseMonths == 0] <- 0
BenzoRegUseMonthsGrp[BenzoUse == 0] <- 0


gender <- dfAll$Soc_Note1.Soc_10 # 1 = male, 2 = female
genderGrp <- rep(NA, nrow(dfAll))
genderGrp[gender == 1] <- 0
genderGrp[gender == 2] <- 1


latino <- dfAll$Soc_Note1.Soc_4 #0 = no, 1 = yes
latinoGrp <- rep(NA, nrow(dfAll))
latinoGrp[latino == 0] <- 0
latinoGrp[latino == 1] <- 1


Wht <- dfAll$Soc_Note1.Soc_5.5
race <- rep(NA, nrow(dfAll))
race[Wht == TRUE] <- 1
race[Wht == FALSE] <- 0


education <- dfAll$Soc_Note1.Soc_19
# 1- did not complete high school
# 2- High school/ GED
# 3- Some college/ associates
# 4- College Graduate
# 5- Some Graduate
# 6- Graduate/Professional

educationGrp <- rep(NA, nrow(dfAll))
educationGrp[education == 1] <- 1
educationGrp[education == 2] <- 2
educationGrp[education == 3] <- 3
educationGrp[education == 4 | education == 5 | education == 6] <- 4


income <- dfAll$Soc_Note1.Soc_23
# 1- $0-$25,000
# 2- $26,000 - $50,000
# 3- $51,000 - $75,000
# 4- $76,000 - $100,000
# 5- $101,000 - $125,000
# 6- $126,000 - $150,000
# 7- $151,000 - $200,000
# 8- $201,000 - $250,000
# 9- $251,000 +
# 77 - NA
# 88 - Don't know
# 99 - Refused to answer

incomeGrp <- rep(NA, nrow(dfAll))
incomeGrp[income == 1 | income == 2] <- 1
incomeGrp[income == 3 | income == 4] <- 2
incomeGrp[income == 5 | income == 6 | income == 7 | income == 8 | income == 9] <- 3


homeless <- dfAll$Soc_Note1.Soc_11 #0 = no, 1 = yes

inj <- dfAll$FirstInj_Note1.FirstInj_1
injGrp <- rep(NA, nrow(dfAll))
injGrp[inj == 0] <- 0
injGrp[inj == 1] <- 1

heroinUseLT <- dfAll$LifetimeSU_Note1.LifetimeSU_6
heroinUseLTGrp <- rep(0, nrow(dfAll))
heroinUseLTGrp[heroinUseLT > 0] <- 1
heroinUseLTGrp[heroinUseLT == 0] <- 0

heroinInjLT <- dfAll$LifetimeSU_Note1.LifetimeSU_8
# 58 people responded with NaN, 56 of them never used heroin in their lifetime

heroinInjLTReg <- dfAll$LifetimeSU_Note1.LifetimeSU_9 #regular use
heroinInjLTRegGrp <- rep(0, nrow(dfAll))
heroinInjLTRegGrp[heroinInjLTReg > 0] <- 1
heroinInjLTRegGrp[heroinInjLTReg == 0] <- 0
heroinInjLTRegGrp[heroinInjLT == 0] <- 0
heroinInjLTRegGrp[heroinUseLT == 0] <- 0

stimulantUseLTReg <- dfAll$LifetimeSU_Note1.LifetimeSU_22
stimulantUseLTRegGrp <- rep(0, nrow(dfAll))
stimulantUseLTRegGrp[stimulantUseLTReg > 0] <- 1
stimulantUseLTRegGrp[stimulantUseLTReg == 0] <- 0

cocaineUse <- dfAll$LifetimeSU_Note1.LifetimeSU_12
cocaineUseGrp <- rep(0, nrow(dfAll))
cocaineUseGrp[cocaineUse > 0] <- 1
cocaineUseGrp[cocaineUse == 0] <- 0

Anxiety <- dfAll$Mental_Note1.Mental_1_group.Mental_7
AnxietyGrp <- rep(0, nrow(dfAll))
AnxietyGrp[Anxiety == 0] <- 0
AnxietyGrp[Anxiety == 1] <- 1

TreatmentAnx <-dfAll$Mental_Note1.Mental_1_group.Mental_8
TreatmentAnxGrp <- rep(0, nrow(dfAll))
TreatmentAnxGrp[TreatmentAnx == 0] <- 0
TreatmentAnxGrp[TreatmentAnx == 1] <- 1

Prescribed <-dfAll$Mental_Note1.Mental_23.1
PrescribedGrp <- rep(0, nrow(dfAll))
PrescribedGrp[Prescribed == FALSE] <- 0
PrescribedGrp[Prescribed == TRUE] <- 1

drugTreatment <- dfAll$Treatment_Note1.Treatment_1
drugTreatmentGrp <- rep(0, nrow(dfAll))
drugTreatmentGrp[drugTreatment == 0] <- 0
drugTreatmentGrp[drugTreatment == 1] <- 1

bingePast30 <- dfAll$Bing_Note1.Bing_1
bingePast30Grp <- rep(0, nrow(dfAll))
bingePast30Grp[bingePast30 > 0] <- 1
bingePast30Grp[bingePast30 == 0] <- 0

overdose <- dfAll$Overdose_8
overdoseGrp <- rep(0, nrow(dfAll))
overdoseGrp[overdose == 0] <- 0
overdoseGrp[overdose == 1] <- 1

ODwithBenzo <-as.numeric(dfAll$Overdose_1_group.Overdose_51)
ODwithBenzoGrp <- rep(0, nrow(dfAll))
ODwithBenzoGrp[ODwithBenzo != 6] <- 1
ODwithBenzoGrp[overdoseGrp == 0] <- 0

EncouragedOthers <- dfAll$Intravention_Note1.Intravention_2
EncouragedOthersGrp <- rep(0, length(EncouragedOthers))
EncouragedOthersGrp[EncouragedOthers>0] <- 1
EncouragedOthersGrp[EncouragedOthers == 0] <- 0

df<- as.data.frame(cbind(BenzoRegUseMonthsGrp,latinoGrp,race,incomeGrp,heroinUseLTGrp,
                        stimulantUseLTRegGrp,ODwithBenzoGrp,heroinInjLTRegGrp,
                         cocaineUseGrp,PrescribedGrp,overdoseGrp,EncouragedOthersGrp,bingePast30Grp,
                         AnxietyGrp,TreatmentAnxGrp,drugTreatmentGrp))

numAndper <- function(number, df, column){
  dfx <- df[column == number, ]
  ODx <- table(dfx$BenzoRegUseMonthsGrp)
  TotOD <- ODx[1] + ODx[2]
  xODper <- ODx[2]/TotOD * 100
  xNotODper <- ODx[1]/TotOD * 100
  return(c(ODx[2], xODper, ODx[1], xNotODper))
}

benzo <- table(df$BenzoRegUseMonthsGrp)
benzo
Totbenzo <- benzo[1] + benzo[2]
benzoper <- benzo[2]/Totbenzo * 100
benzoper
Notbenzoper <- benzo[1]/Totbenzo * 100
Notbenzoper

ethnHis <- numAndper(1, df, df$latinoGrp)
ethnHis
ethnNonHis <- numAndper(0, df, df$latinoGrp)
ethnNonHis

raceWhite <- numAndper(1, df, df$race)
raceWhite
raceNonWhite <- numAndper(0, df, df$race)
raceNonWhite


income1 <- numAndper(1, df, df$incomeGrp)
income1
income2 <- numAndper(2, df, df$incomeGrp)
income2
income3 <- numAndper(3, df, df$incomeGrp)
income3

heroinYes <- numAndper(1, df, df$heroinUseLTGrp)
heroinYes
heroinNo <- numAndper(0, df, df$heroinUseLTGrp)
heroinNo

stimYes <- numAndper(1, df, df$stimulantUseLTRegGrp)
stimYes
stimNo <- numAndper(0, df, df$stimulantUseLTRegGrp)
stimNo

odBenzoYes <- numAndper(1, df, df$ODwithBenzoGrp)
odBenzoYes
odBenzoNo <- numAndper(0, df, df$ODwithBenzoGrp)
odBenzoNo


heroinInjYes <- numAndper(1, df, df$heroinInjLTRegGrp)
heroinInjYes
heroinInjNo <- numAndper(0, df, df$heroinInjLTRegGrp)
heroinInjNo

cocaineYes <- numAndper(1, df, df$cocaineUseGrp)
cocaineYes
cocaineNo <- numAndper(0, df, df$cocaineUseGrp)
cocaineNo

presYes <- numAndper(1, df, df$PrescribedGrp)
presYes
presNo <- numAndper(0, df, df$PrescribedGrp)
presNo

odYes <- numAndper(1, df, df$overdoseGrp)
odYes
odNo <- numAndper(0, df, df$overdoseGrp)
odNo

encYes <- numAndper(1, df, df$EncouragedOthersGrp)
encYes
encNo <- numAndper(0, df, df$EncouragedOthersGrp)
encNo


bingeYes <- numAndper(1, df, df$bingePast30Grp)
bingeYes
bingeNo <- numAndper(0, df, df$bingePast30Grp)
bingeNo

anxYes <- numAndper(1, df, df$AnxietyGrp)
anxYes
anxNo <- numAndper(0, df, df$AnxietyGrp)
anxNo

trtAnxYes <- numAndper(1, df, df$TreatmentAnxGrp)
trtAnxYes
trtAnxNo <- numAndper(0, df, df$TreatmentAnxGrp)
trtAnxNo

drugTrtYes <- numAndper(1, df, df$drugTreatmentGrp)
drugTrtYes
drugTrtNo <- numAndper(0, df, df$drugTreatmentGrp)
drugTrtNo




oddsratioWald.proc <- function(n00, n01, n10, n11, alpha = 0.05){
  #
  #  Compute the odds ratio between two binary variables, x and y,
  #  as defined by the four numbers nij:
  #
  #    n00 = number of cases where x = 0 and y = 0
  #    n01 = number of cases where x = 0 and y = 1
  #    n10 = number of cases where x = 1 and y = 0
  #    n11 = number of cases where x = 1 and y = 1
  #
  OR <- (n00 * n11)/(n01 * n10)
  #
  #  Compute the Wald confidence intervals:
  #
  siglog <- sqrt((1/n00) + (1/n01) + (1/n10) + (1/n11))
  zalph <- qnorm(1 - alpha/2)
  logOR <- log(OR)
  loglo <- logOR - zalph * siglog
  loghi <- logOR + zalph * siglog
  
  ORlo <- exp(loglo)
  ORhi <- exp(loghi)
  SE <- (loghi - loglo)/(2*1.96)
  z <- logOR/SE
  p <- exp(-.717*z-.416*z**2)
  
  oframe <- data.frame(LowerCI = ORlo, OR = OR, UpperCI = ORhi, alpha = alpha, pval = p)
  oframe
}

eth00 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$latinoGrp == 0, ])
eth01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$latinoGrp == 1, ])
eth10 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$latinoGrp == 0, ])
eth11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$latinoGrp == 1, ])
oddsratioWald.proc(eth00, eth01, eth10, eth11)

race00 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$race == 0, ])
race01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$race == 1, ])
race10 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$race == 0, ])
race11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$race == 1, ])
oddsratioWald.proc(race00, race01, race10, race11)

income01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$incomeGrp == 1, ])
income02 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$incomeGrp == 2, ])
income03 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$incomeGrp == 3, ])
income11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$incomeGrp == 1, ])
income12 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$incomeGrp == 2, ])
income13 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$incomeGrp == 3, ])
oddsratioWald.proc(income01, income02, income11, income12)
oddsratioWald.proc(income01, income03, income11, income13)

heroinReg00 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$heroinUseLTGrp == 0, ])
heroinReg01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$heroinUseLTGrp == 1, ])
heroinReg10 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$heroinUseLTGrp == 0, ])
heroinReg11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$heroinUseLTGrp == 1, ])
oddsratioWald.proc(heroinReg00, heroinReg01, heroinReg10, heroinReg11)

heroinInj00 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$heroinInjLTReg == 0, ])
heroinInj01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$heroinInjLTReg == 1, ])
heroinInj10 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$heroinInjLTReg == 0, ])
heroinInj11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$heroinInjLTReg == 1, ])
oddsratioWald.proc(heroinInj00, heroinInj01, heroinInj10, heroinInj11)

stimulantReg00 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$stimulantUseLTRegGrp == 0, ])
stimulantReg01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$stimulantUseLTRegGrp == 1, ])
stimulantReg10 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$stimulantUseLTRegGrp == 0, ])
stimulantReg11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$stimulantUseLTRegGrp == 1, ])
oddsratioWald.proc(stimulantReg00, stimulantReg01, stimulantReg10, stimulantReg11)

ODwithBenzo00 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$ODwithBenzoGrp == 0, ])
ODwithBenzo01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$ODwithBenzoGrp == 1, ])
ODwithBenzo10 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$ODwithBenzoGrp == 0, ])
ODwithBenzo11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$ODwithBenzoGrp == 1, ])
oddsratioWald.proc(ODwithBenzo00, ODwithBenzo01, ODwithBenzo10, ODwithBenzo11)

cocaineReg00 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$cocaineUseGrp == 0, ])
cocaineReg01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$cocaineUseGrp == 1, ])
cocaineReg10 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$cocaineUseGrp == 0, ])
cocaineReg11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$cocaineUseGrp == 1, ])
oddsratioWald.proc(cocaineReg00, cocaineReg01, cocaineReg10, cocaineReg11)

prsbd00 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$PrescribedGrp == 0, ])
prsbd01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$PrescribedGrp == 1, ])
prsbd10 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$PrescribedGrp == 0, ])
prsbd11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$PrescribedGrp == 1, ])
oddsratioWald.proc(prsbd00, prsbd01, prsbd10, prsbd11)


overdose00 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$overdoseGrp == 0, ])
overdose01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$overdoseGrp == 1, ])
overdose10 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$overdoseGrp == 0, ])
overdose11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$overdoseGrp == 1, ])
oddsratioWald.proc(overdose00, overdose01, overdose10, overdose11)

encouraged00 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$EncouragedOthersGrp == 0, ])
encouraged01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$EncouragedOthersGrp == 1, ])
encouraged10 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$EncouragedOthersGrp == 0, ])
encouraged11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$EncouragedOthersGrp == 1, ])
oddsratioWald.proc(encouraged00, encouraged01, encouraged10, encouraged11)

binge00 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$bingePast30Grp == 0, ])
binge01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$bingePast30Grp == 1, ])
binge10 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$bingePast30Grp == 0, ])
binge11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$bingePast30Grp == 1, ])
oddsratioWald.proc(binge00, binge01, binge10, binge11)

Anxiety00 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$AnxietyGrp == 0, ])
Anxiety01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$AnxietyGrp == 1, ])
Anxiety10 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$AnxietyGrp == 0, ])
Anxiety11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$AnxietyGrp == 1, ])
oddsratioWald.proc(Anxiety00, Anxiety01, Anxiety10, Anxiety11)

TreatmentAnx00 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$TreatmentAnxGrp == 0, ])
TreatmentAnx01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$TreatmentAnxGrp == 1, ])
TreatmentAnx10 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$TreatmentAnxGrp == 0, ])
TreatmentAnx11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$TreatmentAnxGrp == 1, ])
oddsratioWald.proc(TreatmentAnx00, TreatmentAnx01, TreatmentAnx10, TreatmentAnx11)

drugTreatment00 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$drugTreatmentGrp == 0, ])
drugTreatment01 <- nrow(df[df$BenzoRegUseMonthsGrp == 0 & df$drugTreatmentGrp == 1, ])
drugTreatment10 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$drugTreatmentGrp == 0, ])
drugTreatment11 <- nrow(df[df$BenzoRegUseMonthsGrp == 1 & df$drugTreatmentGrp == 1, ])
oddsratioWald.proc(TreatmentAnx00, TreatmentAnx01, TreatmentAnx10, TreatmentAnx11)

pvalOR <- function(OR, lo, hi, alpha = .05){
  logOR <- log(OR)
  loglo <- log(lo)
  loghi <- log(hi)
  SE <- (loghi - loglo)/(2*1.96)
  z <- logOR/SE
  p <- exp(-.717*z-.416*z**2)
  df <- data.frame(LowerCI = lo, OR = OR, UpperCI = hi, alpha = alpha, pval = p)
  df
}

#demographic variables only
dfDemo <- as.data.frame(cbind(BenzoRegUseMonthsGrp,latinoGrp,race,incomeGrp))

#drug use variables only

#mental health variables only

#posteriori variables only

#add all variable groups one by one to demographic variable set

#add all variables (indivdually) one by one to demographic set

#determine final list of variables compare to regressions with all variables

dfNA <- na.omit(df)


#multi variable regression all variables
multiSig <- lm(BenzoRegUseMonthsGrp ~ ., data = dfNA)
multiSumSig <- summary(multiSig)
multiSumSig

#multi variable regression, all variables, stepwise removal
multiSigStep <- step(lm(BenzoRegUseMonthsGrp ~ ., data = dfNA), direction = 'backward')
multiSumSigStep <- summary(multiSigStep)
multiSumSigStep

ORs <- exp(cbind(OR = coef(multiSigStep), confint(multiSigStep)))
ORs

dfODben <- pvalOR(ORs[2], ORs[2,2], ORs[2,3])
dfODben

dfInj <- pvalOR(ORs[3], ORs[3,2], ORs[3,3])
dfInj

dfCoc <- pvalOR(ORs[4], ORs[4,2], ORs[4,3])
dfCoc

dfPres <- pvalOR(ORs[5], ORs[5,2], ORs[5,3])
dfPres

dfEnco <- pvalOR(ORs[6], ORs[6,2], ORs[6,3])
dfEnco

dfBing <- pvalOR(ORs[7], ORs[7,2], ORs[7,3])
dfBing

dfAnx <- pvalOR(ORs[8], ORs[8,2], ORs[8,3])
dfAnx




