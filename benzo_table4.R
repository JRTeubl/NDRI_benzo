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

oddsDF <- function(df, col1, col2){
  odds00 <- nrow(df[col1 == 0 & col2 == 0, ])
  odds01 <- nrow(df[col1 == 0 & col2 == 1, ])
  odds10 <- nrow(df[col1 == 1 & col2 == 0, ])
  odds11 <- nrow(df[col1 == 1 & col2 == 1, ])
  print(odds00)
  print(odds01)
  print(odds10)
  print(odds11)
  oddsratioWald.proc(odds00, odds01, odds10, odds11)
}

oddsMultiDF <- function(df, col1, col2){
  odds10 <- nrow(df[col1 == 1 & col2 == 0, ])
  odds11 <- nrow(df[col1 == 1 & col2 == 1, ])
  odds20 <- nrow(df[col1 == 2 & col2 == 0, ])
  odds21 <- nrow(df[col1 == 2 & col2 == 1, ])
  print(odds10)
  print(odds11)
  print(odds20)
  print(odds21)
  df12 <- oddsratioWald.proc(odds10, odds11, odds20, odds21)
  print(df12)
  
  odds20 <- nrow(df[col1 == 2 & col2 == 0, ])
  odds21 <- nrow(df[col1 == 2 & col2 == 1, ])
  odds30 <- nrow(df[col1 == 3 & col2 == 0, ])
  odds31 <- nrow(df[col1 == 3 & col2 == 1, ])
  print(odds20)
  print(odds21)
  print(odds30)
  print(odds31)
  df23 <- oddsratioWald.proc(odds20, odds21, odds30, odds31)
  print(df23)
}

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



dfAll <- read.csv("/Users/jrteubl/Desktop/heroin_data/database_final_464.csv", header=T, sep=",")

BenzoUse <- dfAll$LifetimeSU_Note1.LifetimeSU_11.10
BenzoRegUseMonths <-dfAll$LifetimeSU_Note1.LifetimeSU_21
BenzoRegUseMonthsGrp <- rep(0, nrow(dfAll))
BenzoRegUseMonthsGrp[BenzoRegUseMonths > 0] <- 1

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
educationGrp[education >= 3] <- 3


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

heroinUseLT <- dfAll$LifetimeSU_Note1.LifetimeSU_7
heroinUseLTGrp <- rep(NA, nrow(dfAll))
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
PrescribedGrp <- rep(NA, nrow(dfAll))
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
overdoseGrp <- rep(NA, nrow(dfAll)) #consider anything that isn't a 0 or 1 as NA
overdoseGrp[overdose == 0] <- 0
overdoseGrp[overdose == 1] <- 1

ODwithBenzo <-as.numeric(dfAll$Overdose_1_group.Overdose_51)
ODwithBenzoGrp <- rep(0, nrow(dfAll))
ODwithBenzoGrp[ODwithBenzo != 6] <- 1
ODwithBenzoGrp[overdoseGrp == 0] <- 0

EncouragedOthers <- dfAll$Intravention_Note1.Intravention_2
EncouragedOthersGrp <- rep(NA, length(EncouragedOthers))
EncouragedOthersGrp[EncouragedOthers>0] <- 1
EncouragedOthersGrp[EncouragedOthers == 0] <- 0

stimulantUseLTReg <- dfAll$LifetimeSU_Note1.LifetimeSU_22
stimulantUseLTRegGrp <- rep(0, nrow(dfAll))
stimulantUseLTRegGrp[stimulantUseLTReg > 0] <- 1

POuseLTReg <- dfAll$LifetimeSU_Note1.LifetimeSU_2
POuseLTRegGrp <- rep(0, nrow(dfAll))
POuseLTRegGrp[POuseLTReg > 0] <- 1

cocaineUse <- dfAll$LifetimeSU_Note1.LifetimeSU_12
cocaineUseGrp <- rep(0, nrow(dfAll))
cocaineUseGrp[cocaineUse > 0] <- 1

df<- as.data.frame(cbind(BenzoRegUseMonthsGrp,race,latinoGrp,incomeGrp,heroinUseLTGrp,
                         stimulantUseLTRegGrp,cocaineUseGrp,heroinInjLTRegGrp,AnxietyGrp,
                         TreatmentAnxGrp,PrescribedGrp,drugTreatmentGrp,bingePast30Grp,
                         overdoseGrp,ODwithBenzoGrp,EncouragedOthersGrp))

df$incomeGrp <- as.factor(df$incomeGrp)
### Multi Linear Regression ###

#multi variable regression all variables
multiSig <- glm(BenzoRegUseMonthsGrp ~ ., data = df, family = 'binomial')
multiSumSig <- summary(multiSig)
multiSumSig
1 - pchisq(multiSumSig$deviance, multiSumSig$df.residual)
write.csv(multiSumSig$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/allSig.txt", quote = FALSE, na = "0")

#demographic variables only
dfDemo <- as.data.frame(cbind(BenzoRegUseMonthsGrp,latinoGrp,race,incomeGrp))
dfDemo$incomeGrp <- as.factor(dfDemo$incomeGrp)
demoMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfDemo, family = 'binomial')
demoMultiSum <- summary(demoMulti)
demoMultiSum
1 - pchisq(demoMultiSum$deviance, demoMultiSum$df.residual)
write.csv(demoMultiSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/demo.txt", quote = FALSE, na = "0")

#drug use variables only
dfDrug <- as.data.frame((cbind(BenzoRegUseMonthsGrp, heroinUseLTGrp, stimulantUseLTRegGrp,
                               cocaineUseGrp, heroinInjLTRegGrp, overdoseGrp)))
drugMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfDrug, family = 'binomial')
drugMultiSum <- summary(drugMulti)
drugMultiSum
1 - pchisq(drugMultiSum$deviance, drugMultiSum$df.residual)
write.csv(drugMultiSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/drug.txt", quote = FALSE, na = "0")

#mental health variables only
dfMH <- as.data.frame(cbind(BenzoRegUseMonthsGrp, AnxietyGrp, TreatmentAnxGrp, PrescribedGrp,
                            drugTreatmentGrp))
MHmulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfMH, family = 'binomial')
MHmultiSum <- summary(MHmulti)
MHmultiSum
1 - pchisq(MHmultiSum$deviance, MHmultiSum$df.residual)
write.csv(MHmultiSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/MH.txt", quote = FALSE, na = "0")

#posteriori variables only
dfPost <- as.data.frame(cbind(BenzoRegUseMonthsGrp,ODwithBenzoGrp,bingePast30Grp,EncouragedOthersGrp))
postMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfPost, family = 'binomial')
postMultiSum <- summary(postMulti)
postMultiSum
1 - pchisq(postMultiSum$deviance, postMultiSum$df.residual)
write.csv(postMultiSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/post.txt", quote = FALSE, na = "0")

#add all variable groups one by one to demographic variable set
dfDemoDrug <- as.data.frame(cbind(BenzoRegUseMonthsGrp,latinoGrp,race,incomeGrp,heroinUseLTGrp, 
                                  stimulantUseLTRegGrp,cocaineUseGrp,heroinInjLTRegGrp,overdoseGrp))
dfDemoDrug$incomeGrp <- as.factor(dfDemoDrug$incomeGrp)
demoDrugMulti <- glm(BenzoRegUseMonthsGrp~., data = dfDemoDrug, family = 'binomial')
demoDrugSum <- summary(demoDrugMulti)
demoDrugSum
1 - pchisq( demoDrugSum$deviance, demoDrugSum$df.residual)
write.csv(demoDrugSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/drugDemo.txt", quote = FALSE, na = "0")

dfDemoMH <- as.data.frame(cbind(BenzoRegUseMonthsGrp,latinoGrp,race,incomeGrp,AnxietyGrp,TreatmentAnxGrp,
                                PrescribedGrp,drugTreatmentGrp))
dfDemoMH$incomeGrp <- as.factor(dfDemoMH$incomeGrp)
demoMHmulti <- glm(BenzoRegUseMonthsGrp~., data = dfDemoMH, family = 'binomial')
demoMHsum <- summary(demoMHmulti)
demoMHsum
1 - pchisq(demoMHsum$deviance, demoMHsum$df.residual)
write.csv(demoMHsum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/MHDemo.txt", quote = FALSE, na = "0")


dfDemoPost <- as.data.frame(cbind(BenzoRegUseMonthsGrp,latinoGrp,race,incomeGrp,ODwithBenzoGrp,
                                  bingePast30Grp, EncouragedOthersGrp))
dfDemoPost$incomeGrp <- as.factor(dfDemoPost$incomeGrp)
demoPostMulti <- glm(BenzoRegUseMonthsGrp~., data = dfDemoPost, family = 'binomial')
demoPostSum <- summary(demoPostMulti)
demoPostSum
1 - pchisq(demoPostSum$deviance, demoPostSum$df.residual)
write.csv(demoPostSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/PostDemo.txt", quote = FALSE, na = "0")

#add variables (indivdually) one by one to demographic set

dfDemoAnx <- as.data.frame(cbind(BenzoRegUseMonthsGrp,latinoGrp,race,incomeGrp,AnxietyGrp))
dfDemoAnx$incomeGrp <- as.factor(dfDemoAnx$incomeGrp)
demoAnxMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfDemoAnx, family = 'binomial')
demoAnxSum <- summary(demoAnxMulti)
demoAnxSum
1 - pchisq(demoAnxSum$deviance, demoAnxSum$df.residual)
write.csv(demoAnxSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/AnxDemo.txt", quote = FALSE, na = "0")


dfDemoPres <- as.data.frame(cbind(BenzoRegUseMonthsGrp,latinoGrp,race,incomeGrp,PrescribedGrp))
dfDemoPres$incomeGrp <- as.factor(dfDemoPres$incomeGrp)
demoPresMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfDemoPres, family = 'binomial')
demoPresSum <- summary(demoPresMulti)
demoPresSum
1 - pchisq(demoPresSum$deviance, demoPresSum$df.residual)
write.csv(demoPresSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/PresDemo.txt", quote = FALSE, na = "0")


dfDemoDT <- as.data.frame(cbind(BenzoRegUseMonthsGrp,latinoGrp,race,incomeGrp,drugTreatmentGrp))
dfDemoDT$incomeGrp <- as.factor(dfDemoDT$incomeGrp)
demoDTmulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfDemoDT, family = 'binomial')
demoDTSum <- summary(demoDTmulti)
demoDTSum
1 - pchisq(demoDTSum$deviance, demoDTSum$df.residual)
write.csv(demoDTSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/DrugTreatmentDemo.txt", quote = FALSE, na = "0")

dfDemoCoc <- as.data.frame(cbind(BenzoRegUseMonthsGrp,latinoGrp,race,incomeGrp,cocaineUseGrp))
dfDemoCoc$incomeGrp <- as.factor(dfDemoCoc$incomeGrp)
demoCocmulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfDemoCoc, family = 'binomial')
demoCocSum <- summary(demoCocmulti)
demoCocSum
1 - pchisq(demoCocSum$deviance, demoCocSum$df.residual)
write.csv(demoCocSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/CocaineDemo.txt", quote = FALSE, na = "0")

dfDemoOD <- as.data.frame(cbind(BenzoRegUseMonthsGrp,latinoGrp,race,incomeGrp,overdoseGrp))
dfDemoOD$incomeGrp <- as.factor(dfDemoOD$incomeGrp)
demoODmulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfDemoOD, family = 'binomial')
demoODSum <- summary(demoODmulti)
demoODSum
1 - pchisq(demoODSum$deviance, demoODSum$df.residual)
write.csv(demoODSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/OverdoseDemo.txt", quote = FALSE, na = "0")

dfDemoODwBenz <- as.data.frame(cbind(BenzoRegUseMonthsGrp,latinoGrp,race,incomeGrp,ODwithBenzoGrp))
dfDemoODwBenz$incomeGrp <- as.factor(dfDemoODwBenz$incomeGrp)
demoODwBenzmulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfDemoODwBenz, family = 'binomial')
demoODwBenzSum <- summary(demoODwBenzmulti)
demoODwBenzSum
1 - pchisq(demoODwBenzSum$deviance, demoODwBenzSum$df.residual)
write.csv(demoODwBenzSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/OverdoseWithBenzoDemo.txt", quote = FALSE, na = "0")

dfDemoBinge <- as.data.frame(cbind(BenzoRegUseMonthsGrp,latinoGrp,race,incomeGrp,bingePast30Grp))
dfDemoBinge$incomeGrp <- as.factor(dfDemoBinge$incomeGrp)
demoBingemulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfDemoBinge, family = 'binomial')
demoBingeSum <- summary(demoBingemulti)
demoBingeSum
1 - pchisq(demoBingeSum$deviance, demoBingeSum$df.residual)
write.csv(demoBingeSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/BingeDemo.txt", quote = FALSE, na = "0")

dfDemoEnco <- as.data.frame(cbind(BenzoRegUseMonthsGrp,latinoGrp,race,incomeGrp,EncouragedOthersGrp))
dfDemoEnco$incomeGrp <- as.factor(dfDemoEnco$incomeGrp)
demoEncomulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfDemoEnco, family = 'binomial')
demoEncoSum <- summary(demoEncomulti)
demoEncoSum
1 - pchisq(demoEncoSum$deviance, demoEncoSum$df.residual)
write.csv(demoEncoSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/EncoDemo.txt", quote = FALSE, na = "0")

# compare drug and MH
dfDrugAnx <-  as.data.frame(cbind(BenzoRegUseMonthsGrp,incomeGrp, cocaineUseGrp, overdoseGrp,
                              AnxietyGrp))
dfDrugAnx$incomeGrp <- as.factor(dfDrugAnx$incomeGrp)
DrugAnxMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfDrugAnx, family = 'binomial')
DrugAnxMultiSum <- summary(DrugAnxMulti)
DrugAnxMultiSum
1 - pchisq(DrugAnxMultiSum$deviance, DrugAnxMultiSum$df.residual)
write.csv(DrugAnxMultiSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/DrugAnx.txt", quote = FALSE, na = "0")

dfDrugPres <-  as.data.frame(cbind(BenzoRegUseMonthsGrp,incomeGrp, cocaineUseGrp, overdoseGrp,
                                  PrescribedGrp))
dfDrugPres$incomeGrp <- as.factor(dfDrugPres$incomeGrp)
DrugPresMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfDrugPres, family = 'binomial')
DrugPresMultiSum <- summary(DrugPresMulti)
DrugPresMultiSum
1 - pchisq(DrugPresMultiSum$deviance, DrugPresMultiSum$df.residual)
write.csv(DrugPresMultiSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/DrugPres.txt", quote = FALSE, na = "0")

dfDrugEnc <-  as.data.frame(cbind(BenzoRegUseMonthsGrp,incomeGrp, cocaineUseGrp, overdoseGrp,
                                   EncouragedOthersGrp))
dfDrugEnc$incomeGrp <- as.factor(dfDrugEnc$incomeGrp)
DrugEncMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfDrugEnc, family = 'binomial')
DrugEncMultiSum <- summary(DrugEncMulti)
DrugEncMultiSum
1 - pchisq(DrugEncMultiSum$deviance, DrugEncMultiSum$df.residual)
write.csv(DrugEncMultiSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/DrugEnc.txt", quote = FALSE, na = "0")

dfMHcoc <- as.data.frame(cbind(BenzoRegUseMonthsGrp,incomeGrp,AnxietyGrp,PrescribedGrp,drugTreatmentGrp,
                               cocaineUseGrp))
dfMHcoc$incomeGrp <- as.factor(dfMHcoc$incomeGrp)
MHcocMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfMHcoc, family = 'binomial')
MHcocMultiSum <- summary(MHcocMulti)
MHcocMultiSum
1 - pchisq(MHcocMultiSum$deviance, MHcocMultiSum$df.residual)
write.csv(MHcocMultiSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/MHcoc.txt", quote = FALSE, na = "0")

dfMHod <- as.data.frame(cbind(BenzoRegUseMonthsGrp,incomeGrp,AnxietyGrp,PrescribedGrp,drugTreatmentGrp,
                               overdoseGrp))
dfMHod$incomeGrp <- as.factor(dfMHod$incomeGrp)
MHodMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfMHod, family = 'binomial')
MHodMultiSum <- summary(MHodMulti)
MHodMultiSum
1 - pchisq(MHodMultiSum$deviance, MHodMultiSum$df.residual)
write.csv(MHodMultiSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/MHoverdose.txt", quote = FALSE, na = "0")

#combine groups
dfapriori <- as.data.frame(cbind(BenzoRegUseMonthsGrp,incomeGrp,overdoseGrp,
                                 AnxietyGrp, PrescribedGrp, drugTreatmentGrp))
dfapriori$incomeGrp <- as.factor(dfapriori$incomeGrp)
aprioriMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfapriori, family = 'binomial')
aprioriMultiSum <- summary(aprioriMulti)
aprioriMultiSum
1 - pchisq(aprioriMultiSum$deviance, aprioriMultiSum$df.residual)
write.csv(aprioriMultiSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/apriori.txt", quote = FALSE, na = "0")

dfApriODB <- as.data.frame(cbind(BenzoRegUseMonthsGrp,incomeGrp,overdoseGrp,AnxietyGrp,PrescribedGrp,drugTreatmentGrp,
                                  ODwithBenzoGrp))
dfApriODB$incomeGrp <- as.factor(dfApriODB$incomeGrp)
apriODBMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfApriODB, family = 'binomial')
apriODBsum <- summary(apriODBMulti)
apriODBsum
1 - pchisq(apriODBMulti$deviance, apriODBMulti$df.residual)
write.csv(apriODBsum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/aprioriODB.txt", quote = FALSE, na = "0")

dfApriBinge <- as.data.frame(cbind(BenzoRegUseMonthsGrp, incomeGrp, overdoseGrp, AnxietyGrp,drugTreatmentGrp,
                                   bingePast30Grp))
dfApriBinge$incomeGrp <- as.factor(dfApriBinge$incomeGrp)
apriBingeMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfApriBinge, family = 'binomial')
apriBingeSum <- summary(apriBingeMulti)
apriBingeSum
1 - pchisq(apriBingeSum$deviance, apriBingeSum$df.residual)
write.csv(apriBingeSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/aprioriBinge.txt", quote = FALSE, na = "0")


dfApriEnc <- as.data.frame(cbind(BenzoRegUseMonthsGrp, incomeGrp, overdoseGrp, AnxietyGrp, drugTreatmentGrp,
                                 EncouragedOthersGrp))
dfApriEnc$incomeGrp <- as.factor(dfApriEnc$incomeGrp)
apriEncMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfApriEnc, family = 'binomial')
apriEncSum <- summary(apriEncMulti)
apriEncSum
1 - pchisq(apriEncSum$deviance, apriEncSum$df.residual)
write.csv(apriEncSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/aprioriEncor.txt", quote = FALSE, na = "0")


dfPostInc <- as.data.frame(cbind(BenzoRegUseMonthsGrp,ODwithBenzoGrp,bingePast30Grp,
                                 EncouragedOthersGrp, incomeGrp))
dfPostInc$incomeGrp <- as.factor(dfPostInc$incomeGrp)
postIncMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfPostInc, family = 'binomial')
postIncSum <- summary(postIncMulti)
postIncSum
1 - pchisq(postIncSum$deviance, postIncSum$df.residual)
write.csv(postIncSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/postInc.txt", quote = FALSE, na = "0")


dfPostOD <- as.data.frame(cbind(BenzoRegUseMonthsGrp,ODwithBenzoGrp,bingePast30Grp,
                                 EncouragedOthersGrp, overdoseGrp))
postODMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfPostOD, family = 'binomial')
postODSum <- summary(postODMulti)
postODSum
1 - pchisq(postODSum$deviance, postODSum$df.residual)
write.csv(postODSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/postOD.txt", quote = FALSE, na = "0")

dfPostAnx <- as.data.frame(cbind(BenzoRegUseMonthsGrp,ODwithBenzoGrp,bingePast30Grp,
                                EncouragedOthersGrp, AnxietyGrp))
postAnxMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfPostAnx, family = 'binomial')
postAnxSum <- summary(postAnxMulti)
postAnxSum
1 - pchisq(postAnxSum$deviance, postAnxSum$df.residual)
write.csv(postAnxSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/postAnx.txt", quote = FALSE, na = "0")

dfPostPre <- as.data.frame(cbind(BenzoRegUseMonthsGrp,ODwithBenzoGrp,bingePast30Grp,
                                 EncouragedOthersGrp, PrescribedGrp))
postPreMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfPostPre, family = 'binomial')
postPreSum <- summary(postPreMulti)
postPreSum
1 - pchisq(postPreSum$deviance, postPreSum$df.residual)
write.csv(postPreSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/postPre.txt", quote = FALSE, na = "0")

dfMHBinge <- as.data.frame(cbind(BenzoRegUseMonthsGrp, AnxietyGrp, TreatmentAnxGrp, PrescribedGrp,
                            drugTreatmentGrp, bingePast30Grp))
MHBingeMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfMHBinge, family = 'binomial')
MHBingeSum <- summary(MHBingeMulti)
MHBingeSum
1 - pchisq(MHBingeSum$deviance,MHBingeSum$df.residual)
write.csv(MHBingeSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/MHBinge.txt", quote = FALSE, na = "0")

dfMHEnc <- as.data.frame(cbind(BenzoRegUseMonthsGrp, AnxietyGrp, TreatmentAnxGrp, PrescribedGrp,
                                 drugTreatmentGrp, EncouragedOthersGrp))
MHEncMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfMHEnc, family = 'binomial')
MHEncSum <- summary(MHEncMulti)
MHEncSum
1 - pchisq(MHEncSum$deviance,MHEncSum$df.residual)
write.csv(MHEncSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/MHEnc.txt", quote = FALSE, na = "0")

dfMHodb <- as.data.frame(cbind(BenzoRegUseMonthsGrp, AnxietyGrp, TreatmentAnxGrp, PrescribedGrp,
                               drugTreatmentGrp, ODwithBenzoGrp))
MHodbMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfMHodb, family = 'binomial')
MHodbSum <- summary(MHodbMulti)
MHodbSum
1 - pchisq(MHodbSum$deviance,MHodbSum$df.residual)
write.csv(MHodbSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/MHodb.txt", quote = FALSE, na = "0")

dfMHinc <- as.data.frame(cbind(BenzoRegUseMonthsGrp, AnxietyGrp, TreatmentAnxGrp, PrescribedGrp,
                               drugTreatmentGrp, incomeGrp))
dfMHinc$incomeGrp <- as.factor(dfMHinc$incomeGrp)
MHincMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfMHinc, family = 'binomial')
MHincSum <- summary(MHincMulti)
MHincSum
1 - pchisq(MHincSum$deviance,MHincSum$df.residual)
write.csv(MHincSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/MHinc.txt", quote = FALSE, na = "0")

#determine final list of variables compare to regressions with all variables

dfFinal <- as.data.frame(cbind(BenzoRegUseMonthsGrp,incomeGrp, overdoseGrp, AnxietyGrp, PrescribedGrp,
                               bingePast30Grp, EncouragedOthersGrp, cocaineUseGrp))
dfFinal$incomeGrp <- as.factor(dfFinal$incomeGrp)
finalMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfFinal, family = 'binomial')
finalSum <- summary(finalMulti)
finalSum
1 - pchisq(finalSum$deviance, finalSum$df.residual)
write.csv(finalSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/final.txt", quote = FALSE, na = "0")

dfFinalwoAnx <- as.data.frame(cbind(BenzoRegUseMonthsGrp,incomeGrp, overdoseGrp, PrescribedGrp,
                               bingePast30Grp, EncouragedOthersGrp))
dfFinalwoAnx$incomeGrp <- as.factor(dfFinalwoAnx$incomeGrp)
finalwoAnxMulti <- glm(BenzoRegUseMonthsGrp ~ ., data = dfFinalwoAnx, family = 'binomial')
finalwoAnxSum <- summary(finalwoAnxMulti)
finalwoAnxSum
1 - pchisq(finalwoAnxSum$deviance, finalwoAnxSum$df.residual)
write.csv(finalwoAnxSum$coefficients, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/finalWOanx.txt", quote = FALSE, na = "0")



# Adjusted ORs for Final
ORs <- exp(cbind(OR = coef(finalMulti), confint(finalMulti)))
ORs
write.csv(ORs, "/Users/jrteubl/Desktop/heroin_data/Benzo/tables/finalORsAndConf.txt", quote = FALSE, na = "0")

dfinc2 <- pvalOR(ORs[2], ORs[2,2], ORs[2,3])
dfinc2

dfinc3 <- pvalOR(ORs[3], ORs[3,2], ORs[3,3])
dfinc3

dfOD <- pvalOR(ORs[4], ORs[4,2], ORs[4,3])
dfOD

dfAnx <- pvalOR(ORs[5], ORs[5,2], ORs[5,3])
dfAnx

dfPres <- pvalOR(ORs[6], ORs[6,2], ORs[6,3])
dfPres

dfBing <- pvalOR(ORs[7], ORs[7,2], ORs[7,3])
dfBing

dfEnc <- pvalOR(ORs[8], ORs[8,2], ORs[8,3])
dfEnc

dfCoc <- pvalOR(ORs[9], ORs[9,2], ORs[9,3])
dfCoc
