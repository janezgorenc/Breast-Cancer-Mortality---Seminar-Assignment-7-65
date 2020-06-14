# A


#Reading the table


read.table("cancer_R.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
summary(cancer_R)

#Renaming the headers

names(cancer_R)[names(cancer_R)=="X1"]<-"Mortality"
names(cancer_R)[names(cancer_R)=="X2"]<-"Women"
cancer_R

#Creating a subset - Mortality

cancer_R<-subset(cancer_R, cancer_R$Mortality<361)

summary(Mortality)
attach(cancer_R)

#Creating the frequency histogram

hist(Mortality, breaks = 18)

#Disigning the histogram; bins, labels

min(Mortality)
max(Mortality)
bins=seq(min(Mortality), max(Mortality), 20)
hist(Mortality, breaks = bins,
  main="Breast Cancer Mortality Frequency Distribution",
  ylab="Frequency",
  xlab="Breast Cancer Mortality",
  col = "blue")



Frequency_dis=count(Mortality)
Frequency_dis

attach(Frequency_dis)

Frequency_dis$relative_fre=(freq/301/20)
Frequency_dis

Frequency_dis=relative_fre[order(relative_fre, decreasing=T)]

hist(Mortality, freq=F, breaks = 18,
     main = "Breast Cancer Mortality Relative Frequency Distribution",
     ylab = "Relative Frequency",
     xlab = "Mortality",
     col = "green")

# B

#Computing the population mean (of cancer mortality), population total, population variance, and population standard deviation

##Population mean and population total 
pop_mean=sum(Mortality)/nrow(cancer_R)
pop_mean

sum(Mortality)

##Population variance and SD

pop_var=(sum((Mortality-pop_mean)^2))/nrow(cancer_R)
pop_var


pop_var_SD=sqrt(pop_var)
pop_var_SD

# C

# Histogram of means

## First attempr

samp_25hist<-replicate(1000, {
  samp<-sample(Mortality, 25, replace = F, prob = NULL)
  mean(samp)
})
hist(samp_25hist, freq = F, breaks = 12,
     main = "Breast Cancer Mortality - Histogram of Means",
     ylab = "Relative Frequency",
     xlab = "Mortality",
     col = "green")

## Second Attempt

samp_25hist<-replicate(1000, {
  samp<-sample(Mortality, 25, replace = F)
  mean(samp)
})
hist(samp_25hist, freq = F, breaks = 12,
     main = "Breast Cancer Mortality - Histogram of Means",
     ylab = "Relative Frequency",
     xlab = "Mortality",
     col = "orange")

mean(samp_25hist)

# D

# Mean of sample of 25

samp<-sample(Mortality, 25, replace = F, prob = NULL)
samp
mean(samp)
sum(samp)
samp_estimate_of_total=mean(samp)*nrow(cancer_R)
samp_estimate_of_total

# E

# Estimate of population variance and standard deviation

estimate_pop_var=(sum((samp-mean(samp))^2))/24
estimate_pop_var

unbiased_estimate_popvar=estimate_pop_var*((nrow(cancer_R)-1)/nrow(cancer_R))
unbiased_estimate_popvar                                           
unbiased_estimate_popvar_SD=sqrt(unbiased_estimate_popvar)
unbiased_estimate_popvar_SD

# F

# 95% CI for population mean and total from (d)

# sample variance and standard error of the means

est_SV=(estimate_pop_var/25)*((nrow(cancer_R)-25)/(nrow(cancer_R)-1))
est_SV

SEM=sqrt(est_SV)
SEM

# 95% CI for pop mean

(34.92-1.96*8.308)
(34.92+1.96*8.308)

# standard error of total

((301)^2)*69.033
sqrt(6254459)

# 95% CI for pop total

(10511-1.96*2500.892)
(10511+1.96*2500.892)

#G 

# draw sample of 100

samp<-sample(Mortality, 100, replace = F, prob = NULL)
samp
mean(samp)
sum(samp)
samp_estimate_of_total=mean(samp)*nrow(cancer_R)
samp_estimate_of_total

# Estimate of population variance and standard deviation

estimate_pop_var=(sum((samp-mean(samp))^2))/99
estimate_pop_var

unbiased_estimate_popvar=estimate_pop_var*((nrow(cancer_R)-1)/nrow(cancer_R))
unbiased_estimate_popvar                                           
unbiased_estimate_popvar_SD=sqrt(unbiased_estimate_popvar)
unbiased_estimate_popvar_SD


# 95% CI for population mean and total from (d)

# sample variance and standard error of the means

est_SV=(estimate_pop_var/100)*((nrow(cancer_R)-100)/(nrow(cancer_R)-1))
est_SV

SEM=sqrt(est_SV)
SEM

# 95% CI for pop mean

(32.57-1.96*2.603)
(32.57+1.96*2.603)

# standard error of total

((301)^2)*6.744
sqrt(611013.1)

# 95% CI for pop total

(9801-1.96*781.673)
(9801+1.96*781.673)


# H

cor(Mortality, Women, method = c("pearson"))
plot(Mortality, Women, main = "Scatterplot",
     xlab = "Breast Cancer Mortality", ylab = "Population of White Women", pch=19)
# I

hist_RatioEst<-replicate(5000, {sampMF<-cancer_R[sample(nrow(cancer_R), 25),]
Ratio_Estimator=mean(Women)*(mean(sampMF$Mortality)/mean(sampMF$Women))
})

hist(hist_RatioEst, freq = F, breaks = 20,
     main = "Ratio Estimators of Mean Cancer Mortality",
     ylab = "Relative Frequency",
     xlab = "Ratio Estimators",
     col = "blue"
     )
     

# J

# Sample 25, get pop mean and total canc mort with ratio estimates

## Sample 25

sampMF_no_repl<-cancer_R[sample(nrow(cancer_R), 25),]
View(sampMF_no_repl)

mean(sampMF_no_repl$Mortality)     

mean(sampMF_no_repl$Women)

sum(sampMF_no_repl$Mortality)     

sum(sampMF_no_repl$Women)

Ratio_Estimator_no_repl=mean(Women)*(mean(sampMF_no_repl$Mortality)/mean(sampMF_no_repl$Women))
Ratio_Estimator_no_repl

PopToTRatioEstimator_no_repl=nrow(cancer_R)*mean(Women)*(mean(sampMF_no_repl$Mortality)/mean(sampMF_no_repl$Women))
PopToTRatioEstimator_no_repl


# K

# CI for part J

Ratio=mean(sampMF_no_repl$Mortality)/mean(sampMF_no_repl$Women)
Ratio
SqErPop=(1/24)*(sum(((sampMF_no_repl$Women)-mean(sampMF_no_repl$Women))^2))
SqErPop

SqErMor=(1/24)*(sum(((sampMF_no_repl$Mortality)-mean(sampMF_no_repl$Mortality))^2))
SqErMor


SqErMor=(1/24)*(sum(((sampMF_no_repl$Mortality)-mean(sampMF_no_repl$Mortality))^2))

# Their sample covariance

SampCov=(1/24)*(sum((sampMF_no_repl$Women-mean(sampMF_no_repl$Women))*(sampMF_no_repl$Mortality-mean(sampMF_no_repl$Mortality))))
SampCov


(sum((sampMF_no_repl$Women-mean(sampMF_no_repl$Women))*(sampMF_no_repl$Mortality-mean(sampMF_no_repl$Mortality))))/24


#estimated standerd error of the ratio estimate


EstErOfRatioEst<-sqrt((1/24)*(196/300)*((Ratio^2)*SqErPop+SqErMor-2*Ratio*SampCov))
EstErOfRatioEst

# CIs for estimated sample mean

Ratio_Estimator_no_repl-1.96*EstErOfRatioEst

Ratio_Estimator_no_repl+1.96*EstErOfRatioEst


# CIs for estimated population total

EstErOfPopTotRatEst=nrow(cancer_R)*EstErOfRatioEst
EstErOfPopTotRatEst

11660.79-1.96*EstErOfPopTotRatEst

11660.79+1.96*EstErOfPopTotRatEst


# L

# Stratification

stratum1<-subset(cancer_R, cancer_R$Women<5000)
View(stratum1)

stratum2interim<-subset(cancer_R, cancer_R$Women<10000)
stratum2<-subset(stratum2interim, stratum2interim$Women>5000)
View(stratum2)

stratum3interim<-subset(cancer_R, cancer_R$Women<30000)
stratum3<-subset(stratum3interim, stratum3interim$Women>10000)
View(stratum3)

stratum4<-subset(cancer_R, cancer_R$Women>30000)
View(stratum4)

# statistically describing the respective strata

MU1<-mean(stratum1$Mortality)
VAR1<-var(stratum1$Mortality)
SD1<-sd(stratum1$Mortality)
W1<-nrow(stratum1)/nrow(cancer_R)
W1

MU2<-mean(stratum2$Mortality)
VAR2<-var(stratum2$Mortality)
SD2<-sd(stratum2$Mortality)
W2<-nrow(stratum2)/nrow(cancer_R)
W2

MU3<-mean(stratum3$Mortality)
VAR3<-var(stratum3$Mortality)
SD3<-sd(stratum3$Mortality)
W3<-nrow(stratum3)/nrow(cancer_R)
W3

MU4<-mean(stratum4$Mortality)
VAR4<-var(stratum4$Mortality)
SD4<-sd(stratum4$Mortality)
W4<-nrow(stratum4)/nrow(cancer_R)
W4

# sampling a sample of 6 from each stratum

str1_samp<-sample(stratum1$Mortality, 6, replace = F, prob = NULL)
str1_samp

str2_samp<-sample(stratum2$Mortality, 6, replace = F, prob = NULL)
str2_samp


str3_samp<-sample(stratum3$Mortality, 6, replace = F, prob = NULL)
str3_samp


str4_samp<-sample(stratum4$Mortality, 6, replace = F, prob = NULL)
str4_samp

# computing the means of the respective samples

M1<-mean(str1_samp)
M2<-mean(str2_samp)
M3<-mean(str3_samp)
M4<-mean(str4_samp)

# estimating the population mean

EPM<-(W1*M1)+(W2*M2)+(W3*M3)+(W4*M4)
EPM

# estimating the population total

EPT=nrow(cancer_R)*EPM
EPT

# M

# proportional and optimal allocation

# the denominator for the optimal allocation

SUM=(W1*SD1+W2*SD2+W3*SD3+W4*SD4)
SUM

# sampling fractions for optimal allocation

SFO1=W1*SD1/SUM
SFO1

SFO2=W2*SD2/SUM
SFO2

SFO3=W3*SD3/SUM
SFO3

SFO4=W4*SD4/SUM
SFO4

# comparing variances

# srs & sp - Var(Ysrs)-Var(Ysp)

MU<-mean(cancer_R$Mortality)
MU




SDstrata<-W1*SD1+W2*SD2+W3*SD3+W4*SD4
SDstrata

numerator<-(W1*(MU1-MU)^2)+(W2*(MU2-MU)^2)+(W3*(MU3-MU)^2)+(W4*(MU4-MU)^2)
numerator


denom<-W1*(VAR1)+W2*(VAR2)+W3*(VAR3)+W4*(VAR4)
denom


numerator/denom

VAR1
VAR2
VAR3
VAR4

# srs & so

denom2<-(W1*SD1+W2*SD2+W3*SD3+W4*SD4)^2
denom2

numerator/denom2

#sp &so

numerator2<-(W1*(SD1-SDstrata)^2)+(W2*(SD2-SDstrata)^2)+(W3*(SD3-SDstrata)^2)+(W4*(SD4-SDstrata)^2)
numerator2

numerator2/denom2

MU-EPM
1-MU/EPM
1-EPM/MU

# N

# More strata

stratumA<-subset(cancer_R, cancer_R$Women<3000)

stratumB<-subset(cancer_R, cancer_R$Women>3000 & cancer_R$Women<6000)

stratumC<-subset(cancer_R, cancer_R$Women>6000 & cancer_R$Women<10000)

stratumD<-subset(cancer_R, cancer_R$Women>10000 & cancer_R$Women<16000)

stratumE<-subset(cancer_R, cancer_R$Women>16000 & cancer_R$Women<23000)

stratumF<-subset(cancer_R, cancer_R$Women>23000 & cancer_R$Women<32000)

stratumG<-subset(cancer_R, cancer_R$Women>32000 & cancer_R$Women<42000)

stratumH<-subset(cancer_R, cancer_R$Women>42000)


View(stratumA)
View(stratumB)
View(stratumC)
View(stratumD)
View(stratumE)
View(stratumF)
View(stratumG)
View(stratumH)


MUA<-mean(stratumA$Mortality)
WA<-nrow(stratumA)/nrow(cancer_R)

MUB<-mean(stratumB$Mortality)
WB<-nrow(stratumB)/nrow(cancer_R)

MUC<-mean(stratumC$Mortality)
WC<-nrow(stratumC)/nrow(cancer_R)

MUD<-mean(stratumD$Mortality)
WD<-nrow(stratumD)/nrow(cancer_R)

MUE<-mean(stratumE$Mortality)
WE<-nrow(stratumE)/nrow(cancer_R)

MUF<-mean(stratumF$Mortality)
WF<-nrow(stratumF)/nrow(cancer_R)

MUG<-mean(stratumG$Mortality)
WG<-nrow(stratumG)/nrow(cancer_R)

MUH<-mean(stratumH$Mortality)
WH<-nrow(stratumH)/nrow(cancer_R)


MUA
MUB
MUC
MUD
MUE
MUF
MUG
MUH
WA
WB
WC
WD
WE
WF
WG
WH

WA+WB+WC+WD+WE+WF+WG+WH

# total mean 

MUA*WA+MUB*WB+MUC*WC+MUD*WD+MUE*WE+MUF*WF+MUG*WG+MUH*WH

EMUhist<-replicate(1000, {
strA<-sample(stratumA$Mortality, 6, replace = F, prob = NULL)
strB<-sample(stratumB$Mortality, 6, replace = F, prob = NULL)
strC<-sample(stratumC$Mortality, 6, replace = F, prob = NULL)
strD<-sample(stratumD$Mortality, 6, replace = F, prob = NULL)
strE<-sample(stratumE$Mortality, 6, replace = F, prob = NULL)
strF<-sample(stratumF$Mortality, 6, replace = F, prob = NULL)
strG<-sample(stratumG$Mortality, 6, replace = F, prob = NULL)
strH<-sample(stratumH$Mortality, 6, replace = F, prob = NULL)

MUAst<-mean(strA)
MUBst<-mean(strB)
MUCst<-mean(strC)
MUDst<-mean(strD)
MUEst<-mean(strE)
MUFst<-mean(strF)
MUGst<-mean(strG)
MUHst<-mean(strH)




# estimate of the total mean 

EMU=WA*MUAst+WB*MUBst+WC*MUCst+WD*MUDst+WE*MUEst+WF*MUFst+WG*MUGst+WH*MUHst

})
hist(EMUhist, freq = F,
     main = "Histogram of Estimated Population Means",
     ylab = "Relative Frequency",
     xlab = "Estimates",
     col = "white")


