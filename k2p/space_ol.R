#library----
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)

library(psych)
library(REdaS)

library(psych)
library(readxl)
library(REdaS)
library(GPArotation)

# unused codes ----

ATGC <- read_excel("~/master_research/DATAs/k2p_2022/time_space_ethic_OL/ATGC_expfa.xlsx")

bart_spher(PCA_Example)  

KMO(PCA_Example) 

e <- eigen((cor(PCA_Example)))
e$values

fa(PCA_Example, nfactors = 3, rotate =  "oblimin" )

#PCA_Example ----
#1
cor(PCA_Example)

#2
data1.pca <- princomp(PCA_Example)
summary(data1.pca)

#3 plot

#4
data.fa <- factanal(PCA_Example, factors = 3, rotatation =  "varimax")
data.fa
data.fa$loadings

#5
print(data.fa,digits=8,cutoff=0.3,sort=T)

pa <- fa(PCA_Example, nfactors=3, rotate="varimax",fm="pa", residuals=T)
ml <- fa(PCA_Example, nfactors=3, rotate="varimax",fm="ml", residuals=T)
print(ml$loadings,cutoff = .3,digits = 3)

#6
M1<-fa(PCA_Example, nfactors = 3, rotate =  "oblimin" ) 
fa.diagram(M1,main="PCA_Example") 

# time_space_ethic_ol.csv ----
time_space_ethic_ol <- read.csv("~/master_research/DATAs/k2p_2022/time_space_ethic_OL/time_space_ethic_ol.csv")
tse <- time_space_ethic_ol %>% select(q1_1_social_contacts:q4_5_classroom_more_organized,q9_1_zoom_better_achievements:q10_7_zoom_is_less_safe)
#24
tse <- time_space_ethic_ol %>% select(q1_1_social_contacts:q4_5_classroom_more_organized)

tse <- na.omit(tse)

#1
cor(tse)

#2
tse.pca <- princomp(tse)
summary(tse.pca)

#3 plot
library(nFactors)
e <- eigen((cor(tse)))
nS <- nScree(e$values)
plotnScree(nS, legend = F)

#4
tse.fa <- 
  factanal(tse, factors = 9, rotatation =  "varimax")
tse.fa
tse.fa$loadings
tse.fa$factors

#5
print(tse.fa,digits=4,cutoff=0.3,sort=T)

#6
M1<-fa(tse, nfactors = 9, rotate =  "oblimin" ) 
fa.diagram(M1,main="time_space_ethic_ol")
M1$Vaccounted


# NEW ----------------------------------------------------------------------
EFA.data <- 
  time_space_ethic_ol %>%
  select(q1_1_social_contacts:q4_5_classroom_more_organized,
         q9_1_zoom_better_achievements:q10_7_zoom_is_less_safe)

dim(EFA.data)
summary(EFA.data)

#Missig Data
any(na.omit(EFA.data))
nomiss <- na.omit(EFA.data)
dim(nomiss)

#Outliers
df <-  ncol(nomiss)
cutoff <- qchisq(1-.001,ncol(nomiss))
mahal <- mahalanobis(nomiss,colMeans(nomiss),cov(nomiss))
ncol(nomiss)

summary(mahal<cutoff)
#   WE dont have outliers (outliers are under FALSE)
## exculude outlires 
noout <- subset(nomiss,mahal< cutoff)

# Assumptions

corfact <- cor(noout)
round(corfact,3)
round(corfact,2)

#Check that the determinant for the correlation matriz is posstive. 
det (corfact) 

#Check the determinant for the covariance matriz is postsve
det(cov(noout))

#Prints out the p-values for correlations in matriz
library(Hmisc) 
rcorr (as.matrix(noout))

#Normality
# Run a fake regression random 
rchisq(nrow (noout), df) 
fake = lm(random  -., data = data.frame (noout)) 
standardized = rstudent(fake) 
fitted = scale (fake$fitted.values)
# Histogram
library (lindia) 
gg_reshist (fake, bins = 10)

# KMO
KMO(noout) 

##  Write-up 
library(psych) 
bt <- cortest.bartlett(cor(noout), n= 125)

fa.noout <- fa(noout, nfactors = 9, rotate =  "promax", fm= "ml")

fa.noout <-fa(noout, nfactors = 9, rotate =  "promax", fm= "ml") 
fa.noout <-fa(noout, nfactors = 9, rotate =  "varimax", fm= "ml")#Factoring method = maximum likelihood factor analysis.
fa.noout <-fa(noout, nfactors = 9, rotate =  "oblimin", fm= "ml")

fa.noout <-fa(noout, nfactors = 9, rotate =  "promax", fm= "pa") 
fa.noout <-fa(noout, nfactors = 9, rotate =  "varimax", fm= "pa")# principal factor solution
fa.noout <-fa(noout, nfactors = 9, rotate =  "oblimin", fm= "pa")

print(fa.noout$loadings,digits=3,cutoff=0.3,sort=T)

##reliability 
#I ezcluded item 24 
attach(noout)
factor1 <- c(27,29,30,25,28)
factor2 <- c(31,32,34,35,36,37)
factor3 <- c(3,5,7,2,24,25)


psych:: alpha (noout[, factor1]) 
psych:: alpha (noout[, factor2]) 
psych:: alpha (noout[, factor3]) 

noout$f1 <- apply(noout[ ,factor1], 1, mean)
noout$f2 <- apply(noout[ ,factor2], 1, mean)
noout$f3 <- apply(noout[ ,factor3], 1, mean)

summary(noout)

sd(noout$f1)

rcorr(as.matrix(noout[,c(38:40)]))

# ---------------------------------------
# https://lhbikos.github.io/ReC_Psychometrics/PAF.html#exploratory-factor-analysis-with-a-quick-contrast-to-pca
dfGRMS <- tse

psych::describe(dfGRMS)

#write the simulated data  as a .csv
write.table(dfGRMS, file="dfGRMS.csv", sep=",", col.names=TRUE, row.names=FALSE)
#bring back the simulated dat from a .csv file
dfGRMS <- read.csv ("dfGRMS.csv", header = TRUE)
#to save the df as an .rds (think "R object") file on your computer; it should save in the same file as the .rmd file you are working with
saveRDS(dfGRMS, "dfGRMS.rds")
#bring back the simulated dat from an .rds file
dfGRMS <- readRDS("dfGRMS.rds")

GRMSr <- cor(dfGRMS) #correlation matrix (with the negatively scored item already reversed) created and saved as object
round(GRMSr, 2)

# We return to the KMO (Kaiser-Meyer-Olkin), an index of sampling adequacy that can be used with the actual sample to let us know if the sample size is sufficient (or if we should collect more data).
## bare minimum of .5
## values between .7 and .8 are good
psych::KMO(dfGRMS)


psych::cortest.bartlett(dfGRMS)

#det(GRMSr) 
det(cor(dfGRMS))#if using the raw data
# [1] 5.767166e-11 = 0.00000000005767166

getwd() 

pa9.out <- fa(dfGRMS,
              nfactors = 9,
              fm="pa",
              max.iter = 100,
              rotate = "oblimin")

pa5.out <- fa(dfGRMS,
              nfactors = 5,
              fm="pa",
              max.iter = 100,
              rotate = "oblimin")
fa.diagram(pa6.out)

pa6.out$communality

https://www.uwo.ca/fhs/tc/labs/10.FactorAnalysis.pdf

pa5.out$e.values
pa9.out$e.values