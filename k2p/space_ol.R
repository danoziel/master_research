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
tse.fa <- factanal(tse, factors = 3, rotatation =  "varimax")
tse.fa
tse.fa$loadings

#5
print(tse.fa,digits=8,cutoff=0.3,sort=T)

#6
M1<-fa(tse, nfactors = 3, rotate =  "oblimin" ) 
fa.diagram(M1,main="time_space_ethic_ol")
