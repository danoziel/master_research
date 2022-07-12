#library----
library(readxl)
library(tidyverse)
library(psych)
library(kableExtra)
library(REdaS)

# time_space_ethic_ol.csv ----
time_space_ethic_ol <- read.csv("~/master_research/DATAs/k2p_2022/time_space_ethic_OL/time_space_ethic_ol.csv")
tseEX <- time_space_ethic_ol$q10_1_recorded_refrain_expressing %>% select(q1_1_social_contacts:q10_7_zoom_is_less_safe)

tse <- time_space_ethic_ol %>% select(q1_1:q2_5,q10_1:q10_7)

tse <- na.omit(tse)

describe(tse)%>% select(3:6,11:13) %>% 
  mutate_at(1:7,round,2) %>% kable() %>% kable_classic()

KMO(tse)

bart_spher(tse)  
 
#1
cor(tse)

#2

#3 eigenvalue
e <- eigen((cor(tse)))
e$values %>% kable() %>% kable_minimal()

scree(tse)

#4 varimax OR oblimin
tse.fa <- factanal(tse, factors = 4, rotatation =  "varimax")
tse.fa
tse.fa$loadings

#5
print(tse.fa,digits=2,cutoff=0.3,sort=T)

#6
M1<-fa(tse, nfactors = 4, rotate =  "varimax") 
fa.diagram(M1,main="time_space_ethic_ol")

# OPTIONS ----------------------------------------------------------------------

noout <- tse

fa.noout <-fa(noout, nfactors = 9, rotate =  "promax", fm= "ml") 
fa.noout <-fa(noout, nfactors = 9, rotate =  "varimax", fm= "ml")#Factoring method = maximum likelihood factor analysis.
fa.noout <-fa(noout, nfactors = 9, rotate =  "oblimin", fm= "ml")

fa.noout <-fa(noout, nfactors = 9, rotate =  "promax", fm= "pa") 
fa.noout <-fa(noout, nfactors = 9, rotate =  "varimax", fm= "pa")# principal factor solution
fa.noout <-fa(noout, nfactors = 9, rotate =  "oblimin", fm= "pa")

# https://lhbikos.github.io/ReC_Psychometrics/PAF.html#exploratory-factor-analysis-with-a-quick-contrast-to-pca
