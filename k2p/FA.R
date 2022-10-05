library(psych)
library(GPArotation)
library(tidyverse)

https://www.uwo.ca/fhs/tc/labs/10.FactorAnalysis.pdf

data("bfi")0
#tse <- time_space_ethic_ol %>% select(q1_1_social_contacts:q4_5_classroom_more_organized,q10_1_recorded_refrain_expressing:q10_7_zoom_is_less_safe)
tse <- time_space_ethic_ol

describe(bfi[1:25])
sum(complete.cases(bfi[1:25]))
cortest.bartlett(bfi[1:25])
KMO(bfi[1:25])
scree(bfi[,1:25])
fa.parallel(bfi[1:25],show.legend=F)

pa6.out <- fa(bfi[1:25],
              nfactors = 6,
              fm="pa",
              max.iter = 100,
              rotate = "oblimin")
fa.diagram(pa6.out)

pa5.out <- fa(bfi[1:25],
              nfactors = 5,
              fm="pa",
              max.iter = 100,
              rotate = "oblimin")
fa.diagram(pa5.out)

pa5.out$communality 
pa5.out$e.values[1:5]
pa5.out$values[1:5]


### # ### ##  ## ## ### ### ## ### ## ## ### ## ## ### ## ## ### #

describe(tse[c(2:25,36:42)])
sum(complete.cases(tse[c(2:25,36:42)]))
cortest.bartlett(tse[c(2:25,36:42)])
KMO(tse[c(2:25,36:42)])
scree(tse[,c(2:25,36:42)])
fa.parallel(tse[,c(2:25,36:42)],show.legend=F)

#Principal Axis Factoring (Common Factor Analysis)
pa5.out <- fa(tse[,c(2:25,36:42)], 
              nfactors = 5,
              fm="pa",
              max.iter = 100,
              rotate = "oblimin")
fa.diagram(pa5.out)

pa5.out$communality 

  #Eigenvalues
pa5.out$e.values[1:5]
pa5.out$values[1:5]

  # Percentage of Variance Accounted For
100*pa5.out$e.values[1:5]/length(pa5.out$e.values)
100*pa5.out$values[1:5]/length(pa5.out$values)

  #Rotated Solution
print(pa5.out$loadings, cutoff=0, digits=3)
print(pa5.out$Structure, cutoff=0, digits=3)

#Principal Components Analysis
pc5.out <- principal(tse[,c(2:25,36:42)],
                     nfactors = 5,
                     rotate = "oblimin")
fa.diagram(pc5.out)

pc5.out$communality

  #Eigenvalues and % of Variance Accounted For
pc5.out$values[1:5]
100*pc5.out$values[1:5]/length(pc5.out$values)
  #Rotated Solution
print(pc5.out$loadings, cutoff=0, digits=3)
print(pc5.out$Structure, cutoff=0, digits=3)






tse <- time_space_ethic_ol
Tse <- tse[,c(2:25,36:42)]
TSe <- na.omit(Tse)
cortest.bartlett(TSe)

tse.pca <- princomp(TSe)
summary(tse.pca)

library(nFactors)
e <- eigen((cor(TSe)))
nS <- nScree(e$values)
plotnScree(nS, legend = F)


