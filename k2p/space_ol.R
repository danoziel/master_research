#library----
library(readxl)
library(tidyverse)
library(psych)
library(kableExtra)
library(REdaS)

# time_space_ethic_ol.csv ----
time_space_ethic_ol <- read.csv("~/master_research/DATAs/k2p_2022/time_space_ethic_OL/time_space_ethic_ol.csv")

tse <- time_space_ethic_ol %>% select(q1_1:q2_5,q10_1:q10_7,gender,study)

tse <- na.omit(tse)

describe(tse)%>% select(3:6,11:13) %>% 
  mutate_at(1:7,round,2) %>% kable() %>% kable_classic()

time_space_ethic_ol <- time_space_ethic_ol %>% 
  rename (q3_1=q3_1_private_better,
         q3_2=q3_2_background_noises,
         q3_3=q3_3_embarrassed,
         q3_4=q3_4_not_bother_hear,
         q3_5=q3_5_adjusted_room,
         q3_6=q3_6_similar_to_classroom,
         q3_7=q3_7_family_intervened)

time_space_ethic_ol <- time_space_ethic_ol %>% 
  rename(q4_1=q4_1_dress_nicely,            
         q4_2=q4_2_care_of_appearance,                           
         q4_3=q4_3_satisfied_how_look,                        
         q4_4=q4_4_tend_to_standout,                                             
         q4_5=q4_5_classroom_more_organized)



# 1 business manager
# 2 Education
# 3 Law 
# 4 Health systems management
# 5 pre-academic preparation, management information systems and others
# 
# (1) Strongly disagree
# (2) Disagree
# (3) Neither agree nor disagree
# (4) Agree
# (5) Strongly agree

#t test men women----

library(rstatix)
library(ggpubr)

mydata.long <- tse %>% select(-study) %>% 
  pivot_longer(-gender, names_to = "variables", values_to = "value")

stat.test <- mydata.long %>%
  group_by(variables) %>%
  t_test(value ~ gender) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()

stat.mean <- mydata.long %>%drop_na()%>%  group_by(gender,variables) %>% 
  summarise(mean=mean(value)) %>% mutate(gender = as.character(gender))

stat.sd <- mydata.long %>%drop_na()%>%  group_by(gender,variables) %>% 
  summarise(SD=sd(value)) %>% 
  mutate(gender  = ifelse(gender == 1, "sd_man","sd_woman")) %>% 
  spread(gender,SD) %>% mutate_at(2:3,round,2)

stat.t.n <- stat.test %>% rename(gender=group1) %>%
  left_join(stat.mean) %>% rename(men_mean=mean,men=gender, gender=group2) %>% 
  left_join(stat.mean) %>% rename(women_mean=mean,women=gender) %>% 
  select(variables,statistic,df,p,men_mean,women_mean)
stat.t.n <- stat.t.n %>% mutate_at(2:6,round,3) %>% 
  mutate_at(2:6,round,3) %>% left_join(stat.sd) %>% mutate_at(5:6,round,2) %>% 
  select(variables,statistic,df,p,men_mean,sd_man,women_mean,sd_woman)

t.test(q1_1 ~ gender, data = tse)

kable(stat.t.n) %>% kable_classic()

rm(mydata.long,stat.test,stat.mean,stat.sd,stat.t.n,tse)

# Qs3
stu_data = table(wordy_df$gender,wordy_df$q3_1_private_better) 
stu_data = table(wordy_df$gender,wordy_df$q3_2_background_noises) 
stu_data = table(wordy_df$gender,wordy_df$q3_3_embarrassed) 
stu_data = table(wordy_df$gender,wordy_df$q3_4_not_bother_hear) 
stu_data = table(wordy_df$gender,wordy_df$q3_5_adjusted_room) 
stu_data = table(wordy_df$gender,wordy_df$q3_6_similar_to_classroom) 
stu_data = table(wordy_df$gender,wordy_df$q3_7_family_intervened) 

model <- chisq.test(stu_data)



# Qs4
stu_data = table(wordy_df$gender,wordy_df$q4_1_dress_nicely)
chisq.test(stu_data)
stu_data = table(wordy_df$gender,wordy_df$q4_2_care_of_appearance) 
chisq.test(stu_data)
stu_data = table(wordy_df$gender,wordy_df$q4_3_satisfied_how_look) 
chisq.test(stu_data)
stu_data = table(wordy_df$gender,wordy_df$q4_4_tend_to_standout) 
chisq.test(stu_data)
stu_data = table(wordy_df$gender,wordy_df$q4_5_classroom_more_organized) 
chisq.test(stu_data)




# Qs9

stu_data = table(wordy_df$gender,wordy_df$q9_1_zoom_better_achievements)
chisq.test(stu_data)
stu_data = table(wordy_df$gender,wordy_df$q9_2_look_is_importance)
chisq.test(stu_data)
stu_data = table(wordy_df$gender,wordy_df$q9_3_prefer_zoom)
chisq.test(stu_data)
stu_data = table(wordy_df$gender,wordy_df$q9_4_dont_need_social_classmates)
chisq.test(stu_data)
stu_data = table(wordy_df$gender,wordy_df$q9_5_continue_zoom)
chisq.test(stu_data)
stu_data = table(wordy_df$gender,wordy_df$q9_6_zoom_has_benefit)
chisq.test(stu_data)

#  wordy data for chi test ----

stu_data = table(wordy_df$gender,wordy_df$q3_1_private_better) 
model <- chisq.test(stu_data)
model$expected
model$observed

library(stringr)

wordy_df <- time_space_ethic_ol %>% mutate(id= 1:n())
wordy_df$id <- str_pad(wordy_df$id, 3, pad = "0")

#Qs1 Qs2 Qs10 likart scale 1-5

wordy_df$gender[wordy_df$gender==1] <- "Men"
wordy_df$gender[wordy_df$gender==2] <- "Women"

wordy_scale <- wordy_df %>% 
  select(id,q1_1:q2_5,q10_1:q10_7)

wordy_scale[wordy_scale == 5] <- "Strongly agree"
wordy_scale[wordy_scale == 4] <- "Agree"
wordy_scale[wordy_scale == 3] <- "Neutral"
wordy_scale[wordy_scale == 2] <- "Disagree"
wordy_scale[wordy_scale == 1] <- "Strongly disagree"
  
#Qs3 Qs4 q7 q8 Qs9 Y/N
yesno_scale <- wordy_df %>%
  select(id,q3_1_private_better:q4_5_classroom_more_organized, q7_used_luxury_in_zoom,q8_good_torso,
         q9_1_zoom_better_achievements: q9_6_zoom_has_benefit)
yesno_scale[yesno_scale == 0] <- "No"
yesno_scale[yesno_scale == 1] <- "Yes"

#Q5 Q6 multiply question
multiply_Q5 <- wordy_df %>%
  select(id, q5_how_able._to_standout_zoom)

multiply_Q5[multiply_Q5==0] <-  "It is not possible to stand out because there are many participants"
multiply_Q5[multiply_Q5==1] <-"It is possible to stand out by actively participating during the class"
multiply_Q5[multiply_Q5==2] <-"It is possible to stand out by prolonged silences"
multiply_Q5[multiply_Q5==3] <-"Other"

multiply_Q6 <- wordy_df %>%
  select(id, q6_gain_acceptance_in_zoom)

multiply_Q6[multiply_Q6==1] <-"Help and assistance to friends in the online class"
multiply_Q6[multiply_Q6==2] <-"Active participation in class"
multiply_Q6[multiply_Q6==3] <-"Self-expression through silence and resistance"
multiply_Q6[multiply_Q6==4] <-"Other"

wordy_df <- wordy_df %>% select(id,gender,study) %>% 
  left_join(wordy_scale) %>% 
  left_join(yesno_scale) %>% 
  left_join(multiply_Q5) %>% 
  left_join(multiply_Q6)

rm(wordy_scale,yesno_scale,multiply_Q5,multiply_Q6)

# Q5 ----
stu_data = table(wordy_df$gender,wordy_df$q5_how_able._to_standout_zoom)
chisq.test(stu_data)
# Q6
stu_data = table(wordy_df$gender,wordy_df$q6_gain_acceptance_in_zoom)
chisq.test(stu_data)
# Q7 Y/N
stu_data = table(wordy_df$gender,wordy_df$q7_used_luxury_in_zoom)
chisq.test(stu_data)
# Q8 Y/M
stu_data = table(wordy_df$gender,wordy_df$q8_good_torso)
chisq.test(stu_data)

#2 [Qs3]YN/[Qs2] ----
# Satisfaction with learning/Relationship with the learning guide
data <- time_space_ethic_ol %>%select (q3_1:q3_7,q2_1:q2_5) %>% na.omit()

library("Hmisc")
data.cor <- rcorr(as.matrix(data),type="pearson")
data.cor <-round(data.cor,2)

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(res2$r, res2$P) %>% 
  rename("Satisfaction with learning"=row,
         "Relationship with the learning guide"=column,
         `r`=cor,`p=0.05`=p) %>% mutate_at(3:4,round,2) %>% 
  filter(row_number() %in% c(22:35,37:43,46:52,56:62)) %>% 
  kable() %>% kable_classic()

library(corrplot)
# visualizing correlogram
# as circle
corrplot(M, method="circle")

# as pie
corrplot(M, method="pie")

# as colour
corrplot(M, method="color")

# as number
corrplot(M, method="number")

library("Hmisc")
rcorr(as.matrix(data),type="pearson")


#3 [Qs10]/[Qs4 YN] ----
# Feeling confident in learning on Zoom /Satisfied with the appearance
data104 <- time_space_ethic_ol %>%
  select(q10_1:q10_7,q4_1:q4_5) %>% na.omit()

rcorr(as.matrix(data104),type="pearson")
res2<-rcorr(as.matrix(data104))

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(res2$r, res2$P) %>% 
  rename("Feeling confident in learning on Zoom"=row,
         "Satisfied with the appearance"=column,
         `r`=cor,`p=0.05`=p) %>% mutate_at(3:4,round,2) %>% 
  filter(row_number() %in% c(22:35,37:43,46:52,56:62)) %>% 
  kable() %>% kable_classic()



#4 [Qs3 YN /Qs10] ----
#Does 'presence of convenience' affect 'self-expression ability' in Zoom

data310 <- time_space_ethic_ol %>%
  select(q3_1:q3_7,q10_1:q10_7) %>% na.omit()

rcorr(as.matrix(data310),type="pearson")
res2<-rcorr(as.matrix(data310))

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(res2$r, res2$P) %>% 
  rename("Presence of convenience"=row,
         "Self-expression ability"=column,
         `r`=cor,`p=0.05`=p) %>% mutate_at(3:4,round,2) %>% 
  filter(row_number() %in% c(22:35,37:43,46:52,56:62,67:73,79:85)) %>% 
  kable() %>% kable_classic()


rm(Q3sQs10)
#factor analyses ----

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
