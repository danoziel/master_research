library(tidyverse)
library(readxl)
library(kableExtra)
library(sjPlot)

### fake_A: arrange data ----

Qs_n_code <- read_excel("~/master_research/DATAs/k2p_2022/FakeNews_polls/fake1_practice_431217.xlsx","Qs_n_code")

fake1_practice_431217 <- read_excel("~/master_research/DATAs/k2p_2022/FakeNews_polls/fake1_practice_431217.xlsx","fake1_clean_data")
fake2_tips_431216 <- read_excel("~/master_research/DATAs/k2p_2022/FakeNews_polls/fake2_tips_431216.xlsx","fake2_clean_data")
fake0_control_431212 <- read_excel("~/master_research/DATAs/k2p_2022/FakeNews_polls/fake0_control_431212.xlsx", "fake0_clean_data")

# rbind the databases
fake_A <- rbind(fake1_practice_431217 ,fake2_tips_431216,fake0_control_431212)
rm(fake1_practice_431217 ,fake2_tips_431216,fake0_control_431212)

# combine columns by subject by mean

fake_b <- 
  fake_A %>% 
  mutate(living_cost_opinion = rowMeans(across(Q7A:Q7E)),   # living_cost_opinion
         global_warming_opinion = rowMeans(across(Q8A:Q8E)),# global_warming_opinion
         ev_positive= rowMeans(across(Q3A_1:Q3A_10)),
         ev_negative=rowMeans(across(Q3A_11:Q3A_20)),
         ev_expertness=rowMeans(across(Q5A_1:Q5A_5 )) ,
         ev_trustworthiness =rowMeans(across(Q5A_6:Q5A_10 )) ,
         ev_unreliable =rowMeans(across(Q6A_1:Q6A_6 )) ,
         ev_helpful =rowMeans(across(Q6A_7:Q6A_10 ))
         ) %>% 
  rename(ev_accurate_post=Q4A,#dependent variable
         ev_link=Q7A_1, ev_share=Q7A_2,ev_like=Q7A_3,ev_comment=Q7A_4) %>% 
  
  mutate(hc_positive= rowMeans(across(Q3B_1:Q3B_10)),
         hc_negative=rowMeans(across(Q3B_11:Q3B_20)),
         hc_expertness=rowMeans(across(Q5B_1:Q5B_5 )) ,
         hc_trustworthiness =rowMeans(across(Q5B_6:Q5B_10 )) ,
         hc_unreliable =rowMeans(across(Q6B_1:Q6B_6 )) ,
         hc_helpful =rowMeans(across(Q6B_7:Q6B_10 ))
  ) %>% 
  rename(hc_accurate_post=Q4B,#dependent variable
         hc_link=Q7B_1, hc_share=Q7B_2,hc_like=Q7B_3,hc_comment=Q7B_4) %>% 
  
  mutate(cc_positive= rowMeans(across(Q3C_1:Q3C_10)),
         cc_negative=rowMeans(across(Q3C_11:Q3C_20)),
         cc_expertness=rowMeans(across(Q5C_1:Q5C_5 )) ,
         cc_trustworthiness =rowMeans(across(Q5C_6:Q5C_10 )) ,
         cc_helpful =rowMeans(across(Q6C_1:Q6C_4 ))
  ) %>% 
  rename(cc_accurate_post=Q4C,#dependent variable
         cc_link=Q7C_1, cc_share=Q7C_2,cc_like=Q7C_3,cc_comment=Q7C_4,
         party22=Q6_party22,party21=Q5_party21,
         income=SELFINCOME)

# delete unnecessary vars
fake_c <- 
  fake_b %>%
  select(-starts_with("Q"))%>% 
  select(-starts_with("S1"))%>% 
  select(-starts_with("S2"))%>% 
  select(-starts_with("S3"))%>% 
  select(-starts_with("S4"))

# living_cost_opinion + global_warming_opinion 
fake_c %>% group_by(group,split) %>% 
  summarise_at(c("living_cost_opinion" , "global_warming_opinion"), mean, na.rm = TRUE) %>% 
  kable() %>% kable_minimal()

# electric_vehicle
fake_c %>% group_by(group,split) %>% 
  summarise_at(c("ev_positive", "ev_negative",
                 "ev_expertness", "ev_trustworthiness",
                 "ev_unreliable", "ev_helpful",
                 "ev_accurate_post", "ev_link", "ev_share", "ev_like", "ev_comment")
               , mean, na.rm = TRUE) %>% 
  kable() %>% kable_minimal()

# housing_crisis
fake_c %>% group_by(group,split) %>% 
  summarise_at(c("hc_positive", "hc_negative",
                 "hc_expertness", "hc_trustworthiness",
                 "hc_unreliable", "hc_helpful",
                 "hc_accurate_post", "hc_link", "hc_share", "hc_like", "hc_comment")
               , mean, na.rm = TRUE) %>% 
  kable() %>% kable_minimal()

# climate crisis
fake_c %>% group_by(group,split) %>% 
  summarise_at(c("cc_positive", "cc_negative",
                 "cc_expertness", "cc_trustworthiness",
                  "cc_helpful",
                 "cc_accurate_post", "cc_link", "cc_share", "cc_like", "cc_comment")
               , mean, na.rm = TRUE) %>% 
  kable() %>% kable_minimal()

rm(fake_b)

# Q4  As far as you know. How accurate is the information in the post you read?   -----

fake_d<- fake_c %>% 
  group_by(split,group) %>% 
  count()
fake_d$subgroup<- c("interventionless_1","practice_1","tips_1",
                     "interventionless_2","practice_2","tips_2",
                     "interventionless_3","practice_3","tips_3",
                     "interventionless_4","practice_4","tips_4")

fake_e <- left_join(fake_c,fake_d) %>% 
  select(1:8,income,
         "living_cost_opinion","global_warming_opinion",
       ev_accurate_post,
         "ev_positive","ev_negative","ev_expertness",
         "ev_trustworthiness","ev_unreliable","ev_helpful",
         "ev_link","ev_share","ev_like","ev_comment",
         hc_accurate_post,
         "hc_positive","hc_negative","hc_expertness",
         "hc_trustworthiness","hc_unreliable","hc_helpful",
         "hc_link","hc_share","hc_like","hc_comment",
         cc_accurate_post,
         "cc_positive","cc_negative","cc_expertness",
         "cc_trustworthiness","cc_helpful",
         "cc_link","cc_share", "cc_like","cc_comment",
       n,subgroup,group) %>% rename(n_per_subgroup=n)

fake_reg <- fake_e
rm(fake_c,fake_d,fake_e)
   


#---- reg -----
fake_reg
A=ev
B=hc
C=cc
# bb_ev_video_1A
#             lapid_hc_NO_graph	1B
#             cc_graph	1C
# 
#             bb_ev_NO_video	2A
#             lapid_hc_graph	2B
#             cc_video	2C
# 
# lapid_ev_video_3A
#            bb_hc_graph	3B
#            cc_NO_graph	3C
# 
#            lapid_ev_NO_video	4A
#            bb_hc_NO_graph	4B


#_______________________________________________________________________________
#ev_video   bb_ev_video_1A VS.lapid_ev_video_3A

bb_ev_video_1A<-fake_reg %>% filter(subgroup=="practice_1")
bb_ev_video_1A<-fake_reg %>% filter(subgroup=="tips_1")
bb_ev_video_1A<-fake_reg %>% filter(subgroup=="interventionless_1")
lapid_ev_video_3A <-fake_reg %>% filter(subgroup=="practice_3")
lapid_ev_video_3A <-fake_reg %>% filter(subgroup=="tips_3")
lapid_ev_video_3A <-fake_reg %>% filter(subgroup=="interventionless_3")

reg_ev_video
o_1AI<- lm(ev_accurate_post ~ ev_positive + ev_negative +
                     ev_expertness+ev_trustworthiness+
                     ev_unreliable+ev_helpful+ev_link+ev_share+ev_like+ev_comment+
                     living_cost_opinion+global_warming_opinion,
                   bb_ev_video_1A
                   )

lm(ev_accurate_post ~ ev_positive+ev_negative+ev_expertness+ev_trustworthiness+ev_unreliable+ev_helpful+ev_link+ev_share+ev_like+ev_comment+living_cost_opinion+global_warming_opinion,



tab_model(
  o_1AP,o_1AT,o_1AI,
  digits = 3,
  p.style = "stars",
  show.ci = F)


summary(reg_ev_video)

tab_model(m1,  digits = 2,
          digits.p = 3,
          p.style="numeric",
          show.se = TRUE,
          p.style = "stars",
          
                    string.ci = "Conf. Int (95%)",
          dv.labels = c("Netanyaho WITH video | practice group (1A)"))


tab_model(
  m1,m2,m3, string.ci = "Conf. Int (95%)",show.se = TRUE
  )
  
  
  p.val = "kr", 
  show.zeroinf = T, 
  show.se = T, 
  show.df = T, 
  show.re.var = T,
  # to better distinguish the model columns
  # CSS = list(
  #   modelcolumn1 = "background-color: #f0f0f0;", 
  #   modelcolumn3 = "background-color: #f0f0f0;", 
  #   modelcolumn5 = "background-color: #f0f0f0;"
  )




# --- ------------------------------
# --------------------------------------------- ---- ----

library("corrplot")
M<-cor(mt)
corrplot(M, method="number")

12:22,23:33,34:43

library("PerformanceAnalytics")
chart.Correlation(
  fake_e  %>% filter(group_=="p4")%>% select(12,10:11,13:22),
  histogram=F, pch=19)
  
  
  
P4 <-   fake_e  %>% filter(group_=="p4")%>% select(12,10:11,13:22), # לפיד בלי סרטון רכב
P3 <-   fake_e  %>% filter(group_=="p3")%>% select(12,10:11,13:22),# לפיד סרטון רכב

C4 <- fake_e  %>% filter(group_=="c4")%>% select(12,10:11,13:22), # לפיד בלי סרטון רכב
C3 <-   fake_e  %>% filter(group_=="c3")%>% select(12,10:11,13:22),# לפיד סרטון רכב





p4 p3 c4 c3



c1 <- fake_e %>% filter(group_ == "c1")
p1 <- fake_e %>% filter(group_ == "p1")
t1<- fake_e %>% filter(group_ == "t1")

c2 <-  fake_e %>% filter(group_ == "c2")
p2<- fake_e %>% filter(group_ == "p2")
t2

c3 <-  fake_e %>% filter(group_ == "c3")
p3<- fake_e %>% filter(group_ == "p3")
t3

c4 <-  fake_e %>% filter(group_ == "c4")
p4<- fake_e %>% filter(group_ == "p4")
t4


# lapid ev no
reg_c4 = lm(ev_accurate_post ~ ev_positive+ ev_negative, c4)#_
reg_c4 = lm(ev_accurate_post ~ev_expertness+ev_trustworthiness,  c4)#_
reg_c4 = lm(ev_accurate_post ~ev_unreliable+ev_helpful, c4)#_
reg_c4 = lm(ev_accurate_post ~ev_link+ev_share+ev_like+ev_comment, c4)#___
summary(reg_c4) 
# 
reg_p1 = lm(ev_accurate_post ~ ev_positive, p1)
summary(reg_p1)
reg_p2 = lm(ev_accurate_post ~ ev_positive, p2)
summary(reg_p2)
reg_p3 = lm(ev_accurate_post ~ ev_positive, p3)
summary(reg_p3)

p4 Lapid ev no P
p3 Lapid ev yes P
c4 Lapid ev no C
c3 Lapid ev yes C
# lapid ev 
reg_p4 = lm(ev_accurate_post ~ ev_positive + ev_negative +
              ev_expertness+ev_trustworthiness+
              ev_unreliable+ev_helpful+
              ev_link+ev_share+ev_like+ev_comment+
            living_cost_opinion+global_warming_opinion,C3)


summary(reg_p4)
tab_model(reg_p4,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("ev_accurate_post - apid ev yes C "))


reg_t1 = lm(ev_accurate_post ~ ev_positive, t1)
summary(reg_t1) 
tab_model(reg_t1) 















# write.csv ----
write.csv(fake_reg, file = "C:/Users/Dan/Documents/master_research/DATAs/k2p_2022/fake_reg.csv", row.names=FALSE)
