

# UNDERSTANDING -----

# To what extent was the sample contaminated? ----

sample_contaminated <- 
  rmtl_srvy22 %>% select(farmers_hh,hh_id, mm2_1,mm4,mm5,mw1a ) %>% left_join(irrigation) %>% #count(farmers_hh, mm2_1) # 143 HH - outside_ramthal + mm2_1==1
  filter(farmers_hh=="outside_ramthal", mm2_1 == 1)

# Do you think the income of those farmers covered by the project improved? 
m7 <- rmtl_srvy22 %>% select(farmers_hh,hh_id, mm2_1,mm4,mm5,m7 ) %>% left_join(prt.irri_hh.22) 
m7%>% group_by(irri01) %>% summarise(mean(m7))
m7%>% group_by(farmers_hh) %>% summarise(mean(m7))

m7 %>%t_test(m7 ~ farmers_hh , detailed = T)
m7 %>%t_test(m7 ~ irri01 , detailed = T)


#🟦🟩 who1: overlap  ----

#| Used  source [Gov_supply] / infrastructure [Installed] / water [used] 

WI= a_rmtl_srvy22 %>% select(hh_id,mm4,mm5) %>% #| 🟦
left_join(a_sample[,1:2] )

WI5= WI %>% filter(mm5==1)

WIs=source_irri %>% #| 🟩 
filter(irri_source_num == 5) 

###### df WI5s
WI5s=inner_join( WI5[,c(1,4)] , WIs[,1] ) %>% mutate(overlap=1)
WI5s %>% count(farmers_hh) %>% mutate(grp=c(946,666 )) %>% mutate(n/grp)

# inside_ramthal  213 overlap [23%]
# outside_ramthal  48 overlap [7%]

ovl= a_rmtl_srvy22 %>% 
  select(hh_id,mm4,mm5) %>%    #| 🟦
left_join(source_irri) %>%   #| 🟩 
right_join(hh_irrigation) %>% #| 🟩 
left_join(a_sample[,1:2] )

ovl %>% count(farmers_hh,mm4,mm5,hh_irrigation)

ovl %>% count(farmers_hh,hh_irrigation,mm4,mm5)

ovl %>% count(farmers_hh,hh_drip, mm4,mm5 )

x=
  ovl %>% mutate(hh_6methods =ifelse(hh_6methods  %in% c("flood","furrows"),"furrows_flood",hh_6methods  )) %>% 
  count(farmers_hh,hh_6methods , irri_source  ) %>% 
  group_by(farmers_hh,hh_6methods) %>% 
  filter(!hh_6methods=="rain") %>%  
  group_by(farmers_hh)%>% mutate(ngrp=sum(n) ) %>% 
  mutate(prc=n/ngrp) %>% mutate_at(6,round,2) %>% 
  group_by(farmers_hh,hh_6methods) %>%  mutate(sum_method=sum(prc))



# 🟦  who2: mm2	gov projects but ramthal

# mm2		Is your land coming under such a government project? 
# 1	Ramrhal
# 2	Krishi Honda ( Farm pond)
# 3	Pradhan Mantri Krishi Sinchai Yojana(PMKSY)
# 4	Ganga kalyana
# 5	non

###### df Im_in_ramthal
Im_in_ramthal=a_rmtl_srvy22 %>% select(hh_id, mm4,mm5,mm2_1) %>% left_join(a_sample[,1:2]) 
Im_in_ramthal %>% count(farmers_hh,mm2_1,mm4) %>% 
  pivot_wider(names_from = farmers_hh, values_from = n) %>% 
  mutate(inside_ramthal/946,outside_ramthal/666)

Im_in_ramthal %>% count(farmers_hh,mm2_1,mm4) %>% 
  pivot_wider(names_from = mm4, values_from = n) %>% 
  mutate(inside_ramthal/946,outside_ramthal/666)


###### df but_ramthal
but_ramthal=
  a_rmtl_srvy22 %>% select(hh_id, mm5,starts_with("mm2")) %>% 
  filter(!mm2 %in% c(1,5)) %>% left_join(a_sample[,1:2]) 


###### DS
but_ramthal %>% count(farmers_hh) %>% mutate(grp=c(946,666 )) %>% mutate(n/grp)

# farmers_hh         n     `n/grp`   infrastructure  water_used
# --- --- --- --- --- --- --- --- --- -- --- ---  --- --- --- -
# inside_ramthal    10    0.0106     [1 hh]          [2 hh]
# outside_ramthal   13    0.0195     [2 hh]          [0 hh]




# LOW USE OF WATER ----

rmtl_srvy22 %>% select(hh_id,contains("mw1c"),-mw1c_other,-mw1c__888)%>% filter(mw1c != "") %>% select(-mw1c ) %>% pivot_longer(-hh_id,names_to = "ans",values_to = "value") %>% group_by(ans) %>% summarise(mean(value ))

vars_irri <- 
  rmtl_srvy22 %>% 
  select(farmers_hh,hh_id,
         l7_rank_1, mm4,mm5,mm9,
         mw4,  # <labelled<double>[vars_irri$mw4]>: MW4 : Are you still making use of the water from the project to irrigate your la
         mw1a, # <labelled<double>[vars_irri$mw1a]>: If Yes, in which year did you first make use of the water?
         mw4a, # <labelled<double>[vars_irri$mw4a]>: MW4A:If Yes, in what season?
         
         m59a  # <labelled<double>[vars_irri$m59a]>: M59a : Are you aware of the existence of a Water User Associations (WUA)?
  ) %>%
  left_join(irrigation_HH)
vars_irri$mw1a
vars_irri$mm9[vars_irri$mm9==-999] <- NA


# work/damaged main pipe ----

m35= vars_irri %>% filter(!is.na(m35))%>%mutate(Works1_Damaged0=ifelse(m35==2,0,1)) 
m35  %>% group_by(hh_irrigated ,Works1_Damaged0) %>% count() %>% group_by(hh_irrigated ) %>%  mutate(prt=n/sum(n)) %>% 
  ungroup() %>% mutate(prt = paste0(round(100 * prt, 0), "%"))



# still making use ----
# mw4 [vars_irri$mw4]>: MW4 : Are you still making use of the water from the project to irrigate your la

vars_irri %>% group_by(farmers_hh,mw4) %>% count() %>% group_by(farmers_hh) %>%  mutate(prt=n/sum(n)) %>% 
  ungroup() %>% mutate(prt = paste0(round(100 * prt, 0), "%"))

vars_irri %>% group_by(hh_irrigated ,mw4) %>% count() %>% group_by(hh_irrigated ) %>%  mutate(prt=n/sum(n)) %>% 
  ungroup() %>% mutate(prt = paste0(round(100 * prt, 0), "%"))


# WUA ----
# m59a  # <labelled<double>[vars_irri$m59a]>: M59a : Are you aware of the existence of a Water User Associations (WUA)?
ta=vars_irri %>% group_by(hh_irrigated ,m59a) %>% count() %>% group_by(hh_irrigated ) %>%  mutate(prt=n/sum(n)) %>% 
  ungroup() %>% mutate(prt = paste0(round(100 * prt, 0), "%"))

inner_join(ta[4:6,c(2,4)] %>% rename(irri_prt=prt) ,ta[1:3,c(2,4)] %>% rename(Not_irri_prt=prt) )

# mw1a - year first use  ----
vars_irri %>%filter(!is.na(mw1a))  %>%group_by(irrigation) %>%  count(mw1a) %>% mutate(n/sum(n))

vars_irri %>%filter(!is.na(mw1a))  %>%group_by(farmers_hh) %>%  count(mw1a) %>% mutate(n/sum(n))

#--mm10--enough water-----------------
# <labelled<double>[1612]>: MM10 : Has it ever happened to you that farmers 'before' you have used up a lot

#| mm10 - NO contribution!!!
#| 
rmtl_srvy22$mm10

mm10 <- 
  rmtl_srvy22 %>% select(farmers_hh,hh_id,starts_with("mm10") ) %>%filter(!is.na(mm10)) %>% 
  mutate(yes=1) %>% 
  pivot_wider(names_from = mm10, values_from = yes) %>% 
  pivot_longer(!c(farmers_hh,hh_id ), names_to = "mm10_ans", values_to = "yesno") %>% 
  mutate(yesno=ifelse(is.na(yesno),0,1))%>% left_join(irrigation_HH)  

ta=mm10 %>% group_by(hh_irrigated,mm10_ans) %>% summarise(mn=mean(yesno)) %>%
  ungroup() %>% mutate(mn = paste0(round(100 * mn, 0), "%"))

ta1=ta[6:10,2:3] %>% rename(use_irri_prt=mn)
ta0=ta[1:5,2:3] %>% rename(not_use_prt=mn)
inner_join(ta1,ta0) %>% kbl() %>% kable_styling()

# mm10%>% group_by(mm10_ans) %>% t_test(yesno~ irri01, detailed = T )


#--mw1c--Why didn't you use water? ----------------- 
#| mw1c - NO contribution!!!

rmtl_srvy22$mw1c

mw1c_why_no <- rmtl_srvy22 %>% select(farmers_hh,hh_id,starts_with("mw1c") ) %>%filter( mw1c != "" ) %>% 
  select(-mw1c__888 , -mw1c_other ,-mw1c) %>% 
  pivot_longer(!c(farmers_hh,hh_id ), names_to = "why_no_use", values_to = "yesno") %>% 
  left_join(irrigation_HH)

ta=mw1c_why_no %>% group_by(hh_irrigated,why_no_use ) %>% summarise(mn=mean(yesno)) %>%ungroup() %>% mutate(mn = paste0(round(100 * mn, 0), "%"))
ta1=ta[11:20,2:3] %>% rename(use_irri_prt=mn)
ta0=ta[1:10,2:3] %>% rename(not_use_prt=mn)
inner_join(ta1,ta0) [c(1:2,6,8:9),] %>% kbl() %>% kable_styling()

# mw1c_why_no  %>% group_by(why_no_use) %>%  t_test(yesno~ hh_irrigated , detailed = T )

# count hh is to make sure that significant t test rely on reasonable hh number
mw1c_why_no %>% group_by(why_no_use ,irri01 ) %>% summarise(sum(yesno))



# location_on_pipe ----
distance_from_filter <- 
  rmtl_srvy22 %>% 
  select(hh_id,mm4,mm9,
         mm10, # <labelled<double>[vars_irri$mm10]>: MM10 : Has it ever happened to you that farmers 'before' you have used up a lot
         mw1c_5, # "MW1C: I wanted to irrigate, but other farmers took all the water"         
         farmers_hh) %>% left_join(irrigation_HH) 

distance_from_filter %>% filter(mm9>-1 ) %>% group_by(hh_irrigated ) %>% summarise(mean(mm9))
distance_from_filter%>% filter(mm9>-1 ) %>% count(mm9) %>% as.data.frame()
distance_from_filter%>% filter(mm9>-1 , mm9<31 ) %>% group_by(hh_irrigated ) %>% summarise(mean(mm9))

t1 <- 
  distance_from_filter%>% filter(mm9<31 ) %>% 
  t_test(mm9  ~ hh_irrigated , detailed = T) %>% 
  rename(Not_irrigated=estimate1,Irrigated=estimate2,t=statistic ) %>% 
  select( Irrigated,Not_irrigated,estimate,conf.low,conf.high,t,df,p)

nice_table(t1,title = c("Table mm9A | Distance from valve", "Mean plot location as its distance from the valve"),
           note = c("[mm9] How many farmers are there between you and the valve/pipeline?","🟩" ))

# close_to_valve Yes or No
distance_from_filter_bins <- 
  distance_from_filter %>% filter(!is.na(mm9)) %>% 
  mutate(close_to_valveYN = ifelse(mm9==-999,NA, ifelse( mm9>=0 & mm9 < 5 ,1,0))) %>% #0-5 5+
  
  mutate(loc_on_pipe.5bin = case_when(
    mm9>=0 & mm9 < 5 ~ 1, # "0-5",
    mm9>= 5 & mm9 <11~ 2, #"05-10",
    mm9>= 11 & mm9 <21 ~ 3, #"10-20",
    mm9>= 21 & mm9 <31 ~ 4, #"20-30",
    mm9>= 31 & mm9 <51 ~ 5, #"30-50",
    TRUE ~ NA)) %>% 
  mutate(close1.far2.very3=case_when( 
    mm9>=0 & mm9< 4 ~ 1, #"close"
    mm9>=4 & mm9< 8 ~ 2, #"far",
    mm9>=8 & mm9<51 ~ 3, #"very_far",
    TRUE ~ NA))

distance_from_filter_bins %>% group_by(hh_irrigated ) %>% summarise(sum=sum(close_to_valveYN ),n=n()) %>% mutate(sum/n)
distance_from_filter_bins %>% filter(mm9<31 ) %>% group_by(hh_irrigated ) %>% summarise(sum=sum(close_to_valveYN ),n=n()) %>% mutate(sum/n)
distance_from_filter_bins %>% count(hh_irrigated, close_to_valveYN )%>%group_by(hh_irrigated) %>% summarise(n/sum(n))

# Fig. freq
rmtl_srvy22 %>%
  select(farmers_hh, hh_id,mm9,mm10) %>% mutate(mm10=as.numeric(mm10)) %>% 
  left_join(irrigation_HH) %>% filter(mm9>=0) %>% # rm -999
  count(hh_irrigated,mm9) %>% 
  group_by(hh_irrigated) %>%mutate(pct=n/sum(n)) %>% 
  group_by(mm9) %>% mutate(sum_pct=sum(pct)) %>% mutate(percent=pct/sum_pct) %>% 
  mutate(hh_irrigated=ifelse(hh_irrigated==1,"Irrigation","0irrigation")) %>%
  mutate_at(6,round,2) %>% mutate(percent=percent*100) %>% 
  ggplot(aes(x = mm9, y = percent, fill = hh_irrigated)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#b4b4b4ff", "#03bfffff")) +
  labs(title = "", x = "No. farmers before you", y = "Share of farmers (%)") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentage
  scale_x_continuous(breaks = seq(0, 14, by = 3)) +  # Set x-axis range up to 20 and display all integer values
  theme_minimal() +
  theme(text = element_text(family = "Serif"),  # Set font to Serif
        legend.position = "bottom")  










# irrigated crop_plot by village  ----

geo_rmtl=rmtl_midline2018 %>% 
  select(hh_id, in1_out0, layer, distance_km, around_boundary, south1_north0,a5, a6) %>% 
  rename(elevation = layer)

#### elevation 
area_irri=
  irrigation_HH %>% left_join(geo_rmtl) %>% left_join(a_source_irri[,c(2,6)]) %>% 
  mutate(irri_gov= ifelse(hh_irrigated ==1 & irri_source =="gov_supply",1,0))

#hh_irrigated
area_irri %>%   group_by(elevation ,hh_irrigated ) %>% count() %>% 
  group_by(elevation) %>%  mutate(total_hh=sum(n),prt=n/total_hh) %>% 
  filter(hh_irrigated==1) %>% select(elevation,prt)

#irri_gov
area_irri %>%   group_by(elevation ,irri_gov ) %>% count() %>% 
  group_by(elevation) %>%  mutate(total_hh=sum(n),prt=n/total_hh) %>% 
  filter(irri_gov==1) %>% select(elevation,prt)

#### around_boundary
area_irri$boundary= area_irri$south1_north0
area_irri$boundary[is.na(area_irri$boundary)] <- "inner"
area_irri$boundary[area_irri$boundary==1] <- "south"
area_irri$boundary[area_irri$boundary==0] <- "north"

#hh_irrigated
area_irri %>% group_by(boundary ,hh_irrigated ) %>% count() %>% 
  group_by(boundary) %>%  mutate(total_hh=sum(n),prt=n/total_hh) %>% 
  filter(hh_irrigated==1)

#irri_gov
area_irri %>% group_by(boundary ,irri_gov ) %>% count() %>% 
  group_by(boundary) %>%  mutate(total_hh=sum(n),prt=n/total_hh) %>% 
  filter(irri_gov==1)

#### distance_km
#hh_irrigated
area_irri %>%   group_by(boundary,distance_km ,hh_irrigated ) %>% count() %>% 
  group_by(boundary) %>%  mutate(total_hh=sum(n),prt=n/total_hh) %>% 
  filter(hh_irrigated==1)

#irri_gov
area_irri %>% group_by(boundary,distance_km ,irri_gov ) %>% count() %>% 
  group_by(boundary) %>%  mutate(total_hh=sum(n),prt=n/total_hh) %>% 
  filter(irri_gov==1)

# SOCIAL ----
# M59a : Are you aware of the existence of a Water User Associations (WUA)?
attr(rmtl_srvy22$m59a, "labels")

rmtl_srvy22 %>% left_join(irrigation_HH)%>% 
  group_by(hh_irrigated,m59a) %>% count()%>% 
  group_by(hh_irrigated) %>% mutate(n/sum(n))


social_bl16= 
  hh_2022 %>% 
  inner_join(caste) %>% 
  inner_join(socioeconomic16) %>% 
  inner_join(edu_hh_level) %>% 
  left_join(irrigation_HH)%>% 
  left_join(a_source_irri[,c(2,6)]) %>% 
  mutate(irri_gov= ifelse(hh_irrigated ==1 & irri_source =="gov_supply",1,0))
rm(caste,socioeconomic16, edu_hh_level)

social_bl16 %>% filter(!is.na(caste_cat)) %>% group_by(hh_irrigated,caste_cat) %>% count()%>% group_by(hh_irrigated) %>%  mutate(n/sum(n))
social_bl16 %>% filter(!is.na(caste_cat)) %>% group_by(farmers_hh ,caste_cat) %>% count()%>% group_by(farmers_hh) %>%  mutate(n/sum(n))
social_bl16 %>% filter(!is.na(caste_cat)) %>% group_by(irri_gov,caste_cat) %>% count()%>% group_by(irri_gov) %>%  mutate(n/sum(n))

social_bl16 %>% filter(!is.na(caste_cat)) %>%  
  ggplot() + aes(x = hh_irrigated, fill = factor(caste_cat)) +geom_bar(position = "fill")+ coord_flip()

library(lmtest)
names(social_bl16)
social_bl16 %>% t_test(housing_cstr ~ hh_irrigated,detailed = T)
social_bl16 %>% t_test(total_acre ~ hh_irrigated,detailed = T)
social_bl16 %>% t_test(toal_plots ~ hh_irrigated,detailed = T)
social_bl16 %>% t_test(bl_yr_income ~ hh_irrigated,detailed = T)
social_bl16 %>% filter(!is.na(bpl_card)) %>% group_by(hh_irrigated,bpl_card) %>% count()%>% group_by(hh_irrigated) %>%  mutate(n/sum(n))
social_bl16 %>% filter(!is.na(official_assistance)) %>% group_by(hh_irrigated,official_assistance) %>% count()%>% group_by(hh_irrigated) %>%  mutate(n/sum(n))

m_eco <- lm(hh_irrigated ~ bl_yr_income + bpl_card + official_assistance , social_bl16)
summary(m_eco)

# BPL_Card ggplot  ----
social_bl16 %>% filter(!is.na(bpl_card)) %>%
  group_by(hh_irrigated) %>%
  summarise(
    prt_hh_BPL = mean(bpl_card == 1),
    lower_ci = binom.test(sum(bpl_card == 1), n = n(), conf.level = 0.95)$conf.int[1],
    upper_ci = binom.test(sum(bpl_card == 1), n = n(), conf.level = 0.95)$conf.int[2]
  ) %>% 
  ggplot( aes(x = factor(hh_irrigated), y = prt_hh_BPL, fill = factor(hh_irrigated))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  scale_fill_manual(values = c("lightblue", "gray80"))+theme_minimal()

# official_assistance ggplot ----
social_bl16 %>% filter(!is.na(official_assistance)) %>%
  group_by(hh_irrigated) %>%
  summarise(
    prt_hh_get_assistance = mean(official_assistance == 1),
    lower_ci = binom.test(sum(official_assistance == 1), n = n(), conf.level = 0.95)$conf.int[1],
    upper_ci = binom.test(sum(official_assistance == 1), n = n(), conf.level = 0.95)$conf.int[2]
  ) %>% 
  ggplot( aes(x = factor(hh_irrigated), y = prt_hh_get_assistance, fill = factor(hh_irrigated))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  scale_fill_manual(values = c("lightblue", "gray80"))+theme_minimal()




t5=social_bl16 %>% t_test(prt_educated ~ hh_irrigated ,detailed = T)
t6=social_bl16 %>% t_test(edu_hh_head ~ hh_irrigated ,detailed = T)
social_bl16 %>% t_test(edu_hh ~ hh_irrigated ,detailed = T)
t56=rbind(t5,t6)
nice_table(t56)

# LM ----
#m_edu <- lm(hh_irrigated ~ edu_hh_head + edu_hh + hh_literacyPrt , social_bl16)
m_edu <- lm(hh_irrigated ~ hh_literacyPrt+edu_hh_head , social_bl16)
summary(m_edu)
sjPlot::tab_model(m_edu,show.se = TRUE,
                  pred.labels = c("(Intercept)",  "% literacy in HH","HH head education level"),
                  dv.labels = "`",
                  string.se = "__Std. Err__",
                  string.pred = "Predictors",
                  string.est = "Coef.",
                  string.ci = "CI (95%)",
                  string.p = "p",
                  p.style = "numeric",
                  col.order = c("est","se","ci","p")
)


# regression with fixed effects
model <- lm(hh_irrigated ~ factor(District) + X, data = social_bl16)

broom::tidy(m_edu) %>%mutate(p.value = round(p.value, 3)) %>% 
  kbl() %>% kable_minimal()

#nice_lm
model1 <- lm(mpg ~ cyl + wt * hp, mtcars)
model2 <- lm(qsec ~ disp + drat * carb, mtcars)
mods <- nice_lm(list(model1, model2), standardize = TRUE)
mods
nice_table(mods, highlight = TRUE)



######## castes category [caste]  ----
# a21	What is your religion?	#	rmtl_baseline2016 %>% count(A21)
# a22 What is your caste? # rmtl_baseline2016 %>% count(A22) / rmtl_baseline2016$A22_os
# a23 Which caste category does this fall under?

caste1= rmtl_baseline2016 %>% select(hh_id,A21, A22,A22_os,A23)
caste1 %>% count(A23)

# 1 General Category # 2 Other Backward Caste # 3 Scheduled Caste # 4 Scheduled Tribe
caste1$A23[caste1$A23 %in% c("","-777","-999")] <- NA
caste2=
  caste1%>%mutate(caste_cat=ifelse(A23=="1",3, # GC
                                   ifelse(A23=="2",2, # Other BC
                                          ifelse(A23 %in% c("3","4"), 1,# SC/ST
                                                 NA))))

caste2 %>% count(caste_cat)
caste = caste2[,c(1,6)]
rm(caste1,caste2)

######## socio conomic status [socioeconomic16]  ----
# D2	How many acres (guntas) of land does your household currently own?
#  D3	How many plots of land does your household currently own?
# B1	Is this housing constructed with pucca, semi-pucca, or kutcha materials?1Pucca House/ 2Semi Pucca House/ 3Kutcha House
# B8	Does this household have a BPL ration card?
# B9	In the last 5 years, has the household received any assistance from the municipality/government/gram panchayat? (NOT including ration card)

socioeconomic16=
  rmtl_baseline2016 %>% 
  select(hh_id,D2 ,D3 ,contains(c("B1", "B8","B9","F12_year","F13"))) %>% 
  rename(bpl_card=B8,official_assistance=B9,toal_plots=D3, total_acre=D2) %>% 
  mutate(housing_cstr=ifelse(B1==1,3,ifelse(B1==3,1,2)),
         bl_yr_income= ifelse(is.na(F12_year), F13,
                              ifelse(is.na(F13),F12_year, 
                                     pmax(F12_year,F13, na.rm = TRUE)))
  ) %>% 
  select(-c(B1,F12_year,F13))

######## education [edu_hh_level] ----
# C5	What is their relationship to the head of household?
# C6	Are they literate?
# C7	What is their educational level?

c5= rmtl_baseline2016 %>% select(hh_id,starts_with("C5" ),-contains("_os_") ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "hh_member")
c5$id_member <- sub("^C5_(\\d{1,2})","C_\\1",c5$id_member )

c6= rmtl_baseline2016 %>% select(hh_id, contains("C6") ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "r_they_literate")
c6$id_member <- sub("^C6_(\\d{1,2})","C_\\1",c6$id_member)

c7= rmtl_baseline2016 %>% select(hh_id, starts_with("C7"), -ends_with("_bin") ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "edu_level")
c7$id_member <- sub("^C7_(\\d{1,2})","C_\\1",c7$id_member )

full_join(c5,c6) %>% full_join(c7) %>% filter( !is.na(hh_member), literate==0, !is.na(edu_level))
rm(c5,c6,c7)

#literate=educated  hh_literacyPrt=prt_educated
edu = 
  full_join(c5,c7)%>%  # full_join(c6) %>% 
  filter(!is.na(hh_member)) %>% 
  mutate(educated=ifelse(is.na(edu_level),0 ,ifelse(edu_level == -999 ,0,1) )) %>% 
  mutate(edu_hh_head = 100)
edu$edu_level[is.na(edu$edu_level)] <- 0                         
edu$edu_hh_head=ifelse( edu$hh_member==1,edu$edu_level,NA )

# HH with one head
edu1 = edu %>% filter(!is.na(edu_hh_head)) %>% count(hh_id) %>%filter(n==1) %>% left_join(edu)

# HH with more then one head & without head
edu2 = edu %>%left_join(edu1[,1:2] %>% distinct()) %>% filter(is.na(n)) %>% select(-n)
edu2$edu_hh_head=ifelse(edu2$id_member=="C_1", edu2$edu_level, NA )

# edu1 + edu2 [8,129 × 7]
Edu =  edu1 %>% select(-n) %>% rbind(edu2)

edu_hh_level =  # [1,610 × 6]
  Edu %>% group_by(hh_id) %>% 
  mutate(total_hh_members= n(),
         edu_hh= mean(edu_level),
         prt_educated= (sum(educated))/total_hh_members ) %>% 
  select(hh_id, edu_hh_head, edu_hh, prt_educated) %>% 
  filter(!is.na( edu_hh_head )) %>% ungroup()

rm(edu, edu1, edu2)
########  ----
#--mw4 ---------

#mw4 still making use of the water from the project to irrigate your land?
rmtl_srvy22 %>% select(farmers_hh,hh_id,mw4 ) %>%filter(!is.na(mw4)) %>% count(mw4) %>% mutate(n/297)

#How many years in total did you ever make use of the water for irrigation during ...?
# mw5=Kharif ,mw6=rabi

rmtl_srvy22 %>% select(farmers_hh,hh_id,mw5,mw6 ) %>%
  mutate(kharif_yrs=as.numeric(mw5), rabi_yrs=as.numeric(mw6)) %>% 
  summarise(mean(kharif_yrs,na.rm = T),mean(rabi_yrs,na.rm = T) )

# Are you aware of a drip irrigation project demonstration plot?
rmtl_srvy22 %>% select(farmers_hh,hh_id,contains("m40c") ) %>%summarise(mean(m40c) )
rmtl_srvy22  %>%  t_test(m40c  ~ farmers_hh , detailed = T)
rmtl_srvy22 %>% left_join(prt.irri_hh.22) %>%  t_test(m40c  ~ irri01 , detailed = T)

#Have you ever gone to visit it?
rmtl_srvy22 %>% select(farmers_hh,hh_id,contains("m42") ) %>% filter(m42 != -999) %>%summarise(mean(m42,na.rm = T) )
rmtl_srvy22 %>%  filter(m42 != -999) %>% t_test(m42 ~ farmers_hh , detailed = T)
rmtl_srvy22 %>%  filter(m42 != -999) %>% left_join(prt.irri_hh.22) %>%  t_test(m42 ~ irri01 , detailed = T)

# m51		Are you aware of any trainings on irrigation organized in the area?
rmtl_srvy22 %>% select(farmers_hh,hh_id,contains("m51") ) %>%summarise(mean(m51,na.rm = T) )
rmtl_srvy22 %>%  t_test(m51 ~ farmers_hh , detailed = T)
rmtl_srvy22 %>% left_join(prt.irri_hh.22) %>%  t_test(m51 ~ irri01 , detailed = T)



