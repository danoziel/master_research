library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(summarytools)

gj1B %>% count(villageSI) # 16,188
gj1B %>% count(taluka)    # 227
gj1B %>% count(district)  # 26

gj1 %>% count(villageSI) # 14,019
gj1 %>% count(taluka)    # 227
gj1 %>% count(district)  # 26

# Table1 & fig.: Number of adopters by year of registration --------------------

freq(gj1$year_Registration  , report.nas=F, totals=F, cumul=F, headings=F)

yr <- gj1 %>% count(year_Registration)
yrB <- gj1B %>% count(year_Registration) %>% 
  filter(year_Registration<2014)

yrB   %>% mutate(n=n/1000) %>% 
  ggplot(aes(x = year_Registration, y = n)) +
  geom_line(color = "green4") +    # Add a line without points
  scale_x_continuous(breaks = seq(2005, 2013, 1)) +  # Ensure all x-axis values appear
  labs(title = "Number of MIS Registrations over the Years", 
       x = "Registrations Year", 
       y = "Number of Registrations \n(in thousands)") +
  theme_classic() +
  theme(
    plot.title = element_text(family = "serif"),
    plot.caption = element_text(family = "serif", hjust = 0),
    axis.title = element_text(family = "serif")
  )
# Figure 1 : Crops distribution -----------------------------------------------

crop_ds <- # `ds` is Drip Sprinkler
#  gj1 %>% 
  gj1B %>% 
  filter(!Crop %in% c("","-","NULL" )) %>% 
  group_by(mistype,Crop) %>% 
  summarise(n=n()) %>% group_by(mistype) %>%
  mutate(crop=ifelse(n<1000,"Other",Crop)) %>% 
  group_by(mistype,crop) %>% summarise(n=sum(n)) %>% 
  group_by(mistype) %>% 
  mutate(pct=n/sum(n)) %>% mutate_at(4,round,2)

crop_drip <- crop_ds %>% filter(mistype=="Drip")
crop_sprinkler <- crop_ds  %>% filter(mistype=="Sprinkler")

# PLOT cropping pattern
library(treemap)
plt <- colorRampPalette(c("lightgreen", "darkgreen"))(length(unique(crop_ds$crop)))
treemap(crop_drip,index = "crop",vSize = "pct",type = "index", palette = plt, title = "")
treemap(crop_sprinkler,index = "crop",vSize = "n",type = "index", palette = plt, title = "")

# Plot the treemap with customized title and color palette
treemap(
  crop_treemap,
  index = "crop",
  vSize = "n",
  type = "index",
  palette = plt,
  title = "")
#

# Figure 2: Sample composition by order of adoption ----------------------------

# X-axis is the ranking of the farmers on the date of registration 
# X-axis is a sequence from the first farmer in his village to adopt a MIS

# Custom function to format y-axis labels
custom_label_format <- function(x) {
  ifelse(x >= 1000, paste0(as.integer(x / 1000), "K"), as.character(x))
}

date_rank_villageWISE %>% # IIRELEVNT FOR TALUKA WISE
  count(rank_date_adopters) %>%
  filter(rank_date_adopters < 51) %>%
  ggplot(aes(x = rank_date_adopters, y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = ifelse(rank_date_adopters == min(rank_date_adopters) | rank_date_adopters == max(rank_date_adopters), n, "")),
            vjust = -0.5, size = 3) +
  theme_test() +
  ggtitle("Total observations in ranking place") +
  ylab("") + xlab("Ranking of MIS adoption date ") +
  scale_y_continuous(labels = custom_label_format) +  # Custom label formatting
  theme(plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif"))+
  scale_x_continuous(breaks=seq(1,50,1))







# Figure 3: Sample composition by type of MI adopted ---------------------------

f02=date_rank_villageWISE %>% count(rank_date_adopters,mistype) %>%
  group_by(rank_date_adopters) %>%  mutate(pct=n/sum(n)) %>% 
  ungroup()

Pf02=f02  %>% 
    ggplot(aes(x=rank_date_adopters, y=pct, fill=mistype)) +
    geom_bar(stat="identity" # , width = 1
             )+ theme_light()+ 
  scale_fill_manual(values=c('blue4','royalblue1'),name="")+  
  ggtitle("Share of Drip/Sprinkler installed farms")+
  ylab("% of rankings")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif"))

# Fig2 ----
Pf02 + scale_x_continuous(limits = c(0, 31), breaks = seq(1, 30, 1))
Pf02 + scale_x_continuous(limits = c(0, 101), breaks = seq(0, 100, 10))
#

# Figure 4: Time between adoption for drip and sprinkler adopters.......... --------------
# Time period between systems adoption
# No. weeks between RegistrationDate farmers

# Village WISE ----
# A
date_rank_V_wise <- gj1 %>%   
  select(regno, mistype, RegistrationDate,villageSI,TotalLand )%>%
  filter(!is.na(TotalLand),TotalLand<22, TotalLand>0) %>% 
  group_by(villageSI,mistype) %>% 
  arrange(RegistrationDate) %>%
  mutate( days_gap = c(NA, diff(RegistrationDate))) %>% ungroup()

# B
quantile(date_rank_V_wise$days_gap , 0.99985, na.rm = TRUE)

# C
date_rank_villageWISE <- gj1 %>%   
  select(regno, mistype, RegistrationDate,villageSI,TotalLand )%>%
  filter(!is.na(TotalLand),TotalLand<22, TotalLand>0
  ) %>% 
  group_by(villageSI,mistype) %>% 
  arrange(RegistrationDate) %>%
  mutate(days_gap = c(NA, diff(RegistrationDate))
          ) %>% 
  filter(days_gap < 1500 | is.na(days_gap), # remove outliers
         n() >= 15) %>%                     # rank limit to X-axis
  mutate(rank_date_adopters =  row_number() ,
         weeks_gap1=days_gap/7) %>% 
  group_by(mistype, rank_date_adopters) %>%
  summarise(first_date = min(RegistrationDate),
            n=n(),
            weeks_gap = mean(weeks_gap1, na.rm = TRUE),
            SD=sd(weeks_gap1,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% ungroup()

# PLOT ----
Pf03 = date_rank_villageWISE %>%
  ggplot(aes(rank_date_adopters, weeks_gap)) +
  geom_ribbon(data = filter(date_rank_villageWISE, mistype == "Drip"),
              aes(ymin=weeks_gap-CI95delta, ymax=weeks_gap+CI95delta), fill = "gray75", color = "white") +
  geom_ribbon(data = filter(date_rank_villageWISE, mistype == "Sprinkler"),
              aes(ymin=weeks_gap-CI95delta, ymax=weeks_gap+CI95delta), fill = "gray80", color = "white") +
  geom_line(aes(color = mistype), size = 1) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkler"="royalblue1"),name="") +
  theme_classic()+
  ggtitle("No. of weeks between each MIS registration and the previous one [Village wise]")+
  ylab("Gap in weeks")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))

# Fig3 ----
Pf03+ ylim(0,20) + 
  scale_x_continuous(limits = c(1.5, 15), breaks = seq(2, 15, 1), expand= c(0,0)   )
#700X300



# taluka WISE  ----
# A
date_rank_T_wise <- gj1 %>%   
  select(regno,TotalLand, mistype, RegistrationDate,taluka )%>%
  filter(!is.na(TotalLand),TotalLand<22, TotalLand>0) %>% 
  group_by(taluka,mistype) %>% 
  arrange(RegistrationDate) %>%
  mutate(days_gap = c(NA, diff(RegistrationDate) ))

# B
quantile(date_rank_T_wise$days_gap , 0.99985, na.rm = TRUE)

# C
date_rank_talukaWISE <- gj1 %>%   
  select(regno,TotalLand, mistype, RegistrationDate,taluka )%>%
  filter(!is.na(TotalLand),TotalLand<22, TotalLand>0) %>% 
  group_by(taluka,mistype) %>% 
  arrange(RegistrationDate) %>%
  mutate(days_gap = c(NA, diff(RegistrationDate))
         ) %>% 
  filter(days_gap < 870 | is.na(days_gap), # remove outliers
         n() >= 15) %>%                    
  mutate(rank_date_10adopters = ceiling(row_number() / 10), # Create groups of 10 observations
         weeks_gap1=days_gap/7) %>% 
  group_by(mistype, rank_date_10adopters) %>% # averaging of averages
  summarize(first_date = min(RegistrationDate),
            n=n(),
            weeks_gap = mean(weeks_gap1, na.rm = TRUE),
            SD=sd(weeks_gap1,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% ungroup()

# PLOT ----
Pf032 = date_rank_talukaWISE %>%
  ggplot(aes(rank_date_10adopters, weeks_gap)) +
  geom_ribbon(data = filter(date_rank_talukaWISE, mistype == "Drip"),
              aes(ymin=weeks_gap-CI95delta, ymax=weeks_gap+CI95delta), fill = "gray75", color = "white") +
  geom_ribbon(data = filter(date_rank_talukaWISE, mistype == "Sprinkler"),
              aes(ymin=weeks_gap-CI95delta, ymax=weeks_gap+CI95delta), fill = "gray80", color = "white") +
  geom_line(aes(color = mistype), size = 1) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkler"="royalblue1"),name="") +
  theme_classic()+
  ggtitle("No. of weeks between each MIS registration and the previous one [Subdistricts wise]")+
  ylab("Gap in weeks")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))

# Fig3 ----
Pf032+ ylim(0,3) + 
  scale_x_continuous(limits = c(1, 15), breaks = seq(2, 15, 1), expand= c(0,0))
#
date_rank_talukaWISE %>% mutate(days_gap=weeks_gap*7) %>%  
  filter(rank_date_10adopters %in% c(2:7 ) )





# Figure 4: Average land holding size ..................................... -----------------------------------------

# villageSI WISE  ----

# A
landholding_villageSI <- gj1 %>% 
  select(regno,mistype, RegistrationDate,villageSI,TotalLand,FarmerType) %>%
  filter(!is.na(TotalLand),TotalLand<22, TotalLand>=0) %>% 
  group_by(mistype,villageSI) %>% 
  arrange(RegistrationDate) %>% 
  filter(n() >= 15) %>% 
  mutate(rank_date_adopters = row_number()) %>% ungroup()

# B
rank_landholding_villageSI <- landholding_villageSI %>% 
  group_by(mistype,rank_date_adopters)%>% 
  summarise(n=n(),
            Total_Land=mean(TotalLand,na.rm = T),
            SD=sd(TotalLand,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% ungroup()

# PLOT ----
Pv=
  rank_landholding_villageSI %>% 
  ggplot(aes(x = rank_date_adopters, y = Total_Land, color = mistype, group = mistype)) +
  geom_line(size = 1) +  # Add lines for both types
  geom_ribbon(data = filter(rank_landholding_villageSI, mistype == "Drip"), 
              aes(ymin = Total_Land - CI95delta, ymax = Total_Land + CI95delta), 
              fill = "gray90", color = "white", alpha = 0.5) +
  geom_ribbon(data = filter(rank_landholding_villageSI, mistype == "Sprinkler"), 
              aes(ymin = Total_Land - CI95delta, ymax = Total_Land + CI95delta), 
              fill = "gray95", color = "white", alpha = 0.5) +
  labs(title = "Total Land Holding [Village wise]",
       x = "Adopters ranking by date", 
       y = "Land Holding (in Ha)") +
  scale_color_manual(values = c("Drip" = "blue4", "Sprinkler" = "royalblue1"))+
  theme_classic() +
  theme(
    plot.title = element_text(family = "serif"),
    plot.caption = element_text(family = "serif", hjust = 0),
    axis.title = element_text(family = "serif")
  )

#Fig.----
Pv + ylim(2.6,3.8)+ 
  scale_x_continuous(breaks= seq(1,15,1), limits= c(1,15), expand= c(0,0))+
  labs(caption = "The sample includes villages with 15 or more MIS adopters")
#700X300
# t-test  ----

tlh <- landholding_villageSI %>%   
  filter(rank_date_adopters<=15) %>%
  select(mistype ,rank_date_adopters,TotalLand ) %>% 
  mutate(rank_date_adopters_1 = 
           paste0("rank_", rank_date_adopters)) 


library(rstatix) # ttest "add_significance"
t12=tlh %>% 
  filter(rank_date_adopters_1 %in% c("rank_1","rank_2") ) %>% 
  group_by(mistype) %>% 
  t_test(TotalLand ~ rank_date_adopters , detailed = T) %>% 
  rename(rank_1=estimate1,rank_2=estimate2,t=statistic) %>% 
  select(mistype,rank_1,rank_2,estimate,t,df,p,conf.low,conf.high)

t34=tlh %>% 
  mutate(rank_date_adopters_2= 
           ifelse(rank_date_adopters==1,"rank_1","rank_2_15" )) %>% 
  group_by(mistype) %>% 
  t_test(TotalLand ~ rank_date_adopters_2 , detailed = T) %>% 
  rename(rank_1=estimate1,rank_2_15=estimate2,t=statistic) %>% 
  select(mistype,rank_1,rank_2_15,estimate,t,df,p,conf.low,conf.high)

t5=tlh %>% 
  mutate(rank_date_adopters_1_2= 
           ifelse(rank_date_adopters <= 3,"rank_1_2","rank_3_15" )) %>% 
  group_by(mistype) %>% 
  t_test(TotalLand ~ rank_date_adopters_1_2 , detailed = T) %>% 
  rename(rank_1_2=estimate1,rank_3_15=estimate2,t=statistic) %>% 
  select(mistype,rank_1_2,rank_3_15,estimate,t,df,p,conf.low,conf.high)
nice_table(t5)

library(rempsyc) # ttest # nice_table
nice_table(t12)
nice_table(t34)

#
# taluka WISE     ----

# A
landholding_taluka <- gj1 %>%   
  select(regno,TotalLand, mistype, RegistrationDate,taluka )%>%
  filter(!is.na(TotalLand),TotalLand<22, TotalLand>0
         ) %>%
  group_by(taluka,mistype) %>% 
  arrange(RegistrationDate) %>%
  filter(n() >= 15) %>%
  mutate(rank_date_10adopters=ceiling(row_number()/10) # Create groups of 10 observations
         ) %>%
  group_by(mistype, taluka,rank_date_10adopters) %>% # averaging of averages
  summarise(
    first_date = min(RegistrationDate),
    Total_Land = mean(TotalLand, na.rm = TRUE),
    n = n(),
    SD=sd(TotalLand,na.rm = T),
    CI95delta= 1.96*(SD/sqrt(n))) %>% ungroup()

# B
rank_landholding_taluka <- landholding_taluka %>%   
  group_by(mistype, rank_date_10adopters) %>% # averaging of averages
  summarise(
            first_date = min(first_date),
            TotalLand = mean(Total_Land, na.rm = TRUE),
            n = n(),
            SD=sd(Total_Land,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% ungroup()

rank_landholding_taluka %>% filter(rank_date_10adopters < 6)


#PLOT ----
Pt=
  rank_landholding_taluka %>% 
  ggplot(aes(x = rank_date_10adopters, y = Total_Land, color = mistype, group = mistype)) +
  geom_line(size = 1) +  # Add lines for both types
  geom_ribbon(data = filter(rank_landholding_taluka, mistype == "Drip"), 
              aes(ymin = Total_Land - CI95delta, ymax = Total_Land + CI95delta), 
              fill = "gray90", color = "white", alpha = 0.5) +  # CI ribbon for "Drip"
  geom_ribbon(data = filter(rank_landholding_taluka, mistype == "Sprinkler"), 
              aes(ymin = Total_Land - CI95delta, ymax = Total_Land + CI95delta), 
              fill = "gray95", color = "white", alpha = 0.5) +  # CI ribbon for "Sprinkler"
  labs(title = "Total Land Holding [Subdistricts wise]",
       caption = "The sample includes talukas with 15 or more MIS adopters",
       x = "Adopters ranking by date", 
       y = "Total Land (in Ha)") +
  scale_color_manual(values = c("Drip" = "blue4", "Sprinkler" = "royalblue1"))+
  theme_classic() +
  theme(
    plot.title = element_text(family = "serif"),
    plot.caption = element_text(family = "serif", hjust = 0),
    axis.title = element_text(family = "serif")
  )
#Fig.----
Pt + ylim(2.6,3.9)+ 
  scale_x_continuous(breaks = seq(1, 15, 1), limits = c(1, 15), expand = c(0, 0))


# t-test  ----

tlh_T <- landholding_taluka %>%   
  filter(rank_date_10adopters<=15) %>%
  select(mistype ,rank_date_10adopters,TotalLand ) %>% 
  mutate(rank_date_adopters_1 = 
           paste0("rank_", rank_date_10adopters)) 


library(rstatix) # ttest "add_significance"
t112=tlh_T %>% 
  filter(rank_date_adopters_1 %in% c("rank_1","rank_2") ) %>% 
  group_by(mistype) %>% 
  t_test(TotalLand ~ rank_date_adopters , detailed = T) %>% 
  rename(rank_1=estimate1,rank_2=estimate2,t=statistic) %>% 
  select(mistype,rank_1,rank_2,estimate,t,df,p,conf.low,conf.high)

t34=tlh_T %>% 
  mutate(rank_date_adopters_2= 
           ifelse(rank_date_adopters==1,"rank_1","rank_2_15" )) %>% 
  group_by(mistype) %>% 
  t_test(TotalLand ~ rank_date_adopters_2 , detailed = T) %>% 
  rename(rank_1=estimate1,rank_2_15=estimate2,t=statistic) %>% 
  select(mistype,rank_1,rank_2_15,estimate,t,df,p,conf.low,conf.high)

t5=tlh_T %>% 
  filter(mistype=="Drip") %>% 
  mutate(rank_date_adopters_1_2= 
           ifelse(rank_date_adopters <= 3,"rank_1_2","rank_3_15" )) %>% 
  t_test(TotalLand ~ rank_date_adopters_1_2 , detailed = T) %>% 
  rename(rank_1_2=estimate1,rank_3_15=estimate2,t=statistic) %>% 
  select(rank_1_2,rank_3_15,estimate,t,df,p,conf.low,conf.high)


library(rempsyc) # ttest # nice_table
nice_table(t12)
nice_table(t34)

#



# FarmerType ----
rank_villageSI_FarmerType <- gj1 %>% 
  select(regno,mistype, RegistrationDate,villageSI,TotalLand,FarmerType) %>% 
  filter(!is.na(TotalLand),TotalLand<22, TotalLand>=0) %>% 
  mutate(farmer_category=
           ifelse(FarmerType %in% c("Small Farmer" ,"Marginal Farmer"),
                  "Small or Marginal Farmer",
           ifelse(FarmerType%in%c("Medium Farmer","Large Farmer"),
                         "Medium or Large Farmer",
                  FarmerType))) %>% 
  mutate(farmer_land_size=
           ifelse(FarmerType %in% c("Small Farmer" ,"Marginal Farmer"),
                  "Up to 2 Ha",
                  ifelse(FarmerType%in%c("Semi-Medium Farmer"),
                         "Between 2 and 4 Ha",
                         "4 Ha and more"))) %>% 
  group_by(mistype,villageSI) %>% 
  filter(n() >= 15) %>% 
  arrange(RegistrationDate) %>% 
  mutate(rank_date_adopters = row_number()
  ) %>% 
  group_by(mistype,farmer_land_size,rank_date_adopters)%>% 
  summarise(n=n(),
            Total_Land=mean(TotalLand,na.rm = T),
            SD=sd(TotalLand,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% 
  ungroup()


rank_villageSI_FarmerType %>%
  group_by(farmer_land_size) %>%
  summarize(
    min_total_land = min(Total_Land, na.rm = TRUE),
    max_total_land = max(Total_Land, na.rm = TRUE)
  )


Pf=
  rank_villageSI_FarmerType %>% 
#  filter(mistype =="Drip") %>% 
  filter(mistype =="Sprinkler") %>% 
  ggplot(aes(x = rank_date_adopters, y = Total_Land, color = farmer_land_size, group = farmer_land_size)) +
  geom_line(size = 0.85) +  # Add lines for both types
  geom_ribbon(
              aes(ymin = Total_Land - CI95delta, ymax = Total_Land + CI95delta), 
              fill = "gray90", color = "white", alpha = 0.5) +
 # labs(title = "Drip | Total Land Holding by Farmer Type [Village wise]",
       labs(title = "Sprinkler | Total Land Holding by Farmer Type [Village wise]",
       x = "Adopters ranking by date", 
       y = "Land Holding (in Ha)") +
  scale_color_manual(values = c("Up to 2 Ha" = "olivedrab2",
                                "Between 2 and 4 Ha" = "chartreuse3", 
                                "4 Ha and more" = "forestgreen"))+
  theme_classic() +
  theme(legend.title = element_blank(),
        plot.title = element_text(family = "serif"),
    plot.caption = element_text(family = "serif", hjust = 0),
    axis.title = element_text(family = "serif")
  )
Pf + ylim(1,7.5)+ 
  scale_x_continuous(breaks= seq(1,15,1), limits= c(1,15), expand= c(0,0))

rank_villageSI_FarmerType %>% 
  filter(rank_date_adopters %in%c(1,6)) %>% 
  filter(mistype=="Drip", farmer_land_size=="4 Ha and more")

rank_villageSI_FarmerType %>% 
  filter(rank_date_adopters%in%c(1,3)) %>% 
  filter(mistype!="Drip", farmer_land_size =="4 Ha and more")






########  land installed MI    ################################################ ####

# A
misarea_villageSI <- gj1 %>% 
  select(regno, RegistrationDate,villageSI,TotalLand,misarea,mistype) %>%
  filter(!is.na(TotalLand),TotalLand<22, TotalLand>=0) %>% 
  group_by(mistype,villageSI) %>% 
  arrange(RegistrationDate) %>% 
  filter(n() >= 15) %>% 
  mutate(rank_date_adopters = row_number()) %>% ungroup()

# B
rank_misarea_villageSI <- misarea_villageSI %>% 
  group_by(mistype,rank_date_adopters)%>% 
  summarise(n=n(),
            mis_land=mean(misarea,na.rm = T),
            SD=sd(misarea,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% 
  ungroup()

rank_misarea_villageSI %>% filter(rank_date_adopters < 6)

# PLOT ----
Pmi=
  rank_misarea_villageSI %>% 
  ggplot(aes(x = rank_date_adopters, y = mis_land, color = mistype, group = mistype)) +
  geom_line(size = 1) +  # Add lines for both types
  geom_ribbon(data = filter(rank_misarea_villageSI, mistype == "Drip"), 
              aes(ymin = mis_land - CI95delta, ymax = mis_land + CI95delta), 
              fill = "gray90", color = "white", alpha = 0.4) +
  geom_ribbon(data = filter(rank_misarea_villageSI, mistype == "Sprinkler"), 
              aes(ymin = mis_land - CI95delta, ymax = mis_land + CI95delta), 
              fill = "gray95", color = "white", alpha = 0.4) +
  labs(title = "Farms' installed area size by drip or sprinkler [Village wise]",
       x = "Adopters ranking by date", 
       y = "Installed MIS land (in Ha)") +
  scale_color_manual(values = c("Drip" = "blue4", "Sprinkler" = "royalblue2"))+
  theme_classic() +
  theme(legend.title = element_blank(),
    plot.title = element_text(family = "serif"),
    plot.caption = element_text(family = "serif", hjust = 0),
    axis.title = element_text(family = "serif")
  )



#Fig.----
Pmi + ylim(1.4,1.8)+ 
  scale_x_continuous(breaks= seq(1,30,1), limits= c(1,30), expand= c(0,0))
#

# t-test  ----

t7 <- misarea_villageSI %>%   
  filter(rank_date_adopters<=15) %>%
  select(mistype ,rank_date_adopters,misarea  ) %>% 
  mutate(rank_date_adopters_1 = 
           paste0("rank_", rank_date_adopters)) 


library(rstatix) # ttest "add_significance"
t12=t7 %>% 
  filter(rank_date_adopters_1 %in% c("rank_1","rank_2") ) %>% 
  group_by(mistype) %>% 
  t_test(misarea ~ rank_date_adopters , detailed = T) %>% 
  rename(rank_1=estimate1,rank_2=estimate2,t=statistic) %>% 
  select(mistype,rank_1,rank_2,estimate,t,df,p,conf.low,conf.high)



t5=t7 %>% 
  mutate(rank_date_adopters_3= 
           ifelse(rank_date_adopters == 1,"rank_1","rank_2_15" )) %>% 
  group_by(mistype) %>% 
  t_test(misarea ~ rank_date_adopters_3 , detailed = T) %>% 
  rename(rank_1=estimate1,rank_2_15=estimate2,t=statistic) %>% 
  select(mistype,rank_1,rank_2_15,estimate,t,df,p,conf.low,conf.high)
nice_table(t5)

library(rempsyc) # ttest # nice_table
nice_table(t12)




# taluka WISE     ----

# A
misarea_taluka <- gj1 %>%   
  select(regno,TotalLand, mistype, RegistrationDate,taluka, misarea )%>%
  filter(!is.na(TotalLand),TotalLand<22, TotalLand>0
  ) %>%
  group_by(taluka,mistype) %>% 
  arrange(RegistrationDate) %>%
  filter(n() >= 15) %>%
  mutate(rank_date_10adopters=ceiling(row_number()/10) # Create groups of 10 observations
  ) %>%
  group_by(mistype, taluka,rank_date_10adopters) %>% # averaging of averages
  summarise(
    first_date = min(RegistrationDate),
    mis_area = mean(misarea, na.rm = TRUE),
    n = n(),
    SD=sd(misarea,na.rm = T),
    CI95delta= 1.96*(SD/sqrt(n))) %>% ungroup()

# B
rank_misarea_taluka <- misarea_taluka %>%   
  group_by(mistype, rank_date_10adopters) %>% # averaging of averages
  summarise(
    first_date = min(first_date),
    MI_area = mean(mis_area, na.rm = TRUE),
    n = n(),
    SD=sd(mis_area,na.rm = T),
    CI95delta= 1.96*(SD/sqrt(n))) %>% ungroup()

rank_misarea_taluka %>% filter(rank_date_10adopters < 6)


#PLOT ----
Pt=
  rank_misarea_taluka %>% 
  ggplot(aes(x = rank_date_10adopters, y = MI_area, color = mistype, group = mistype)) +
  geom_line(size = 1) +  # Add lines for both types
  geom_ribbon(data = filter(rank_misarea_taluka, mistype == "Drip"), 
              aes(ymin = MI_area - CI95delta, ymax = MI_area + CI95delta), 
              fill = "gray90", color = "white", alpha = 0.5) +  # CI ribbon for "Drip"
  geom_ribbon(data = filter(rank_misarea_taluka, mistype == "Sprinkler"), 
              aes(ymin = MI_area - CI95delta, ymax = MI_area + CI95delta), 
              fill = "gray95", color = "white", alpha = 0.5) +  # CI ribbon for "Sprinkler"
       labs(title = "Farms' installed area size by drip or sprinkler [Subdistricts wise]",
            x = "Adopters ranking by date", 
            y = "Installed MIS land (in Ha)") +
         scale_color_manual(values = c("Drip" = "blue4", "Sprinkler" = "royalblue1"))+
  theme_classic() +
  theme(
    plot.title = element_text(family = "serif"),
    plot.caption = element_text(family = "serif", hjust = 0),
    axis.title = element_text(family = "serif")
  )

#Fig.----
Pt + ylim(1.4,1.9)+ 
  scale_x_continuous(breaks= seq(1,30,1), limits= c(1,30), expand= c(0,0))


# t-test  ----

t77 <- misarea_taluka %>%   
  filter(rank_date_10adopters <=15) %>%
  select(mistype ,rank_date_10adopters ,mis_area  ) %>% 
  mutate(rank_date_adopters_1 = 
           paste0("rank_", rank_date_10adopters )) 

library(rstatix) # ttest "add_significance"
t777=t77 %>% 
  filter(rank_date_adopters_1 %in% c("rank_1","rank_2") ) %>% 
  group_by(mistype) %>% 
  t_test(mis_area ~ rank_date_10adopters  , detailed = T) %>% 
  rename(rank_1=estimate1,rank_2=estimate2,t=statistic) %>% 
  select(mistype,rank_1,rank_2,estimate,t,df,p,conf.low,conf.high)
nice_table(t777)







######## % Irrigated Land  ###################################### ####
# The percentage of the area installed in the system out of the total area owned by the farmer


df_mi_pct_land= 
  gj1 %>%   
  select(regno,TotalLand, mistype, RegistrationDate,villageSI,mi_pct_land )%>%
  mutate_at(6,round,4) %>% 
  filter(!is.na(TotalLand),TotalLand<22, TotalLand>=0) %>% 
  group_by(mistype,villageSI) %>% 
  filter(n() >= 15) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
    group_by(mistype,rank_date_adopters)%>% 
    summarise(n=n(),
              pct_ir_land=mean(mi_pct_land,na.rm = T),
              SD=sd(mi_pct_land,na.rm = T),
              CI95delta= 1.96*(SD/sqrt(n)))

df_mi_pct_land %>% filter(rank_date_adopters<6)

#PLOT ----
Pct=
  df_mi_pct_land %>% 
  ggplot(aes(x = rank_date_adopters, y = pct_ir_land, color = mistype, group = mistype)) +
  geom_line(size = 1) + 
  geom_ribbon(data = filter(df_mi_pct_land, mistype == "Drip"), 
              aes(ymin = pct_ir_land - CI95delta, ymax = pct_ir_land + CI95delta), 
              fill = "gray90", color = "white", alpha = 0.4) +
  geom_ribbon(data = filter(df_mi_pct_land, mistype == "Sprinkler"), 
              aes(ymin = pct_ir_land - CI95delta, ymax = pct_ir_land + CI95delta), 
              fill = "gray95", color = "white", alpha = 0.4) +
  ggtitle("Share of installed system area by drip or sprinkler")+
  ylab("% Installed area")+xlab("Ranking of MIS adoption date ") +
  
  scale_color_manual(values = c("Drip" = "blue4", "Sprinkler" = "royalblue2"))+
  theme_classic() +
  theme(legend.title = element_blank(),
        plot.title = element_text(family = "serif"),
        plot.caption = element_text(family = "serif", hjust = 0),
        axis.title = element_text(family = "serif")
  )
#Fig.----
Pct + ylim(0.34,0.75)+ 
  scale_x_continuous(breaks= seq(1,30,1), limits= c(1,30), expand= c(0,0))
#



########  Caste           ###########################################################

gj1 %>% filter(!is.na(caste)) %>% 
  count(caste) %>%   
  mutate(prt = n / sum(n))

gj1 %>% filter(!is.na(caste)) %>% 
  count(caste,mistype) %>%   
  group_by(caste) %>% 
  mutate(prt = n / sum(n) )


dfc=
  gj1 %>%   
  select(regno, mistype, RegistrationDate,villageSI,caste) %>%
  group_by(mistype,villageSI) %>% 
  arrange(RegistrationDate) %>%
  filter(n() >= 15) %>% 
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  count(mistype,caste,rank_date_adopters) %>% 
  group_by(mistype,rank_date_adopters) %>% 
  mutate(pct_mistype_wise=n/sum(n)) %>% ungroup()

dfc %>% filter( rank_date_adopters<6)

#Fig.     ----

dfc %>% 
  ggplot(aes(x=rank_date_adopters, y=pct_mistype_wise, fill=caste)) +
  geom_bar(stat="identity")+
  facet_wrap(~mistype,  ncol=1, strip.position = "left")+
  
  theme_classic() +
  scale_fill_manual(values=c("#5F6EA0",  "#5F9EA0" ),name="Social Status")+  
  ggtitle("Household Social Status by drip or sprinkler [Village wise]")+
  ylab("% of rankings")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif"))+ 
  scale_x_continuous(limits = c(0, 16), breaks = seq(1, 15, 1), expand= c(0,0))


# taluka WISE     ----

# A
caste_taluka <- gj1 %>%   
  select(regno,TotalLand, mistype, RegistrationDate,taluka, caste )%>%
  filter(!is.na(TotalLand),TotalLand<22, TotalLand>0
  ) %>%
  group_by(mistype, taluka) %>% 
  arrange(RegistrationDate) %>%
  filter(n() >= 15) %>%
  mutate(rank_date_10adopters=ceiling(row_number()/10) # Create groups of 10 observations
  ) %>%
  ungroup() %>% 
  count(mistype,taluka,rank_date_10adopters,caste) %>% 
  group_by(mistype,rank_date_10adopters,caste) %>% 
  summarise(n=sum(n)) %>% 
  group_by(mistype,rank_date_10adopters) %>% 
  mutate(pct=n/sum(n)) %>% ungroup()

caste_taluka %>% filter(rank_date_10adopters < 6)


#Fig.     ----

caste_taluka %>% 
  ggplot(aes(x=rank_date_10adopters, y=pct, fill=caste)) +
  geom_bar(stat="identity")+
  facet_wrap(~mistype,  ncol=1, strip.position = "left")+
  
  theme_classic() +
  scale_fill_manual(values=c("#5F6EA0",  "#5F9EA0" ),name="")+  
  ggtitle("Household Social Status by drip or sprinkler [Subdistrict wise]")+
  ylab("% of rankings")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif"))+ 
  scale_x_continuous(limits = c(0, 31), breaks = seq(1, 30, 1), expand= c(0,0))









#subcaste ----
library(stringr)
dfC=
  gj1 %>%   
  select(regno,farmername,farmercaste, mistype, RegistrationDate,villageSI,caste) %>%
  filter(!is.na(RegistrationDate),!is.na(villageSI)) %>% 
  mutate(name1 = word(farmername, 1),
         name2 = word(farmername, 2),
         name3 = word(farmername, 3))

df=
  gj1 %>%   
  select(regno,farmername,farmercaste, mistype, RegistrationDate,villageSI,caste) %>%
  filter(!is.na(RegistrationDate),!is.na(villageSI)) 

contains_PATEL <- grepl("PATEL", df$farmername, ignore.case = TRUE)
df$name_caste <- ifelse(contains_PATEL, "hc_PATEL", df$caste)
contains_KOLI <- grepl("KOLI", df$farmername, ignore.case = TRUE)
df$name_caste <- ifelse(contains_KOLI, "hc_KOLI", df$caste)

  
df %>% group_by(mistype,name_caste,villageSI) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  count(mistype,name_caste,rank_date_adopters) %>% 
  group_by(mistype,rank_date_adopters) %>% 
  mutate(pct_mistype_wise=n/sum(n)) %>% ungroup() %>% 
  
  filter(rank_date_adopters<100) %>% 
  ggplot(aes(x=rank_date_adopters, y=pct_mistype_wise, fill=name_caste)) +
  geom_bar(stat="identity")+
  facet_wrap(~mistype,  ncol=1, strip.position = "left")
  

contains <- grepl("SHAH", dfCst$farmername, ignore.case = TRUE)
freq(contains)



df <- separate(df, names, into = c("First_Name", "Middle_Name", "Last_Name"), sep = " ")




#########  Loan  ............................................         ################################################################
gj1 %>%   
  select(regno, mistype, RegistrationDate,villageSI,Loan )%>%
  group_by(mistype,villageSI) %>% 
  filter(n() >= 30) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  count(mistype,Loan)

villageSI_loan= 
  gj1 %>%   filter(mistype!= "Sprinkler") %>%
  select(regno, RegistrationDate,villageSI,Loan )%>%
  group_by(villageSI) %>% 
  filter(n() >= 15) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>%
  group_by(rank_date_adopters) %>% 
  summarise(n=n(),
            LoanPCT=mean(Loan,na.rm = T),
            SD=sd(Loan,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% ungroup()

villageSI_loan %>% filter(rank_date_adopters %in% c(1:4))

#Fig.     ----
villageSI_loan %>%  
  ggplot(aes(x=rank_date_adopters, y=LoanPCT)) +
  geom_bar(stat = "identity", fill = "lightblue3") +
  geom_errorbar(aes(ymin = LoanPCT - CI95delta, ymax = LoanPCT + CI95delta),
                width = 0.4, color = "gray30") +
  theme_classic()+ 
  ggtitle("Drip | Share of farms who took a loan [village wise]")+
  ylab("Likelihood of taking loan")+xlab("Ranking of MI adoption date ") +
  theme(plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif"))+
  ylim(0,0.172)+ 
  scale_x_continuous(limits = c(0, 16), breaks = seq(1, 15, 1), expand= c(0,0))

# taluka wise ----
subdistrict_loan= 
  gj1 %>%   filter(mistype!= "Sprinkler") %>%
  select(regno, RegistrationDate,taluka,Loan )%>%
  group_by(taluka) %>% 
  filter(n() >= 15) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_10adopters=ceiling(row_number()/10)
  ) %>%
  group_by(rank_date_10adopters) %>% 
  summarise(n=n(),
            LoanPCT=mean(Loan,na.rm = T),
            SD=sd(Loan,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% ungroup()


#Fig.     ----
subdistrict_loan %>% 
  ggplot(aes(x = rank_date_10adopters, y = LoanPCT)) +
  geom_bar(stat = "identity", fill = "lightblue3") +
  geom_errorbar(aes(ymin = LoanPCT - CI95delta, ymax = LoanPCT + CI95delta),
                width = 0.4, color = "gray30") +
  theme_classic() + 
  ggtitle("Drip | Share of farms who took a loan [Sub-District wise]") +
  ylab("Likelihood of taking loan") +
  xlab("Ranking of MI adoption date") +
  theme(plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif")) +
  ylim(0,0.172)+ 
  scale_x_continuous(limits = c(0, 16), breaks = seq(1, 15, 1), expand = c(0, 0))
#

 
#

########  Distance        ############################################################

# Function to calculate distance using Haversine formula
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert latitude and longitude from degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Earth radius in kilometers
  R <- 6371 
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- R * c
  
  return(distance) # Distance in kilometers
}

dis= 
  gj1 %>%   
  select(regno,mistype, RegistrationDate,villageSI ,Latitude,Longitude) %>%
  filter(Latitude>20.25,Latitude<24.5, Longitude>68.75,Longitude<74.15) %>% 
  
  group_by(mistype,villageSI) %>% 
  filter(n() >= 15) %>% 
  arrange(RegistrationDate) %>% 
  
  mutate(rank_date_adopters = row_number()) %>% 
  group_by(mistype,villageSI) %>% 
  mutate(distance=haversine_distance(Latitude[1], Longitude[1], 
                                     Latitude, Longitude) )%>% 
  ungroup(
  ) %>% 
  filter(distance<5) %>% 
  filter(rank_date_adopters!=1) %>% 
  group_by(mistype,rank_date_adopters)%>% 
  summarise(n=n(),
            Distance_Km=mean(distance,na.rm = T),
            SD=sd(distance,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% 
  ungroup()

#Fig.----

dis %>% 
  ggplot(aes(rank_date_adopters,Distance_Km, color = mistype, group = mistype)) +
  geom_line(size=1)+
  
  geom_ribbon(data = filter(dis, mistype == "Drip"), 
              aes(ymin=Distance_Km-CI95delta, ymax=Distance_Km+CI95delta), 
              fill = "gray80", color = "white", alpha = 0.4) +
  geom_ribbon(data = filter(dis, mistype == "Sprinkler"), 
              aes(ymin=Distance_Km-CI95delta, ymax=Distance_Km+CI95delta), 
              fill = "gray85", color = "white", alpha = 0.4) +
  scale_color_manual(values = c("Drip" = "blue4", "Sprinkler" = "royalblue2"))+
  theme_classic() +
  ggtitle("Farmer's distance from the previous one [Village wise]")+
  ylab("Distance (in Km)")+xlab("Ranking of MIS adoption date ") +
  theme(legend.title = element_blank(),
        plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif")
  )+ ylim(1,2.1)+ 
  scale_x_continuous(breaks= seq(1,30,1), limits= c(1,30), expand= c(0,0))
#

# taluka WISE     ----

# A
dis_taluka <- gj1 %>%   
  select(regno,mistype, RegistrationDate,taluka ,Latitude,Longitude) %>%
  filter(Latitude>20.25,Latitude<24.5, Longitude>68.75,Longitude<74.15) %>% 

  group_by(taluka,mistype) %>% 
  arrange(RegistrationDate) %>%
  filter(n() >= 15) %>%
  
  mutate(distance=haversine_distance(Latitude[1], Longitude[1], 
                                     Latitude, Longitude) )%>% 
  mutate(rank_date_10adopters=ceiling(row_number()/10)) %>%
  
  group_by(mistype, taluka,rank_date_10adopters) %>% 
  summarise(Distance = mean(distance, na.rm = TRUE)) %>% ungroup()

# B
rank_dis_taluka <- dis_taluka %>%   
  group_by(mistype, rank_date_10adopters) %>% # averaging of averages
  summarise(
    Distance_Km = mean(Distance, na.rm = TRUE),
    n = n(),
    SD=sd(Distance,na.rm = T),
    CI95delta= 1.96*(SD/sqrt(n))) %>% ungroup()


rank_dis_taluka %>% filter(rank_date_10adopters < 6)


#Fig.----

rank_dis_taluka %>% 
  ggplot(aes(rank_date_10adopters,Distance_Km, color = mistype, group = mistype)) +
  geom_line(size=1)+
  
  geom_ribbon(data = filter(rank_dis_taluka, mistype == "Drip"), 
              aes(ymin=Distance_Km-CI95delta, ymax=Distance_Km+CI95delta), 
              fill = "gray80", color = "white", alpha = 0.4) +
  geom_ribbon(data = filter(rank_dis_taluka, mistype == "Sprinkler"), 
              aes(ymin=Distance_Km-CI95delta, ymax=Distance_Km+CI95delta), 
              fill = "gray85", color = "white", alpha = 0.4) +
  scale_color_manual(values = c("Drip" = "blue4", "Sprinkler" = "royalblue2"))+
  theme_classic() +
  ggtitle("Farmer's distance from the previous one [Sub-District wise]")+
  ylab("Distance (in Km)")+xlab("Ranking of MIS adoption date ") +
  theme(legend.title = element_blank(),
        plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif")
  )+ ylim(25,75)+ 
  scale_x_continuous(breaks= seq(1,30,1), limits= c(1,30), expand= c(0,0))
#







#### PROB  Irrigation method    ########################################################
# אחוז המאמצים בכפר הדומים למאמץ הראשון
# ההסתברות שחקלאי בכפר יאמץ בדומה למאמץ הראשון

prb_villageSI=
  gj1 %>% 
  select(regno, RegistrationDate,villageSI,mistype) %>%
  group_by(villageSI) %>%
  filter(n() >= 15) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  group_by(villageSI) %>%
  mutate(prob = 
           if_else(rank_date_adopters != 1 & mistype == first(mistype), 1, 0)) %>% 
  mutate(prob=ifelse(rank_date_adopters == 1,NA,prob))

# PLOT ----

prob_df <- prb_villageSI %>% 
  filter(rank_date_adopters!=1) %>% 
  group_by(rank_date_adopters) %>% 
  summarise(n=n(),
            Mean=mean(prob,na.rm = T),
            SD=sd(prob,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% ungroup()

Pim <- prob_df %>% 
  ggplot(aes(rank_date_adopters, Mean)) +
  geom_ribbon(aes(ymin=Mean-CI95delta, ymax=Mean+CI95delta), fill = "gray90", color = "white") +
  geom_line(size = 1, colour = "lightblue4") +  # Change color here
  theme_classic() +
  ggtitle("Probability of farmers owning the same irrigation method as the first farmer [Village wise]")+
  xlab("Ranking of MIS adoption date ") +ylab("Probability")+
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))

#Fig.----
Pim + ylim(.7,0.86)+ 
  scale_x_continuous(breaks= seq(1,15,1), limits= c(1,15), expand= c(0,0))

prob_df %>% filter(rank_date_adopters %in% c(2,15,30))


# prob ----

prb_mis_village <- 
  prb_villageSI%>%  
  mutate(mistype_as_1st = mistype[rank_date_adopters == 1]) %>% 
  group_by(villageSI) %>%
  mutate(mistype_majority = names(which.max(table(mistype)))) %>%
  ungroup() %>% 
  mutate(mistype_majority_as_1st= 
           ifelse( mistype_as_1st == mistype_majority,1,0))


prb_mis_village %>% count(mistype_majority_as_1st) %>% 
  mutate(total_n = sum(n),pct = (n / total_n))
  
# hist - mayjority
prob_1 <- 
  prb_mis_village %>%
  count(villageSI, mistype) %>%
  group_by(villageSI) %>%
  mutate(total_n = sum(n),pct = (n / total_n)
  ) %>% 
  mutate(majority_pct= ifelse( n == max(n),pct,NA)) %>% 
  ungroup()

# Filter data for Drip and Sprinkler
drip_data <- prob_1 %>% filter(mistype == "Drip")
sprinkler_data <- prob_1 %>% filter(mistype == "Sprinkler")

# Histogram for all villages
# ggplot(drip_data, aes(x = pct)) +
#   geom_histogram(binwidth = 0.05, fill = "blue4", color = "black", alpha = 0.7) +
#   geom_vline(aes(xintercept = mean(pct, na.rm = TRUE)), color = "red3", linetype = "dashed", size = .5) +
#   annotate( "text", x=mean(drip_data$pct, na.rm = TRUE), y=max(table(cut(drip_data$pct, breaks = 10))), 
#             label = paste("Av.", round(mean(drip_data$pct, na.rm = TRUE), 2)), color = "red3", hjust = -0.1)+
#   theme_classic()

hd1=
  ggplot(drip_data, aes(x = pct)) +
  geom_histogram(bins = 10, fill = "blue4", color = "black", alpha = 0.7) +
  theme_classic()
mean(drip_data$pct)

hs1=
  ggplot(sprinkler_data, aes(x = pct)) +
  geom_histogram(bins = 10, fill = "royalblue2", color = "black", alpha = 0.7)+
  theme_classic()
mean(sprinkler_data$pct)

library(patchwork)
hd1+hs1 #700X200
# Histogram for MI majority_pct 
hd2=
  ggplot(drip_data, aes(x = majority_pct)) +
  geom_histogram(bins = 10, fill = "blue4", color = "black", alpha = 0.7) +
  theme_classic()
mean(drip_data$majority_pct,na.rm = T)

# Histogram for Sprinkler
hs2=
  ggplot(sprinkler_data, aes(x = majority_pct)) +
  geom_histogram(bins = 10, fill = "royalblue2", color = "black", alpha = 0.7)+
  theme_classic()
mean(sprinkler_data$majority_pct,na.rm = T)

hd2+hs2








# AB
prb_mis_village %>% filter(rank_date_adopters !=1) %>% 
  group_by(villageSI) %>% 
  summarise(mn=mean(prob)) %>%
  summarise(mean_result = mean(mn))

# AC
prb_mis_village %>% filter(rank_date_adopters !=1) %>% 
  group_by(prob_mistype) %>% 
  summarise(MEAN=mean(prob))




















# hist - mayjority
result_15=
  gj1 %>%
  count(villageSI, mistype) %>%
  group_by(villageSI) %>%
  mutate(total_n = sum(n),percent = (n / total_n)
  ) %>%  
  filter(total_n >15, n == max(n)) %>% 
  group_by(mistype) %>%
  reframe(pct_v15 = mean(percent, na.rm =T),n_v15=n())

gj1 %>%
  count(taluka, mistype) %>%
  group_by(taluka) %>%
  mutate(total_n = sum(n),percent = (n / total_n)
  ) %>%  
  filter(total_n >15, n == max(n)) %>% 
  group_by(mistype) %>%
  reframe(pct = mean(percent, na.rm =T),n=n())

# Combine all results into one table
library(kableExtra)

result_all %>%
  left_join(result_15, by = "mistype") %>%
  left_join(result_30, by = "mistype") %>%
  left_join(result_50, by = "mistype") %>% 
  mutate(
    pct_v15 = round(pct_v15, 2),
    pct_v30 = round(pct_v30, 2),
    pct_v50 = round(pct_v50, 2),
    pct_v = round(pct_v, 2)
  ) %>%
  kbl() %>%
  kable_classic() %>%
  add_header_above(c(" " = 1, 
                     "All Villages" = 2, 
                     "Villages of n > 15" = 2, 
                     "Villages of n > 30" = 2, 
                     "Villages of n > 50" = 2))






# LM PROB
dt=prb_mis_village %>% filter(rank_date_adopters<30)
M2 <- lm(prob ~ rank_date_adopters + factor(), data = dt)
sjPlot::tab_model(M2, digits = 4, show.se = T)


# taluka wise ----


gj1 %>%
  count(taluka, mistype) %>%
  group_by(taluka) %>%
  mutate(total_n = sum(n),percent = (n / total_n)
  ) %>%  
  filter(total_n >15, n == max(n)) %>% 
  group_by(mistype) %>%
  reframe(pct = mean(percent, na.rm =T),n=n())




#### PROB  MIS supplier         ########################################################

prob_supplier <- gj1 %>% 
  select(regno, RegistrationDate,villageSI,mistype,supplier) %>%
  group_by(mistype, villageSI) %>% 
  filter(n() >= 15) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number(),
         days_gap = c(NA, diff(RegistrationDate))) %>% 
  mutate(prob = 
           if_else(rank_date_adopters != 1 & supplier == first(supplier), 1, 0)) %>% 
  mutate(prob=ifelse(rank_date_adopters == 1,NA,prob))%>% 
  group_by(mistype, rank_date_adopters) %>% 
  summarise(n=n(),
            IS_prob=mean(prob,na.rm = T),
            SD=sd(prob,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% 
  filter(rank_date_adopters!=1)

# PLOT ----
Ps=
  prob_supplier %>% 
  ggplot(aes(x = rank_date_adopters, y = IS_prob, color = mistype, group = mistype)) +
  geom_line(size = 1) + 
  geom_ribbon(data = filter(prob_supplier, mistype == "Drip"), 
              aes(ymin = IS_prob - CI95delta, ymax = IS_prob + CI95delta), 
              fill = "gray80", color = "white", alpha = 0.4) +
  geom_ribbon(data = filter(prob_supplier, mistype == "Sprinkler"), 
              aes(ymin = IS_prob - CI95delta, ymax = IS_prob + CI95delta), 
              fill = "gray85", color = "white", alpha = 0.4) +
  ggtitle("Farmers' probability of holding the same MIS as the 1st farmer")+
  xlab("Ranking of MIS adoption date ") +ylab("Probability")+
  scale_color_manual(values = c("Drip" = "blue4", "Sprinkler" = "royalblue2"))+
  theme_classic() +
  theme(legend.title = element_blank(),
        plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif"))
#Fig.----
Ps + ylim(0,.55)+ 
  scale_x_continuous(breaks= seq(1,30,1), limits= c(1,30), expand= c(0,0))

prob_supplier %>% filter(rank_date_adopters %in% c(2,15,27))




#########  Crop                 ################################################################
prob_crop <- gj1 %>% 
  select(regno, RegistrationDate,villageSI,mistype ,Crop) %>%
  group_by(mistype ,villageSI) %>% 
  filter(n() >= 15) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>% 
  mutate(prob = if_else(rank_date_adopters != 1 & Crop == first(Crop), 1, 0)) %>% 
  mutate(prob=ifelse(rank_date_adopters == 1,NA,prob)
  ) %>% 
  group_by(mistype,rank_date_adopters) %>% 
  summarise(n=n(),
            crp_prob=mean(prob,na.rm = T),
            SD=sd(prob,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% 
  filter(rank_date_adopters!=1)


#Fig.----
prob_crop %>% 
  ggplot(aes(x = rank_date_adopters, y = crp_prob, color = mistype, group = mistype)) +
  geom_line(size = 1) + 
  geom_ribbon(data = filter(prob_crop, mistype == "Drip"), 
              aes(ymin = crp_prob - CI95delta, ymax = crp_prob + CI95delta), 
              fill = "gray80", color = "white", alpha = 0.4) +
  geom_ribbon(data = filter(prob_crop, mistype == "Sprinkler"), 
              aes(ymin = crp_prob - CI95delta, ymax = crp_prob + CI95delta), 
              fill = "gray85", color = "white", alpha = 0.4) +
  scale_color_manual(values = c("Drip" = "blue4", "Sprinkler" = "royalblue2"))+
  ggtitle("Farmers' probability cultivating the same crop as the 1st farmer")+
  xlab("Ranking of MIS adoption date ") +ylab("Probability")+
  theme_classic() +
  theme(legend.title = element_blank(),
        plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif"))+
  ylim(.68,.88)+ 
  scale_x_continuous(breaks= seq(1,15,1), limits= c(1,15), expand= c(0,0))

prob_crop %>% filter(rank_date_adopters %in% c(2,15,27))




# crop freq ----

df <- gj1 %>% 
  select(regno,RegistrationDate,villageSI, Crop, mistype,taluka) %>%
  mutate(Crop = case_when(
    Crop %in% c("BITTER GUARD", "BOTTLE GUARD", "GOURDS", "SPONGE GOURD") ~ "GOURD",
    Crop %in% c("GRAM", "GREEN GRAM") ~ "GRAM",
    TRUE ~ as.character(Crop)
  )) %>% 
  filter(Crop %in% c (
    "Banana" , "CASTOR", "CHILLI", "COTTON","GRAM","GROUNDNUT",
    "MAIZE" , "Mango","Pomogranate", "SUGARCANE","WHEAT","GRAM"
    )) %>% 
  group_by(mistype ,villageSI) %>% 
  filter(n() >= 15) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>% ungroup() %>% 
  arrange(villageSI,mistype, rank_date_adopters)


df %>% 
  count(villageSI,mistype, Crop) %>% 
  group_by(villageSI) %>%
  mutate(total_n = sum(n),percent = (n / total_n)
  ) %>%  
  filter(total_n >15, n == max(n)) %>% 
  group_by(Crop) %>%
  reframe(pct_v15 = mean(percent, na.rm =T),n_v15=n())





# Crop & MI Type ----
contingency_table <- table(df$mistype, df$Crop)
print(contingency_table)
chisq.test(contingency_table)

df2=df %>%
  group_by(rank_date_adopters) %>%
  filter(rank_date_adopters <= 30) %>%
  count(Crop, mistype) %>%
  mutate(percent = n / sum(n))

ggplot(df2, aes(x = rank_date_adopters, y = percent, fill = interaction(Crop, mistype)))+
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Crop-MI Dominance Among Early Adopters",
       x = "Village", y = "Percentage", fill = "Crop & MI Type")





#       FUNCTIONS  ################################################################


summarise(n=n(),Mean=mean(prbtly,na.rm = T), SD=sd(prbtly,na.rm = T), se = SD / sqrt(n), ciMult= qt(.975, n - 1), ci95 = se * ciMult) 

ca=
  prb %>% 
  filter(rank_date_adopters!=1) %>% 
  group_by(rank_date_adopters) %>% 
  summarise(n=n(),
            Mean=mean(prbtly,na.rm = T),
            SD=sd(prbtly,na.rm = T),
            CI95= qnorm(0.975)*(SD/sqrt(n)),
            lw=IM_prob-CI95,hi=IM_prob+CI95,
            se = SD / sqrt(n),
            ciMult= qt(.975, n - 1),
            ci95 = se * ciMult
  ) 

group_by(rank_date_adopters)%>% 
  summarise(n=n(),
            Total_Land=mean(TotalLand,na.rm = T),
            SD=sd(TotalLand,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n)))
P=
  df %>%
  ggplot(aes(rank_date_adopters, Total_Land)) +
  geom_ribbon(data = filter(df, mistype == "Drip"),
              aes(ymin=Total_Land-CI95delta, ymax=Total_Land+CI95delta), fill = "gray90", color = "white") +
  geom_ribbon(data = filter(df, mistype == "Sprinkler"),
              aes(ymin=Total_Land-CI95delta, ymax=Total_Land+CI95delta), fill = "gray90", color = "white") +
  geom_line(aes(color = mistype), size = 1) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkler"="royalblue1"),name="") +
  theme_light() +
  ggtitle("Farms' ")+ylab("%")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))



######## MIS costs ######################################################## ####
descr(GGRC$TotalMISCost,stats = c("mean", "sd", "min", "max"),transpose = T)
quantile(GGRC$TotalMISCost , 0.99, na.rm = TRUE)
quantile(GGRC$misarea , 0.01, na.rm = TRUE)
quantile(GGRC$misarea , 0.99, na.rm = TRUE)


MIS_Cost=
  gj1 %>%
  select(regno, RegistrationDate, mistype, villageSI,misarea, TotalMISCost) %>%
  filter(!is.na(TotalMISCost), TotalMISCost < 497012, misarea != 0) %>%
  mutate(MISCost_per_ha= TotalMISCost/misarea )

MIS_Cost %>% filter(misarea<10,TotalMISCost<150000) 

mis_cost=
  MIS_Cost %>% 
  group_by(mistype,villageSI) %>% 
  arrange(RegistrationDate) %>% 
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  group_by(mistype,rank_date_adopters)%>% 
  summarise(n=n(),
            MISCost=mean(TotalMISCost,na.rm = T),
            SD=sd(TotalMISCost,na.rm = T),
            CI95= qnorm(0.975)*(SD/sqrt(n)),
            lw=MISCost-CI95,hi=MISCost+CI95,
            se = SD / sqrt(n),
            ciMult= qt(.975, n - 1),
            ci95 = se * ciMult) 


PCo=
  mis_cost  %>% 
  ggplot(aes(rank_date_adopters, MISCost)) +
  geom_ribbon(data = filter(mis_cost, mistype == "Drip"),
              aes(ymin=MISCost-CI95, ymax=MISCost+CI95), fill = "gray70", color = "white") +
  geom_ribbon(data = filter(mis_cost, mistype == "Sprinkler"),
              aes(ymin=MISCost-CI95, ymax=MISCost+CI95), fill = "gray60", color = "white") +
  geom_line(aes(color = mistype), size = 1) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkler"="royalblue1"),name="") +
  theme_light()+
  ggtitle("Cost per hectar by drip or sprinkler")+
  ylab("Rs. per Ha")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))

#Fig.----

PCo  + 
  scale_x_continuous(limits = c(1, 31), breaks = seq(1, 30, 1)) + 
  scale_y_continuous(labels = comma) + ylim(0,170000)








