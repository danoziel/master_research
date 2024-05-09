library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(summarytools)

GGRC$m_ccode11 <- read_dta("C:/Users/Dan/OneDrive - mail.tau.ac.il/world_bank/GGRCDATA_20141219_a_1.dta")
names(GGRC)
#### DFs   ################################################################# ####    
# mis_type       442,456 obs. | Drip & Sprinkler                      ----

mis_type=
  GGRC %>% select(regno, mistype) %>% filter(mistype != "")

# gj_village      16,194 obs. | Gujarat villages list and frequency   ----

gj_village=
  GGRC %>% select(farmervillage,taluka,district) %>% 
  filter(farmervillage != "") %>%
  count(farmervillage,taluka,district) %>%  # 16,194 obs.
  mutate(villageSI = paste0("V", 100001+row_number()-1)) %>% select(-n)
#
# gj1 <- GGRC    483,692 obs.                             ----

gj11 <- GGRC %>% 
  select(regno,year_Registration, RegistrationDate,farmername,
         Latitude,Longitude,
         farmercaste,FarmerType,Loan,TotalMISCost,
         TotalLand,misarea,farmervillage,taluka,district,
         mistype, EnergySource, WaterSourceDetail,
         "supplier", "RKVY","Crop",49:50
  ) %>% 
  mutate(farmer_category=
           ifelse(FarmerType %in% c("Small Farmer" ,"Marginal Farmer"),"Small.Marginal Farm",
                  ifelse(FarmerType %in% c("Large Farmer","Medium Farmer","Semi-Medium Farmer"),"Large.Medium Farm",NA))
  ) %>% 
  mutate(caste=
           ifelse(farmercaste =="Others", "high caste",
                  ifelse(farmercaste %in% c("OBC","ST","SC"),"low caste",NA))) %>% 
  mutate(mi_pct_land=misarea/TotalLand )


gj1 <- gj11 %>% left_join(gj_village)
rm(gj11)
rm(gj_village)

#
# date_rank      442,456 obs.                             ----
#
# X-axis is the ranking of the farmers on the date of registration - village wise. 
# X-axis is a sequence from the first farmer in his village to adopt a MIS

date_rank=
  gj1 %>%   
  filter(!is.na (RegistrationDate),!is.na(villageSI) ) %>% 
  select(regno, mistype, RegistrationDate,villageSI )%>%
  group_by(villageSI) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number(),
         days_gap = c(NA, diff(RegistrationDate))) %>% ungroup()

library(ggplot2)

# Custom function to format y-axis labels
custom_label_format <- function(x) {
  ifelse(x >= 1000, paste0(as.integer(x / 1000), "K"), as.character(x))
}

date_rank %>%
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
  
freq(GGRC$farmercaste )
freq(gj1$year_Registration  , report.nas=F, totals=F, cumul=F, headings=F)

#### FREQ  first to adopt ################################################## ####

df=date_rank %>% count(rank_date_adopters,mistype) %>%
  group_by(rank_date_adopters) %>%  mutate(pct=n/sum(n)) %>% 
  ungroup()

Pf=df  %>% 
    ggplot(aes(x=rank_date_adopters, y=pct, fill=mistype)) +
    geom_bar(stat="identity" # , width = 1
             )+ theme_light()+ 
  scale_fill_manual(values=c('blue4','royalblue1'),name="")+  
  ggtitle("Share of Drip/Sprinkler installed farms")+
  ylab("% of rankings")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif"))
#Fig.----
Pf + scale_x_continuous(limits = c(0, 31), breaks = seq(1, 30, 1))
Pf + scale_x_continuous(limits = c(0, 101), breaks = seq(0, 100, 10))

#
# crop freq treemap ----

library(treemap)

crop_count= 
  gj1 %>% 
  filter(!Crop %in% c("","-","NULL" )) %>% 
  group_by(mistype,Crop) %>% 
  summarise(n=n()) %>% group_by(mistype) %>% mutate(N=sum(n)) %>% 
  ungroup()

crop_drip <- 
  crop_count %>% filter(mistype=="Drip") %>% 
  mutate(n/N) %>% mutate_at(5,round,4)
crop_drip <-crop_drip %>% 
  mutate(crop=ifelse(n<1000,"Other",Crop)) %>% 
  group_by(crop) %>% summarise(n=sum(n),pct=n/177747) %>% 
  mutate_at(3,round,2)

plt <- colorRampPalette(c("lightgreen", "darkgreen"))(length(unique(crop_drip$crop)))
treemap(crop_drip,index = "crop",vSize = "pct",type = "index", palette = plt, title = "")

#

crop_sprinkler <- 
  crop_count %>% filter(mistype=="Sprinkler") %>% 
  mutate(n/N) %>% mutate_at(5,round,4)
crop_sprinkler <-crop_sprinkler %>% 
  mutate(crop=ifelse(n<200,"Other",Crop)) %>% 
  group_by(crop) %>% summarise(n=sum(n),pct=n/233153)%>% 
  mutate_at(3,round,2)

plt <- colorRampPalette(c("lightgreen", "darkgreen"))(length(unique(crop_sprinkler$crop)))
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
########   Weeks gap      ##################################################### ####

# Time period between systems adoption
# No. weeks between RegistrationDate farmers

df <- 
  date_rank %>% group_by(mistype,rank_date_adopters) %>% 
  mutate(weeksgap=days_gap/7) %>% 
  summarise(n=n(),
            weeks_gap=mean(weeksgap,na.rm = T),
            SD=sd(weeksgap,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% 
  filter(rank_date_adopters!=1) %>% 
  ungroup()

#_____________
df %>% ggplot(aes(rank_date_adopters,weeks_gap,color = mistype)) +
  geom_line(size=1)+ 
  xlim(2,30)


Pg=
  df %>%
  ggplot(aes(rank_date_adopters, weeks_gap)) +
  geom_ribbon(data = filter(df, mistype == "Drip"),
              aes(ymin=weeks_gap-CI95delta, ymax=weeks_gap+CI95delta), fill = "gray60", color = "white") +
  geom_ribbon(data = filter(df, mistype == "Sprinkler"),
              aes(ymin=weeks_gap-CI95delta, ymax=weeks_gap+CI95delta), fill = "gray70", color = "white") +
  geom_line(aes(color = mistype), size = 1) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkler"="royalblue1"),name="") +
  theme_light() +
  ggtitle("No. weeks between registration date farmers")+ylab("Gap in weeks")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))


#Fig.----
Pg + scale_x_continuous(limits = c(2, 31), breaks = seq(2, 30, 1))
Pg + xlim(2,30)
#
Pg + xlim(2,100)
#
# accumulated_week_gap ----
library(patchwork) # To display 2 charts together
library(hrbrthemes)

# A few constants
coeff <- 5
weeks <- "#69b3a2"
acc_weeks <- rgb(0.2, 0.6, 0.9, 1)

date_gap2 <- 
  date_rank %>% group_by(rank_date_adopters) %>% 
  filter(rank_date_adopters!=1) %>%
  summarise(days_gap=mean(days_gap)) %>%
  mutate(week_gap=days_gap/7) %>% 
  mutate(accumulated_week_gap = cumsum(week_gap))

Pdg=
  date_gap2 %>% filter(rank_date_adopters<30) %>%  
  ggplot( aes(x=rank_date_adopters)) +
  geom_line( aes(y=week_gap), size=1, color=weeks) + 
  geom_line( aes(y=accumulated_week_gap / coeff), size=1, color=acc_weeks) +
  scale_y_continuous(
    name = "Weeks Gap",
    sec.axis = sec_axis(~.*coeff, name="Accumulated Weeks Gap")
  )  + 
  theme_ipsum() +
  theme(axis.title.y = element_text(color = weeks, size=13),
        axis.title.y.right = element_text(color = acc_weeks, size=13))
#Fig.----
Pdg + ggtitle("Total weeks between registration date")+
  scale_x_continuous(breaks=seq(1,30,5))

########  Total Land       ################################################# ####

#### mistype=="Drip"
rank_TotalLand_D=
  gj1 %>% filter(mistype=="Drip" ) %>% 
  select(regno, RegistrationDate,villageSI,TotalLand) %>%
  group_by(villageSI) %>% 
  arrange(RegistrationDate) %>% 
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56
  ) %>% 
  group_by(rank_date_adopters)%>% 
  summarise(n=n(),
            Total_Land=mean(TotalLand,na.rm = T),
            SD=sd(TotalLand,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n)))


#### mistype=="Sprinkler"
rank_TotalLand_S=
  gj1 %>% filter(mistype=="Sprinkler" ) %>% 
  select(regno, RegistrationDate,villageSI,TotalLand) %>%
  group_by(villageSI) %>% 
  arrange(RegistrationDate) %>% 
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56
  ) %>% 
  group_by(rank_date_adopters)%>% 
  summarise(n=n(),
            Total_Land=mean(TotalLand,na.rm = T),
            SD=sd(TotalLand,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n)))

# Combine the data frames and create a grouping variable
combined_data_rank_totalland <- bind_rows(
  mutate(rank_TotalLand_D, type = "Drip"),
  mutate(rank_TotalLand_S, type = "Sprinkle"))

Ptt=
  combined_data_rank_totalland %>%
  ggplot(aes(rank_date_adopters, Total_Land)) +
  geom_ribbon(data = filter(combined_data_rank_totalland, type == "Drip"),
              aes(ymin=Total_Land-CI95delta, ymax=Total_Land+CI95delta), 
              fill = "gray90", color = "white") +
  geom_ribbon(data = filter(combined_data_rank_totalland, type == "Sprinkle"),
              aes(ymin=Total_Land-CI95delta, ymax=Total_Land+CI95delta), 
              fill = "gray90", color = "white") +
  geom_line(aes(color = type), size = 1) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkle"="royalblue1"),name="") +
  theme_light() +
  ggtitle("Farms' total land size")+ylab("Land (in Ha)")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))

#Fig.----
Ptt + ylim(2.6,3.55)+xlim(1,30)
#
Ptt+ylim(2.25,3.6)+xlim(1,100)
#

########  Installed area    ################################################ ####

#### mistype=="Drip"
rank_misarea_D=
  gj1 %>% filter(mistype=="Drip" ) %>% 
  select(regno, RegistrationDate,villageSI,TotalLand,misarea) %>%
  group_by(villageSI) %>% 
  arrange(RegistrationDate) %>% 
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56
         ) %>% 
  group_by(rank_date_adopters)%>% 
  summarise(n=n(),
            irrigated_land_ha=mean(misarea,na.rm = T),
            SD=sd(misarea,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n)))


#### mistype=="Sprinkler"
rank_misarea_S=
  gj1 %>% filter(mistype=="Sprinkler" ) %>% 
  select(regno, RegistrationDate,villageSI,TotalLand,misarea) %>%
  group_by(villageSI) %>% 
  arrange(RegistrationDate) %>% 
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56
         ) %>% 
  group_by(rank_date_adopters)%>% 
  summarise(n=n(),
            irrigated_land_ha=mean(misarea,na.rm = T),
            SD=sd(misarea,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n)))

# Combine the data frames and create a grouping variable
combined_data_rank_misarea <- bind_rows(
  mutate(rank_misarea_D, type = "Drip"),
  mutate(rank_misarea_S, type = "Sprinkle"))

Pmisarea=
  combined_data_rank_misarea %>%
  ggplot(aes(rank_date_adopters, irrigated_land_ha)) +
  geom_ribbon(data = filter(combined_data_rank_misarea, type == "Drip"),
              aes(ymin=irrigated_land_ha-CI95delta, ymax=irrigated_land_ha+CI95delta), 
              fill = "gray90", color = "white") +
  geom_ribbon(data = filter(combined_data_rank_misarea, type == "Sprinkle"),
              aes(ymin=irrigated_land_ha-CI95delta, ymax=irrigated_land_ha+CI95delta), 
              fill = "gray90", color = "white") +
  geom_line(aes(color = type), size = 1) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkle"="royalblue1"),name="") +
  theme_light() +
  ggtitle("Farms' installed area size by drip or sprinkler")+
  ylab("Irrigated Land (in Ha)")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif"))
#Fig.----
Pmisarea + ylim(1.2,1.7) + xlim(1,30)
#
Pmisarea + ylim(1.1,1.85) + xlim(1,100)
#

######## % Irrigated Land  ###################################### ####
# The percentage of the area installed in the system out of the total area owned by the farmer


df_mi_pct_land= 
  gj1 %>%   
  filter(!is.na (RegistrationDate),!is.na(villageSI)) %>% 
  select(regno,TotalLand, mistype, RegistrationDate,villageSI,mi_pct_land )%>%
  mutate_at(6,round,4) %>% 
  group_by(mistype,villageSI) %>% arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
    filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56
           ,!is.na(mi_pct_land) ) %>% 
    group_by(mistype,rank_date_adopters)%>% 
    summarise(n=n(),
              pct_ir_land=mean(mi_pct_land,na.rm = T),
              SD=sd(mi_pct_land,na.rm = T),
              CI95delta= 1.96*(SD/sqrt(n)))
  

df_mi_pct_land %>%  
  ggplot(aes(rank_date_adopters,pct_ir_land,color = mistype)) +
  geom_line(size=1)+theme_bw()+
  scale_color_manual(values=c('blue4','royalblue1'),name="")+  
  ylim(0,0.8)


P=df_mi_pct_land  %>%
  ggplot(aes(rank_date_adopters, pct_ir_land)) +
  geom_ribbon(data = filter(df_mi_pct_land, mistype == "Drip"),
              aes(ymin=pct_ir_land-CI95delta, ymax=pct_ir_land+CI95delta), 
              fill = "gray70", color = "white") +
  geom_ribbon(data = filter(df_mi_pct_land, mistype == "Sprinkler"),
              aes(ymin=pct_ir_land-CI95delta, ymax=pct_ir_land+CI95delta), 
              fill = "gray60", color = "white") +
  geom_line(aes(color = mistype), size = 1) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkler"="royalblue1"),name="") +
  theme_light()+
  ggtitle("Share of installed system area by drip or sprinkler")+
  ylab("% Installed area")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif"))

#Fig.----

P + ylim(0,0.7) + scale_x_continuous(limits = c(1, 31), breaks = seq(1, 30, 1))
#
P + ylim(0,0.8) + xlim(1,100)
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
  filter(!is.na (RegistrationDate) ) %>% 
  select(regno,mistype, RegistrationDate,villageSI ,Latitude,Longitude) %>%
  group_by(villageSI) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>% ungroup() %>% 
  filter(Latitude>20,Latitude<25, Longitude>68.5,Longitude<74.5) %>% 
  #distance_from_1st_farmer
  group_by(villageSI) %>%
  mutate(distance = 
           haversine_distance(Latitude[1], Longitude[1], Latitude, Longitude) )%>% 
  ungroup() %>% 
  filter(distance<5)

df=
  dis %>% filter(rank_date_adopters!=1) %>% 
  group_by(mistype,rank_date_adopters)%>% 
  summarise(n=n(),
            Distance_Km=mean(distance,na.rm = T),
            SD=sd(distance,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% 
  ungroup()

df %>% 
  ggplot(aes(rank_date_adopters,Distance_Km, color = mistype )) +
  geom_line(size=1)

Pd=
  df %>%
  ggplot(aes(rank_date_adopters, Distance_Km)) +
  geom_ribbon(data = filter(df, mistype == "Drip"),
              aes(ymin=Distance_Km-CI95delta, ymax=Distance_Km+CI95delta), fill = "gray85", color = "white") +
  geom_ribbon(data = filter(df, mistype == "Sprinkler"),
              aes(ymin=Distance_Km-CI95delta, ymax=Distance_Km+CI95delta), fill = "gray80", color = "white") +
  geom_line(aes(color = mistype), size = 1) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkler"="royalblue1"),name="") +
  theme_light() +
  ggtitle("Farms' Distance from the 1st farmer")+ylab("Distance (in Km)")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))

#Fig.----
Pd+ xlim(2,30)+ylim(0,1.7)
#
Pd+ xlim(2,100)+ylim(0,2.1)
#

########  Caste           ###########################################################

gj1 %>% filter(!is.na(caste)) %>% 
  count(caste) %>%   
  mutate(prt = n / sum(n))

gj1 %>% filter(!is.na(caste)) %>% 
  count(caste,mistype) %>%   
  group_by(caste) %>% 
  mutate(prt = n / sum(n) )


dfC=
  gj1 %>%   
  select(regno, mistype, RegistrationDate,villageSI,caste) %>%
  filter(!is.na(RegistrationDate),!is.na(villageSI)) %>%
  group_by(mistype,villageSI) %>% arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  count(mistype,caste,rank_date_adopters) %>% 
  group_by(mistype,rank_date_adopters) %>% 
  mutate(pct_mistype_wise=n/sum(n)) %>% ungroup()

#Fig.----
dfC %>% 
  filter(rank_date_adopters<100) %>% 
  ggplot(aes(x=rank_date_adopters, y=pct_mistype_wise, fill=caste)) +
  geom_bar(stat="identity", width = 1)+
  facet_wrap(~mistype,  ncol=1, strip.position = "left")+

theme_light()+ scale_fill_manual(values=c("#5F6EA0",  "#5F9EA0" ),name="Social Status")+  
  ggtitle("Social Status HH / Ranked adopters by date")+
  ylab("% of rankings")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif"))


dfC %>% 
  filter(rank_date_adopters<30) %>% 
  ggplot(aes(x=rank_date_adopters, y=pct_mistype_wise, fill=caste)) +
  geom_bar(stat="identity")+
  facet_wrap(~mistype,  ncol=1, strip.position = "left")+
  
  theme_light()+ scale_fill_manual(values=c("#5F6EA0",  "#5F9EA0" ),name="Social Status")+  
  ggtitle("Social Status HH / Ranked adopters by date")+
  ylab("% of rankings")+xlab("Ranking of MIS adoption date ") +
  theme(plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif"))+ 
  scale_x_continuous(limits = c(1, 31), breaks = seq(1, 30, 1))

#subcaste
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




#########  Loan           ################################################################
#________BAR_______________________________
df_loan= 
  gj1 %>%   
  filter(!is.na (RegistrationDate) ) %>%
  select(regno, mistype, RegistrationDate,villageSI,Loan )%>%
  group_by(villageSI) %>% arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  count(rank_date_adopters,mistype,Loan) %>%
  
  group_by(rank_date_adopters) %>%  # Loaners % of rank_date_adopters
  mutate(pct1=n/sum(n)) %>% ungroup()%>%
  filter(Loan ==1)

#Fig.----
# loan barplot pct1
df_loan %>%  filter(rank_date_adopters<31) %>% 
  ggplot(aes(x=rank_date_adopters, y=pct1, fill=mistype)) +geom_bar(stat="identity" )+theme_light()+ scale_fill_manual(values=c( "blue4", "royalblue1"), name = "" )+ggtitle("Share of farms who took a loan")+ylab("% of rankings")+xlab("Ranking of MIS adoption date ") +theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))+scale_x_continuous(limits = c(0, 31), breaks = seq(1, 30, 1))

df_loan %>%  filter(rank_date_adopters<100) %>% 
  ggplot(aes(x=rank_date_adopters, y=pct1, fill=mistype)) +geom_bar(stat="identity",width = 1 )+theme_light()+ scale_fill_manual(values=c( "blue4", "royalblue1"), name = "" )+ggtitle("Share of farms who took a loan")+ylab("% of rankings")+xlab("Ranking of MIS adoption date ") +theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))



# % of Drip | % of Sprinkler____________________________________________________
df= 
  gj1 %>%   
  filter(!is.na (RegistrationDate) ) %>%
  select(regno, mistype, RegistrationDate,villageSI,Loan )%>%
  group_by(villageSI) %>% arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>%
  group_by(rank_date_adopters,mistype) %>% 
  summarise(n=n(),
            LoanPCT=mean(Loan,na.rm = T),
            SD=sd(Loan,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% 
  ungroup()
#

#Fig.----
# loan barplotc pct2
df %>%  filter(rank_date_adopters<30) %>% 
  ggplot(aes(x=rank_date_adopters, y=LoanPCT, fill=mistype)) +geom_bar(stat="identity", position=position_dodge())+theme_light()+ scale_fill_manual(values=c( "blue4", "royalblue1"), name = "" )+ggtitle("Share of farms who took a loan")+ylab("% of rankings")+xlab("Ranking of MIS adoption date ") +theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))

df %>%  filter(rank_date_adopters<100) %>% 
  ggplot(aes(x=rank_date_adopters, y=LoanPCT, fill=mistype)) +geom_bar(stat="identity", position=position_dodge()) +theme_light()+ scale_fill_manual(values=c( "blue4", "royalblue1"), name = "" )+ggtitle("Share of farms who took a loan")+ylab("% of rankings")+xlab("Ranking of MIS adoption date ") +theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))
#
#________LINE_______________________________

# loan lineplot
PLP=
  df %>%
  ggplot(aes(rank_date_adopters, LoanPCT)) +
  geom_ribbon(data = filter(df, mistype == "Drip"),
              aes(ymin=LoanPCT-CI95delta, ymax=LoanPCT+CI95delta), fill = "gray80", color = "white") +
  geom_ribbon(data = filter(df, mistype == "Sprinkler"),
              aes(ymin=LoanPCT-CI95delta, ymax=LoanPCT+CI95delta), fill = "gray85", color = "white") +
  geom_line(aes(color = mistype), size = 1) +scale_color_manual(values=c("Drip"="blue4", "Sprinkler"="royalblue1"),name="") +
  theme_light() +
  ggtitle("Farms' ")+  ylab("% of rankings")+xlab("Ranking of MIS adoption date ") +theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))

#Fig.----
PLP+xlim(1,30)+ylim(0,0.45)
#
PLP+xlim(1,100)+ylim(0,0.45)
#

#### PROB  Irrigation method    ########################################################

prb=
  gj1 %>% 
  select(regno, RegistrationDate,villageSI,mistype) %>%
  filter(mistype !="" ) %>% 
  group_by(villageSI) %>%
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  group_by(villageSI) %>%
  mutate(prob = 
           if_else(rank_date_adopters != 1 & mistype == first(mistype), 1, 0)) %>% 
  mutate(prob=ifelse(rank_date_adopters == 1,NA,prob))
  
df1=
  prb %>% 
  filter(rank_date_adopters!=1) %>% 
  group_by(rank_date_adopters) %>% 
  summarise(n=n(),
            Mean=mean(prob,na.rm = T),
            SD=sd(prob,na.rm = T),
            se = SD / sqrt(n),
            ciMult= qt(.975, n - 1),
            ci95 = se * ciMult
            ) 

Pim=
  df1 %>% filter(rank_date_adopters<100) %>%
  ggplot(aes(rank_date_adopters, Mean)) +
  geom_ribbon(aes(ymin=Mean-ci95, ymax=Mean+ci95), fill = "gray80", color = "white") +
  geom_line(size = 1, colour = "lightblue4") +  # Change color here
  theme_light() +
  ggtitle("Farmers' probability of owen same irrigation method (drip or sprikler) as the 1st farmer")+xlab("Ranking of MIS adoption date ") +ylab("Probability")+
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))

#Fig.----
Pim
#
Pim+xlim(0,30)
#

#### PROB  MIS supplier         ########################################################

prob_supplier_Drip=
#prob_supplier_Sprinkler=
  gj1 %>% 
 filter(mistype =="Drip") %>% 
#  filter(mistype=="Sprinkler") %>% 
  select(regno, RegistrationDate,villageSI,supplier) %>%
  group_by(villageSI) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number(),
         days_gap = c(NA, diff(RegistrationDate))) %>% 
  ungroup() %>% 
  group_by(villageSI) %>%
  mutate(prob = 
           if_else(rank_date_adopters != 1 & supplier == first(supplier), 1, 0)) %>% 
  mutate(prob=ifelse(rank_date_adopters == 1,NA,prob))%>% 
  group_by(rank_date_adopters) %>% 
  summarise(n=n(),
            IS_prob=mean(prob,na.rm = T),
            SD=sd(prob,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n)))
  filter(rank_date_adopters!=1)

# Combine the data frames and create a grouping variable
df <- 
  bind_rows(
  mutate(prob_supplier_Drip, mistype = "Drip"),
  mutate(prob_supplier_Sprinkler, mistype = "Sprinkler"))

PS=
  df %>%
  ggplot(aes(rank_date_adopters, IS_prob)) +
  geom_ribbon(data = filter(df, mistype == "Drip"),
              aes(ymin=IS_prob-CI95delta, ymax=IS_prob+CI95delta), fill = "gray80", color = "white") +
  geom_ribbon(data = filter(df, mistype == "Sprinkler"),
              aes(ymin=IS_prob-CI95delta, ymax=IS_prob+CI95delta), fill = "gray85", color = "white") +
  geom_line(aes(color = mistype), size = 1) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkler"="royalblue1"),name="") +
  theme_light() +
  ggtitle("Farmers' probability of holding the same MIS as the 1st farmer")+xlab("Ranking of MIS adoption date ") +ylab("Probability")+
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))

#Fig.----
PS+xlim(1,30)+ylim(0,0.45)
#
PS+xlim(1,100)+ylim(0,0.45)
#

#########  Crop                 ################################################################

prob_crop_Drip=
#prob_crop_Sprinkler=
  gj1 %>% 
  filter(mistype =="Drip") %>% # filter(mistype=="Sprinkler") %>% 
  select(regno, RegistrationDate,villageSI,Crop) %>%
  group_by(villageSI) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>% ungroup() %>% 
  filter(!Crop %in% c("","-","NULL" )) %>% 
  group_by(villageSI) %>%
  mutate(prob = if_else(rank_date_adopters != 1 & Crop == first(Crop), 1, 0)) %>% 
  mutate(prob=ifelse(rank_date_adopters == 1,NA,prob)
         ) %>% 
  group_by(rank_date_adopters) %>% 
  summarise(n=n(),
            crp_prob=mean(prob,na.rm = T),
            SD=sd(prob,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% 
filter(rank_date_adopters!=1)

# Combine the data frames and create a grouping variable
df <- bind_rows(
  mutate(prob_crop_Drip, mistype = "Drip"),
  mutate(prob_crop_Sprinkler, mistype = "Sprinkler"))

PC=
  df %>%
  ggplot(aes(rank_date_adopters, crp_prob)) +
  geom_ribbon(data = filter(df, mistype == "Drip"),
              aes(ymin=crp_prob-CI95delta, ymax=crp_prob+CI95delta), fill = "gray80", color = "white") +
  geom_ribbon(data = filter(df, mistype == "Sprinkler"),
              aes(ymin=crp_prob-CI95delta, ymax=crp_prob+CI95delta), fill = "gray85", color = "white") +
  geom_line(aes(color = mistype), size = 1) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkler"="royalblue1"),name="") +
  theme_light() +
  ggtitle("Farmers' probability cultivating the same crop as the 1st farmer")+xlab("Ranking of MIS adoption date ") +ylab("Probability")+
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))

#Fig.----
PC+xlim(1,30)+ylim(0,1)
#
PC+xlim(1,100)+ylim(0,1)
#


#Fig.----
combined_data %>%
  filter(rank_date_adopters<30) %>% 
  ggplot(aes(rank_date_adopters, prob_same_1st, color = type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Drip" = "blue4", "Sprinkle" = "royalblue1")) +
  theme_light() +
  ggtitle("Farmers' probability cultivating the same crop as the 1st farmer")+
  ylim(0,1)+  scale_x_continuous(breaks=seq(0,30,5))+
  xlab("Ranking of MIS adoption date ") +ylab("Probability")+
  theme(plot.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif"))






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
  select(regno, RegistrationDate, mistype, c_code01new, villageSI,misarea, TotalMISCost) %>%
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








