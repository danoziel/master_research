#|1st 09/04/2024


library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(summarytools)
library(modelsummary )
datasummary_skim(EnergySource, type = "categorical")

EnergySource=  gj1$EnergySource %>% as.data.frame()

GGRC <- read_dta("C:/Users/Dan/OneDrive - mail.tau.ac.il/world_bank/GGRCDATA_20141219_a_1.dta")
names(GGRC)
                 

# organize the data ----

#| loaneenonloanee==Loan
#| 
#| 41246NA in many vars

# Time var: Year Or Date
GGRC %>% select(contains("year"),contains("date") )
                                
# we choose | year_Registration | RegistrationDate
  
IN <- GGRC %>% 
  select (
    regno,"TotalLand","FarmerType","Crop","supplier","RKVYArea","WaterSourceDetail",
    "farmercaste" ,
    "EnergySource","OutletSize", "ApplicationSource",misarea, mistype,Dark,TypeDark, 
    TribalTaluka, PartnerModel, Loan, RKVY, TrialRunDate, TPADate, DisCom, 
    ElecConNumStr, c_code01, c_code01new,MISTypeNum, merge_ugvcl, "year_Registration",
    "year_TrialRun","year_TPA" ,district,
    tribal, No_HH, TOT_P,  P_ST, P_LIT, TribalProp, LitProp, m_ccode11, `_merge`)

infr_vars= GGRC %>% 
  select (regno,PVCPipeLength,PlainLateralPipeLength,EmittingPipeLength,
          PumpHP,TotalHead,UniqueVillageComb,TriSubByTotal,ccode11)

geo_vars= GGRC %>% 
  select(regno, Longitude,Latitude,dupecon, circle,division, subdivision,
         village_ugvcl,farmervillage,taluka,district)

Rs_vars= GGRC %>%
  select(regno, TotalMISCost, EstimatedSubsidy,FarmerPayment,SupplierShare,
         SupplierShare, MISPartnerShare, GGRCShare, TribalSubsidy, GWRDCSubsidy, 
         RKVYSubsidy, RKVYSumpSubsidy,InStudy, EstSubByTotal,TriSubByTotal,TotalSubsidy,
         TotSubByTot,GGRCSubByTotal, SupplierSubByTotal, PartnerSubByTotal, RKVYSubByTotal)

OUT=GGRC %>% 
  select(farmername,FarmerContact,SurveyNo,WellSurveyNo,
         electricityconsumernumber, Village,RegistrationYear, 
         month,month_TrialRun,month_TPA,year )




# gj1 <- GGRC       ----

GGRC[,1] %>% distinct()

# df with vars tus_rent_income

freq(GGRC$FarmerType )
freq(GGRC$farmercaste )

gj11 <- GGRC %>% 
  select(regno,year_Registration, RegistrationDate,
         farmercaste,FarmerType,Loan,
         TotalLand,misarea,farmervillage,taluka,district,
         mistype, EnergySource, WaterSourceDetail,
         "supplier", "RKVY","Crop"
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
rm(gj11,gj_village1,gj_village)

gj1 # 483,702
gj1 %>% select(TotalLand) %>%filter(TotalLand==0)       # 1,300
gj1 %>% select(TotalLand) %>%  filter(TotalLand=="")      # 0
gj1 %>% select(TotalLand) %>%filter(is.na (TotalLand)) # 64,111
1300+64111 
483702 - 1300-64111
gj1 %>%filter(TotalLand>0)        # 418,291

rm(GGRC)
  #
--------------------------------------------------------------------------------
        # DESC                                      ----
--------------------------------------------------------------------------------
library(summarytools)
library(kableExtra)
library(dplyr)



############### year_Registration ###########  
freq(gj1$year_Registration  , report.nas=F, totals=F, cumul=F, headings=F)

yr <- gj1 %>% count(year_Registration) %>% 
  filter(year_Registration<2014)

ggplot(yr, aes(x = year_Registration, y = n)) +
  geom_line(color = "green4") +    # Add a line without points
  scale_x_continuous(breaks = seq(2005, 2013, 1)) +  # Ensure all x-axis values appear
  labs(title = "Number of Registrations over the Years", 
       x = "Registrations Year", 
       y = "Number of Registrations") +
  theme_minimal()

  

###########  TotalLand  ###########  
quantile(GGRC$TotalLand , 0.01, na.rm = TRUE)
quantile(GGRC$TotalLand , 0.99, na.rm = TRUE)

t1 <- 
  GGRC %>% select(TotalLand) %>% 
  filter(!is.na(TotalLand), between(TotalLand, 0.56, 13.88)) %>% 
  descr(stats = c("mean", "sd", "min", "max"),transpose = T) %>% as.data.frame()

###########  misarea    ###########  
quantile(GGRC$misarea , 0.01, na.rm = TRUE)
quantile(GGRC$misarea , 0.99, na.rm = TRUE)

t2 <- 
  GGRC %>% select(misarea) %>% filter(between(misarea, 0.39, 5.61)) %>% 
  descr(stats = c("mean", "sd", "min", "max"),transpose = T) %>% as.data.frame()

########### mi_pct_land ###########  
quantile(gj1$mi_pct_land , 0.01, na.rm = TRUE)
t3 <- 
  gj1 %>% select(mi_pct_land) %>% filter(between(mi_pct_land, 0.08, 1)) %>% 
  descr(stats = c("mean", "sd", "min", "max"),transpose = T) %>% as.data.frame()
  
########### TotalMISCost ############ ####
descr(GGRC$TotalMISCost,stats = c("mean", "sd", "min", "max"),transpose = T)
quantile(GGRC$TotalMISCost , 0.01, na.rm = TRUE)
quantile(GGRC$TotalMISCost , 0.99, na.rm = TRUE)

t4=
  GGRC %>%
  filter(!is.na(TotalMISCost), TotalMISCost < 497012) %>% 
  descr(TotalMISCost,stats = c("mean", "sd", "min", "max"),transpose = T)%>% 
  as.data.frame() %>% mutate_at(1:4,round)

GGRC %>%
  filter(!is.na(TotalMISCost), TotalMISCost < 497012) %>% 
  group_by(mistype) %>%
  summarise(descr(TotalMISCost,stats = c("mean", "sd", "min", "max"),transpose = T))


# % ########## FarmerType | farmer_category ###########  
freq_table_FarmerType=
  gj1 %>% filter(!is.na(farmer_category)) %>% count(farmer_category) %>% mutate(pct=n/sum(n)*100, pct=paste0(round(pct),"%"), n=format(n,big.mark=","))

# % ########## farmercaste | caste  ----------------------------
gj1 %>% filter(!is.na(caste)) %>% count(caste) %>% mutate(pct=n/sum(n)*100, pct=paste0(round(pct),"%"), n=format(n,big.mark=","))

freq_table_4cst= gj1 %>% filter(!is.na(farmercaste),farmercaste != "") %>% count(farmercaste) %>% mutate(pct=n/sum(n)) %>% rename(Variable=farmercaste)
freq_table_2cst= gj1 %>% filter(!is.na(caste),caste != "") %>% count(caste) %>% mutate(pct=n/sum(n))%>% rename(Variable=caste)
ft_cst= rbind(freq_table_2cst,freq_table_4cst)
ft_cst

# % ########## mistype  -----
freq(gj1$mistype)

freq_table_mistype <-
  gj1 %>% filter(mistype != "") %>% count(mistype) %>%mutate(pct=n/sum(n)) %>%
  mutate_at(3,round,2)%>% rename(Variable=mistype)
# % ########## WaterSourceDetail  -----
gj1 %>% filter(! WaterSourceDetail %in% c("", "NULL")) %>% count(WaterSourceDetail) %>%mutate(pct=n/sum(n))

ft_WaterSourceDetail <-
  gj1 %>% filter(!WaterSourceDetail %in% c("","NULL"))%>%count(WaterSourceDetail) %>%
  mutate(pct=n/sum(n)) %>% mutate_at(3,round,4)%>% rename(Variable=WaterSourceDetail)


gj1 %>% filter(! WaterSourceDetail %in% c("", "NULL")) %>% count(mistype,WaterSourceDetail) %>%mutate(pct=n/sum(n))

# % ########## supplier  ----
tf_supplier=
  gj1 %>% filter(supplier!="") %>% count(supplier) %>%
  mutate(pct=n/sum(n)) %>%filter(pct>0.06) %>% mutate_at(3,round,2) %>%
  arrange(desc(pct)) %>% rename(Variable=supplier)

# % ########## Crop ----
ft_crop=
  GGRC %>% filter(! Crop %in% c("", "NULL")) %>% count(Crop) %>%
  mutate(pct=n/sum(n)) %>%filter(pct>0.01) %>% mutate_at(3,round,2) %>% 
  arrange(desc(pct)) %>% rename(Variable=Crop)
  
# % ########## Loan  ----
ft_loan=
  GGRC %>% filter(!is.na(Loan)) %>% count(Loan) %>%
  mutate(pct=n/sum(n)) %>% mutate_at(3,round,3) %>% 
  filter(Loan==1) %>%rename(Variable=Loan)
ft_loan$Variable[ft_loan$Variable==1]="Took Loan"


        # DATA TABLE ### ###                        ####


library(kableExtra)
rbind(t1,t2,t3,t4)%>% 
  mutate_at(1:4,round,2) %>% 
  kable() %>% kable_paper()

freq_table_FarmerType

rbind(
  ft_cst,
  freq_table_mistype,
  ft_WaterSourceDetail,
  tf_supplier,
  ft_crop,
  ft_loan
)%>% 
  kable() %>% kable_paper()





library(DT)
# Render the frequency table as an interactive HTML table
datatable(freq_table, options = list(dom = 't', paging = FALSE, searching = FALSE))
datatable(freq_table)

--------------------------------------------------------------------------------
# ANALSYS #            ----
--------------------------------------------------------------------------------
###########  caste     -----
--------------------------------------------------------------------------------

mis_type=
  gj1 %>%select(regno,mistype) %>% filter(mistype != "")

    #__Fig___
# group_by(caste)
clr_caste <- c("#5F6EA0",  "#5F9EA0" )

gj1 %>% filter(!is.na(caste)) %>% 
  count(year_Registration,caste) %>%   
  group_by(caste) %>% 
  mutate(sum(n),prt = n / sum(n) * 100) %>% 
  ggplot(aes(year_Registration, prt, color = caste)) +
  geom_line(size=1.5)+theme_light() + 
  scale_x_continuous(breaks=seq(2005,2014,1))+
  scale_colour_manual(values=clr_caste)

CD=gj1 %>% filter(!is.na(caste)) %>% inner_join (mis_type) %>% filter(mistype=="Drip")
CS=gj1 %>% filter(!is.na(caste)) %>% inner_join (mis_type) %>% filter(mistype=="Sprinkler")
CD %>% count(year_Registration,caste) %>% group_by(caste) %>% mutate(sum(n),prt = n / sum(n) * 100) %>% ggplot(aes(year_Registration, prt, color = caste)) + geom_line(size=1)+theme_light() + scale_x_continuous(breaks=seq(2005,2014,1))+scale_colour_manual(values=clr_caste) +
  ggtitle("Drip-irrigated farms")
CS %>% count(year_Registration,caste) %>% group_by(caste) %>% mutate(sum(n),prt = n / sum(n) * 100) %>% ggplot(aes(year_Registration, prt, color = caste)) + geom_line(size=1)+theme_light() + scale_x_continuous(breaks=seq(2005,2014,1))+scale_colour_manual(values=clr_caste) +
  ggtitle("Sprinkler-irrigated farms")




--------------------------------------------------------------------------------
###########  Loan       ----
--------------------------------------------------------------------------------

freq(gj1$Loan)

loan_year=
  gj1 %>%filter(!is.na(Loan)) %>% count(year_Registration,Loan) %>%
  group_by(year_Registration) %>% 
  mutate(pct=n/sum(n)*100) %>% mutate_at(4,round) %>% filter(Loan==1) %>% 
  ungroup() %>% mutate(Loan="year_wise")

loan_farmers=
  gj1 %>%filter(!is.na(Loan)) %>% count(year_Registration,Loan) %>%
  group_by(Loan) %>% 
  mutate(pct=n/sum(n)*100) %>% mutate_at(4,round) %>% filter(Loan==1) %>% 
  ungroup() %>% mutate(Loan="HHs_wise")

loan=
  gj1 %>%filter(!is.na(Loan)) %>% count(year_Registration,Loan) %>%
  group_by(year_Registration) %>% 
  mutate(pct_year_wise=n/sum(n)*100) %>% ungroup(
  ) %>% 
  group_by(Loan) %>% 
  mutate(pct_hh_wise=n/sum(n)*100) %>%ungroup() %>%  
  mutate_at(4:5,round) %>% filter(Loan==1) %>% 
  select(year_Registration,pct_year_wise, pct_hh_wise
  ) %>% pivot_longer(
    !year_Registration, names_to = "wise", values_to = "pct")


clr_loan <- c("gold2",  "gold4" )
ggplot(loan,aes(x=year_Registration, y=pct, color = wise)) +
  geom_line(size=1)+
  ylim(0,70)+scale_x_continuous(breaks=seq(2005,2014,1))+
  scale_colour_manual(values=clr_loan)+
  ggtitle("% HH who took loan") +theme_bw()

# Loan BY MIS _______________________
mis_type=gj1 %>%select(regno,mistype) %>% filter(mistype != "")

# mistype=="Drip
LD=gj1 %>% filter(!is.na(Loan)) %>% inner_join (mis_type) %>% 
  filter(mistype=="Drip") %>% 
  count(year_Registration,Loan) %>%group_by(year_Registration) %>% mutate(pct_year_wise=n/sum(n)*100) %>% ungroup() %>% 
  group_by(Loan) %>% mutate(pct_hh_wise=n/sum(n)*100) %>%ungroup() %>% filter(Loan==1) %>% 
  select(year_Registration,pct_year_wise, pct_hh_wise) %>% pivot_longer(!year_Registration, names_to = "wise", values_to = "pct")
  
LD %>%
  ggplot(aes(year_Registration, pct, color = wise)) + geom_line(size=1)+theme_light()+scale_colour_manual(values=clr_loan) +
  ylim(0,70)+ #xlim(2005,2013)+ scale_x_continuous(breaks=seq(2005,2014,1))+
  ggtitle("Drip-irrigated farms")


# mistype=="Sprinkler
LS=
  gj1 %>% filter(!is.na(Loan)) %>% inner_join (mis_type) %>% 
  filter(mistype=="Sprinkler") %>% 
  count(year_Registration,Loan) %>%group_by(year_Registration) %>% mutate(pct_year_wise=n/sum(n)*100) %>% ungroup() %>% 
  group_by(Loan) %>% mutate(pct_hh_wise=n/sum(n)*100) %>%ungroup() %>%filter(Loan==1) %>% 
  select(year_Registration,pct_year_wise, pct_hh_wise) 

LS %>%
  # add_row(year_Registration = 2011, pct_year_wise = 0, pct_hh_wise = 0) %>%
  # add_row(year_Registration = 2012, pct_year_wise = 0, pct_hh_wise = 0) %>%
  # add_row(year_Registration = 2013, pct_year_wise = 0, pct_hh_wise = 0) %>% 
  pivot_longer(!year_Registration, names_to = "wise", values_to = "pct") %>% 
  ggplot(aes(year_Registration, pct, color = wise)) + geom_line(size=1)+theme_light()+scale_colour_manual(values=clr_loan) +
  ylim(0,70)+xlim(2005,2013)+ #scale_x_continuous(breaks=seq(2005,2014,1))+
  ggtitle("Sprinkler-irrigated farms")

--------------------------------------------------------------------------------






--------------------------------------------------------------------------------
###########  TotalLand  misarea                                                       ----
--------------------------------------------------------------------------------
summary_1_99(gj1$TotalLand)
summary_1_99(gj1$misarea)
summary_1_99(gj1$mi_pct_land)

land11=
  gj1 %>% select(regno, year_Registration,TotalLand, misarea, mi_pct_land ) %>% 
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56 ) %>% 
  group_by(year_Registration) %>% 
  summarise(
    total_land=mean(TotalLand),
    irrigated_land=mean(misarea),
    pct_irrigated_land=mean(mi_pct_land)*100) %>% 
  mutate_at(4,round) %>% 
  pivot_longer(!year_Registration, names_to = "land", values_to = "size") %>% 
  mutate(mistype="All farm")

#__Fig___Total land & Irrigated Land [In Ha]
colour3 <- c("royalblue1", "brown4")
land1 %>% filter(land != "pct_irrigated_land" ) %>% 
  ggplot(aes(year_Registration, size, color = land)) +
  geom_line(size=1.5)+theme_light() +
  scale_x_continuous(breaks=seq(2005,2014,1))+
  scale_colour_manual(values = colour3)

######## filter(mistype %in% c("Drip","Sprinkler"))
  
land2=
  gj1 %>% select(regno, year_Registration,TotalLand, misarea, mi_pct_land,mistype ) %>%
  filter(mistype %in% c("Drip","Sprinkler")) %>% 
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56 ) %>% 
  group_by(mistype,year_Registration) %>% 
  summarise(
    land_holding=mean(TotalLand),
    land_irrigated=mean(misarea),
    pct_irrigated_land=mean(mi_pct_land)*100) %>% mutate_at(5,round) %>% 
  ungroup() %>% 
  pivot_longer(!c(mistype,year_Registration), names_to = "land", values_to = "size") %>% 
  unite(land_mistype, land, mistype, sep = "_", remove = FALSE)

#__Fig___Total land & Irrigated Land [In Ha]
clr4 <- c("brown3","brown2" , "royalblue4" ,"royalblue1")
land2 %>% filter(land != "pct_irrigated_land" ) %>% 
  ggplot(aes(year_Registration, size, color = land_mistype)) +
  geom_line(size=1)  +
  theme_light() +
  scale_x_continuous(breaks = seq(2005, 2014, 1)) +
  scale_colour_manual(values = clr4) 



# share of irrigated land [In %]_______________________

#__Fig___share of irrigated land [In %] 
land1 %>% filter(land == "pct_irrigated_land" ) %>% 
  ggplot(aes(year_Registration,size)) +
  geom_line(color="royalblue4", size=1)+theme_light() +
  scale_x_continuous(breaks=seq(2005,2014,1))

#__Fig___share of irrigated land [In %] 
clr5 <- c("royalblue4"  ,"royalblue1")
land2 %>% filter(land == "pct_irrigated_land" ) %>% 
  ggplot(aes(year_Registration, size, color = land_mistype)) +
  geom_line(size=1)  +
  theme_light() +
  scale_x_continuous(breaks = seq(2005, 2014, 1)) +
  scale_colour_manual(values = clr5) 


# ______________________________________________________________________________
# first to adopt        ----
# ______________________________________________________________________________



# first to adopt + date_gap ----

date_rank=
  gj1 %>% 
  select(regno, RegistrationDate,villageSI )%>%
# right_join(gj_village1) %>% group_by(farmervillage) %>% 
  group_by(villageSI) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number(),
         days_gap = c(NA, diff(RegistrationDate))) %>% ungroup()

#__Fig___No. weeks between RegistrationDate farmers_______________________________________________
date_gap <- 
  date_rank %>% group_by(rank_date_adopters) %>% filter(rank_date_adopters!=1) %>%
  summarise(days_gap=mean(days_gap)) %>%
  mutate(week_gap=days_gap/7) %>% 
  mutate(accumulated_week_gap = cumsum(week_gap))

date_gap %>% ggplot(aes(rank_date_adopters,week_gap)) +
  geom_line(color="#69b3a2", size=1)+theme_light()+
  xlim(2,30)# +
scale_x_continuous(breaks=seq(2,20,2))


# accumulated_week_gap ----
#__Fig_________________________________________________
library(patchwork) # To display 2 charts together
library(hrbrthemes)

# A few constants
coeff <- 5
weeks <- "#69b3a2"
acc_weeks <- rgb(0.2, 0.6, 0.9, 1)


date_gap %>% filter(rank_date_adopters<30) %>%  
  ggplot( aes(x=rank_date_adopters)) +
  geom_line( aes(y=week_gap), size=1, color=weeks) + 
  geom_line( aes(y=accumulated_week_gap / coeff), size=1, color=acc_weeks) +
  scale_y_continuous(
    name = "Weeks Gap",
    sec.axis = sec_axis(~.*coeff, name="Accumulated Weeks Gap")
  )  + 
  theme_ipsum() +
  theme(axis.title.y = element_text(color = weeks, size=13),
        axis.title.y.right = element_text(color = acc_weeks, size=13))+
  ggtitle("Total weeks between registration date of first 20 farmers")
scale_x_continuous(breaks=seq(2,30,2))



--------------------------------------------------------------------------------


# first to adopt + TotalLand ----

date_rank1

date_rank1=
  gj1 %>% 
  select(regno, RegistrationDate,farmervillage,TotalLand) %>%
#  right_join(gj_village1) %>% group_by(farmervillage) %>% 
  right_join(gj_village) %>% group_by(villageSI) %>%
  arrange(RegistrationDate) %>% 
  mutate(rank_date_adopters = row_number(),
         days_gap = c(NA, diff(RegistrationDate))) %>% 
  ungroup() %>% 
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56)

#__Fig__
date_rank1 %>% group_by(rank_date_adopters)%>% 
  summarise(mean_land_ha=mean(TotalLand,na.rm = T))%>%
  ggplot(aes(rank_date_adopters,mean_land_ha)) +
  geom_line(color="#69b3a2", size=1.5)+theme_light() +
  scale_x_continuous(breaks=seq(1,20,2))

# date_rank11=date_rank1 %>% group_by(rank_date_adopters)%>% summarise(mean_land_ha=mean(TotalLand,na.rm = T))
# plot_line_1color(date_rank11, x_var = "rank_date_adopters", y_var = "mean_land_ha")

#__Fig__with CI 
df_date_rank1=date_rank1 %>% group_by(rank_date_adopters)%>% 
  summarise(n=n(),
            mean_land_ha=mean(TotalLand,na.rm = T),
            SD=sd(TotalLand,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))
  )
df_date_rank1
ggplot(df_date_rank1,aes(rank_date_adopters, mean_land_ha)) + 
  geom_ribbon(aes(ymin = mean_land_ha - CI95delta, ymax = mean_land_ha + CI95delta),    # shadowing cnf intervals
              fill = "grey90") + 
  geom_line(color = "#69b3a2",size = 1)+theme_light() +
  scale_x_continuous(breaks=seq(2,20,2)) + ylim(3,3.6)




# first to adopt + misarea  ----

summary_1_99(gj1$misarea)

rank_misarea=
  gj1 %>% 
  select(regno, RegistrationDate,farmervillage,TotalLand,misarea) %>%
  right_join(gj_village[,1]) %>% 
  group_by(farmervillage) %>% 
  arrange(RegistrationDate) %>% 
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56,
         rank_date_adopters<21)
  
#__Fig__
rank_misarea %>% group_by(rank_date_adopters)%>% 
  summarise(irrigated_land_ha=mean(misarea)
            )%>%
  ggplot(aes(rank_date_adopters,irrigated_land_ha)) +
  geom_line(color="#69c3a2", size=1.5)+theme_light() +
  scale_x_continuous(breaks=seq(2,20,2))

#__Fig__with CI 
df_rank_misarea=rank_misarea %>% group_by(rank_date_adopters)%>% 
  summarise(n=n(),
            irrigated_land_ha=mean(misarea,na.rm = T),
            SD=sd(misarea,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))
  )
df_rank_misarea
ggplot(df_rank_misarea,aes(rank_date_adopters, irrigated_land_ha)) + 
  geom_ribbon(aes(ymin = irrigated_land_ha - CI95delta, 
                  ymax = irrigated_land_ha + CI95delta),    # shadowing cnf intervals
              fill = "grey90") + 
  geom_line(color = "#69b3a2",size = 1)+ theme_light() +
  scale_x_continuous(breaks=seq(2,20,2))+ ylim(1.4,1.65)


#### mistype=="Drip"
rank_misarea_D=
  gj1 %>% filter(mistype=="Drip" ) %>% 
  select(regno, RegistrationDate,villageSI,TotalLand,misarea) %>%
  group_by(villageSI) %>% 
  arrange(RegistrationDate) %>% 
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56,rank_date_adopters<21
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
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56,rank_date_adopters<21
  ) %>% 
  group_by(rank_date_adopters)%>% 
  summarise(n=n(),
            irrigated_land_ha=mean(misarea,na.rm = T),
            SD=sd(misarea,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n)))


# Combine the data frames and create a grouping variable
combined_data_rank_misarea <- bind_rows(
  mutate(rank_misarea_D, type = "Drip"),
  mutate(rank_misarea_S, type = "Sprinkle")
)


# Plot the combined data
combined_data_rank_misarea %>%
  ggplot(aes(rank_date_adopters, irrigated_land_ha, color = type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Drip" = "blue4", "Sprinkle" = "royalblue1")) +
  theme_light() +
  scale_x_continuous(breaks = seq(2, 20, 2))+
  ggtitle("irrigated land (Ha)| Drip or Sprinkler-irrigated farms")

combined_data_rank_misarea %>%
  ggplot(aes(rank_date_adopters, irrigated_land_ha)) +
  geom_ribbon(data = filter(combined_data_rank_misarea, type == "Drip"),
      aes(ymin=irrigated_land_ha-CI95delta, ymax=irrigated_land_ha+CI95delta), 
      fill = "gray90", color = "white") +
  geom_ribbon(data = filter(combined_data_rank_misarea, type == "Sprinkle"),
      aes(ymin=irrigated_land_ha-CI95delta, ymax=irrigated_land_ha+CI95delta), 
      fill = "gray90", color = "white") +
  geom_line(aes(color = type), size = 1) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkle"="royalblue1")) +
  theme_light() +
  scale_x_continuous(breaks = seq(2, 20, 2)) +
  ggtitle("Irrigated Land (Ha) | Drip or Sprinkler-irrigated farms")+ 
  ylim(1.2,1.7)



########### supplier date_rank freq & prob ----
# ______________________________________________________________________________
freq(GGRC$supplier)
gj1 %>%filter(supplier != "") %>% count(year_Registration,supplier) %>%
  group_by(year_Registration) %>% 
  mutate(pct=n/sum(n)*100) %>% top_n(3,pct) %>% mutate_at(4,round) %>% 
  ggplot(aes(x=year_Registration, y=pct, group=supplier, fill=supplier)) +
  geom_bar(stat = "identity")+ theme_bw()+
  scale_x_continuous(breaks=seq(2005,2014,1))+
  scale_fill_manual(values=
                      c("gray50", "gray60", "gray70","gray80", 
                        "green4", "blue4","red3") )
# prob supplier
gj1 %>% count(supplier)
gj1 %>%filter(supplier != "") %>% count(supplier)

prob_supplier=
  gj1 %>% 
  filter()
  select(regno, RegistrationDate,farmervillage,supplier) %>%
  right_join(gj_village[,1]) %>% # RegistrationDate NAs removed also
  group_by(farmervillage) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number(),
         days_gap = c(NA, diff(RegistrationDate))) %>% 
  ungroup() %>% 
  filter(rank_date_adopters<21) %>% 
  group_by(farmervillage) %>%
  mutate(prob = 
           if_else(rank_date_adopters != 1 & supplier == first(supplier), 1, 0)) %>% 
  mutate(prob=ifelse(rank_date_adopters == 1,NA,prob))

prob_splr <- 
  prob_supplier %>% group_by(rank_date_adopters) %>% 
  summarise(prob_same_1st=mean(prob)) %>% 
  filter(rank_date_adopters!=1)
  
prob_splr %>% ggplot(aes(rank_date_adopters,prob_same_1st)) +
  geom_line(color="#69b3a2", size=1.5)+theme_light()  +
  scale_x_continuous(breaks=seq(2,20,2))+
  ggtitle( "The probability that farmers purchased same company as the 1st farmer
")
  

####### prob supplier MIS
# prob_supplier_Drip=
 prob_supplier_Sprinkler=
  gj1 %>% 
    # filter(mistype=="Drip") %>% 
     filter(mistype=="Sprinkler") %>% 
  select(regno, RegistrationDate,farmervillage,supplier) %>%
  right_join(gj_village[,1]) %>% # RegistrationDate NAs removed also
  group_by(farmervillage) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number(),
         days_gap = c(NA, diff(RegistrationDate))) %>% 
  ungroup() %>% 
  filter(rank_date_adopters<21) %>% 
  group_by(farmervillage) %>%
  mutate(prob = 
           if_else(rank_date_adopters != 1 & supplier == first(supplier), 1, 0)) %>% 
  mutate(prob=ifelse(rank_date_adopters == 1,NA,prob))%>% 
  group_by(rank_date_adopters) %>% 
  summarise(prob_same_1st=mean(prob)) %>% 
  filter(rank_date_adopters!=1)

  
  # Combine the data frames and create a grouping variable
  combined_data_supplier <- bind_rows(
    mutate(prob_supplier_Drip, type = "Drip"),
    mutate(prob_supplier_Sprinkler, type = "Sprinkle")
  )
  
  # Plot the combined data
  combined_data_supplier %>%
    ggplot(aes(rank_date_adopters, prob_same_1st, color = type)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("Drip" = "blue4", "Sprinkle" = "royalblue1")) +
    theme_light() +
    scale_x_continuous(breaks = seq(2, 20, 2))+
    ggtitle("purchased MIS company with Drip or Sprinkler-irrigated farms")
--------------------------------------------------------------------------------
# prob crop         ----
--------------------------------------------------------------------------------
gj1 %>% count(Crop)
gj1 %>%filter(!Crop %in% c("","-","NULL" ))

  
#prob_crop_Drip=
prob_crop_Sprinkler=
# prob_crop=
  gj1 %>% 
#  filter(mistype=="Drip") %>% 
  filter(mistype=="Sprinkler") %>% 
  select(regno, RegistrationDate,farmervillage,Crop) %>%
  right_join(gj_village[,1]) %>% # RegistrationDate NAs removed also
  group_by(farmervillage) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number(),
         days_gap = c(NA, diff(RegistrationDate))) %>% 
  ungroup() %>% 
  filter(rank_date_adopters<21) %>% filter(!Crop %in% c("","-","NULL" )) %>% 
  group_by(farmervillage) %>%
  mutate(prob = 
           if_else(rank_date_adopters != 1 & Crop == first(Crop), 1, 0)) %>% 
  mutate(prob=ifelse(rank_date_adopters == 1,NA,prob))

#prob_crp_Drip <- prob_crop_Drip %>% 
 prob_crp_Sprinkle <- prob_crop_Sprinkler %>% 
# prob_crp <- prob_crop %>% 
  group_by(rank_date_adopters) %>% 
  summarise(prob_same_1st=mean(prob)) %>% 
  filter(rank_date_adopters!=1)

prob_crp %>% ggplot(aes(rank_date_adopters,prob_same_1st)) +
  geom_line(color="#69b3a2", size=1.5)+theme_light()  +
  scale_x_continuous(breaks=seq(2,20,2))+
  ggtitle("The probability farmers cultivate same crop as the 1st farmer")

prob_crp_Drip %>% ggplot(aes(rank_date_adopters,prob_same_1st)) +
  geom_line(color="blue4", size=1)+theme_light()  +
  scale_x_continuous(breaks=seq(2,20,2))+
  ggtitle("Drip-irrigated farms")

prob_crp_Sprinkler %>% ggplot(aes(rank_date_adopters,prob_same_1st)) +
  geom_line(color= "royalblue1", size=1)+theme_light()  +
  scale_x_continuous(breaks=seq(2,20,2))+
  ggtitle("Sprinkler-irrigated farms")


# Combine the data frames and create a grouping variable
combined_data <- bind_rows(
  mutate(prob_crp_Drip, type = "Drip"),
  mutate(prob_crp_Sprinkle, type = "Sprinkle")
)

# Plot the combined data
combined_data %>%
  ggplot(aes(rank_date_adopters, prob_same_1st, color = type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Drip" = "blue4", "Sprinkle" = "royalblue1")) +
  theme_light() +
  scale_x_continuous(breaks = seq(2, 20, 2))+
  ggtitle("crop cultivation with Drip or Sprinkler-irrigated farms")

######## distance  -----
--------------------------------------------------------------------------------

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
  GGRC %>% 
  select(regno, RegistrationDate,farmervillage,Latitude,Longitude) %>%
  right_join(gj_village[,1]) %>% 
  group_by(farmervillage) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number()) %>% 
  ungroup() %>% 
  filter(rank_date_adopters<21) %>% 
  filter(Latitude>20,Latitude<25, Longitude>68.5,Longitude<74.5)
  
# mutate(prob = 
#          if_else(rank_date_adopters != 1 & supplier == first(supplier), 1, 0)) %>% 
#   mutate(prob=ifelse(rank_date_adopters == 1,NA,prob))


# Distance from each farmer to the first farmer in rank_date_adopters ----
dis1 <- dis %>% #distance_from_1st_farmer
  group_by(farmervillage) %>%
  mutate(distance = 
      haversine_distance(Latitude[1], Longitude[1], Latitude, Longitude) )%>% 
  ungroup() %>% 
  filter(distance<20.821)

dis11=dis1 %>% filter(rank_date_adopters!=1) %>% 
  group_by(rank_date_adopters)%>% 
  summarise(Distance_Km=mean(distance,na.rm = T)) %>% 
  ungroup()

dis11 %>% ggplot(aes(rank_date_adopters,Distance_Km)) +
  geom_line(color="#99a3f5", size=1.5)+theme_light()  +
  scale_x_continuous(breaks=seq(2,20,2))+
  ggtitle("Distance from the 1st farmer")

# mistype Drip Sprinkler______________________
mis_type = GGRC %>% select(regno,mistype) %>% filter(mistype != "")

dis1_mis_type=dis1 %>% filter(rank_date_adopters!=1) %>% 
  left_join(mis_type) %>% 
  group_by(mistype ,rank_date_adopters)%>% 
  summarise(Distance_Km=mean(distance,na.rm = T)) %>% 
  ungroup()

clr <- c("royalblue4"  ,"royalblue1")
dis1_mis_type %>% 
  ggplot(aes(rank_date_adopters,Distance_Km, color = mistype )) +
  geom_line(size=1)  +
  theme_light() +
  scale_x_continuous(breaks = seq(2, 20, 2)) +
  scale_colour_manual(values = clr) +
  ggtitle("Distance from the 1st farmer| drip and sprinkler irrigation")


# Distance from farmer to farmer B4 him in rank_date_adopters ----
dis2 <- dis %>%
  group_by(farmervillage) %>%
  mutate(distance = 
           c(0, haversine_distance(Latitude[-n()], Longitude[-n()], 
                                   Latitude[-1], Longitude[-1]))
  ) %>%ungroup()%>% 
  filter(distance<20821)

dis22=dis2 %>% filter(rank_date_adopters!=1) %>% 
  group_by(rank_date_adopters)%>% 
  summarise(Distance_Km=mean(distance,na.rm = T)) %>% 
ungroup()

dis22 %>% ggplot(aes(rank_date_adopters,Distance_Km)) +
  geom_line(color="#99a3f5", size=1.5)+theme_light()  +
  scale_x_continuous(breaks=seq(2,20,2))+
  ggtitle("Distance from farmer to farmer registered before him")

    
    
    
  

# ___________OLD_STUFF___________ ----

#  FarmerType   ----

  #__Fig___group_by(FarmerType)
  clr_farmer_category <- c("#5F6EA0",  "#5F9EA0" )
gj1 %>% filter(!is.na(farmer_category)) %>% 
  #  filter(mistype=="Drip") %>% 
  #  filter(mistype=="Sprinkler") %>% 
  count(year_Registration,farmer_category) %>%
  group_by(farmer_category) %>% 
  mutate(sum(n),prt = n / sum(n) * 100) %>% 
  ggplot(aes(year_Registration, prt, color = farmer_category)) +
  geom_line(size=1)+theme_light() + scale_x_continuous(breaks=seq(2005,2014,1))+
  scale_colour_manual(values=clr_farmer_category)


#__Fig___group_by(year_Registration)
# gj1 %>% filter(!is.na(farmer_category)) %>% 
#   count(year_Registration,FarmerType) %>%   
#   group_by(year_Registration) %>% 
#   mutate(sum(n),prt = n / sum(n) * 100) %>%
#     ggplot(aes(year_Registration, prt, color = FarmerType)) +
#     geom_line(size=1)+theme_light() + scale_x_continuous(breaks=seq(2005,2014,1))+ ylim(0, 40)


#  RKVY         ----

  freq(GGRC$RKVY)
rk1=gj1  %>% count(year_Registration,RKVY) %>%
  group_by(year_Registration) %>% 
  mutate(pct=n/sum(n)*100) %>% mutate_at(4,round,2) %>% filter(RKVY>0) %>%
  ungroup() %>% mutate(RKVY="year_wise")

rk2=gj1  %>% count(year_Registration,RKVY) %>%
  group_by(RKVY) %>% 
  mutate(pct=n/sum(n)*100) %>% mutate_at(4,round,2) %>% filter(RKVY>0) %>%
  ungroup() %>% mutate(RKVY="HHs_wise")

clr_loan <- c("gold2",  "gold4" )
rbind(rk1,rk2) %>% 
  ggplot(aes(x=year_Registration, y=pct, color = RKVY)) +
  geom_line(size=1)+
  scale_x_continuous(breaks=seq(2005,2014,1))+
  scale_colour_manual(values=clr_loan)+
  ggtitle("% HH ") +theme_bw()+
  coord_cartesian(xlim = c(2005,2014))

#_______________________________________________________________________________


# EnergySource 
freq(gj1$EnergySource  , report.nas=F, totals=F, cumul=F, headings=F)
datasummary_skim(EnergySource, type = "categorical")
EnergySource=  gj1$EnergySource %>% as.data.frame()
  
colour <- c( "#E1B378" ,"#5F9EA0")

gj1 %>% filter(EnergySource %in% c("Electric","Diesel")) %>% 
  count(EnergySource, year_Registration) %>%   
  group_by(EnergySource) %>% 
  mutate(sum(n) ,prt = n / sum(n) * 100) %>% 
  ggplot(aes(year_Registration, prt, color = EnergySource)) +
  geom_line(size=1)+theme_light() + scale_x_continuous(breaks=seq(2005,2014,1))+ ylim(0, 40) + 
  scale_colour_manual(values=colour)

# mistype
freq(gj1$mistype  , report.nas=F, totals=F, cumul=F, headings=F)

colour <- c( "blue4" ,"royalblue1")
gj1 %>% filter(mistype %in% c("Drip","Sprinkler")) %>% 
  count(mistype, year_Registration) %>%   
  group_by(mistype) %>% 
  mutate(sum(n) ,prt = n / sum(n) * 100) %>% 
  ggplot(aes(year_Registration, prt, color = mistype)) +
  geom_line(size=1)+theme_light() + scale_x_continuous(breaks=seq(2005,2014,1))+ ylim(0, 40) + 
  scale_colour_manual(values=colour)

# WaterSourceDetail
freq(gj1$WaterSourceDetail  , report.nas=F, totals=F, cumul=F, headings=F)

gj1 %>% filter(!WaterSourceDetail %in% c("","NULL")) %>% 
  count(WaterSourceDetail, year_Registration) %>%   
  group_by(WaterSourceDetail) %>% 
  mutate(sum(n) ,prt = n / sum(n) * 100) %>% 
  ggplot(aes(year_Registration, prt, color = WaterSourceDetail)) +
  geom_line(size=1)+theme_light() + scale_x_continuous(breaks=seq(2005,2014,1))+ ylim(0, 40)

# DisCom
freq(gj1$DisCom  , report.nas=F, totals=F, cumul=F, headings=F)

gj1 %>% filter(!DisCom %in% c("") ) %>% 
  count(DisCom, year_Registration) %>%   
  group_by(DisCom) %>% 
  mutate(sum(n) ,prt = n / sum(n) * 100) %>% 
  ggplot(aes(year_Registration, prt, color = DisCom)) +
  geom_line(size=1)+theme_light() + scale_x_continuous(breaks=seq(2005,2013,1))+ ylim(0, 40)

# district
freq(gj1$district  , report.nas=F, totals=F, cumul=F, headings=F)

gj1 %>% filter(!district %in% c("")) %>% 
  count(district, year_Registration) %>%   
  group_by(district) %>% 
  mutate(sum(n) ,prt = n / sum(n) * 100) %>% 
  ggplot(aes(year_Registration, prt, color = district)) +
  geom_line(size=0.1)+theme_light() + scale_x_continuous(breaks=seq(2005,2014,1))+ ylim(0, 50)

# map ----
mapGJ= GGRC %>% 
  select(regno,Latitude,Longitude,TotalLand,misarea,year_Registration,
         division,village_ugvcl,farmervillage,taluka,district) %>% 
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56 )%>% 
  filter( !is.na(misarea),Latitude>10, Longitude>20 ) %>% # lon<75
  rename(long=Longitude ,lat=Latitude,group=year_Registration )


library(ggrepel)
ggplot() +
  geom_polygon(data =mapGJ, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data =mapGJ, aes(x=long, y=lat, alpha=misarea)) +
  
  geom_text_repel( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5) +
  geom_point( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
  theme_void() + ylim(50,59) + coord_map() +
  theme(legend.position="none")


ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), 
               fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, alpha=pop)) +
  geom_text_repel( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5) +
  geom_point( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
  theme_void() + ylim(50,59) + coord_map() +
  theme(legend.position="none")




# essantials ----
summary_1_99 <- function(x) {
  c(
    Count = sum(!is.na(x)),
    Mean = format(mean(x, na.rm = TRUE), digits = 6),
    Median = format(median(x, na.rm = TRUE), digits = 6),
    p = format(quantile(x, 0.01, na.rm = TRUE), digits = 6),
    P = format(quantile(x, 0.99, na.rm = TRUE), digits = 6),
    Min = format(min(x, na.rm = TRUE), digits = 6),
    Max = format(max(x, na.rm = TRUE), digits = 6)
  )
}
compute_summary <- function(x) {
  c(
    Count = sum(!is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    p = quantile(x, 0.01, na.rm = TRUE),
    P = quantile(x, 0.99, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
}





# Function to create plot
plot_line_1color <- function(data, x_var, y_var) {
  ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_line(color = "#69b3a2", size = 1.5) +
    labs(x = x_var, y = y_var, title = paste("The", y_var, "by", x_var)) +
    theme_light()+
    scale_x_continuous(breaks=seq(2,20,2))
}

# Generate sample data
set.seed(123) # for reproducibility
data <- tibble(x = 1:10,y = rnorm(10))

plot1 <- create_plot(data, x_var = "x", y_var = "y")
plot1

plot_line_1color(df, x_var = "rank_date_adopters", y_var = "mean_land_ha")





write.csv(gj1, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/world_bank/gj1.csv", row.names=FALSE)

















