5/5/2024
library(readxl)
library(dplyr)
library(tidyr)

# clean data ----
# ____installation_______ dt2020_2024__________________________________________ ----
data_2024 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/comrt_me/data_for_research_paper.xlsx", sheet = "2024 (Jan and Feb)")
data_2023 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/comrt_me/data_for_research_paper.xlsx", sheet = "2023")
data_2022 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/comrt_me/data_for_research_paper.xlsx", sheet = "2022", col_types = c("text", "text", "text", "numeric", "date"))
data_2021 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/comrt_me/data_for_research_paper.xlsx", sheet = "2021")
data_2020 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/comrt_me/data_for_research_paper.xlsx", sheet = "2020")

dt_24 <- data_2024[rowSums(is.na(data_2024)) != ncol(data_2024),] %>%fill(community, electricity_before_comet)
dt_24=dt_24 %>% mutate(year="2024")

dt_23 <- data_2023[rowSums(is.na(data_2023)) != ncol(data_2023),] %>%fill(community, electricity_before_comet)
dt_23=dt_23 %>% mutate(year="2023")

dt_22 <- data_2022[rowSums(is.na(data_2022)) != ncol(data_2022),] %>%fill(community, electricity_before_comet)
dt_22=dt_22 %>% mutate(year="2022")

dt_21 <- data_2021[rowSums(is.na(data_2021)) != ncol(data_2021),] %>%fill(community, electricity_before_comet)
dt_21=dt_21 %>% mutate(year="2021")

dt_20 <- data_2020[rowSums(is.na(data_2020)) != ncol(data_2020),] %>%fill(community, electricity_before_comet)
dt_20=dt_20 %>% mutate(year="2020")

################# dt2020_2024 ##################-
dt2020_2024=rbind(dt_24,dt_23,dt_22,dt_21,dt_20)

rm(dt_24,dt_23,dt_22,dt_21,dt_20,data_2024,data_2023,data_2022,data_2021,data_2020)


################# cm_instl ----
dt2020_2024 %>%  # -----------------------------------------------------# 5,987 obs.
  dplyr::select(account_number) %>% distinct() %>% #--------------------# 1,916 obs. 
  filter(!is.na(account_number))  #-------------------------------------# 1,915 accounts

cm_instl  <-
  dt2020_2024 %>%  # ---------------------------------------------------# 5,987 obs.
  select(account_number,installation_date,electricity_before_comet) %>% 
  distinct() %>% #------------------------------------------------------# 1,919 obs. 
  filter(!is.na(account_number),!is.na(installation_date)) %>% # -------# 1,865  
  mutate(install_year=year(installation_date)) %>% 
  mutate(hh_number=as.character(account_number),
         installation_date=as.Date(installation_date),
         power_B4=ifelse(electricity_before_comet=="Yes",1,0)
         )
  #
  
  
# ___________ paymnt_12_24  ________________________________________________________________ ----
cometME_pay_12_24 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/comrt_me/cometME_pay_12_24.xlsx")
names(cometME_pay_12_24)

paymnt_12_24A= cometME_pay_12_24 %>% 
  rename(account_number = `Account No.`,
         meter_number =`Meter No.`,
         actual_amount=`Actual Amount`,
         receipt_number = `Receipt No.`,
         pay_method = `Pay Method`) 
# paymnt_12_24A %>% count(Branch)
# paymnt_12_24A %>% count(Arrear)
# paymnt_12_24A %>% count(Fee)
# paymnt_12_24A %>% count(pay_method)
# paymnt_12_24A %>% count(Type)
# paymnt_12_24A %>% count(Tariff )
# paymnt_12_24A %>% count(MPU )

library(lubridate)
library(hms)

paymnt_12_24A <- paymnt_12_24A %>%
  select(-c(Branch,Arrear,Fee,MPU,Tariff,pay_method,Type)) %>% 
  mutate(TotalAmt = as.numeric(TotalAmt),
         Amount  = as.numeric(Amount),      
         actual_amount = as.numeric(actual_amount),
         kWh = as.numeric(kWh)) %>% 
  mutate(Date = ymd_hms(Date),
         date = as.Date(Date),
         time = as_hms(format(Date, "%H:%M:%S")))
#
--------------------------------------------------------------------------------

paymnt_12_24A %>% dplyr::select(meter_number) %>% distinct()   # 1,970
paymnt_12_24A %>% dplyr::select(account_number) %>% distinct()  # 2,005

# בדיקה
paymntA=paymnt_12_24A %>% select(meter_number, account_number)
  paymntA$account_number[is.na(paymntA$account_number)] <- "NA"
  paymntA$meter_number[is.na(paymntA$meter_number)] <- "NA"
paymntA=paymntA %>% distinct() %>% 
  mutate(user_number=ifelse(meter_number == account_number,1,0))
  
# meter_number is the index coralets paymnt_12_24 and dt2020_2024_accnts
# 6 paymnt_12_24$account_number NAs have meter_number fits to dt2020_2024_accnts
# 33 paymnt_12_24$meter_number NAs need an examination:
paymnt_12_24_meter_number_NAs = paymntA %>% filter(meter_number=="NA") %>% 
  mutate(number=account_number)

dt2020_2024_accnts %>% mutate(number=as.character(meter_number)) %>% 
  select(number) %>% 
  inner_join(paymnt_12_24_meter_number_NAs)
## 0 overlap -  33 paymnt_12_24$meter_number doesnt exist in dt2020_2024_accnts
  
# pay1=
paymnt_12_24A %>% select(meter_number,date,time) %>% 
  count(meter_number,date)%>% filter(n>1) # A tibble: 3,417

# case 37232660086 37232652125       

# המשך  ----

# paymnt
paymnt_12_24B <- 
  paymnt_12_24A %>%  # ---------------------------------------------- 30,814
  select(meter_number,TotalAmt,kWh,date) %>% # distinct() %>%  # ---  27,345
  group_by(meter_number ,date) %>%
  slice(which.max(TotalAmt)) %>% ungroup() %>% # -------------------  26,992
  filter(TotalAmt>50,TotalAmt<200) # -------------------------------  20,543

last_day=as.Date(2024-11-21)

paymnt_12_24C <- 
    paymnt_12_24B %>%
    group_by(meter_number) %>%
    summarise(
      total_paymnt=n(),
      earliest_pymnt = min(date),
      latest_pymnt = max(date),
      total_amt=sum(TotalAmt),total_kWh=sum(kWh),
    ) %>%  #  1,724
  rename(hh_number=meter_number) %>% 
  inner_join(cm_instl[,-1]) %>% # add DF cm_instl
  
  mutate(
    # days_from_1th_pay=latest_pymnt-earliest_pymnt,
    # days_from_1th_pay=as.numeric(days_from_1th_pay), 
    # days_intsl_to_latestD=as.numeric(latest_pymnt-installation_date),
    days_intsl_to_lastD=as.numeric(as.Date("2024-02-25")-installation_date)) %>% filter(days_intsl_to_lastD>1) %>% 
    
  mutate(amt_per_yr=total_amt/(days_intsl_to_lastD/365), 
        paymntS_per_yr=total_paymnt/(days_intsl_to_lastD/365) )

# STAT ----     
cm_instl %>%
  count(install_year) %>% filter(n > 2) %>%
  ggplot(aes(x = install_year, y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = n), vjust = -0.5, size = 3, color = "black") +
  theme_test() +
  scale_x_continuous(breaks=seq(2012,2023,1))

paymnt_12_24C %>% count(electricity_before_comet) %>% mutate(pct=n/sum(n))

paymnt_12_24C %>% summarise(mean(days_intsl_to_lastD/365))
                            
paymnt_12_24C %>% summarise(mean(total_amt),mean(total_paymnt))

paymnt_12_24C %>% summarise(mean(amt_per_yr),mean(paymntS_per_yr))


  ggplot(paymnt_12_24C, aes(x = total_paymnt, y = total_amt)) +geom_point()
  
  ggplot(paymnt_12_24C, aes(x = amt_per_yr, y = paymntS_per_yr)) +geom_point()+ ylim(0,12)+xlim(15,1200)
  
cor(paymnt_12_24C$total_paymnt,paymnt_12_24C$total_amt)
cor(paymnt_12_24C$amt_per_yr,paymnt_12_24C$paymntS_per_yr)

library("ggpubr")
ggscatter(paymnt_12_24C, x = "total_paymnt", y = "total_amt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(paymnt_12_24C, x = "amt_per_yr", y = "paymntS_per_yr", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

paymnt_12_24C %>% group_by(power_B4) %>% 
  summarise(mean(amt_per_yr),mean(paymntS_per_yr) )



m1 <- lm(amt_per_yr ~ power_B4 , paymnt_12_24C)
summary(m1)
sjPlot::tab_model(m1, digits = 4, show.se = T)

m2 <- lm(paymntS_per_yr ~ power_B4 , paymnt_12_24C)
summary(m2)
sjPlot::tab_model(m2, digits = 4, show.se = T)










library(nasapower)

se_i <- get_power(
  community = "RE",
  lonlat = c(35.09, 31.53),
  dates = c("2012-01-15", "2024-02-25"),
  temporal_api = "daily",
  pars = c("CLRSKY_SFC_SW_DWN", "ALLSKY_SFC_SW_DWN"))


