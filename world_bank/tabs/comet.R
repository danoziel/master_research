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


################# dt2020_2024_accnts  
dt2020_2024_accnts= # -------------------------------------------   # 5,987 obs.
  dt2020_2024 %>% dplyr::select(
  community, electricity_before_comet, account_number) %>% 
  distinct()   #  ---------------------------------------  # 1,916 obs. accounts

dt2020_2024_accnts[,3] %>% count(account_number) %>% arrange(desc( n ))# 1,916
dt2020_2024_accnts[,3] %>% distinct() # A tibble: 1,916

dt2020_2024_accnts$meter_number <- dt2020_2024_accnts$account_number

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

paymnt_12_24B <- 
  paymnt_12_24A %>%  # ------------------------------------------------- 30,814
  select(-c(SN,receipt_number,Date)) %>% distinct() %>%  # ------------- 30,797
  
  
  filter(TotalAmt !=0)  # ---------------------------------------------- 27,385  





paymnt_12_24B <- 
  paymnt_12_24A %>%  # ------------------------------------------------- 30,814
  select(-c(SN,receipt_number,Date,time)) %>% distinct() %>%  # ------- 27,390
  filter(TotalAmt !=0)  # --------------------------------------------- 27,385  

# pay2=
paymnt_12_24B %>% select(meter_number,date) %>% 
  count(meter_number,date) %>% filter(n>1) # A tibble: 326

paymnt_12_24C <- 
  paymnt_12_24B %>% 


library(lubridate)
paymnt_12_24[,-1] %>% distinct() %>% mutate(date=as.Date(Date)) %>% 
  mutate(year = year(date))

cm_date %>% group_by(year) %>% count()
cm_date %>% count(account_number,year)








library(nasapower)

se_i <- get_power(
  community = "RE",
  lonlat = c(35.09, 31.53),
  dates = c("2012-01-15", "2024-02-25"),
  temporal_api = "daily",
  pars = c("CLRSKY_SFC_SW_DWN", "ALLSKY_SFC_SW_DWN"))


