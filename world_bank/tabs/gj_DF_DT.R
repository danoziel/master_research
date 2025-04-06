#|1st 09/04/2024

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(summarytools)
library(modelsummary )

GGRC <- read_dta("C:/Users/Dan/OneDrive - mail.tau.ac.il/world_bank/GGRCDATA_20141219_a_1.dta")
names(GGRC)

freq(GGRC$RegistrationYear)


#filtering empty rows
GGRC_filtered <- 
  GGRC %>%             #  483,702
  filter(`_merge`!= 2) #  442,456

#### DFs   ############################################################### ####    

# Gujarat villages list and frequency [gj_village] .........................----
#  16,194 villages

gj_village=
  GGRC_filtered %>% select(farmervillage,taluka,district) %>% 
  count(farmervillage,taluka,district) %>%  # 16,194 obs.
  mutate(villageSI = paste0("V", 100001+row_number()-1)) 

write.csv(gj_village, "C:/Users/Dan/OneDrive - mail.tau.ac.il/gj_village.csv", 
          row.names = FALSE)

#
#  Study's relevant varible ................................................----

names(GGRC)

#|⬛️ #dont know what it is
# GGRC %>% select(No_HH,m_ccode11)

#|⬛️ #SAME VALUES
# GGRC %>% select("year_Registration", RegistrationYear,year)
# GGRC %>% select("c_code01new")
# "Loan" == loaneenonloanee

#|⬛️ #empty cells
# GGRC %>% select (division,subdivision)

# [gj01] ...................................................................----
gj01=  
  GGRC_filtered %>%  #: 442,456
  select(regno,
         mistype,
         farmervillage, taluka , district,
         RegistrationDate , year_Registration,
         misarea,
         farmercaste,
         Loan,
         supplier,
         Crop,
         
         FarmerType ,          #: 442,434
         TotalLand,            #: 419,591
         Longitude , Latitude, #: 243,098
  ) 

# vars-locations, Machine learning AFEKA Collage  ..........................----
micro_irrigation_locations_gujarat=  
  GGRC_filtered %>%  #: 442,456
  select(regno,
         RegistrationDate,
         Longitude , Latitude, #: 243,098
         # mistype,
         # misarea,
         # TotalLand,           
  ) %>% filter(!is.na(Longitude)) %>% 
  rename(
    plot_uid =regno,
    irrigation_date =RegistrationDate ,
    # irrigation_type = mistype ,
    # area_installed_irrigation= misarea,
    # area_cultivated= TotalLand,
    longitude = Longitude , latitude = Latitude,
         )

# Save the data frame as a CSV file
write.csv(micro_irrigation_locations_gujarat, "micro_irrigation_locations_gujarat.csv", row.names = FALSE)
write_xlsx(micro_irrigation_locations_gujarat, "micro_irrigation_locations_gujarat.xlsx")
write_json(micro_irrigation_locations_gujarat, "micro_irrigation_locations_gujarat.json")
# Get the current working directory
getwd()
# Set the working directory to a specific path (replace with your desired path)
setwd("C:/Users/Dan/OneDrive - mail.tau.ac.il/")




# vars just for descriptiv statistics  .....................................----
GGRC_filtered %>% 
  select(regno,TotalMISCost,WaterSourceDetail, EnergySource)

# OUT vars of the data .................................................... ----

OUT <- GGRC_filtered %>% 
  select (
    "RKVYArea","OutletSize", "ApplicationSource", 
    TribalTaluka, PartnerModel, RKVY, TrialRunDate, TPADate, DisCom, 
    ElecConNumStr, c_code01 ,MISTypeNum, merge_ugvcl,
    "year_TrialRun","year_TPA" ,
    tribal, No_HH, TOT_P,  P_ST, P_LIT, TribalProp, LitProp, `_merge`,
    
    # infr_vars
    PVCPipeLength,PlainLateralPipeLength,EmittingPipeLength,
    PumpHP,TotalHead,UniqueVillageComb,TriSubByTotal,ccode11,
    
    # geo_vars
    dupecon, circle,division, subdivision, village_ugvcl,
    
    # Rs_vars
    EstimatedSubsidy,FarmerPayment,SupplierShare, MISPartnerShare, 
    GGRCShare, TribalSubsidy, GWRDCSubsidy, 
    RKVYSubsidy, RKVYSumpSubsidy,InStudy, EstSubByTotal,TriSubByTotal,
    TotalSubsidy, TotSubByTot,GGRCSubByTotal, SupplierSubByTotal, 
    PartnerSubByTotal, RKVYSubByTotal,
    
    FarmerContact,SurveyNo,WellSurveyNo, electricityconsumernumber, 
    Village,RegistrationYear, month,month_TrialRun,month_TPA,year )






# adding villages list to the main df  .....................................----
gj02 <- gj01 %>% 
  left_join( gj_village %>% select(-n) )


# create vars: farmer_category | caste | mi_pct_land .......................----
gj03 <- gj02 %>% 
  mutate(farmer_category=
  ifelse(FarmerType %in% c("Small Farmer" ,"Marginal Farmer"),
         "Small.Marginal Farm",
  ifelse(FarmerType%in%c("Large Farmer","Medium Farmer","Semi-Medium Farmer"),
         "Large.Medium Farm",NA))
    ) %>% 
  mutate(caste=
    ifelse(farmercaste =="Others", "high caste",
    ifelse(farmercaste %in% c("OBC","ST","SC"),"low caste",NA))
    ) %>% 
  mutate(mi_pct_land=misarea/TotalLand )

gj03 %>% count(villageSI)



# [gj1] [gj1B] Complete database of all variables for each observation ............----

gj04 <- gj03 %>%
  filter(
    TotalLand>0,!is.na(TotalLand),
    Crop != "NULL",
    !is.na(Longitude) ) %>% 
  filter(year_Registration<2014)  

# ____________________________________
gj1B <- gj03   # Total 441,700 HH 
# ____________________________________
gj1  <- gj04  # Total 242,110 HH         
# ____________________________________

rm(gj01, gj02, gj03, gj04)
rm(GGRC_filtered,GGRC)

# DESCRIPTIVE STAT .................................................... ####    

library(summarytools)
library(kableExtra)
library(dplyr)



gj1B %>% select(mistype,TotalLand) %>% filter(!is.na(TotalLand), between(TotalLand, 0, 22)) %>% 
  group_by(mistype) %>% 
  descr(stats = c("mean", "sd"),transpose = T)

gj1B %>% select(mistype,TotalLand,misarea) %>% filter(!is.na(TotalLand), between(TotalLand, 0, 22)) %>% 
  select(mistype,misarea) %>% group_by(mistype) %>% 
  descr(stats = c("mean", "sd"),transpose = T)

gj1 %>% select(mistype,mi_pct_land) %>% filter(between(mi_pct_land, 0.08, 1)) %>% 
  group_by(mistype) %>% 
  descr(stats = c("mean", "sd"),transpose = T)




########### t1 TotalLand ----
quantile(gj1B$TotalLand , 0.01, na.rm = TRUE)
quantile(gj1B$TotalLand , 0.99, na.rm = TRUE)

t1 <- 
  gj1B %>% select(TotalLand) %>% 
  filter(!is.na(TotalLand), between(TotalLand, 0, 22)) %>% 
  descr(stats = c("mean", "sd"),transpose = T) %>% 
  as.data.frame()

########### t2 misarea ----
quantile(GGRC$misarea , 0.01, na.rm = TRUE)
quantile(GGRC$misarea , 0.99, na.rm = TRUE)

t2 <- 
  GGRC_filtered %>% select(misarea) %>% filter(between(misarea, 0.39, 5.61)) %>% 
  descr(stats = c("mean", "sd", "min", "max"),transpose = T) %>% as.data.frame()

########### t3 mi_pct_land ----
quantile(gj1$mi_pct_land , 0.01, na.rm = TRUE)
t3 <- 
  gj1 %>% select(mi_pct_land) %>% filter(between(mi_pct_land, 0.08, 1)) %>% 
  descr(stats = c("mean", "sd", "min", "max"),transpose = T) %>% as.data.frame()
  
########### t4 TotalMISCost ----
descr(GGRC$TotalMISCost,stats = c("mean", "sd", "min", "max"),transpose = T)
quantile(GGRC$TotalMISCost , 0.01, na.rm = TRUE)
quantile(GGRC$TotalMISCost , 0.99, na.rm = TRUE)

t4=
  GGRC %>%
  filter(!is.na(TotalMISCost), TotalMISCost < 497012) %>% 
  descr(TotalMISCost,stats = c("mean", "sd", "min", "max"),transpose = T)%>% 
  as.data.frame() %>% mutate_at(1:4,round)

GGRC_filtered %>%
  filter(!is.na(TotalMISCost), TotalMISCost < 497012) %>% 
  group_by(mistype) %>%
  summarise(descr(TotalMISCost,stats = c("mean", "sd", "min", "max"),transpose = T))


########### % FarmerType | farmer_category ----

gj1 %>% filter(!is.na(caste)) %>% count(caste) %>% mutate(pct=n/sum(n)*100, pct=paste0(round(pct),"%"), n=format(n,big.mark=","))

freq_table_4cst= gj1 %>% filter(!is.na(farmercaste),farmercaste != "") %>% count(farmercaste) %>% mutate(pct=n/sum(n)) %>% rename(Variable=farmercaste)
freq_table_2cst= gj1 %>% filter(!is.na(caste),caste != "") %>% count(caste) %>% mutate(pct=n/sum(n))%>% rename(Variable=caste)
ft_cst= rbind(freq_table_2cst,freq_table_4cst)
ft_cst

########### % mistype ----
freq(gj1$mistype)

freq_table_mistype <-
  gj1 %>% filter(mistype != "") %>% count(mistype) %>%mutate(pct=n/sum(n)) %>%
  mutate_at(3,round,2)%>% rename(Variable=mistype)

########### % WaterSourceDetail ----
GGRC_filtered %>% filter(! WaterSourceDetail %in% c("", "NULL")) %>% count(WaterSourceDetail) %>%mutate(pct=n/sum(n))

ft_WaterSourceDetail <-
  GGRC_filtered %>% filter(!WaterSourceDetail %in% c("","NULL"))%>%count(WaterSourceDetail) %>%
  mutate(pct=n/sum(n)) %>% mutate_at(3,round,4)%>% rename(Variable=WaterSourceDetail)


GGRC_filtered %>% filter(! WaterSourceDetail %in% c("", "NULL")) %>% count(mistype,WaterSourceDetail) %>%mutate(pct=n/sum(n))

########### % supplier ----
tf_supplier=
  gj1 %>% filter(supplier!="") %>% count(supplier) %>%
  mutate(pct=n/sum(n)) %>%filter(pct>0.06) %>% mutate_at(3,round,2) %>%
  arrange(desc(pct)) %>% rename(Variable=supplier)

########### % Crop ----
ft_crop=
  GGRC %>% filter(! Crop %in% c("", "NULL")) %>% count(Crop) %>%
  mutate(pct=n/sum(n)) %>%filter(pct>0.01) %>% mutate_at(3,round,2) %>% 
  arrange(desc(pct)) %>% rename(Variable=Crop)
  
########### % Loan ----
ft_loan=
  GGRC %>% filter(!is.na(Loan)) %>% count(Loan) %>%
  mutate(pct=n/sum(n)) %>% mutate_at(3,round,3) %>% 
  filter(Loan==1) %>%rename(Variable=Loan)
ft_loan$Variable[ft_loan$Variable==1]="Took Loan"


# DATA TABLE ..........................................................----

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

total_land= gj1 %>% select(TotalLand)
write.csv(total_land, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/world_bank/total_land.csv", row.names=FALSE)


df <- gj1 %>% select(1,TotalLand)
df_clean = df %>% filter( TotalLand > 0 )

hist(df_clean$TotalLand, breaks = 20, col = "green4", 
     main = "Histogram of TotalLand (Entire Sample)", 
     xlab = "TotalLand (in acres or units)", 
     ylab = "Frequency")

df_up_to_100 <- df_clean[df_clean$TotalLand <= 100, ]
hist(df_up_to_100$TotalLand, breaks = 10, col = "green4", 
     main = "Histogram of TotalLand (Up to 100 Acres)", 
     xlab = "TotalLand (in acres or units)", 
     ylab = "Frequency")

# Plotting the histogram for TotalLand up to 40 acres
df_up_to_40 <- df_clean[df_clean$TotalLand <= 40, ]
hist(df_up_to_40$TotalLand, breaks = 10, col = "green4", 
     main = "Histogram of TotalLand (Up to 40 Acres) | 10 Bins", 
     xlab = "TotalLand (in acres or units)", 
     ylab = "Frequency")


df_up_to_100 %>%
  mutate(Bin = cut(TotalLand, breaks = 
                  seq(0, 100, by = 10), right = FALSE)) %>%
#                 seq(0, 100, by = 20), right = FALSE)) %>%
  group_by(Bin) %>%
  summarise(Observations = n()) %>%
  arrange(Bin) %>% 
  kbl() %>%
  kable_styling(full_width = FALSE, 
                bootstrap_options = c("hover", "condensed"))

nrow(gj1B)
total_farmers <- nrow(df_clean)
total_farmers
total_farmers_NEW <- nrow(df_clean %>% filter(TotalLand<=22))
total_farmers_NEW
(1-(total_farmers_NEW/total_farmers))*100

gj1 %>% filter(TotalLand>0,TotalLand<=22) %>% 
  summarise(mean (TotalLand),SD(TotalLand),n())

gj1B %>% filter(TotalLand>0,TotalLand<=22) %>% 
  summarise(mean (TotalLand),SD(TotalLand),n())




