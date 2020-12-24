# Organize data----
library(haven)
# Lands_ -combined
Lands_Baseline_2017_ <- read_dta("~/Nepal Data/Saptari/Baseline 73-74 (Saptari)/Lands_Baseline(2017).dta")
Lands_Midline_2018_ <- read_dta("~/Nepal Data/Saptari/Midline 2074-75 (Saptari)/Lands_Midline(2018).dta")
Lands_Endline_2019_Saptari <- read_dta("~/Nepal Data/Saptari/Saptari-Endline(2019)/Lands_Endline(2019)-Saptari.dta")

land_17_18_19 <- rbind(Lands_Baseline_2017_,Lands_Midline_2018_,Lands_Endline_2019_Saptari)
land_17_18_19 <- inner_join(land_17_18_19,Control_and_treatment_4_districts)

# Lands_I_ -combined
Lands_I_Baseline_2017_ <- read_dta("~/Nepal Data/Saptari/Baseline 73-74 (Saptari)/Lands_I_Baseline(2017).dta")
Lands_I_Midline_2018_ <- read_dta("~/Nepal Data/Saptari/Midline 2074-75 (Saptari)/Lands_I_Midline(2018).dta")
Lands_I_Endline_2019_Saptari <- read_dta("~/Nepal Data/Saptari/Saptari-Endline(2019)/Lands_I_Endline(2019)-Saptari.dta")

lands_I_17_18_19 <- rbind(Lands_I_Baseline_2017_,Lands_I_Midline_2018_,Lands_I_Endline_2019_Saptari)
lands_I_17_18_19 <- inner_join(lands_I_17_18_19,Control_and_treatment_4_districts)

#add "land_for_cultivitation" (nca) TO land_17_18_19
land_17_18_19 <- left_join(land_17_18_19,lands_I_17_18_19[,c(1,37,3)])

# Objects omitted with NAs in all variables
land_17_18_19 <- land_17_18_19 %>% filter(!is.na(total_land_cultivated))

#creat "total_land_cultivated_year"
land_17_18_19 <- land_17_18_19 %>% group_by(year,household_questionnaire_id) %>% 
  mutate(total_land_cultivated_year=sum(total_land_cultivated))

write.csv(land_17_18_19,"C:/Users/Dan/Documents/R/Saptari/land_17_18_19.csv", row.names = FALSE)

# total_ownland_cultivated        ----
lso <- land_17_18_19%>% 
  filter(total_land_cultivated_year>0,season=="Summer") %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(own=sum(total_ownland_cultivated)*0.0338) %>% 
  group_by(TreatmentControl,year) %>%
  summarise(N=n(), Mean=mean(own)) %>% 
  mutate(across(is.numeric, round, 2))

lsoC <- lso[1:3,-1]
lsoT <- lso[4:6,3:4]
lsoC_lsoT <- cbind(lsoC,lsoT)


kable(lsoC_lsoT, booktabs = T,align = "lcccc",linesep = "") %>%
  column_spec(1, bold = T) %>%
  kable_styling(latex_options = "striped", position = "center") %>% 
  column_spec(1:5, width = "1.5cm",border_left = F) %>%column_spec(3,border_right = T ,width = "1.5cm") %>%  row_spec(0, font_size= 9) %>% 
  add_header_above(c(" " = 1, "Control" = 2, "Treatment" = 2), bold = F, align = "c")


# total_land_cultivated           -----
lsc <- land_17_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(cult=sum(total_land_cultivated)*0.0338) %>% 
  group_by(TreatmentControl,year) %>%
  summarise(N=n(), Mean=mean(cult))

# crop_intensity                  ----
x <- land_17_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  mutate(NEW_total_land_cult= case_when(
    TreatmentControl=="Control" & land_for_cultivation < total_land_cultivated ~ NA_real_,
    TRUE ~ total_land_cultivated)) %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(cult=sum(NEW_total_land_cult,na.rm = T),net=mean(land_for_cultivation))%>%
  mutate(crop_intensity=cult/net*100) %>% 
  group_by(TreatmentControl,year) %>%
  summarise(N=n(),Mean=mean(crop_intensity)) %>% 
  mutate(across(is.numeric, round))




# irrigated_out_of_tot_land_cult  ----
lsi <- land_17_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(irrigated=sum(irrigated_out_of_tot_land_cult,na.rm = T)*0.0338) %>% 
  group_by(TreatmentControl,year) %>%
  summarise(N=n(), Mean=mean(irrigated))

# Time to irrigate 1 ha [6.21] ----
# How long does it take to irrigate 1 katha of land with this pump, in min? [6.21]

wem_liter_fuel_17_18_19 %>% group_by(TreatmentControl,year) %>% 
  summarise(N=n(),Mean=mean(time_to_irrigate_1_katha__p_1,na.rm = T))

# irrigation- IN HOURS By year [5.0]----
lsay<- Agriculture_17_18_19%>% 
  group_by(TreatmentControl,year, household_questionnaire_id) %>% 
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season)) %>% 
  summarise(N=n(),average_hr_per_ha=mean(hr_per_ha,na.rm = T),
            total_irrigate_hr=mean(irrigate_hr,na.rm = T)) %>% 
  mutate(across(is.numeric, round))




# irrigation- IN HOURS By season [5.0]-----

lsas <- Agriculture_17_18_19 %>% filter(season_of_crop!="Annual") %>% 
  group_by(year,season_of_crop,TreatmentControl,household_questionnaire_id) %>%
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season))%>% 
  group_by(season_of_crop,TreatmentControl,year) %>%
  summarise(n=n(),average_hr_per_ha=mean(hr_per_ha,na.rm = T),
            total_irrigate_hr=mean(irrigate_hr,na.rm = T)) %>% 
  mutate(across(is.numeric, round))


# irrigate_intensity [4.0] seasons----

land_17_18_19 %>% filter(total_land_cultivated_year>0,season!="Annual") %>% 
  mutate(irrigation_intens=irrigated_out_of_tot_land_cult/total_land_cultivated*100) %>% 
  filter(irrigation_intens>= 0,irrigation_intens<=100) %>% 
  group_by(TreatmentControl,season,year) %>% 
  summarise(mean(irrigation_intens)) %>% 
  mutate(across(is.numeric, round, 2))


# irrigate_intensity [4.0] year----
 <- 
  land_17_18_19 %>% filter(total_land_cultivated_year>0,season!="Annual") %>% 
  group_by(TreatmentControl,year,household_questionnaire_id) %>%
  summarise(gross_irri=sum(irrigated_out_of_tot_land_cult),gross_crop=sum(total_land_cultivated),ii=gross_irri/gross_crop*100) %>% 
  filter(ii>= 0,ii<=100) %>% 
  group_by(TreatmentControl,year) %>% 
  summarise(N=n(),Mean=mean(ii)) %>% 
  mutate(across(is.numeric, round, 2)) 

# Frequency of households who irrigate [4.9]----
scount <- land_17_18_19 %>% filter(irrigated_out_of_tot_land_cult>0,season!="Annual") %>% 
  group_by(TreatmentControl,season,year) %>% count()

sc <- land_17_18_19 %>% filter(season!="Annual") %>% 
  group_by(TreatmentControl,season,year) %>%summarise(N=n()) %>% 
  right_join(scount) %>% mutate(per=n/N*100)

library(data.table)
dtci <-tribble(~" ", ~"n", ~"Percent",~"n", ~"Percent",~"n", ~"Percent",
               ~"n", ~"Percent",~"n", ~"Percent",~"n", ~"Percent",
               2017, 83, "88%", 18, "82%",  57, "61%"  ,16, "73%" ,81, "86%"  ,20,  "91%", 
               2018 ,77, "89%"  ,17,  "77%" ,50, " 53% ",17 , "77%" ,79, "84%" ,20, "91%",
               2019,  81, "88%" ,22, "96%"  ,49, "53%"  ,16, "70%"  ,78, "85%"   ,21, "91%")


# Total litres of diesel/kerosene consumed for agriculture pumps in a YEAR [7.16]----- 

psf <- Procurement_17_18_19 %>%  
  group_by(TreatmentControl,year) %>%
  summarise(N=n(), Mean=mean(total_litres_consumed_dieselkero,na.rm = T)) %>% 
  mutate(across(is.numeric, round, 2))



# fuel [6.2]----
# fuel use by the 'Water_extraction_mechanism'files
wem_liter_fuel_17_18_19%>%
  group_by(TreatmentControl,year) %>% 
  summarise(Monsoon=mean(p123_m,na.rm = T),
            Summer=mean(p123_s,na.rm = T),Winter=mean(p123_w,na.rm = T),
            Year=mean(p123_year,na.rm = T))

# Frequency of households who use fuel----

# The number of farmers who purchase fuels
# is the number of fuel pump owners - 
#   the rest use electricity pumps

#[7.12] Did you buy fuel for the pump?
per <- Procurement_17_18_19 %>%
  filter(!is.na(do_you_buy_fuel_for_the_pump)) %>% 
  group_by(TreatmentControl,year,do_you_buy_fuel_for_the_pump) %>%
  count() %>% group_by(TreatmentControl,year) %>%
  summarise(sum(n))

fuelcount <- Procurement_17_18_19 %>%filter(!is.na(total_litres_consumed_dieselkero)) %>% 
  group_by(TreatmentControl,year) %>% count() %>% 
  right_join(per) %>% mutate(P=n/`sum(n)`*100,across(is.numeric, round)) 



# aquaculture [4.1c]-----
lands_I_17_18_19 %>% filter(land_for_aquaculture_ponds>0) %>% 
  group_by(TreatmentControl,year) %>% 
  summarise(N=n(),Mean= mean(land_for_aquaculture_ponds,na.rm = T)*0.0339)

#----


Agriculture_17_18_19 %>%filter(name_of_crop!="") %>% 
  group_by(year,TreatmentControl, household_questionnaire_id, name_of_crop) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  group_by(TreatmentControl,year ,name_of_crop) %>% 
  summarise(mean(cult_area)) %>% 
  mutate(across( is.numeric,round,2))



# Vegetables----
Vegetables_17_18_19 <- Agriculture_17_18_19 %>%filter(name_of_crop=="Vegetables") 
Vegetables_18_19 <- Agriculture_18_19 %>%filter(name_of_crop=="Vegetables") 
vegetables <- rbind(Vegetables_17_18_19,Vegetables_18_19)
write.csv(Agriculture_17_18_19, file = "C:/Users/Dan/Documents/R/Saptari/data/Agriculture_17_18_19.csv", row.names=FALSE)

Agriculture_17_18_19 %>% filter(name_of_crop=="Vegetables") %>% 
  group_by(name_of_crop,year,TreatmentControl, household_questionnaire_id) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  group_by(name_of_crop,TreatmentControl,year) %>% 
  summarise(N=n(),Mean=mean(cult_area)) %>% 
  mutate(across( is.numeric,round,2))

veg_tb <- cbind(veg[1:3,3:5],veg[4:6,4:5])


veg_tb%>% kable() %>% kable_styling(full_width = F, position = "left")

kable(veg_tb, booktabs = T,align = "lcccc",linesep = "") %>%
  column_spec(1, bold = T) %>%
  kable_styling(latex_options = "striped",stripe_index = 2, position = "left") %>% 
  column_spec(1:5, width = "1.5cm",border_left = F) %>%
  column_spec(3,border_right = T ,width = "1.5cm") %>% 
  row_spec(0, font_size= 7) %>% 
  add_header_above(c("SAPTARI" = 1, "Control" = 2, "Treatment" = 2), bold = F, align = "c")

ggplot(veg , aes(year, Mean, color = TreatmentControl)) +
  stat_summary(geom = 'line') +
  theme_minimal()+
  ggtitle("Vegetables") +
  xlab(" ") +
  ylab(" ")+
  scale_x_continuous(breaks = c(2017,2018,2019))+
  theme(legend.position = "none",
      plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))

Oilseeds <- Agriculture_17_18_19 %>%filter(name_of_crop=="Oilseeds") %>% 
  group_by(year,TreatmentControl, household_questionnaire_id, name_of_crop) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  group_by(name_of_crop,TreatmentControl,year) %>% 
  summarise(N=n(),Mean=mean(cult_area)) %>% 
  mutate(across( is.numeric,round,2))


Pulses <- Agriculture_17_18_19 %>%filter(name_of_crop=="Pulses") %>% 
  group_by(year,TreatmentControl, household_questionnaire_id, name_of_crop) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  group_by(name_of_crop,TreatmentControl,year) %>% 
  summarise(N=n(),Mean=mean(cult_area)) %>% 
  mutate(across( is.numeric,round,2))

#   "Paddy","Wheat","Maize"

pwm <- Agriculture_17_18_19 %>%
  filter(name_of_crop %in% c("Paddy","Wheat","Maize")) %>% 
  group_by(year,TreatmentControl, household_questionnaire_id, name_of_crop) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  group_by(name_of_crop,TreatmentControl,year) %>% rename( Cereals = name_of_crop ) %>% 
  summarise(Mean=mean(cult_area)) %>% 
  mutate(across( is.numeric,round,2))

pwm_tb <- spread(pwm, TreatmentControl, Mean)

kable(pwm_tb, align = "c") %>%
  kable_styling(full_width = T) %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 1, valign = "top")

ggplot(pwm[1:6,] , aes(year, Mean, color = TreatmentControl)) +
  stat_summary(geom = 'line') +
  theme_minimal()+
  ggtitle("Maize") +
  xlab(" ") +
  ylab(" ")+
  scale_x_continuous(breaks = c(2017,2018,2019))+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))


