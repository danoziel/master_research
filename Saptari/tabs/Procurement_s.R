# o-----
library(haven)
Procurement_Baseline_2017_ <-
  read_dta("~/Nepal Data/Saptari/Baseline 73-74 (Saptari)/Procurement_Baseline(2017).dta")
Procurement_Midline_2018_ <-
  read_dta("~/Nepal Data/Saptari/Midline 2074-75 (Saptari)/Procurement_Midline(2018).dta")
Procurement_Endline_2019_Saptari <-
  read_dta("~/Nepal Data/Saptari/Saptari-Endline(2019)/Procurement_Endline(2019)-Saptari.dta")

Procurement_17_18_19 <- rbind(Procurement_Baseline_2017_,
                              Procurement_Midline_2018_,
                              Procurement_Endline_2019_Saptari) %>% 
  inner_join(Control_and_treatment_4_districts)

write.csv(Procurement_17_18_19, file = "C:/Users/Dan/Documents/R/Saptari/data/Procurement_17_18_19.csv", row.names=FALSE)


# fuel [7.16]----
#  Total litres of diesel/kerosene consumed for agriculture pumps in a YEAR
Procurement_17_18_19 %>%  
  group_by(TreatmentControl,year) %>%
  summarise(N=n(), liters_yearly=mean(total_litres_consumed_dieselkero,na.rm = T)) %>% 
  mutate(across(is.numeric, round, 2))


