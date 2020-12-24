library(haven)
Agriculture_Midline_2018_ <- read_dta("~/Nepal Data/Saptari/Midline 2074-75 (Saptari)/Agriculture_Midline(2018).dta")
Agriculture_Endline_2019_Saptari <- read_dta("~/Nepal Data/Saptari/Saptari-Endline(2019)/Agriculture_Endline(2019)-Saptari.dta")

Agriculture_17_18_19 <- 
  rbind(Agriculture_Baseline_2017_,
        Agriculture_Midline_2018_,
        Agriculture_Endline_2019_Saptari) %>% 
  inner_join(Control_and_treatment_4_districts)

Agriculture_17_18_19 <- Agriculture_17_18_19 %>%
  mutate(irri_for_season=cult_area_under_crop*hrs_irr_1katha*no_of_irrigation_for_1_katha)

Agriculture_17_18_19$season_of_crop[Agriculture_17_18_19$season_of_crop == "1"] <- "Monsoon"
Agriculture_17_18_19$season_of_crop[Agriculture_17_18_19$season_of_crop == "2"] <- "Winter"
Agriculture_17_18_19$season_of_crop[Agriculture_17_18_19$season_of_crop == "3"] <- "Summer"
Agriculture_17_18_19$season_of_crop[Agriculture_17_18_19$season_of_crop == "4"] <- "Annual"



write.csv(Agriculture_17_18_19,"C:/Users/Dan/Documents/R/Saptari/data/Agriculture_17_18_19.csv", row.names = FALSE)











S_Agriculture_Baseline_2017_ <-inner_join(Agriculture_Baseline_2017_,Control_and_treatment_4_districts) %>%   
  filter(!household_questionnaire_id %in% c("T106006001", "T107609001","T106006002")) %>% 
  mutate(irri_for_season=cult_area_under_crop*hrs_irr_1katha*no_of_irrigation_for_1_katha) %>% 
  mutate( name_of_crop_detail = name_of_crop ) 

S_Agriculture_Baseline_2017_$name_of_crop[S_Agriculture_Baseline_2017_$name_of_crop %in%
                                            c("BELL PEPPER","BITTER GOURD","BOTTLE GOURD","BRINJAL",
                                              "CABBAGE","CAULIFLOWER","CHILLIES","CORIANDER","CUCUMBER",
                                              "CUUCMBER","GARLIC","GREEN PEA","GREENLEAFY VEGETABLE",
                                              "OKRA","ONIONS","PEA","POINTED GOURD", "POTAT0",
                                              "POTATO","SPONGE GOURD","TOMATO", "YARD LONG BEANS"
                                            )]<- "vegetables"
S_Agriculture_Baseline_2017_$name_of_crop[S_Agriculture_Baseline_2017_$name_of_crop %in%
                                            c("GRASS PEA","GREEN GRAM","HORSE GRAM","OTHER PULSES",
                                              "RED GRAM","RED LENTIL" )] <- "pulses"
S_Agriculture_Baseline_2017_$name_of_crop[S_Agriculture_Baseline_2017_$name_of_crop %in%
                                            c("LINSEED","MUSTARD","SESAME")] <- "oilseeds"
S_Agriculture_Baseline_2017_$name_of_crop[S_Agriculture_Baseline_2017_$name_of_crop %in%
                                            c("OTHER","OTHER CROPS","OTHER CEREALS")] <- "others"
                                            
