# Observations organization

# 26/11/2023
library(haven)
agriBase_sap <- read_dta("~/Nepal Data/Saptari/Baseline 73-74 (Saptari)/Agriculture_Baseline(2017).dta")
agriBase_sap=agriBase_sap %>% mutate(survey="baseline")

agriEnd_sap <- read_dta("~/Nepal Data/Saptari/Saptari-Endline(2019)/Agriculture_Endline(2019)-Saptari.dta")
agriEnd_sap=agriEnd_sap %>% mutate(survey="endline")

agriBase_RBS <- read_dta("~/Nepal Data/REWSSPC/Baseline/Agriculture_Baseline(2018).dta")
agriBase_RBS=agriBase_RBS %>% select(1:14,16,15) %>% mutate(survey="baseline")

agriEnd_RBS <- read_dta("~/Nepal Data/REWSSPC/Endline/Agriculture_Endline-EPC(2019).dta")
agriEnd_RBS=agriEnd_RBS %>% mutate(survey="endline")

names(agriBase_RBS)
agri <- rbind(agriBase_sap,agriEnd_sap,agriBase_RBS,agriEnd_RBS) %>% 
  rename(HH=household_questionnaire_id)
hh_prc$HH[hh_prc$HH=="E0104705010"] <- "E104705010"

agri= right_join(agri,hh_prc)%>% filter(type_of_crop!="") %>%
  mutate(crop="")
AGR=agri[,c(1:3,21)]
agri$crop[agri$name_of_crop %in% c("PADDY","paddy")] <- "Paddy"
agri$crop[agri$name_of_crop == "WHEAT"] <- "Wheat"
agri$crop[agri$name_of_crop == "MAIZE"] <- "Maize"
agri$crop[agri$name_of_crop %in% c("SUAGRCANE","SUGARCANE")] <- "Sugarcane"

                      
# ____________________________ 
Agriculture_18_19 <- rbind(Agriculture_Baseline_2018_,Agriculture_Endline_EPC_2019_) %>% 
  inner_join(Control_and_treatment_4_districts )%>% filter(type_of_crop!="")

Agriculture_18_19 <- Agriculture_18_19 %>%
  mutate( name_of_crop_detail = name_of_crop ) #(copy=original)

# Agriculture----
# Replace values in the data frame
Agriculture_18_19$name_of_crop[
  Agriculture_18_19$name_of_crop %in% c("PADDY","paddy")] <- "Paddy"

Agriculture_18_19$name_of_crop[
  Agriculture_18_19$name_of_crop == "WHEAT"] <- "Wheat"

Agriculture_18_19$name_of_crop[
  Agriculture_18_19$name_of_crop == "MAIZE"] <- "Maize"

Agriculture_18_19$name_of_crop[
  Agriculture_18_19$name_of_crop %in% c("SUAGRCANE","SUGARCANE")] <- "Sugarcane"

agri$crop[agri$name_of_crop %in%# 26/11/2023
# Agriculture_18_19$name_of_crop[Agriculture_18_19$name_of_crop %in%
                                            c("TOMATO","BITTER","BRINJAL","OKRA","GARLIC","ONIONS","RADISH",
                                              "BOTTLE GOURD", "ONION", "CAULIFLOWER", "PUMPKIN","BITTER GOURD",
                                              "POTATO","CABBAGE","CAULI","BOTTLE","BELL","SPONGE",
                                              "GREENLEAFY VEGETABLE","CORIANDER","CORIANDER",
                                              "CHILLIES","CHILIES", "CUCUMBER","ONOINS","vegetables",
                                              "GREEN LEAFY VEGETABLE"
                                            )] <- "Vegetables"

agri$crop[agri$name_of_crop %in%# 26/11/2023
# Agriculture_18_19$name_of_crop[Agriculture_18_19$name_of_crop %in%
                                            c("KIDNEY", "GREEN GRAM","BLACK GRAM","GRASS PEA","PEA",
                                              "GREEN PEA","HORSE GRAM",
                                              "OTHER PULSES","YARD LONG","RED LENTIL","RED LENTILS",
                                              "RED LENTI","RED LENTLE"
                                            )] <- "Pulses"

agri$crop[agri$name_of_crop %in%# 26/11/2023
# Agriculture_18_19$name_of_crop[Agriculture_18_19$name_of_crop %in%
                                            c("GRASS","OATS","BARLEY","OTHER",
                                              "OTHER CROPS","CEREALS"
                                            )] <- "Others"

agri$crop[agri$name_of_crop %in%# 26/11/2023
                                  c("LINSEED","MUSTARD","SESAME"
                                  )] <- "Oilseeds"

Agriculture_18_19$name_of_crop[
  Agriculture_18_19$name_of_crop =="MUSTARD"] <- "Oilseeds"

agri$season_of_crop <- as.character(agri$season_of_crop)
agri$season_of_crop[agri$season_of_crop == "1"] <- "Monsoon"
agri$season_of_crop[agri$season_of_crop == "2"] <- "Winter"
agri$season_of_crop[agri$season_of_crop == "3"] <- "Summer"
agri$season_of_crop[agri$season_of_crop == "4"] <- "Annual"






# omit
# R.Agriculture_Baseline_2018_ <- R.Agriculture_Baseline_2018_ %>%
#   filter(household_questionnaire_id != "T301502075" |
#            name_of_crop != "WHEAT" )

# R.Agriculture_Baseline_2018_ <- R.Agriculture_Baseline_2018_ %>% 
#  filter(household_questionnaire_id != "T304905099" | season_of_crop != "4" )

Agriculture_18_19$season_of_crop <- as.character(Agriculture_18_19$season_of_crop)

Agriculture_18_19$season_of_crop[Agriculture_18_19$season_of_crop == "1"] <- "Monsoon"
Agriculture_18_19$season_of_crop[Agriculture_18_19$season_of_crop == "2"] <- "Winter"
Agriculture_18_19$season_of_crop[Agriculture_18_19$season_of_crop == "3"] <- "Summer"
Agriculture_18_19$season_of_crop[Agriculture_18_19$season_of_crop == "4"] <- "Annual"

Agriculture_18_19 <- Agriculture_18_19 %>%
  mutate(irri_for_season=cult_area_under_crop*hrs_irr_1katha*no_of_irrigation_for_1_katha)

# --------------------------------------------------------------#
Agriculture_17_18_19 <- Agriculture_17_18_19 %>%
  mutate( name_of_crop_detail = name_of_crop ) #(copy=original)

Agriculture_17_18_19$name_of_crop[Agriculture_17_18_19$name_of_crop %in%
                                            c("BELL PEPPER" ,"BITTER GOURD","BOTTLE GOURD","BOTTLE",
                                              "BRINJAL","CABBAGE" ,"CAULIFLOWER" ,"CHILLIES","SPONGE",
                                              "CORIANDER" ,"CUCUMBER" ,"CUUCMBER" ,"GARLIC","RADISH", "PUMPKIN",
                                              "GREEN PEA","GREENLEAFY","OKRA","ONIONS","PEA","POINTED",
                                              "POINTED GOURD","POTAT0" ,"POTATO" ,"BELL PEPER","BELLPEPPER",
                                              "GREENLEAFY VEGETABLE","BELL","CAULI","BITTER","JUTE","oNION",
                                              "GREEN LEAF","GREEN LEAFY VEGETABLE","COLOCASIA FRUIT","ONION",
                                              "SPONGE GOURD","TOMATO","YARD LONG" ,"YARD LONG BEANS"
                                            )] <- "Vegetables"

Agriculture_17_18_19$name_of_crop[Agriculture_17_18_19$name_of_crop %in%
                                    c("GRASS PEA", "HORSE GRAM","OTHER PULSES","BLACK GRAM",
                                      "RED GRAM","RED LENTIL", "GREEN GRAM","PULSES"
                                    )] <- "Pulses"

Agriculture_17_18_19$name_of_crop[Agriculture_17_18_19$name_of_crop %in%
                                    c("SESAME","LINSEED","MUSTARD","MUSTARD LEAF"
                                    )] <- "Oilseeds"

Agriculture_17_18_19$name_of_crop[Agriculture_17_18_19$name_of_crop %in%
                                    c("OTHER CROPS","BARLEY","GRASS"
                                    )] <- "Others"

Agriculture_17_18_19$name_of_crop[Agriculture_17_18_19$name_of_crop %in%
                                            c("CEREALS","OTHER CEREALS"
                                            )] <- "Cereals"

Agriculture_17_18_19$name_of_crop[
  Agriculture_17_18_19$name_of_crop=="PADDY"] <- "Paddy"

Agriculture_17_18_19$name_of_crop[
  Agriculture_17_18_19$name_of_crop=="WHEAT"] <- "Wheat"

Agriculture_17_18_19$name_of_crop[
  Agriculture_17_18_19$name_of_crop=="MAIZE"] <- "Maize"


xx <- Agriculture_17_18_19 %>%
  mutate(kg=case_when(
    unit_harvest %in% c(1,2,5) ~37.32,
    unit_harvest == 4 ~100,
    unit_harvest == 3 ~1))
    


