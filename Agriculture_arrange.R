# Observations organization

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

Agriculture_18_19$name_of_crop[Agriculture_18_19$name_of_crop %in%
                                            c("TOMATO","BITTER","BRINJAL","OKRA","GARLIC","ONIONS","RADISH"
                                              ,"POTATO","CABBAGE","CAULI","BOTTLE","BELL","SPONGE",
                                              "CHILLIES","CHILIES", "CUCUMBER","ONOINS","vegetables",
                                              "GREEN LEAFY VEGETABLE"
                                            )] <- "Vegetables"

Agriculture_18_19$name_of_crop[Agriculture_18_19$name_of_crop %in%
                                            c("KIDNEY", "GREEN GRAM","BLACK GRAM","GRASS PEA","PEA",
                                              "OTHER PULSES","YARD LONG","RED LENTIL","RED LENTILS",
                                              "RED LENTI","RED LENTLE"
                                            )] <- "Pulses"

Agriculture_18_19$name_of_crop[Agriculture_18_19$name_of_crop %in%
                                            c("GRASS","OATS","BARLEY","OTHER"
                                            )] <- "Others"

Agriculture_18_19$name_of_crop[
  Agriculture_18_19$name_of_crop =="MUSTARD"] <- "Oilseeds"

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
    


