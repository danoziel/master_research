
library(dplyr)
library(tools)
library(tidyr)
library(ggplot2)
library(scales)
library(kableExtra)
library(readxl)
BLF_Interact <-    read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/BLF_satara/BLF_Interact_-_all_versions_-_False_-_2025-04-30-18-15-56.xlsx")
# BLF_Interact_Qs <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/BLF_satara/BLF_Interact_-_all_versions_-_English_en_-_2025-04-30-18-17-43.xlsx")
BLF_InteractCSV <- read.csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/BLF_satara/BLF_Interact_-_all_versions_-_labels_-_2025-05-03-20-01-16.csv", sep=";")

### uid and clean [shop_interact] ........................................ ----

# Few significant updates were made to the questionnaire
#     version 1 | 2024.10.24 - 2024.10.24 | _index 1:8
#     version 2 | 2024.10.24 - 2024.11.26 | _index 9:116
#     version 3 | 2024.11.26 - 2025.04.06 | _index 117:1025
#     version 4 | 2025-04-06 - ____.__.__ | _index 1026: ____

# Create uniqe id by version

# 1st df
shop_interact <- BLF_Interact %>% rename(index=`_index`,id=`_id` ) %>%   
  mutate(uid = case_when(
    index >= 1 & index <= 8 ~ paste0("A", phone),
    index >= 9 & index <= 116 ~ paste0("B", phone),
    index >= 117 & index <= 1025 ~ paste0("C", phone),
    TRUE ~ paste0("D", phone)
  )) %>% 
  rename(crop_acre=decimal_tu4sx43)

# 2nd df
shop_interactCSV <- BLF_InteractCSV %>% filter(!is.na (X_index)) %>% 
  rename(id=X_id ) %>% 
  mutate(uid = case_when(
    X_index >= 1 & X_index <= 8 ~ paste0("A", phone),
    X_index >= 9 & X_index <= 116 ~ paste0("B", phone),
    X_index >= 117 & X_index <= 1025 ~ paste0("C", phone),
    TRUE ~ paste0("D", phone)
  )) %>% 
  rename(crop_acre=decimal_tu4sx43)


### crop ................................................................  ----
# What crop have you come to shop for? COL:crop_focus
library(tidyr)
library(RColorBrewer)

# Count individual crops in the crop_focus column when multiple crops are listed in a single observation
# Remove NAs of "other_crop_1" in multiple crops obs ONLY

df_crop <- 
  shop_interact %>% filter(!is.na(crop_focus)) %>% 
  select(uid,crop_focus,crop_focus_other) %>% 
  mutate(crop_focus = strsplit(crop_focus, " ")) %>%
  unnest(crop_focus)  %>% 
  mutate(crop = ifelse(
    crop_focus == "Other_crop_1","Other_crop",crop_focus )) %>% 
  filter(!(crop == "Other_crop" & is.na(crop_focus_other))
         ) %>% 
  count(crop) %>% mutate(pct=n/sum(n)) %>% 
  mutate(crop_focus = sapply(crop_focus, function(x) toTitleCase(x))) %>% # Capitalize the first letter
  

total_N <- shop_interact %>% filter(!is.na(crop_focus)) %>% nrow()

library(ggplot2)
library(scales)
# 600 X 400
ggplot(df_crop, aes(x = crop, y = pct, fill = crop)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(title = "What crop have you come to shop for?", 
       subtitle = paste("Percent of all respondents, some gave more than one answer | N =", total_N, "\n___________________________________________________________________________________________________"),        
       x = "", y = "% of farmers") +
  theme_minimal() +
  theme(legend.position = "none", 
        text = element_text(family = "serif"),
        axis.text.x = element_text(size = 12)
        ) +
  scale_fill_brewer(palette = "BrBG") +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3)




### VARS to stat ........................................................ ----
# acres         ----
# How many acres of land do you farm? COL:total_acres 
# How many acres of land do you cultivate for ${crop_focus}? COL:decimal_tu4sx43 

land_acre_99 <- quantile(shop_interact$crop_acre, 0.99, na.rm = TRUE)

land_acre <- 
  shop_interact %>% select(uid,crop_acre,crop_focus) %>% 
  filter(!is.na(crop_acre),crop_acre <= land_acre_99)

smryCrop <- 
  land_acre %>% 
  summarise(Mean = mean(crop_acre),SD= sd(crop_acre),
            Min=min(crop_acre),Max=max(crop_acre)) %>% 
  mutate(Crop="All") %>% select(Crop,everything())

smrySC <- 
  land_acre %>% filter(crop_focus=="Sugarcane") %>% 
  summarise(Mean = mean(crop_acre),SD= sd(crop_acre),
            Min=min(crop_acre),Max=max(crop_acre))%>% 
  mutate(Crop="Sugarcane") %>% select(Crop,everything())

rbind(smryCrop,smrySC) %>% 
  kable() %>% kable_minimal()


# farmer_age    ----
# Age: 	
#   [farmer_age]
BLF_InteractCSV %>% 
  summarise(Mean = mean(farmer_age,na.rm=T),SD= sd(farmer_age,na.rm=T),
            Min=min(farmer_age,na.rm=T),Max=max(farmer_age,na.rm=T)) %>% 
  kable() %>% kable_styling()
  
  
# Shop Location ----
# What is the name of the village where BLF is located?
#   	[location]

location <- shop_interact %>% select(location) %>% 
  count(location) %>% mutate(pct=n/sum(n))

total_N <- location %>% 
  summarise(N = sum(n)) %>% pull(N)

# 400 X 450
ggplot(location,aes(x = location, y = pct)) +
  geom_bar(stat = "identity",fill="royalblue4") +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(title = "Shop location", 
       subtitle = paste("Percent of all respondents | N =", total_N, "\n___________________________________________________________________________________________________"),   
       x = "", y = "% of responsers") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "serif")) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3)
  

### reason_come_in_new ...................................................  ----
# What problem did you come up with for this crop today?
# version 1 2 3 | label{reason_come_in}
# version 4     | label{reason_come_in_new}

# DF version 1 2 3 
#_____________________________
#      DF reason_old          |
#_____________________________|

reason_old <- 
  shop_interact %>% filter(!is.na(reason_come_in)) %>% 
  select(uid,reason_come_in) %>% 
  mutate(reason_come_in = strsplit(reason_come_in, " ")) %>%
  unnest(reason_come_in)%>% 
  count(reason_come_in) %>% rename(n_old=n)

reason_old_N <- # Extract the total N as an integer
  shop_interact %>% filter(!is.na(reason_come_in)) %>% 
  count() %>% pull(n)


#_____________________________
#      DF reason_new          |
#_____________________________|
reason_new <- 
  shop_interact %>%
  select(uid,starts_with("reason_come_in_new/")) %>% 
  pivot_longer(!uid, names_to = "reason", values_to = "count") %>% 
  mutate(reason_come_in = gsub("reason_come_in_new/", "", reason)) %>% 
  filter(count==TRUE) %>% 
  count(reason_come_in) %>% rename(n_new=n)

reason_new_N <- # Extract the total N as an integer
  shop_interact %>%
  select(uid,starts_with("reason_come_in_new/")) %>% 
  drop_na()%>%count() %>% pull(n)


#_____________________________
#   disease combined_data    |
#____________________________|
combined_data <- reason_old %>%
  full_join(reason_new, by = "reason_come_in") %>%
  mutate(n_combined = # Combine counts
           coalesce(n_old, 0) + coalesce(n_new, 0),  
         pct = # Calculate percentage
           ifelse(is.na(n_old), n_new / reason_new_N, 
                  n_combined / (reason_old_N + reason_new_N)))











#___________________________________________________________
#            POLT reason_come_in                            |
#___________________________________________________________|
total_N = reason_old_N + reason_new_N

# 800 X 400
ggplot(combined_data, aes(x = reason_come_in, y = pct)) +
  geom_bar(stat = "identity",, fill="mediumpurple4") +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(title = paste("What problem did you come up with for this crop today?"),
       subtitle = paste("Percent of all respondents, some gave more than one answer | N =", total_N, "\n___________________________________________________________________________________________________"),       
       x = "", y = "% of farmers") +
  theme_minimal() +
  theme(legend.position = "none", 
        text = element_text(family = "serif"),
        axis.text.x = element_text(size = 12)
  ) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3) +
  scale_fill_brewer(palette = "BrBG") +  # Paired /  Pastel1
  scale_x_discrete(labels = c(
  "disease_issue" = "Disease \nproblems", 
  "pest_problems" = "Pest \nproblems", 
  "weed_control" = "Weed \ncontrol",
  "cropping_system" = "Cropping \nsystem", 
  "soil_treatment" = "Soil \ntreatment", 
  "irrigation" = "Irrigation \nissue", 
  "need_equipment" = "Equipment \nbuying", 
  "need_fertilizer" = "Fertilizer \nbuying", 
  "need_seeds" = "Seeds \nbuying", 
  "other_issues" = "Other")
  )
  
  









### disease ............................................................... ----
#   What kind of disease are you dealing with?
#..............................................................................

#_____________________________
#      DF disease_old         |
#_____________________________|
disease_old <- 
  shop_interact %>% 
  filter(index %in% (1:1025)) %>% 
  select(uid,starts_with("id_disease_problem/")) %>% 
  pivot_longer(!uid, names_to = "disease", values_to = "count") %>% 
  mutate(disease = gsub("id_disease_problem/", "", disease)) %>% 
  filter(count==1) %>% count(disease) %>%  rename(n_old=n)

old_N <-  # Extract the total N as an integer
  shop_interact %>%
  filter(index %in% (1:1025), !is.na(id_disease_problem)) %>% 
  nrow()


#_____________________________
#        DF disease_new      |
#____________________________|
disease_new <- 
  shop_interact %>% 
  filter( ! index %in% (1:1025)) %>% 
  select(uid,starts_with("id_disease_problem/")) %>% 
  pivot_longer(!uid, names_to = "disease", values_to = "count") %>% 
  mutate(disease = gsub("id_disease_problem/", "", disease)) %>% 
  filter(count==1) %>% count(disease) %>%  rename(n_new=n)

new_N <- # Extract the total N as an integer
  shop_interact %>%
  filter( ! index %in% (1:1025), !is.na(id_disease_problem)) %>% 
  nrow()


#_____________________________
#   disease combined_data    |
#____________________________|

combined_data <- disease_old %>%
  full_join(disease_new, by = "disease") %>%
  mutate(n_combined = # Combine counts
           coalesce(n_old, 0) + coalesce(n_new, 0),  
         pct = # Calculate percentage
           ifelse(is.na(n_old), n_new / new_N, 
                  n_combined / (old_N + new_N))) %>% 
  mutate(disease = sapply(disease, function(x) toTitleCase(x))) %>%  # Capitalize the first letter
  mutate(disease = case_when(
      disease == "Dont_know_disease" ~ "Don't \nknow",
      disease == "Other_disease" ~ "Other",
      disease == "Top_rot_Pokkah_boeng" ~ "Top rot",
      TRUE ~ as.character(disease)
  )) 
  


#___________________________________________________________
#            disease POLT                                   |
#___________________________________________________________|
total_N = old_N + new_N

# 500 X 400
ggplot(combined_data, aes(x = disease, y = pct)) +
  geom_bar(stat = "identity",fill="tan4") +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(title = paste("What kind of disease are you dealing with?"),
       subtitle = paste("Percent of all respondents, some gave more than one answer | N =", total_N, "\n___________________________________________________________________________________________________"),       x = "", y = "% of farmers") +
  theme_minimal() +
  theme(legend.position = "none", 
        text = element_text(family = "serif"),
        axis.text.x = element_text(size = 12)
  ) +  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3)


### pest  ................................................................. ----
#   What kind of pest are you dealing with?
#..............................................................................

#  "id_pest_problem/white_grub" was added to version 4 - new_data should be added here

pest <- 
  shop_interact %>% 
  filter(!is.na(id_pest_problem)) %>% 
  mutate(version=ifelse(index %in% 1:1025,"old","new")) %>% 
  group_by(version) %>% mutate(N=n()) %>% ungroup() %>% 
  
  select(version,N,starts_with("id_pest_problem/")) %>% 
  pivot_longer(!c(version,N) , 
               names_to = "pest", 
               values_to = "COUNT") %>% 
  mutate(pest = gsub("id_pest_problem/", "", pest)
  ) %>% 
  group_by(version,N,pest) %>% 
  summarise(n=sum(COUNT,na.rm = T)) %>% ungroup()


#____________________________
#      DF pest_old           |
#____________________________|

old_N <- pest %>% filter(version=="old") %>% 
  summarise(N = unique(N)) %>% pull(N)


#____________________________
#      DF pest_old           |
#____________________________|

new_N <- pest %>% filter(version=="new") %>% 
  summarise(N = unique(N)) %>% pull(N)


#____________________________
#   pest combined_data       |
#____________________________|
total_N <- old_N + new_N

combined_data <- pest %>%
  group_by(pest) %>% 
  summarise(n=sum(n,na.rm = T)) %>% ungroup() %>% 
  mutate(pct=n/total_N) %>% 
  
  mutate(
    # consult = as.character(consult),  # Convert factor to character
    pest = factor(
      pest, levels = c("aphids", "caterpillars", "fruitflies", "leafhoppers", 
                       "leafminers", "white_grub", "whiteflies", 
                       "other_pest", "dont_know_pest"))
  )



#___________________________________________________________
#            pest POLT                                      |
#___________________________________________________________|
total_N = old_N + new_N
library(RColorBrewer)

# 800 X 400
ggplot(combined_data, aes(x = pest, y = pct)) +
  geom_bar(stat = "identity",fill="tan4") +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(
    title = paste("What kind of pest are you dealing with??"),
    subtitle = paste("Percent of all respondents, some gave more than one answer | N =", total_N, "\n___________________________________________________________________________________________________"),   
    x = "", y = "% of farmers") +
  theme_minimal() +
  theme(legend.position = "none", 
        text = element_text(family = "serif"),
        axis.text.x = element_text(size = 12)
  ) +  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3) +
    scale_x_discrete(labels = c(# Custom x-axis labels
    "aphids" = "Aphids", 
    "caterpillars" = "Caterpillar", 
    "fruitflies" = "Fruitflie", 
    "leafhoppers" = "Leafhopper", 
    "leafminers" = "Leaf \nMiners",
    "white_grub" = "White \nGrub", 
    "whiteflies" = "Whiteflie", 
    "other_pest" = "Other", 
    "dont_know_pest" = "Don't \nKnow"
  ))






### consult  .............................................................. ----
# Who advised you to buy what you are buying today?
# {consult}
#..............................................................................

# This question has 4 versions of the answer composition
#     version 1     | index 1:116
#     version 2 3 4 | index 117: ____

#________________________________________________________________
consult <- 
  shop_interact %>% 
  mutate(version=ifelse(index %in% 1:116,"old","new")) %>% 
  group_by(version) %>% mutate(N=n()) %>% ungroup() %>% 
  select(id,uid,version,N,consult
         ) %>% 
  separate_rows(consult, sep = " ") %>%  # Split by space and create new rows
  group_by(uid) %>%
  mutate(consult_count = n()) %>%  # Count number of consult values per uid
  filter(!(consult == "consult_other" & consult_count > 1)) %>%  # Remove consult_other when there's more than one consult value
  ungroup() %>%
  select(-consult_count)  %>% filter(consult != "consult_bayer" 
  ) %>% 
  mutate(consult = gsub("consult_", "", consult),
         consult=ifelse(consult == "friend","farmer",consult)) 

  
#________________________________________________________________
# DF consult_old
consult_old <- consult %>% filter(version=="old") %>% 
  count(consult,N) %>% rename(N_old=N, n_old=n)

# DF consult_new
consult_new <- consult %>% filter(version=="new") %>% 
  count(consult,N) %>% rename(N_new=N, n_new=n)

#________________________________________________________________
# consult combined_data   

old_N <- consult_old %>% summarise(N = unique(N_old)) %>% pull(N)
new_N <- consult_new %>% summarise(N = unique(N_new)) %>% pull(N)
total_N = old_N + new_N

combined_data <- consult_old %>%
  full_join(consult_new, by = "consult") %>% 
  mutate(pct = ifelse(is.na(n_old), n_new / N_new, 
                      (n_old +n_new) / (N_old + N_new))
         ) %>% 
  mutate(group = case_when(
    consult == "farmer" ~ "B",  # Group B
    consult == "blf_center_vendor"~ "A",  # Group A
    consult %in% c("app", "google", "youtube") ~ "D",  # Group D
    consult %in% c("other", "no") ~ "E", # Group E
    TRUE ~ "C") # Group C
    ) %>% 
  mutate(consult = factor(consult, 
            levels = c("farmer", "blf_center_vendor", "agronomist", "private_company_agronomist", 
                       "government_agronomist", "app", "google", "youtube", "other", "no")) 
    )

#________________________________________________________________
# consult POLT

library(RColorBrewer)

# 800 X 400
ggplot(combined_data, aes(x = consult, y = pct, fill = group)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(title = paste("Who advised you to buy what you are buying today?"),
       subtitle = paste("Percent of all respondents, some gave more than one answer | N =", total_N, "\n___________________________________________________________________________________________________"),   
       x = "", y = "% of farmers") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "serif")) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3) +
  scale_fill_brewer(palette = "Dark2") +  # Paired /  Pastel1
  scale_x_discrete(labels = c(# Custom x-axis labels
    "farmer" = "Other \nFarmer", "blf_center_vendor" = "Shop \nVendor", 
    "agronomist" = "Agronomist", 
    "private_company_agronomist" = "Agronomist \nPrivate \nCompany", 
    "government_agronomist" = "Agronomist \nGovernment",
    "app" = "App", "google" = "Google", "youtube" = "YouTube", "other" = "Other", 
    "no" = "Didn't \nConsult"
  ))


# consult with the BLF  ....................................................----
# Did you consult with the BLF owner before coming to the store today, or just now?	
#   {consult_BLF}

consult_shop <- BLF_InteractCSV %>% 
  filter(consult_BLF != "") %>% rename(consult_shop_owner=consult_BLF) %>% 
  count(consult_shop_owner) %>% mutate(pct=n/sum(n))%>%   
  mutate(consult_shop_owner= case_when(
    consult_shop_owner== "consult_BLF_call" ~ "Consult on \nthe phone", 
    consult_shop_owner== "consult_BLF_shop_only" ~ "Consult in \nthe shop", 
    consult_shop_owner== "consult_BLF_whatsapp_call" ~ "Sent WhatsApp \nphoto & consult \non the phone",
    TRUE ~ "other"
  ))


BLF_InteractCSV %>% 
  select(X_index,consult_BLF) %>% 
  filter(X_index %in% c(
    1087, # consult_BLF_whatsapp_call | I sent the photo on WhatsApp and spoke to the BLF owner on phone.
    1088, # consult_BLF_shop_only     | had not consulted the BLF owner before coming to the shop.
    1089  # consult_BLF_call          | I just spoke to the BLF owner on the phone.
  ))


total_N <- consult_shop %>% 
  summarise(N = sum(n)) %>% pull(N)

# 400 X 450
ggplot(consult_shop, aes(x = consult_shop_owner, y = pct)) +
  geom_bar(stat = "identity",fill="royalblue4") +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(title = "Did you consult with the BLF owner before \ncoming to the store today, or just now?", 
       subtitle = paste("Percent of all respondents | N =", total_N, "\n___________________________________________________________________________________________________"),   
       x = "", y = "% of responsers") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "serif")) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3)






# credit....................................................................----
# Did you pay some or all of the amount on credit (loan) to the BLF owner?
#   	{buying_credit}

credit_df <- shop_interactCSV %>% 
  select(uid,buying_credit) %>% filter(buying_credit != "") %>%   
  mutate(buying_credit= case_when(
    buying_credit== "loan_entire" ~ "Credit payment \nEntire amount", 
    buying_credit== "loan_no_use" ~ "Did not \ntook credit", 
    buying_credit== "loan_part" ~ "Credit payment \npart of the amount", 
    buying_credit== "loan_use  " ~ "Credit payment",
    TRUE ~ "other"
  )) %>% as_tibble()

credit1 <- credit_df %>% 
  count(buying_credit) %>%  mutate(pct=n/sum(n))

total_N <- credit1 %>% summarise(N = sum(n)) %>% pull(N)

# 400 X 450
ggplot(credit1, aes(x = buying_credit, y = pct)) +
  geom_bar(stat = "identity",fill="royalblue4") +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(title = "Did you took some or all of the amount \non credit from the BLF owner?", 
       subtitle = paste("Percent of all respondents | N =", total_N, "\n___________________________________________________________________________________________________"),   
       x = "", y = "% of responsers") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "serif")) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3)














# COMBINATIONS ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| ----


library(lubridate)

# Create a new column 'month' by extracting the month from the 'start' column
shop <- shop_interact %>%
  select(start,crop_focus ) %>% 
  mutate(crop_focus = sapply(crop_focus, function(x) toTitleCase(x))) %>% # Capitalize the first letter
  separate_rows(crop_focus, sep = " ") %>%  # Split by space and create new rows
  mutate(month_nm = month(start, label = TRUE, abbr = T),  # label = TRUE gives month names, abbr = FALSE gives full names
         month_nu = month(start)) %>% 
  mutate(season = case_when(
    month_nm %in% c("Jun", "Jul", "Aug", "Sep") ~ "Monsoon",  # Monsoon season
    month_nm %in% c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar") ~ "Rabi",  # Rabi season
    TRUE ~ "Summer"  # April and May are classified as "Others"
  ))

# Create month data frame
months_df <- data.frame(
  month_nm = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  month_nu = 1:12) %>% 
  mutate(season = case_when(
    month_nm %in% c("Jun", "Jul", "Aug", "Sep") ~ "Monsoon",  # Monsoon season
    month_nm %in% c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar") ~ "Rabi",  # Rabi season
    TRUE ~ "Summer"  # April and May are classified as "Others"
  )) 

# Distribution of questions by month ------

# CROP | month    ----

#|=============================================================================
# PLOT Sugarcane crop

library(tidyr)
library(RColorBrewer)


df2_crop <- 
  shop %>% 
  select(season, month_nm,crop_focus) %>% 
  mutate(crop_focus = strsplit(crop_focus, " ")) %>%
  unnest(crop_focus)%>% filter(crop_focus == "Sugarcane") %>% 
  count(season,crop_focus,month_nm)%>% 
  mutate(N=sum(n), pct=n/N) %>% ungroup() %>% full_join(months_df) %>% 
  mutate(month_nm = factor(month_nm, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")))   # Adjust month order
  
ggplot(df2_crop, aes(x = month_nm, y = pct, fill = season)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Monsoon" = "skyblue2", "Rabi" = "tan4", "Summer" = "gray")) +  # Set custom colors
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(title = "Sugarcane Crop by Month",
    x = "",y = "% of farmers",fill = "Season") +  # Label for the legend
  theme_minimal() +
  theme(legend.position = "right", 
        text = element_text(family = "serif"),
        axis.text.x = element_text(size = 12))+
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3)


#|=============================================================================
# PLOT different crops

df3_crop <- 
  shop %>% filter(
    crop_focus %in% c("Sugarcane", "Maize", "Chili","Tomato") ) %>% 
  select(month_nm,crop_focus) %>% 
  mutate(crop_focus = strsplit(crop_focus, " ")) %>%
  unnest(crop_focus)%>% count(crop_focus,month_nm)%>% 
  group_by(crop_focus) %>% mutate(N=sum(n), pct=n/N) %>% ungroup() %>% 
  select(crop_focus, month_nm,pct)

library(ggplot2)
library(RColorBrewer)

ggplot(df3_crop, aes(x = month_nm, y = pct, color = crop_focus, group = crop_focus)) +
  geom_line(linewidth = 1) +  # Use geom_line for the line plot
  scale_color_brewer(palette = "BrBG") +  # Use color palette from RColorBrewer
  labs(title = "Crop Percentage by Month",
    x = "", y = "% of Farmers", color = "Crop"  # Label for the legend
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    text = element_text(family = "serif"),
    axis.text.x = element_text(size = 12) )





# 
# AGE df [farmer_AGE]  ------

farmer_AGE <- shop_interactCSV %>% select(id,uid,farmer_age) %>% 
  filter(!is.na(farmer_age)) %>% 
  mutate(age_grp1 = case_when(
    farmer_age >= 1 & farmer_age <= 25 ~ "<25",
    farmer_age >= 26 & farmer_age <= 35 ~ "25-35",
    farmer_age >= 36 & farmer_age <= 45 ~ "35-45",
    TRUE ~ "45<"
  )) %>% 
  mutate(age_grp2 = case_when(
    farmer_age >= 1 & farmer_age <= 35 ~ "<35",
    TRUE ~ "35<" ))

age_above35_N <- farmer_AGE %>% count(age_grp2) %>% filter(age_grp2=="<35") %>%  pull(n)
age_below35_N <- farmer_AGE %>% count(age_grp2) %>% filter(age_grp2=="35<") %>%  pull(n)
total_N = age_above35_N + age_below35_N

# AGE |  consult 2 3   ------

#______________________________________________________________
# consult3 | by age

consult2 <- 
  consult %>% 
  select(id,consult) %>% 
  right_join(farmer_AGE ) %>% filter(!is.na(consult)) %>%
  group_by(consult) %>% 
  summarise(farmer_age=mean(farmer_age)) %>% 
  mutate(consult = factor(consult, 
                          levels = c("farmer", "blf_center_vendor", "agronomist", "private_company_agronomist", 
                                     "government_agronomist", "app", "google", "youtube", "other", "no")) 
  )

library(RColorBrewer)
total_N = age_above35_N + age_below35_N

# PLOT
consult2 %>%
  ggplot(aes(x = consult, y = farmer_age )) +
  geom_bar(stat = "identity") +
  labs(title = paste("Who was consulted with | by age"),
       subtitle = paste(" N =", total_N,"\n___________________________________________________________________________________________________"),   
    x = "", y = "Age") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "serif")) +
  scale_x_discrete(labels = c(# Custom x-axis labels
    "farmer" = "Other \nFarmer", "blf_center_vendor" = "Shop \nVendor", 
    "agronomist" = "Agronomist", "private_company_agronomist" = "Agronomist \nPrivate \nCompany", 
    "government_agronomist" = "Agronomist \nGovernment","app" = "App", 
    "google" = "Google", "youtube" = "YouTube", "other" = "Other", "no" = "Did't \nConsult"
  ))

#______________________________________________________________
# consult3 | by age group"
consult3 <- consult %>% 
  select(id,consult) %>% 
  right_join(farmer_AGE ) %>% 
  count(consult,age_grp2) %>% filter(!is.na(consult)) %>% 
  # filter(consult != "other") %>% group_by(age_grp2) %>% mutate(N=sum(n),pct=n/N)
  mutate(pct=ifelse(age_grp2=="<35",n/age_below35_N,n/age_above35_N))

consult_vars  <- tibble(
  consult = c("app","private_company_agronomist"),
  age_grp2 = c("35<","<35"),
  n = c(0,0), pct = c(0,0))

# consult3 PLOT
consult3 %>%
  full_join(consult_vars) %>% 
  mutate(consult = factor(consult, 
                          levels = c("farmer", "blf_center_vendor", "agronomist", "private_company_agronomist", 
                                     "government_agronomist", "app", "google", "youtube", "other", "no")) 
  ) %>%  
  ggplot(aes(x = consult, y = pct, fill=age_grp2 )) +
  geom_bar(stat = "identity", position=position_dodge())+
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  
  labs(title = paste("Who was consulted with | by age group"),
       subtitle = paste("Percent of Age group | N 35< =", age_below35_N, " | N 35< =", age_above35_N,"\n___________________________________________________________________________________________________"),   
       x = "", y = "% of farmers",fill = "Age Group" ) +
  theme_minimal() +
  theme(text = element_text(family = "serif")) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            position = position_dodge(0.9),
            vjust = -0.5, color = "black", size = 3) +
  scale_fill_brewer(palette="Paired")+
  
  scale_x_discrete(labels = c(
    "farmer" = "Other \nFarmer", "blf_center_vendor" = "Shop \nVendor", 
    "agronomist" = "Agronomist", "private_company_agronomist" = "Agronomist \nPrivate \nCompany", 
    "government_agronomist" = "Agronomist \nGovernment","app" = "App", 
    "google" = "Google", "youtube" = "YouTube", "other" = "Other", "no" = "Did't \nConsult"
  ))









# AGE | buying_credit  ....................................................----

credit2 <- credit_df %>% 
  full_join(farmer_AGE) %>% 
count(buying_credit,age_grp2) %>% 
  group_by(age_grp2) %>% 
  mutate(N=sum(n),pct=n/N)

credit2 %>% 
  ggplot(aes(x = buying_credit,y = pct, fill=age_grp2 )) +
  geom_bar(stat = "identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(title = "Did you took some or all of the amount on credit from the BLF owner?", 
       subtitle = paste("Percent of Age group | N 35< =", age_below35_N, " | N 35< =", age_above35_N,"\n___________________________________________________________________________________________________"),   
       x = "", y = "% of responses") +
  theme_minimal() +
  theme(text = element_text(family = "serif")) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            position = position_dodge(0.9),
            vjust = -0.5, color = "black", size = 3)







# ARCHIVE  -----




shop_interact %>% filter(!is.na(consult)) %>% 
  select(id,uid,consult) %>% 
  separate_rows(consult, sep = " ") %>%  # Split by space and create new rows
  group_by(uid) %>%
  mutate(consult_count = n()) %>%  # Count number of consult values per uid
  filter(!(consult == "consult_other" & consult_count > 1)) %>%  # Remove consult_other when there's more than one consult value
  ungroup() %>%
  select(-consult_count) %>% 
  right_join(farmer_AGE) %>% 
  count(consult,age_grp2)











# crop_focus_other
shop_interact %>% filter(!is.na(crop_focus)) %>% 
  select(crop_focus,crop_focus_other) %>% 
  mutate(crop_focus = strsplit(crop_focus, " ")) %>%
  unnest(crop_focus) %>% 
  mutate(crop = ifelse(
    crop_focus == "other_crop_1",crop_focus_other, ifelse(
      crop_focus == "other_crop",crop_focus_other,crop_focus))
  ) %>% 
  count(crop) %>% mutate(n/sum(n)*100)




mutate(
  # consult = as.character(consult),  # Convert factor to character
  reason_come_in = factor(reason_come_in, 
                          levels = c("disease_issue", "pest_problems", "weed_control", "cropping_system","soil_treatment","irrigation",
                                     "need_equipment", "need_fertilizer", "need_seeds", 
                                     "Other")) )%>% 
  mutate(
    group = case_when(
      reason_come_in %in% c("disease_issue", "pest_problems", "weed_control", "cropping_system","soil_treatment","irrigation" ) ~ "C",  # Group C
      reason_come_in %in% c("need_equipment", "need_fertilizer", "need_seeds") ~ "D",  # Group D
      reason_come_in =="Other" ~ "E")
  )