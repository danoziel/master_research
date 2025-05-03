
library(dplyr)
library(tools)
library(tidyr)
library(ggplot2)
library(scales)

library(readxl)
BLF_Interact <-    read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/BLF_satara/BLF_Interact_-_all_versions_-_False_-_2025-04-30-18-15-56.xlsx")
BLF_Interact_Qs <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/BLF_satara/BLF_Interact_-_all_versions_-_English_en_-_2025-04-30-18-17-43.xlsx")
BLF_InteractCSV <- read.csv("C:/Users/Dan/Downloads/BLF_Interact_-_all_versions_-_labels_-_2025-05-03-20-01-16.csv", sep=";")
BLF_InteractCSV <- BLF_InteractCSV %>% filter(!is.na (X_index))

### uid and clean [shop_interact] ........................................ ----

# Few significant updates were made to the questionnaire
#     version 1 | 2024.10.24 - 2024.10.24 | _index 1:8
#     version 2 | 2024.10.24 - 2024.11.26 | _index 9:116
#     version 3 | 2024.11.26 - 2025.04.06 | _index 117:1025
#     version 4 | 2025-04-06 - ____.__.__ | _index 1026: ____

# Create uniqe id by version
library(tools)

shop_interact <- BLF_Interact %>% rename(index=`_index` ) %>%   
  mutate(uid = case_when(
    index >= 1 & index <= 8 ~ paste0("A", phone),
    index >= 9 & index <= 116 ~ paste0("B", phone),
    index >= 117 & index <= 1025 ~ paste0("C", phone),
    TRUE ~ paste0("D", phone)
  )) %>% 
  mutate(crop_focus = sapply(crop_focus, function(x) toTitleCase(x))) %>% # Capitalize the first letter
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
  count(crop) %>% mutate(pct=n/sum(n))

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
  theme(legend.position = "none", text = element_text(family = "serif")) +
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


# farmer_age    00000000 ----
# Age: 	
#   [farmer_age]


#Shop Location  00000000 ----
# What is the name of the village where BLF is located?
#   	[location]



### reason_come_in_new ...................................  ----
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
  geom_bar(stat = "identity",, fill="mediumpurple3") +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(title = paste("What problem did you come up with for this crop today?"),
       subtitle = paste("Percent of all respondents, some gave more than one answer | N =", total_N, "\n___________________________________________________________________________________________________"),       
       x = "", y = "% of farmers") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "serif")) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3)


### disease .............................................................. ----
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
                  n_combined / (old_N + new_N)))


#___________________________________________________________
#            disease POLT                                   |
#___________________________________________________________|
total_N = old_N + new_N

# 800 X 400
ggplot(combined_data, aes(x = disease, y = pct)) +
  geom_bar(stat = "identity",, fill="mediumpurple3") +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(title = paste("What kind of disease are you dealing with?"),
       subtitle = paste("Percent of all respondents, some gave more than one answer | N =", total_N, "\n___________________________________________________________________________________________________"),       x = "", y = "% of farmers") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "serif")) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3)


### pest  .............................................................. ----
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
  geom_bar(stat = "identity",, fill="mediumpurple4") +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(
    title = paste("What kind of pest are you dealing with??"),
    subtitle = paste("Percent of all respondents, some gave more than one answer | N =", total_N, "\n___________________________________________________________________________________________________"),   
    x = "", y = "% of farmers") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "serif")) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3) +
    scale_x_discrete(labels = c(# Custom x-axis labels
    "aphids" = "Aphids", 
    "caterpillars" = "Caterpillars", 
    "fruitflies" = "Fruitflies", 
    "leafhoppers" = "Leafhoppers", 
    "leafminers" = "Leaf Miners",
    "white_grub" = "White Grub", 
    "whiteflies" = "Whiteflie", 
    "other_pest" = "Other", 
    "dont_know_pest" = "Don't Know"
  ))






### consult  .............................................................. ----
# Who advised you to buy what you are buying today?
# {consult}
#..............................................................................

# This question has 4 versions of the answer composition
#     version 1     | index 1:116
#     version 2 3 4 | index 117: ____

#..............................................
consult <- 
  shop_interact %>% 
  mutate(version=ifelse(index %in% 1:116,"old","new")) %>% 
  group_by(version) %>% mutate(N=n()) %>% ungroup() %>% 
  select(version,N,starts_with("consult/")) %>% 
  pivot_longer(!c(version,N) , 
               names_to = "consult", 
               values_to = "COUNT") %>% 
  mutate(consult = gsub("consult/", "", consult),   
         consult = gsub("consult_", "", consult),
         consult=ifelse(consult == "friend","farmer",consult)
  ) %>% 
  filter(consult != "bayer") %>%  
  group_by(version,N,consult) %>% 
  summarise(n=sum(COUNT,na.rm = T)) %>% ungroup()
  
#____________________________
#      DF consult_old        |
#____________________________|
consult_old <- consult %>% 
  filter(version == "old") %>% select(-version) %>% 
  rename(N_old=N, n_old=n) %>% select(consult,n_old,N_old)

old_N <- consult_old %>% 
  summarise(N = unique(N_old)) %>% pull(N)


#____________________________
#      DF consult_new        |
#____________________________|

consult_new <- consult %>% 
  filter(version == "new") %>% select(-version) %>% 
  rename(N_new=N, n_new=n) %>% select(consult,n_new,N_new)

new_N <- consult_new %>% 
  summarise(N = unique(N_new)) %>% pull(N)

#____________________________
#   consult combined_data    |
#____________________________|

combined_data <- consult_old %>%
  full_join(consult_new, by = "consult") %>%
  mutate(n_combined = # Combine counts
           coalesce(n_old, 0) + coalesce(n_new, 0),  
         pct = # Calculate percentage
           ifelse(n_old==0, n_new / N_new, 
                  n_combined / (N_old + N_new))) %>% 
  mutate(
    consult = as.character(consult),  # Convert factor to character
    consult = factor(consult, levels = c("farmer", "blf_center_vendor", "agronomist", "private_company_agronomist", 
                                         "government_agronomist", "app", "google", "youtube", "other", "no")),
    group = case_when(
      consult == "farmer" ~ "B",  # Group B
      consult == "blf_center_vendor" ~ "A",  # Group A
      consult %in% c("agronomist", "private_company_agronomist", "government_agronomist") ~ "C",  # Group C
      consult %in% c("app", "google", "youtube") ~ "D",  # Group D
      consult %in% c("other", "no") ~ "E"  # Group E
    )
  )

#___________________________________________________________
#            consult POLT                                   |
#___________________________________________________________|
total_N = old_N + new_N
library(RColorBrewer)

# 800 X 400
ggplot(combined_data, aes(x = consult, y = pct, fill = group)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(
    title = paste("Who advised you to buy what you are buying today?"),
    subtitle = paste("Percent of all respondents, some gave more than one answer | N =", total_N, "\n___________________________________________________________________________________________________"),   
    x = "", y = "% of farmers") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "serif")) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3) +
  scale_fill_brewer(palette = "BrBG") +  # Paired /  Pastel1
  scale_x_discrete(labels = c(# Custom x-axis labels
    "farmer" = "Other \nFarmer", 
    "blf_center_vendor" = "Shop \nVendor", 
    "agronomist" = "Agronomist", 
    "private_company_agronomist" = "Agronomist \nPrivate \nCompany", 
    "government_agronomist" = "Agronomist \nGovernment",
    "app" = "App", 
    "google" = "Google", 
    "youtube" = "YouTube", 
    "other" = "Other", 
    "no" = "Did't \nConsult"
  ))


# consult with the BLF  .......................................----
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






# credit    0000000----
# Did you pay some or all of the amount on credit (loan) to the BLF owner?
#   	{buying_credit}




credit <- BLF_InteractCSV %>% 
  select(buying_credit) %>% filter(buying_credit != "") %>% 
  count(buying_credit) %>%  mutate(pct=n/sum(n))%>%   
  mutate(buying_credit= case_when(
    buying_credit== "loan_entire" ~ "Credit payment \nEntire amount", 
    buying_credit== "loan_no_use" ~ "Did not \ntook credit", 
    buying_credit== "loan_part" ~ "Credit payment \npart of the amount", 
    buying_credit== "loan_use  " ~ "Credit payment",
    TRUE ~ "other"
  ))

total_N <- credit %>% 
  summarise(N = sum(n)) %>% pull(N)
# 400 X 450
ggplot(credit, aes(x = buying_credit, y = pct)) +
  geom_bar(stat = "identity",fill="royalblue4") +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(title = "Did you took some or all of the amount \non credit from the BLF owner?", 
       subtitle = paste("Percent of all respondents | N =", total_N, "\n___________________________________________________________________________________________________"),   
       x = "", y = "% of responsers") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "serif")) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3)












# ARCHIVE  -----

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