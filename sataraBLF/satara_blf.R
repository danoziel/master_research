library(dplyr)
library(ggplot2)
library(tidyr)

library(readxl)
#import df ----
BLF_Interact_Final <- read_excel("C:/Users/Dan/Downloads/BLF_Interact_Final.xlsx", sheet = "Y24_25")
View(BLF_Interact_Final)



# What crops do you grow?  ----

df_farmer_crop <- BLF_Interact_Final %>%
  select(crop_Okra:crop_Rice) %>%
  summarise(across(everything(), ~mean(. == 1, na.rm = TRUE) * 100))

df_farmer_crop_long <- df_farmer_crop %>%
  pivot_longer(cols = everything(), names_to = "crop", values_to = "percentage") %>% 
  mutate(
    percentage = round(percentage, 1),
    crop = gsub("crop_", "", crop)    
  )


# bar plot
ggplot(df_farmer_crop_long, aes(x = crop, y = percentage, fill = crop)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Fraction of crop types",
       subtitle = "(out of responses)",
       x = "",
       y = "Fraction of crop") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "YlOrBr") +
  geom_text(aes(label = paste0(round(percentage), "%")), vjust = -0.5)



# How many acres of land do you farm?  ----
land <- 
  BLF_Interact_Final %>% 
  select(farm_acre2, farmer_experience, msg_consultation ) %>% 
  mutate(consultation=ifelse(msg_consultation=="no",0,1)) %>% 
  select(-"msg_consultation")
land$farmer_experience[land$farmer_experience=="Less than 5 years"] <- "0-5 years"
land$farmer_experience[land$farmer_experience=="More than 20 years"] <- "20+ years"

  

# How long have you been a farmer?  ----
# Table
exp <- BLF_Interact_Final %>% count(farmer_experience) %>% 
  mutate(N=sum(n),pct = paste0(round(n / N * 100), "%"))
exp$farmer_experience[exp$farmer_experience=="Less than 5 years"] <- "0-5 years"
exp$farmer_experience[exp$farmer_experience=="More than 20 years"] <- "20+ years"

exp %>% arrange() %>%  select(-N) %>% kbl() %>% kable_styling()



## What crop have you come to shop for? -----

df_shop_crop <- BLF_Interact_Final %>%
  select(shopOkra:shopRice) %>%
  summarise(across(everything(), ~mean(. == 1, na.rm = TRUE) * 100))


df_shop_crop_long <- df_shop_crop %>%
  pivot_longer(cols = everything(), names_to = "crop", values_to = "percentage") %>% 
  mutate(
    percentage = round(percentage, 1),
    crop = gsub("shop", "", crop)    
  )


# COMBINE  df_shop_crop_long df_farmer_crop_long

d1 <- df_farmer_crop_long %>% 
  mutate(col1="Farmer Crop" )

d2 <- df_shop_crop_long %>% 
  mutate(col1="Farmer Crop shop" )

d12 <- 
  rbind(d1,d2) %>% 
  filter(crop != "Rice" )

# bar plot

ggplot(d12, aes(x = crop, y = percentage, fill = col1)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "What crops do you grow? \nWhat crop have you come to shop for?",
       subtitle = "(out of responses)",
       x = "",
       y = "Fraction of crop") +
    geom_text(aes(label = paste0(round(percentage), "%")), vjust = -0.6,
            position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()



## What problem did you come up with for this crop today? ----


df_crop_prob <- BLF_Interact_Final %>%
  select(crop_problem_seeds:crop_problem_other) %>%
  summarise(across(everything(), ~mean(. == 1) * 100))

df_crop_prob_long <- df_crop_prob %>%
  pivot_longer(cols = everything(), names_to = "crop", values_to = "percentage") %>% 
  mutate(
    percentage = round(percentage, 1),
    crop = gsub("crop_problem_", "", crop)    
  )


# bar plot 
ggplot(df_crop_prob_long, aes(x = crop, y = percentage, fill = crop)) +
  geom_bar(stat = "identity", fill = "royalblue4") +
  theme_minimal() +
  labs(title = "What problem did you come up with for this crop today?",
       subtitle = "(out of responses)",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Purples") +
  geom_text(aes(label = paste0(round(percentage), "%")), vjust = -0.5)


# Is it a pest problem? ----
# Table 
BLF_Interact_Final %>% count(pest_problem_yn) %>% 
  mutate(N=sum(n),pct = paste0(round(n / N * 100), "%"))%>% 
  select(-N) %>% kbl() %>% kable_styling()


# If it is a pest problem, what kind of pest are you dealing with?

df_pest_prob <- BLF_Interact_Final %>%
  select(starts_with("pest"), -c(pest_problem_yn,pest_problem_kind_name,pest_problem_other_name )) %>%
  summarise(across(everything(), ~mean(. == 1, na.rm = TRUE) * 100))

df_pest_prob_long <- df_pest_prob %>%
  pivot_longer(cols = everything(), names_to = "crop", values_to = "percentage") %>% 
  mutate(
    percentage = round(percentage, 1),
    crop = gsub("pest_problem_", "", crop),  
    crop = gsub("kind_", "", crop)
  )  

# bar plot
df_pest_prob_long %>% 
  filter(crop!="other" ) %>% 
ggplot(aes(x = crop, y = percentage)) +
  geom_bar(stat = "identity", fill = "royalblue4") +  # Set fill color to royalblue for all bars
  theme_minimal() +
  labs(title = "What kind of pest are you dealing with?",
       subtitle = "(out of responses)",
       x = "",
       y = "") +
  theme(legend.position = "none") +
  geom_text(aes(label = paste0(round(percentage), "%")), vjust = -0.5)






# disease_problem_yn ----
# Table
BLF_Interact_Final %>% count(disease_problem_yn) %>% 
  mutate(N=sum(n),pct = paste0(round(n / N * 100), "%"))%>% 
  select(-N) %>% kbl() %>% kable_styling()

# If it is a disease problem, what kind of disease are you dealing with?
disease_prob <- BLF_Interact_Final %>%
  select(disease_problem_donKnow:disease_problem_other ) %>%
  summarise(across(everything(), ~mean(. == 1, na.rm = TRUE) * 100))

disease_prob_long <- disease_prob %>%
  pivot_longer(cols = everything(), names_to = "crop", values_to = "percentage") %>% 
  mutate(
    percentage = round(percentage, 1),
    crop = gsub("disease_problem_", "", crop)
    )

# bar plot 
df_pest_prob_long %>% 
  filter(crop!="other" ) %>% 
  ggplot(aes(x = crop, y = percentage)) +
  geom_bar(stat = "identity", fill = "royalblue4") +
  theme_minimal() +
  labs(title = "What kind of disease are you dealing with?",
       subtitle = "(out of responses)",
       x = "",
       y = "") +
  theme(legend.position = "none") +
  geom_text(aes(label = paste0(round(percentage), "%")), vjust = -0.5)







# Did you consult with anyone else before coming to the BLF Center? ----
consultation_prob_1 <- BLF_Interact_Final %>%
  select(msg_consultation_another_farmer:msg_consultation_BLF_Vendor ) 

names(consultation_prob)
consultation_prob_2 <- consultation_prob_1 %>% 
  mutate(Another_farmer= 
           ifelse(msg_consultation_friends==1,1,
                  msg_consultation_another_farmer ))%>% 
  mutate(Expert= 
           ifelse(msg_consultation_agricultural_experts==1,1,
                  msg_consultationPrivate_Company_Agricultural_Expert )) %>% 
  summarise(across(everything(), ~mean(. == 1, na.rm = TRUE) * 100))

consultation_prob_long <- consultation_prob_2 %>%
  pivot_longer(cols = everything(), names_to = "ans", values_to = "percentage") %>% 
  mutate(
    ans = gsub("msg_consultation_", "", ans)
  ) %>% filter(! ans %in% c("friends","another_farmer","agricultural_experts","msg_consultationPrivate_Company_Agricultural_Expert" ))
consultation_prob_long$ans[consultation_prob_long$ans=="no"] <- "Not consulting"


# bar plot
consultation_prob_long %>%
  mutate(ans = factor(ans, levels = c("Not consulting", "BLF_Vendor", "Another_farmer", "Expert","GovAgronomist",
                                      "other",
                                      "Google","YouTube","Mobile_App","BayerSeminis_WhatsApp_Chatbot"))) %>%
  ggplot(aes(x = ans, y = percentage)) +
  geom_bar(stat = "identity", fill = "royalblue4") +
  theme_minimal() +
  labs(title = "Did you consult with anyone else before coming to the BLF Center?",
       subtitle = "(out of responses)",
       x = "",
       y = "") +
  theme(legend.position = "none") +
  geom_text(aes(label = paste0(round(percentage), "%")), vjust = -0.5)


# Do you want to use Whatsapp Chatbot for consultation with Bayer Seminis or similar? ----
# Table 
BLF_Interact_Final %>% count(want_msg_consultation) %>% 
  mutate(N=sum(n),pct = paste0(round(n / N * 100), "%"))%>% 
  select(-N) %>% kbl() %>% kable_styling()





# farm size ----

# Calculate the average farm size for each experience group
land$farm_acre2= as.numeric(land$farm_acre2)
# hist(land$farm_acre2)
unique(land$farm_acre2)

land %>%
  filter(!is.na(farm_acre2)) %>%
  group_by(farmer_experience) %>%
  summarise(avg_farm_size = mean(farm_acre2, na.rm = TRUE))

# Check unique values in farm_acre2
unique(land$farm_acre2)

land %>%
  filter(farm_acre2!=5100) %>% 
  mutate(farm_acre = as.numeric(farm_acre2)) %>%
  filter(!is.na(farm_acre)) %>% # Remove rows with NA values in farm_acre
  group_by(farmer_experience) %>%
  summarise(avg_farm_size = mean(farm_acre, na.rm = TRUE))








