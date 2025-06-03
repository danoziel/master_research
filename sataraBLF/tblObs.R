library(dplyr)
library(tidyr)

library(readxl)
tblOBS <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/BLF_satara/BLF_Observations_-_all_versions_-_False_-_2025-05-28-13-02-36.xlsx")

names(tblOBS)
tblObs<- 
  tblOBS %>% filter(!is.na(crop_grow)) %>% 
  rename(phone=Farmer_Phone_Number) %>% 
  mutate(phone=as.numeric(phone) ) %>% 
  mutate(start = as.Date(start),end = as.Date(end)) # %>% 
  select(phone ,Farmer_Name ,start ,end  )

shop_interact     # df from blf2.R
shop_interactCSV  # df from blf2.R
tblIntCSV <- shop_interactCSV


tblInt <- shop_interact %>%
  # select(phone, name, start, end) %>% 
  mutate(start = as.Date(start),end = as.Date(end)) %>% 
  filter(start >= as.Date("2025-01-23"),end <= as.Date("2025-04-03") )

library(tidyr)
tblComb <- inner_join(tblObs,tblInt,by = join_by(phone,end)) %>% 
  select(phone, end,
         starts_with("crop_grow/"),
         starts_with("id_pest_problem/"),
         starts_with("id_disease_problem/"))
  

# 8605963016
# 9765884316


tblComb <- inner_join(tblObs,tblInt,by = join_by(phone,end)) %>% 
  # 1. keep only phone, end, and columns ending in .x or .y
  select(
    phone, 
    end, 
    ends_with(".x"), 
    ends_with(".y")
  ) %>% 
  # 2. pivot to long (crop + suffix), then back to wide as crop_grow_x / crop_grow_y
pivot_longer(
  cols = -c(phone, end),
  names_to = c("var", "suffix"),
  names_pattern = "^var/([^\\.]+)\\.([xy])$",
  values_to = "val"
)

library(dplyr)
library(tidyr)

tbl_processed <- tblComb %>%
  # 1. keep only phone, end, and columns ending in .x or .y
  select(
    phone, 
    end, 
    ends_with(".x"), 
    ends_with(".y")
  ) %>%
  # 2. pivot to long (crop + suffix), then back to wide as crop_grow_x / crop_grow_y
  pivot_longer(
    cols = -c(phone, end),
    names_to = c("crop", "suffix"),
    names_pattern = "^crop_grow/([^\\.]+)\\.([xy])$",
    values_to = "val"
  ) %>%
  pivot_wider(
    names_from  = suffix,
    values_from = val,
    names_prefix = "crop_grow_"
  ) %>%
  # 3. new indicator column
  mutate(
    crop_flag = case_when(
      crop_grow_x == 1 & crop_grow_y == 1 ~ "1",
      crop_grow_x == 0 & crop_grow_y == 0 ~ "0",
      crop_grow_x == 1 & crop_grow_y == 0 ~ "x",
      crop_grow_x == 0 & crop_grow_y == 1 ~ "y",
      TRUE                               ~ NA_character_
    )
  )

# View the result
tbl_processed







tblComb <- inner_join(tblObs,tblInt,by = join_by(phone,end)) %>% 
  rename(farmerID=phone, location=location.y) %>% 
  mutate(across(everything(), as.character)) %>% 
  select(farmerID, end,location, matches("/.*\\.(x|y)$") # keep any column containing “/” and ending in .x or .y
  ) %>%  pivot_longer(
    cols= -c(farmerID, end,location),
    names_to    = c("subject", "topic", "suffix"),
    names_pattern = "([^/]+)/(.+)\\.([xy])$",values_to   = "value"
  )%>% pivot_wider( 
    id_cols= c(location,farmerID, end, subject, topic),
    names_from  = suffix,
    values_from = value,
    names_prefix = ""
  ) %>% mutate(
    crop_flag = case_when(x == 1 & y == 1 ~ "1",
                          x == 0 & y == 0 ~ "0",
                          x == 1 & y == 0 ~ "x",
                          x == 0 & y == 1 ~ "y",TRUE ~ NA_character_)) 

tblComb %>% filter(crop_flag != "0") %>% 
  count(farmerID, subject,crop_flag)


tblComb %>% filter(crop_flag != "0") %>% 
  count(subject,crop_flag) %>% 
  mutate(match= ifelse(crop_flag == "1","Match","No_Match")) %>% 
  group_by(subject,match) %>% summarise(total=sum(n), .groups = "drop") %>% 
  pivot_wider(
    id_cols     = c(subject),
    names_from  = "match" ,
    values_from = "total"
  ) %>% mutate(No_Match=ifelse(is.na(No_Match),0,No_Match ),
               pct_Matching=(1-(No_Match/Match))*100 ) %>% 
  kableExtra::kable() %>% kable_styling()
  




# COR location

matching_tbl <- 
  tblComb %>% filter(crop_flag != "0") %>% count(location,crop_flag) %>% 
  mutate(match= ifelse(crop_flag == "1","Match","No_Match")) %>% 
  group_by(location,match) %>% summarise(total=sum(n), .groups = "drop") %>% 
  pivot_wider(id_cols= c(location),names_from = "match" ,values_from = "total") %>%
  mutate(No_Match=ifelse(is.na(No_Match),0,No_Match ),pct_Matching=(1-(No_Match/Match))*100 )

tb1 <- tblInt %>% count(location) %>% 
  mutate(pct_of_sample=n/sum(n)*100) %>% rename(respondents =n)

full_join(matching_tbl,tb1) %>% 
  kableExtra::kable() %>% kable_styling()








