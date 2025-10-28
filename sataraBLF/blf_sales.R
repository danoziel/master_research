
#|-------------------------------------------------------------------------------
#    Review sales records                                                   ----
#|------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(kableExtra)


library(readxl)
shop_sales <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/BLF_satara/BLF_Interact_-_all_versions_-_False_-_2025-10-12-07-38-57.xlsx")

shop_sales <- shop_sales %>% 
  rename( id = `_id`, uuid =`_uuid`)

## df_index 
df_index <- shop_sales %>% 
  filter(
    str_starts(weed_control_photo_URL, "https") |
    str_starts(pest_problem_photo_URL, "https") |
    str_starts(disease_problem_photo_URL, "https"))


### [df_prblm_url]  ...................... ----

df_prblm_url <- df_index  %>% 
  select( id, uuid, end ,
          `reason_come_in_new/weed_control`,
          starts_with("id_pest_problem/"),
          starts_with("id_disease_problem/"),
          weed_control_photo_URL, 
          pest_problem_photo_URL, 
          disease_problem_photo_URL)
  


df_pest <- df_prblm_url  %>% 
  select(id,uuid, end,
         starts_with("id_pest_problem/"),
         pest_problem_photo_URL) %>% 
  pivot_longer( -c(id, uuid,end,pest_problem_photo_URL ),
                names_to = "problem",
                values_to = "problem_specific" ) %>% 
  filter(problem_specific==1)%>% select(problem,everything()) %>% 
  mutate(problem_specific = str_replace(problem , "id_pest_problem/", "")) %>% 
  mutate(problem_category ="Pest") %>% 
  mutate(problem = gsub("/", ".", problem)) %>% 
  rename(product_URL=pest_problem_photo_URL)

df_disease <- df_prblm_url  %>% 
  select(id,uuid, end,
         starts_with("id_disease_problem/"),
         disease_problem_photo_URL) %>% 
  pivot_longer( -c(id, uuid,end,disease_problem_photo_URL ),
                names_to = "problem",
                values_to = "problem_specific" ) %>% 
  filter(problem_specific==1)%>% select(problem,everything()) %>% 
  mutate(problem_specific = str_replace(problem , "id_disease_problem/", "")) %>% 
  mutate(problem_category ="Disease") %>% 
  mutate(problem = gsub("/", ".", problem)) %>% 
  rename(product_URL=disease_problem_photo_URL)

df_weed <- df_prblm_url  %>% 
  select(id,uuid, end,`reason_come_in_new/weed_control`,weed_control_photo_URL) %>% 
  filter(`reason_come_in_new/weed_control`==1) %>% 
  mutate(
    problem = "id_weeds_problem.weed_control", 
    problem_specific = "weed_control",
    problem_category ="Weeds"
  ) %>% rename(product_URL=weed_control_photo_URL) %>% 
  select(problem,id,uuid, end,product_URL, problem_specific,problem_category)


### [df_consult] ...................... -----
df_consult <- 
  df_index%>% 
  select( id, uuid, end , consult) %>% 
  separate_wider_delim(consult, delim = " ", names_sep = "_",
                       too_few = "align_start"   # fills missing pieces with NA instead of error
  ) %>% select( id, consult_1,uuid, end) %>% 
  mutate(consult_1 = str_replace(consult_1, "consult_", "")) %>% 
  rename(consult =consult_1) %>% 
  mutate(
    consult = case_when(
      consult == "no" ~"Self",
      consult == "blf_center_vendor" ~"Shop_owner",
      consult %in% c("agronomist","government_agronomist") ~ "Gov_agronomist",
      TRUE ~ consult )) %>% 
  mutate(consult = str_to_sentence(consult))

length(df_consult$id)
length(unique(df_consult$id))


### [df_crop]   ...................... ----

# df_crop_focus
df_crop_focus <- df_index %>% 
  select( id, uuid, end, 
          crop_focus ) %>% 
  mutate(crop_focus=ifelse(crop_focus=="other_crop","other_crop_1",crop_focus ))
  
  # df_crop_other
  df_crop_other_1 <- df_index %>% 
  select( id, uuid, end, crop_focus_other ,crop_focus) %>%
  mutate(crop_focus=ifelse(crop_focus=="other_crop","other_crop_1",crop_focus )) %>% 
  filter(!is.na(crop_focus_other))

df_crop_other_1 %>% count(crop_focus_other)

df_crop_other_2 <- df_crop_other_1 %>% 
  mutate(
    crop_other = case_when(
      str_detect(crop_focus_other, "soyabean|soyaben|suyabean|Soyabean|Soyaben|सोयाबीन") ~ "Soybean",
      str_detect(crop_focus_other, "हळद") ~      "turmeric",
      str_detect(crop_focus_other, "ginger|gingrer|आले Jinger|Ginger|Gingrer") ~ "Ginger", 
      str_detect(crop_focus_other, "Grass|grass|elephantgrass") ~ "Grass",
      str_detect(crop_focus_other, "Groundnut|groundnut|ground|Ground") ~ "Groundnut",
      str_detect(crop_focus_other, "banana|Banana") ~ "Banana",
      str_detect(crop_focus_other, "mango|Mango") ~ "Mango",
      str_detect(crop_focus_other, "strawberry|Strawberry|strawberry") ~ "Strawberry",
      str_detect(crop_focus_other, "sugarcane|Sugarcane") ~ "Sugarcane",  
      str_detect(crop_focus_other, "jowar|Jowar") ~ "Jowar",
      str_detect(crop_focus_other, "gahu|Wheat") ~ "Wheat", 
      str_detect(crop_focus_other, "Rice|rice|basamati") ~ "Rice",
      str_detect(crop_focus_other, "empty land|home|near home|none|pump|spray pump|2|ala|alavni|band|dukan|ghar|k|nahi|ran|tan|vand|गुलाब") ~ "Other",
      TRUE ~ "Other" )) 

df_crop_other <- df_crop_other_2 %>% 
  select(-crop_focus_other)

### df_crop
df_crop <- 
  df_crop_focus %>% left_join(df_crop_other) %>% 
  mutate(
    crop=ifelse(is.na(crop_other), crop_focus ,crop_other)) %>%
  mutate(crop = str_to_sentence(crop)) %>% 
  select(id,crop,uuid,end)



### [df_prblm]  ...................... ----

df_location <-   df_index%>% 
  select( id, uuid, end , location) 


shop_records <- 
  rbind(df_pest,df_disease,df_weed) %>%
  left_join(df_crop) %>% 
  left_join(df_consult) %>% 
  left_join(df_location) %>% 
  rename(date=end) %>% 
  select(problem, problem_category, problem_specific, 
         crop,consult,location,product_URL, id, uuid, date )


names(shop_records)



library(kableExtra)
shop_records %>%
  select(id, problem_category, problem_specific, 
         crop,consult,location,product_URL,date ) %>% 
  kable() %>% kable_minimal()


# common_problems
shop_records %>%
  count(crop, problem_specific, problem_category, 
        sort = TRUE) %>% 
  select(n,problem_category,crop, problem_specific) %>% 
  kable() %>% kable_minimal()


#_____________________________________________________________________


# names
names(shop_interactCSV)

## DF
shop_interact_index <- shop_interactCSV %>% 
  select(name:location,X_index,id,uid) %>% as_tibble()


## DF
advice_1= shop_interactCSV %>% as_tibble() %>% 
  select(end,crop_focus, crop_focus_other, #crop_acre,
         contains("photo"),contains("URL"), # these cols cover the shop advice
         reason_come_in_new:consult,
         name:location,X_index,id
  ) %>% 
  filter(weed_control_photo != "" | 
           pest_problem_photo != "" | 
           disease_problem_photo !=""  )

# Other pest disease
advice_1 %>% 
  select(id_disease_problem.other_disease, id_disease_problem_other, 
         id_pest_problem.other_pest, id_pest_problem_other) %>% 
  filter(id_disease_problem.other_disease==1 |id_pest_problem_other==1)

# id_pest cols: 7 pests + 2 Others
# id_disease cols: 6 diseases + 2 Others

## DF
advice_2  <- advice_1  %>% 
  select(X_index, consult,ends_with("photo"),
         id_pest_problem.aphids:id_pest_problem_other,
         id_disease_problem.fungal:id_disease_problem_other,
         ends_with("photo_URL"), -contains("other")
  ) %>% 
  mutate(id_weed_control= ifelse(weed_control_photo == "",NA,1)) %>% 
  pivot_longer( -c(X_index,consult,contains("photo") ),
                names_to = "problem_1",values_to = "problem_yn" )  %>% 
  filter(problem_yn==1) %>% 
  left_join(
    df_crop_wide %>% rename(X_index=index) 
  ) %>% 
  select(X_index, consult, problem_1, crop_1st, crop_2nd,ends_with("photo_URL") )

############## pest DFs

df_pest_URL <- 
  advice_2 %>% 
  select(X_index,pest_problem_photo_URL) %>% distinct() %>% 
  filter(pest_problem_photo_URL != "") %>% 
  mutate(X_index =  paste0("P_",X_index))  


df_pest <- 
  advice_2 %>% 
  select(X_index, crop_1st, consult, problem_1) %>% 
  filter(str_detect(problem_1, "^id_pest")) %>% 
  group_by(X_index) %>% mutate(index_row_num = row_number()) %>% ungroup() %>% 
  mutate(X_index =  paste0("P_",X_index)) %>% 
  mutate(X2_index =  paste0(X_index,"_", index_row_num)) %>% 
  select(-index_row_num) %>% 
  mutate(problem_1 = str_remove(problem_1, "^id_pest_problem.")) %>% 
  mutate(consult = word(consult, 1)) %>% 
  mutate(consult = str_remove(consult, "^consult_"))  

list_crop_pest  <- 
  df_pest %>% select(crop_1st,problem_1 ) %>% 
  count(crop_1st,problem_1 )


############## disease DFs

df_disease_URL <- 
  advice_2 %>% 
  select(X_index,disease_problem_photo_URL) %>% distinct() %>% 
  filter(disease_problem_photo_URL != "") %>% 
  mutate(X_index =  paste0("D_",X_index))  

df_disease <- 
  advice_2 %>% 
  select(X_index, crop_1st, consult, problem_1) %>% 
  filter(str_detect(problem_1, "^id_disease")) %>% 
  group_by(X_index) %>% mutate(index_row_num = row_number()) %>% ungroup() %>% 
  mutate(X_index =  paste0("D_",X_index)) %>% 
  mutate(X2_index =  paste0(X_index,"_", index_row_num)) %>% 
  select(-index_row_num) %>% 
  mutate(problem_1 = str_remove(problem_1, "^id_disease_problem.")) %>% 
  mutate(consult = word(consult, 1)) %>% 
  mutate(consult = str_remove(consult, "^consult_")) 

list_crop_disease <- 
  df_disease %>% select(crop_1st,problem_1 ) %>% 
  count(crop_1st,problem_1 )


############# weed DFs

df_weed_URL <- # df for photos downloads
  advice_2 %>% 
  select(X_index,weed_control_photo_URL) %>% distinct() %>% 
  filter(weed_control_photo_URL != "") %>% 
  mutate(X_index =  paste0("W_",X_index))  

df_weed <- 
  advice_2 %>% 
  select(X_index, crop_1st, consult, problem_1) %>% 
  filter(str_detect(problem_1, "^id_weed")) %>% 
  group_by(X_index) %>% mutate(index_row_num = row_number()) %>% ungroup() %>% 
  mutate(X_index =  paste0("W_",X_index)) %>% 
  mutate(X2_index =  paste0(X_index,"_", index_row_num)) %>% 
  select(-index_row_num) %>% 
  mutate(problem_1 = str_remove(problem_1, "^id_")) %>% 
  mutate(consult = word(consult, 1)) %>% 
  mutate(consult = str_remove(consult, "^consult_")) 

list_crop_weed <- 
  df_weed %>% select(crop_1st,problem_1 ) %>% 
  count(crop_1st,problem_1 )

#| ----------------
#     NEXT STEP  
#| ----------------

# formula to download photos  ----

df_pest_URL
df_disease_URL
df_weed_URL

library(writexl)
write_xlsx(df_pest_URL, "df_pest_URL.xlsx")

# downloud  receipt_URL
# In the first step, 
# we will scan the receipts that 
# # do not have a photo per item 
# # and that are related to a "pest" "disease" or "weeding"

receipt_photos <- shop_interactCSV %>% 
  filter(X_index>116 ) %>% 
  select(end ,X_index, contains("URL"), location,
         "reason_come_in_new.pest_problems",
         "reason_come_in.pest_problems",
         "reason_come_in_new.disease_issue" , 
         "reason_come_in.disease_issue" ,
         "reason_come_in_new.weed_control"
  ) %>%
  mutate(
    itemPic_01 = if_else(
      !(is.na(weed_control_photo_URL) | weed_control_photo_URL == "") |
        !(is.na(pest_problem_photo_URL)  | pest_problem_photo_URL == "") |
        !(is.na(disease_problem_photo_URL) | disease_problem_photo_URL == ""),
      1, 0
    ),
    receipt_01 = if_else(
      !(is.na(receipt_URL) | receipt_URL == ""), 1, 0
    ),
    buy_pest    = if_else(reason_come_in_new.pest_problems    == 1 | reason_come_in.pest_problems    == 1, 1, 0),
    buy_disease = if_else(reason_come_in_new.disease_issue    == 1 | reason_come_in.disease_issue    == 1, 1, 0),
    buy_weed    = if_else(reason_come_in_new.weed_control     == 1, 1, 0)
  )  %>% 
  filter(itemPic_01 == 0,
         if_any(c(buy_pest, buy_disease, buy_weed), ~ .x == 1)) %>% 
  rename(index=X_index) %>% 
  left_join(df_crop_wide) %>% filter(!is.na(crop_1st)) %>% # NA are obs of "other crop" without followup "what other?" Q 
  select(end ,index,buy_pest,buy_disease,buy_weed,crop_1st, crop_2nd,location,receipt_URL )




# import the URLs to google sheet
library(googlesheets4)

sheetURL1 <- "https://docs.google.com/spreadsheets/d/11nlKWx_njQx04jdUx6maef2-56MeVlUXfKMeV0r91mY/edit?gid=0#gid=0"

# Write the data frame to a specific sheet (e.g., named "R_data")
# If the sheet "R_data" exists, its content will be overwritten.
# If it doesn't exist, a new sheet will be created.
sheet_write(receipt_photos, ss = sheetURL1, sheet = "receipt_photos")




#| ----------------
#     NEXT STEP
#| ----------------

# Download each image (change 'photo_url' to your actual column name)

setwd("C:/Users/Dan/Documents/product_blf")
getwd()

# df_product <- df_pest_URL
# df_product <- df_disease_URL
# df_product <- df_weed_URL %>% rename(photo_url=weed_control_photo_URL)

library(readxl)
library(httr)
for(i in seq_len(nrow(df_product))) {
  url <- df_product$photo_url[i]
  photo_index <- df_product$X_index[i]
  if(!is.na(url) && grepl("^http", url)) {
    file_name <- paste0("photo_", photo_index, ".jpg")
    httr::GET(url, httr::write_disk(file_name, overwrite = TRUE))
  }
}

#| ----------------
#     NEXT STEP
#| ----------------
# Extracting data from a captured image - Optical Character Recognition (OCR).
#    Scanning and interpreting text within the image 
#    to convert it into ChatGPT-readable data.

#| ----------------
#     NEXT STEP
#| ----------------
# Import of product sales file by the store owner


library(googlesheets4)

sheetURL1 <- "https://docs.google.com/spreadsheets/d/11nlKWx_njQx04jdUx6maef2-56MeVlUXfKMeV0r91mY/edit?gid=0#gid=0"
df_pest_sheet <- read_sheet(sheetURL1)

df_disease_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/11nlKWx_njQx04jdUx6maef2-56MeVlUXfKMeV0r91mY/edit?gid=913058388#gid=913058388", sheet = "df_disease")

df_weed_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/11nlKWx_njQx04jdUx6maef2-56MeVlUXfKMeV0r91mY/edit?gid=418493598#gid=418493598", sheet = "df_weed")


rbind(df_pest, df_disease) %>% rbind(df_weed) %>% 
  
  
  
  # TEST
  # df_pest[1:4,] %>% left_join( df_pest_sheet[1:4,] )
  
  co_p <- 
  df_pest %>% 
  left_join( 
    df_pest_sheet %>% select(X_index,product_name, product_subname,Company),
    relationship = "many-to-many")

co_d <- 
  df_disease %>% 
  left_join( 
    df_disease_sheet %>% select(X_index,product_name, product_subname,Company),
    relationship = "many-to-many")

co_w <- 
  df_weed %>% 
  left_join( 
    df_weed_sheet %>% select(X_index,product_name, product_subname,Company),
    relationship = "many-to-many")



# Problem list

list_crop_pest
list_crop_disease
list_crop_weed

list_crop_pdw <- 
  rbind(list_crop_pest, list_crop_disease) %>% 
  rbind(list_crop_weed) %>% 
  filter(crop_1st != "Other") %>% 
  arrange(desc(n)) 

sheetURL1 <- "https://docs.google.com/spreadsheets/d/11nlKWx_njQx04jdUx6maef2-56MeVlUXfKMeV0r91mY/edit?gid=0#gid=0"

# Write the data frame to a specific sheet (e.g., named "R_data")
# If the sheet "R_data" exists, its content will be overwritten.
# If it doesn't exist, a new sheet will be created.
sheet_write(list_crop_pdw, ss = sheetURL1, sheet = "crop_problem")

# 1.  fill the new df/tab with recommendations 
# 2.  import it back here to analiz


recommend_item <- read_sheet("https://docs.google.com/spreadsheets/d/11nlKWx_njQx04jdUx6maef2-56MeVlUXfKMeV0r91mY/edit?gid=418493598#gid=418493598", 
                             sheet = "recommend_item")

recommend_item <- recommend_item %>% 
  select(crop_1st,  problem_1,recommend_item_govSite  )


# consult df s
co_pdw123 <- 
  rbind(co_p, co_d) %>% rbind(co_w) %>% 
  mutate(index = str_remove(X_index, "^[A-Z]_"))

co_pdw <- 
  rbind(co_p, co_d) %>% rbind(co_w) %>% 
  mutate(index = str_remove(X_index, "^[A-Z]_")) %>% 
  filter(!is.na(product_name))


# Total products sold

rbind(df_pest, df_disease) %>% rbind(df_weed) %>% 
  mutate(prob_type = word(X2_index, 1, sep = "_")) %>% 
  count(prob_type) %>% 
  mutate(match_pct = paste0(round((n/sum(n)) * 100, 0), "%") ) 

total_product_sold0 = co_pdw123 %>% count() %>% pull()
total_product_sold1 = co_pdw %>% count() %>% pull()

# Total products sold per visit
co_pdw123 %>% count(index) %>% rename(total_product_visit=n)

tpv = co_pdw %>% count(index) %>% rename(total_product_visit=n)

#| uid      # id of farmers
#| index    # id of visit in shop
#| X_index  # id of visit by type P D or W
#| X2_index # id of purchase 1 item

# Total visits
Total_visits= co_pdw123 %>% count(index) %>% count() %>% pull(n)

# sample of df for slide
co_pdw %>% 
  select(X_index,X2_index, crop_1st, problem_1, product_name)%>%
  left_join( recommend_item, relationship = "many-to-many")%>% distinct() %>% 
  mutate(match= ifelse(product_name==recommend_item_govSite,1,0)) %>% 
  filter(X_index %in% c("P_1132", "P_1134","P_1157" )) %>% select(-X_index) %>% 
  kable() %>% kable_styling()


# DF of  matches for sold/recommended items
co_match_gov <- 
  co_pdw %>% 
  select(X_index,X2_index, crop_1st, problem_1, product_name)%>%
  left_join( recommend_item, relationship = "many-to-many")%>% distinct() %>% 
  mutate(match= ifelse(product_name==recommend_item_govSite,1,0)) %>% 
  group_by(X2_index, crop_1st, problem_1) %>% 
  summarise(match = sum(match), .groups = "drop") %>% 
  mutate(match = ifelse(match==0,0,1))

# pct of matches - general
co_match_gov %>% summarise(match_pct= sum(match)/n() ) %>% 
  mutate(match_pct = paste0(round(match_pct * 100, 0), "%") ) 


# pct of matches By item type
co_match_gov %>% 
  mutate(prob_type = word(X2_index, 1, sep = "_")) %>% 
  group_by(prob_type) %>%  summarise(match_pct= sum(match)/n() ) %>% 
  mutate(match_pct = paste0(round(match_pct * 100, 0), "%") ) 


# pct of matches By consult
co_pdw %>% select(consult, X2_index) %>% distinct() %>% 
  right_join(co_match_gov) %>% 
  group_by(consult) %>%  summarise(match_pct= sum(match)/n() ) %>% 
  mutate(match_pct = paste0(round(match_pct * 100, 0), "%") ) 


# pct of matches By crop
co_match_gov %>% 
  group_by(crop_1st) %>%  summarise(match_pct= sum(match)/n() ) %>% 
  mutate(match_pct = paste0(round(match_pct * 100, 0), "%") ) 



