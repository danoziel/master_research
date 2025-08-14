library(dplyr)
library(tidyr)
library(kableExtra)
library(readxl)


#------------------------------------------------------------------------------
#      Evaluate shops surveys                                              ----
#----------------------------------------------------------------------------

tbl_OBS_old <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/BLF_satara/BLF_Observations_-_all_versions_-_False_-_2025-05-28-13-02-36.xlsx")
names(tbl_OBS_old)
tbl_OBS <- read_excel("C:/Users/Dan/Downloads/BLF_Observations_NEW_-_all_versions_-_False_-_2025-08-07-09-01-43.xlsx")
names(tbl_OBS)

### Database [tbl_OBS_old]
# # VERSION 1 # From 2024-12-03 to 2025-01-21 (index 1-53)
#             # Only included a questions regarding (1)survey participation (2)payment method
# 
# # VERSION 2 # From 25-01-23 to end
#             # Extended questionnaire- mirror the interaction BLF survey [shop_interact / shop_interactCSV]
#             # DOES NOT include VERSION 1 questions

### Database [tbl_OBS]
# # VERSION 3 # From 	2025-07-22 onward
#             # Extended questionnaire- mirror the interaction BLF survey [shop_interact / shop_interactCSV]
#             # INCLUDE the `participation question`

#
# .... DS TABLE ...........................................................  ----
# Total participates [total_OBS]
total_OBS <- 
  tbl_OBS_old %>% select(end ,Farmer_Phone_Number ) %>% 
  rename(phone=Farmer_Phone_Number) %>% 
  rbind(
    tbl_OBS %>% select(end ,phone )
  ) %>%   mutate(date = as.Date(end))
total_n <- total_OBS %>% count() %>% pull(n)


# Total shop visits [total_visits]
total_visits <- 
  total_OBS %>% count(phone) %>% count() %>% 
  mutate(var="Total visitors") %>% 
  select(var,n) %>% rename(value =n )

##########  TABLE  
total_OBS %>% 
  group_by(date) %>% 
  summarise(n=n() ) %>% ungroup() %>% 
  summarise(`Days sample`=n(),`Total visits`=sum(n)) %>% 
  pivot_longer(1:2,names_to = "var",values_to = "n") %>% 
  rename(value =n )  %>% 
  rbind(total_visits) %>% 
  kbl() %>% kable_styling()


# .... farmer_participate .................................................  ----

####### DFs
# sinece "2025-01-21 09:38:03" this Q didnt asked

dfOLD <- 
  tbl_OBS_old %>% filter(end < "2025-01-21 09:38:03") %>% 
  select(Did_the_farmer_participate_in_,
         Why_didn_t_the_farmer_fill_the_survey) %>% 
  rename(participate=Did_the_farmer_participate_in_,
         whyNo=Why_didn_t_the_farmer_fill_the_survey)
#
dfNEW <- tbl_OBS %>%
  select(farmer_participate, Why_didn_t_the_farmer_fill_the) %>% 
  rename(participate=farmer_participate,
         whyNo=Why_didn_t_the_farmer_fill_the)


####### Categorization of open-ended responses

rbind(dfOLD,dfNEW) %>% count(whyNo)

library(stringr)
participation <- rbind(dfOLD,dfNEW) %>% 
  mutate(
    whyNo_cat = case_when(
      str_detect(whyNo, regex("fill|file|filled|form|survey", ignore_case = TRUE)) ~ "Filled survey earlier",
      str_detect(whyNo, regex("busy|no time|to busy|to rush|time pass", ignore_case = TRUE)) ~ "Too busy / No time",
      str_detect(whyNo, regex("not interested|don.t want", ignore_case = TRUE)) ~ "Not interested",
      str_detect(whyNo, regex("proper product", ignore_case = TRUE)) ~ "Product issue",
      is.na(whyNo) ~ "Filled survey",
      TRUE ~ "Other"
    )
  ) %>% 
  count(whyNo_cat) %>%
  mutate(percent = n / sum(n) * 100) %>% 
  mutate_at(3,round) %>% 
  mutate(label = paste0(round(percent, 1), "%"))



####### PLOT

# Assign colors: same color for "Already filled form/survey" and "No response"
participation_colors <- setNames(
  c(
    "steelblue3",  # Filled survey
    "steelblue2",  # Already filled survey
    "grey70",  # Too busy / No time
    "grey90",  # Not interested
    "grey40",  # Product issue
    "grey20"), # Other
  c("Filled survey", "Filled survey earlier","Too busy / No time", "Not interested", "Product issue", "Other")
)


participation %>% 
  ggplot(aes(x = 1.5, y = n, fill = whyNo_cat)) +
  geom_col() +
  xlim(0, 3) +
  theme_void() +
  labs(fill = NULL, 
       title = paste0("Farmers who responded to the survey [N = ", total_n, "]")) +
  geom_text(aes(label = label),
            position = position_stack(vjust = .5),
            family = "serif") +
  scale_fill_manual(values = participation_colors) +
  theme(text = element_text(family = "serif"),
        legend.text = element_text(size = 11) )+  coord_flip()





# matching tblObs to tblInt  ----

tb_obs_old<- tbl_OBS_old %>% 
  filter(`_index` > 53) %>%  # Why_didn_t_the_farmer_fill_the_survey
  mutate(end = as.Date(end), start = as.Date(start)) %>% 
  rename(phone=Farmer_Phone_Number, index= `_index`) %>% 
  mutate(phone = gsub("[\\s_/\\-\\.']", "", phone))

tb_obs <- tbl_OBS %>% filter(farmer_participate==1) %>% 
  mutate(end = as.Date(end), start = as.Date(start)) %>% 
  mutate(phone=as.character(phone)) %>% 
  rename(Farmer_Name=name,
         reason_come_in = reason_come_in_new )

tb_int  <- shop_interactCSV  %>% 
  mutate(end = as.Date(end), start = as.Date(start)) %>% 
  mutate(phone=as.character (phone),
         phone = gsub("[\\s_/\\-\\.']", "", phone),
         phone = if_else(
           nchar(phone) == 12 & substr(phone, 1, 2) == "91",
           substr(phone, 3, 12),
           phone
         )  ) %>% 
  mutate(
    reason_come_in = if_else(
      reason_come_in == "" | is.na(reason_come_in),
      reason_come_in_new,
      reason_come_in
    )
  )


###
tb1_obs_old <- tb_obs_old %>%  
  select(end,Farmer_Name,phone,
         crop_focus,
         reason_come_in,
         id_pest_problem,id_disease_problem,
         consult
         ) %>% mutate(id_fertilizer_problem=NA,buy_weed_control=NA)
###
tb1_obs <- tb_obs %>%  
  select(end,Farmer_Name,phone,
         crop_focus,
         reason_come_in,
         id_pest_problem,id_disease_problem,
         id_fertilizer_problem,buy_weed_control,
         consult
         ) 
###
tb1_int <- tb_int %>% 
  select(location,end, name, phone,
         crop_focus,
         reason_come_in,
         id_pest_problem,id_disease_problem,
         id_fertilizer_problem,buy_weed_control,
         consult
  )
###
###

tb_comb1 <- rbind(tb1_obs_old,tb1_obs) %>% 
  inner_join(tb1_int,by = join_by(phone,end))


library(dplyr)
library(stringr)
library(purrr)
 ## mC crop_focus ----
tb_comb2 <- tb_comb1 %>%
  mutate( 
    match_crop = map2_int(  # at least one match
      crop_focus.x, crop_focus.y,
      ~{
        x_crops <- str_split(.x, "\\s+")[[1]]
        y_crops <- str_split(.y, "\\s+")[[1]]
        sum(x_crops %in% y_crops)
      }
    )) %>% 
  mutate(match = match_crop >= 1) %>% 
  mutate(match_precisely = # Exactly the same answer
           ifelse(crop_focus.x == crop_focus.y,T,F))

mC = tb_comb2 %>% summarise( 
  n = n(),
  match = round(mean(match),3),
  match_precisely = round(mean(match_precisely),3)
  ) %>% mutate(Subject = "Target crop")

  
 ## mR reason_come_in  ----
tb_comb3 <- tb_comb1 %>%
  mutate(
    match_reason = map2_int(
    reason_come_in.x, reason_come_in.y,
    ~{
      x_reason <- str_split(.x, "\\s+")[[1]]
      y_reason <- str_split(.y, "\\s+")[[1]]
      sum(x_reason %in% y_reason) }
    )) %>% 
  mutate(match = match_reason >= 1,
         match_precisely = reason_come_in.x == reason_come_in.y # Exactly the same answer
  )

mR = tb_comb3 %>% summarise( 
  n = n(),
  match = round(mean(match),3),
  match_precisely = round(mean(match_precisely),3)
  ) %>% mutate(Subject = "Store visit reason")


## id_pest_problem  ----

# This code creates a new column (pest_match) that counts the number of 
# matching pest problem identifiers between id_pest_problem.x and id_pest_problem.y 
# for each row. If both columns are empty or NA, it returns NA; 
# otherwise, it returns the count of matches.

tb_comb4 <- tb_comb1 %>%
  mutate(
    pest_match = map2_int(
      id_pest_problem.x, id_pest_problem.y,
      ~{
        # Consider both "" and NA as "missing"
        x_na <- is.na(.x) || .x == ""
        y_na <- is.na(.y) || .y == ""
        if (x_na && y_na) {
          return(NA_integer_)
        } else {
          x_vec <- str_split(ifelse(is.na(.x), "", .x), "\\s+")[[1]]
          y_vec <- str_split(ifelse(is.na(.y), "", .y), "\\s+")[[1]]
          sum(x_vec %in% y_vec)
        }
      }
    )
  ) %>% 
  mutate(match = pest_match >= 1,
         match_precisely = id_pest_problem.x == id_pest_problem.y # Exactly the same answer
  )# %>% 
  select(end,id_pest_problem.x,id_pest_problem.y ,pest_match,match,match_Ex)


mP = tb_comb4 %>% 
  filter(!is.na(pest_match)) %>% 
  summarise( 
  n = n(),
  match = round(mean(match),3),
  match_precisely = round(mean(match_precisely),3)
  ) %>% mutate(Subject = "Visit reason | Pest problem ")



## id_disease_problem   ----


tb_comb5 <- tb_comb1 %>%
  mutate(
    disease_match = map2_int(
      id_disease_problem.x, id_disease_problem.y,
      ~{
        x_na <- is.na(.x) || .x == ""
        y_na <- is.na(.y) || .y == ""
        if (x_na && y_na) {
          return(NA_integer_)
        } else {
          x_vec <- str_split(ifelse(is.na(.x), "", .x), "\\s+")[[1]]
          y_vec <- str_split(ifelse(is.na(.y), "", .y), "\\s+")[[1]]
          sum(x_vec %in% y_vec)
        }
      }
    ),
    match = disease_match >= 1,
    match_precisely = id_disease_problem.x == id_disease_problem.y
  ) # %>% 
  select(end,id_disease_problem.x,id_disease_problem.y ,disease_match, match_disease,match_disease_Ex)

# Summary for percentage matches:
mD = tb_comb5 %>%
  filter(!is.na(disease_match)) %>%
  summarise(
    n = n(),
    match = round(mean(match),3),
    match_precisely = round(mean(match_precisely),3)
    ) %>% mutate(Subject = "Visit reason | Disease problem ")


## id_fertilizer_problem   ----

tb_comb6 <- tb_comb1 %>%
  mutate(
    fertilizer_match = map2_int(
      id_fertilizer_problem.x, id_fertilizer_problem.y,
      ~{
        x_na <- is.na(.x) || .x == ""
        y_na <- is.na(.y) || .y == ""
        if (x_na && y_na) {
          return(NA_integer_)
        } else {
          x_vec <- str_split(ifelse(is.na(.x), "", .x), "\\s+")[[1]]
          y_vec <- str_split(ifelse(is.na(.y), "", .y), "\\s+")[[1]]
          sum(x_vec %in% y_vec)
        }
      }
    ),
    match = fertilizer_match >= 1,
    match_precisely = id_fertilizer_problem.x == id_fertilizer_problem.y
  )

# Summary for fertilizer problem matches:
mF = tb_comb6 %>%
  filter(!is.na(fertilizer_match)) %>%
  summarise(
    n = n(),
    match = round(mean(match),3),
    match_precisely = round(mean(match_precisely),3)
  ) %>% mutate(Subject = "Visit reason | Buying fertilizer ")





## mW buy_weed_control   ----

tb_comb8 <- tb_comb1 %>%
  mutate(
    weed_match = map2_int(
      buy_weed_control.x, buy_weed_control.y,
      ~{
        x_na <- is.na(.x) || .x == ""
        y_na <- is.na(.y) || .y == ""
        if (x_na && y_na) {
          return(NA_integer_)
        } else {
          x_vec <- str_split(ifelse(is.na(.x), "", .x), "\\s+")[[1]]
          y_vec <- str_split(ifelse(is.na(.y), "", .y), "\\s+")[[1]]
          sum(x_vec %in% y_vec)
        }
      }
    ),
    match = weed_match >= 1,
    match_precisely = buy_weed_control.x == buy_weed_control.y
  )

mW = tb_comb8 %>%
  filter(!is.na(weed_match)) %>%
  summarise(
    n = n(),
    match = round(mean(match),3),
    match_precisely = round(mean(match_precisely),3)
  ) %>% mutate(Subject = "Visit reason | Buying Weed control")


## consult_match   ----

tb_comb7 <- tb_comb1 %>%
  mutate(
    consult_match = map2_int(
      consult.x, consult.y,
      ~{
        x_na <- is.na(.x) || .x == ""
        y_na <- is.na(.y) || .y == ""
        if (x_na && y_na) {
          return(NA_integer_)
        } else {
          x_vec <- str_split(ifelse(is.na(.x), "", .x), "\\s+")[[1]]
          y_vec <- str_split(ifelse(is.na(.y), "", .y), "\\s+")[[1]]
          sum(x_vec %in% y_vec)
        }
      }
    ),
    match = consult_match >= 1,
    match_precisely = consult.x == consult.y
  )


mCons = tb_comb7 %>%
  filter(!is.na(consult_match)) %>%
  summarise(
    n = n(),
    match = round(mean(match),3),
    match_precisely = round(mean(match_precisely),3)
  ) %>% mutate(Subject = "Consultation regarding treatment")



  # general match ----

g_match <- tb_comb2 %>% 
  select(end,phone,location,match,match_precisely) %>% 
  rbind( tb_comb3 %>% select(end,phone,location,match,match_precisely)
  ) %>% 
  rbind( tb_comb4  %>% filter(!is.na(pest_match))%>% select(end,phone,location,match,match_precisely)
  ) %>% 
  rbind( tb_comb5 %>%  filter(!is.na(disease_match)) %>% select(end,phone,location,match,match_precisely)
  ) %>% 
  rbind( tb_comb6 %>% filter(!is.na(fertilizer_match)) %>% select(end,phone,location,match,match_precisely)
  ) %>%   
  rbind( tb_comb8 %>% filter(!is.na(weed_match)) %>% select(end,phone,location,match,match_precisely)
  ) %>% 
  rbind( tb_comb7 %>% filter(!is.na(consult_match)) %>% select(end,phone,location,match,match_precisely)
  )


g_match_1 <- g_match %>%
  summarise(
    n = n(),
    match = round(mean(match),3),
    match_precisely = round(mean(match_precisely),3)
  ) %>% mutate(Subject = "Entire Survey")


g_match_2 <- 
  rbind(mC,mR) %>% 
  rbind(mP) %>% 
  rbind(mD) %>% 
  rbind(mF) %>% 
  rbind(mW) %>% 
  rbind(mCons) 

g_match %>%
  group_by(location) %>% 
  summarise(
    n = n(),
    Match = paste0(round(mean(match) * 100, 1), "%"),
    `Match precisely` = paste0(round(mean(match_precisely) * 100, 1), "%")
    )%>% rename(Store = location ) %>% 
  select(Store , Match, `Match precisely`, n) %>% 
  kable() %>% kable_styling()


rbind(g_match_1 ,g_match_2) %>% 
  mutate(
    Match = paste0(round(match * 100, 1), "%"),
    `Match precisely` = paste0(round(match_precisely * 100, 1), "%")
  ) %>% 
  select(Subject, Match, `Match precisely`, n) %>% 
  kable() %>% kable_styling()







#-------------------------------------------------------------------------------
#    Review sales records                                                   ----
#|------------------------------------------------------------------------------
#|------------------------------------------------------------------------------

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



