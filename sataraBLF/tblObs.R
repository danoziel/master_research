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


library(openxlsx)
write.xlsx(tbl_Qs_new, "C:/Users/Dan/OneDrive - mail.tau.ac.il/BLF_satara/tbl_Qs_new.xlsx")
write.xlsx(tbl_OBS_old, "C:/Users/Dan/OneDrive - mail.tau.ac.il/BLF_satara/tbl_OBS_old.xlsx")
write.xlsx(tbl_OBS, "C:/Users/Dan/OneDrive - mail.tau.ac.il/BLF_satara/tbl_OBS.xlsx")


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







