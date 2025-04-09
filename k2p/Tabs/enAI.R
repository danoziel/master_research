library(dplyr)

library(readr)
library(readxl)
sky1 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/k2p/Copy of english_leapAI_cleanDF.xlsx")
sky2 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/k2p/SKY2.xlsx")
sky2=sky2 %>% rename(Session_date=`Session Date`,name=Name)
sky=rbind(sky1,sky2)
leapAI_14032025=sky %>% 
  distinct() %>% 
  mutate(Valide_session = ifelse(!is.na(Words_per_message) | !is.na(Messages_in_session), 1, 0))
rm(sky,sky1,sky2)


Copy_of_english_learning_ai_cleanDF <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/k2p/Copy_of_english_learning_ai_cleanDF.xlsx")
english_learning_ai=Copy_of_english_learning_ai_cleanDF %>% 
  rename(
    fearAI_1=threat_challenge_categoryA_intimidating,
    fearAI_2=threat_challenge_categoryB_Unpleasant_feeling,
    fearAI_3=threat_challenge_categoryC_original_Uplifting_challenge,
    fearAI= threat_challenge
  )
english_learning_ai$Post_course <- ifelse(english_learning_ai$Timestamp== "pre_course",0,1 )
rm(Copy_of_english_learning_ai_cleanDF)

names(english_learning_ai)


############ t-tests self_efficacy    ####

df_self_efficacy <- english_learning_ai%>% 
  select(Timestamp, self_efficacy_1:self_efficacy_4)
  

# Perform t-tests for each self-efficacy variable
t_test_results <- lapply(paste0("self_efficacy_", 1:4), function(var) {
  t.test(df_self_efficacy[[var]][df_self_efficacy$Timestamp == "pre_course"],
         df_self_efficacy[[var]][df_self_efficacy$Timestamp == "post_course"],
         var.equal = TRUE)  # Assume equal variance (change to FALSE if needed)
})

# Extract t-test summary for each variable
t_test_self_efficacy <- data.frame(
  variable = paste0("self_efficacy_", 1:4),
  t_value = sapply(t_test_results, function(x) x$statistic),
  p_value = sapply(t_test_results, function(x) x$p.value),
  mean_pre = sapply(t_test_results, function(x) x$estimate[1]),
  mean_post = sapply(t_test_results, function(x) x$estimate[2]),
  df = sapply(t_test_results, function(x) x$parameter)
)
# Display the results
library(rempsyc)
nice_table(t_test_self_efficacy)


############ t-tests threat_challenge ####

df_fear_AIchallenge <- english_learning_ai%>% 
  select(Timestamp, threat_challenge_1:threat_challenge_13)

# Perform t-tests for each threat_challenge variable
t_test_results <- lapply(paste0("threat_challenge_", 1:13), function(var) {
  t.test(df_fear_AIchallenge[[var]][df_fear_AIchallenge$Timestamp == "pre_course"],
         df_fear_AIchallenge[[var]][df_fear_AIchallenge$Timestamp == "post_course"],
         var.equal = TRUE)  # Assume equal variance (change to FALSE if needed)
})

# Extract t-test summary for each variable
t_test_threat_challenge <- data.frame(
  variable = paste0("threat_challenge_", 1:13),
  t_value = sapply(t_test_results, function(x) x$statistic),
  p_value = sapply(t_test_results, function(x) x$p.value),
  mean_pre = sapply(t_test_results, function(x) x$estimate[1]),
  mean_post = sapply(t_test_results, function(x) x$estimate[2]),
  df = sapply(t_test_results, function(x) x$parameter)
)
# Display the results
nice_table(t_test_threat_challenge)


############ t-tests self_image      ####

# Perform t-tests for each threat_challenge variable
t_test_results <- lapply(paste0("self_image_", 1:10), function(var) {
  t.test(english_learning_ai[[var]][english_learning_ai$Timestamp == "pre_course"],
         english_learning_ai[[var]][english_learning_ai$Timestamp == "post_course"],
         var.equal = TRUE)  # Assume equal variance (change to FALSE if needed)
})

# Extract t-test summary for each variable
t_test_self_image <- data.frame(
  variable = paste0("self_image_", 1:10),
  t_value = sapply(t_test_results, function(x) x$statistic),
  p_value = sapply(t_test_results, function(x) x$p.value),
  mean_pre = sapply(t_test_results, function(x) x$estimate[1]),
  mean_post = sapply(t_test_results, function(x) x$estimate[2]),
  df = sapply(t_test_results, function(x) x$parameter)
)
# Display the results
nice_table(t_test_self_image)



###########  cor_matrix   ########
self_image_vars <- paste0("self_image_", 1:10)
self_efficacy_vars <- paste0("self_efficacy_", 1:4)
fearAI_cat_vars <- c("fearAI_1","fearAI_2","fearAI_3","fearAI")
threat_challenge_vars <- c("threat_challenge_1","threat_challenge_2","threat_challenge_3","threat_challenge_4",
                           "threat_challenge_6", "threat_challenge_7", "threat_challenge_8" ,"threat_challenge_9",
                           "threat_challenge_11",
                           "threat_challenge_5_original",                            
                           "threat_challenge_10_original",  
                           "threat_challenge_12_original",     
                           "threat_challenge_13_original")

######################   cor_matrix | self_efficacy_vars / threat_challenge_vars

# Compute correlation matrix
cor_matrix <- cor(english_learning_ai[, c(self_efficacy_vars, threat_challenge_vars)], use = "pairwise.complete.obs")

# Convert to dataframe for display
cor_df <- as.data.frame(cor_matrix[self_efficacy_vars, threat_challenge_vars])
cor_df <- cbind(Self_Efficacy = rownames(cor_df), cor_df)  # Add row names

# ✅ Display interactive HTML table
library(DT)
datatable(cor_df, options = list(pageLength = 5, autoWidth = TRUE))


######################   cor_matrix | self_efficacy_vars / fearAI_cat_vars
cor_matrix <- cor(english_learning_ai[, c(self_efficacy_vars, fearAI_cat_vars)], use = "pairwise.complete.obs")
cor_df <- as.data.frame(cor_matrix[self_efficacy_vars, fearAI_cat_vars])
cor_df <- cbind(Self_Efficacy = rownames(cor_df), cor_df)  # Add row names
datatable(cor_df, options = list(pageLength = 5, autoWidth = TRUE))

######################   cor_matrix | self_image_vars / self_efficacy_vars
cor_matrix <- cor(english_learning_ai[, c(self_image_vars, self_efficacy_vars)], use = "pairwise.complete.obs")
cor_df <- as.data.frame(cor_matrix[self_image_vars, self_efficacy_vars])
cor_df <- cbind(SI = rownames(cor_df), cor_df)  # Add row names
datatable(cor_df, options = list(pageLength = 5, autoWidth = TRUE))

######################   cor_matrix | self_image_vars / threat_challenge_vars
cor_matrix <- cor(english_learning_ai[, c(self_image_vars,threat_challenge_vars)], use = "pairwise.complete.obs")
cor_df <- as.data.frame(cor_matrix[self_image_vars, threat_challenge_vars])
cor_df <- cbind(SI = rownames(cor_df), cor_df)  # Add row names
datatable(cor_df, options = list(pageLength = 5, autoWidth = TRUE))

######################   cor_matrix | self_image_vars / fearAI_cat_vars
cor_matrix <- cor(english_learning_ai[, c(self_image_vars,fearAI_cat_vars)], use = "pairwise.complete.obs")
cor_df <- as.data.frame(cor_matrix[self_image_vars, fearAI_cat_vars])
cor_df <- cbind(SI = rownames(cor_df), cor_df)  # Add row names
datatable(cor_df, options = list(pageLength = 5, autoWidth = TRUE))






########### multiple regression  ###########

self_image_vars
self_efficacy_vars
fearAI_cat_vars
threat_challenge_vars

df_self_image <- english_learning_ai %>% 
  mutate(self_image = rowMeans(select(., self_image_1:self_image_10), na.rm = TRUE))


library(sjPlot)

# self_efficacy Predictor for fearAI

m1 <- lm(fearAI_1 ~ self_efficacy + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)
m2 <- lm(fearAI_2 ~self_efficacy + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)
m3 <- lm(fearAI_3 ~  self_efficacy+ Post_course +
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)
m4 <- lm(fearAI ~  self_efficacy+ Post_course +
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)
tab_model(m1, m2, m3, m4, collapse.se = TRUE, show.ci = FALSE, digits = 4)



# self_image Predictor for fearAI

m1 <- lm(fearAI_1 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai) 

m2 <- lm(fearAI_2 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)

m3 <- lm(fearAI_3 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)

m0 <- lm(fearAI ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)
tab_model(m1,m2,m3,m0, show.se = TRUE, show.ci = FALSE, digits = 4)


# self_image Predictor for self_efficacy
m1 <- lm(self_efficacy_1 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)

m2 <- lm(self_efficacy_2 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)

m3 <- lm(self_efficacy_3 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)

m4 <- lm(self_efficacy_4 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)

m4 <- lm(self_efficacy_4 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)
tab_model(m1,m2, m3,m4, show.se = TRUE, show.ci = FALSE, digits = 4)


# ___________________ Leap AI SKY [leapAI_14032025] _______________________####

names(leapAI_14032025)

students_crs <-  
  english_learning_ai %>% 
  mutate(in_srvy=1) %>% 
  select(me1, in_srvy) %>%
  rename(name=me1) %>% 
  mutate(name = gsub("\\s+", "", name)) %>% # Remove all spaces between first and last name
  distinct()

students_bot <- 
  leapAI_14032025 %>% 
  mutate(in_leap=1) %>% 
  select(name, in_leap) %>% 
  mutate(name = gsub("\\s+", "", name)) %>% # Remove all spaces between first and last name
  distinct()

students_index <- 
  full_join(students_crs,students_bot ) %>% 
  # Generate unique IDs starting from B10001
  mutate(uid = paste0("B", 10000 + match(name, unique(name)))) %>%
  mutate(in_srvy_leap=ifelse(in_srvy==1 & in_leap==1,1,0 ))


leap_practice_0 <- 
  leapAI_14032025 %>% 
  mutate(name = gsub("\\s+", "", name)) %>% # Remove all spaces in name
  left_join(students_index) %>% 
  rename(date = Session_date)

# df Words_per_message
median(leap_practice_0$Words_per_message,na.rm = T)
library(lubridate)

leap_practice_A <- 
  leap_practice_0 %>% 
  rename(wordy=Words_per_message) %>% 
  filter(!is.na(wordy)) %>%  # Remove rows with NA in wordy
  group_by(uid) %>%
  summarise(
    wordy_mean=mean(wordy),
    wordy_01=ifelse(wordy_mean > 9,1,0),
    earliest_date = min(ymd_hms(date[!is.na(wordy)])),  # Earliest date with a value
    wordy_earliest = wordy[which.min(ymd_hms(date[!is.na(wordy)]))],  # wordy value at the earliest date
    wordy_highest = max(wordy),  # Highest wordy value
    wordy_progress = wordy_highest - wordy_earliest  # Gap between highest value and earliest value
  ) %>% mutate(wordy_progress=ifelse(wordy_progress<0,0,wordy_progress)) %>% 
  select(uid,wordy_progress,wordy_mean,wordy_01)
  

# df Messages_in_session
median(leap_practice_0$Messages_in_session,na.rm = T)

leap_practice_B <- 
  leap_practice_0 %>% 
  rename(msg =Messages_in_session) %>% 
  filter(!is.na(msg)) %>%
  group_by(uid) %>%
  summarise(
    interaction_mean=mean(msg),
    interaction_01=ifelse(interaction_mean > 11,1,0),
    earliest_date = min(ymd_hms(date[!is.na(msg)])),
    msg_earliest = msg[which.min(ymd_hms(date[!is.na(msg)]))],
    msg_highest = max(msg),
    interaction_progress = msg_highest - msg_earliest
  ) %>% select(uid,interaction_mean,interaction_progress,interaction_01) 

# df Messages_in_session
leap_practice_0 %>% filter(name != "Name") %>% 
  group_by(uid) %>% 
  summarise(total_ssn=sum(Valide_session)) %>% ungroup() %>%
  summarise(quantile(total_ssn, 0.99),median(total_ssn))

library(lubridate)
leap_practice_C <- 
  leap_practice_0 %>% 
  group_by(uid) %>% 
  summarise(
    participation_rate=sum(Valide_session,na.rm = TRUE),
  ) %>% mutate(
    participation_rate=ifelse(participation_rate>33,NA,participation_rate),
    participation_rate_01=ifelse(participation_rate>6,1,0))

# combin leap_practice dfs [leap_practice_achievements]
leap_practice_achievements <- 
  left_join(leap_practice_C,leap_practice_B) %>% 
  left_join(leap_practice_A) %>% 
  filter(participation_rate > 0 )

# creat "post_course" df
df_en_AI_post_course <- 
  english_learning_ai %>% 
  filter(Timestamp=="post_course") %>% 
  rename(name=me1) %>% 
  mutate(name = gsub("\\s+", "", name)) %>%
  left_join(students_index) %>% 
  left_join(leap_practice_achievements, by = "uid") 
any(is.na(df_en_AI_post_course$uid))

# creat "pre_course" df
df_en_AI_pre_course <- 
  english_learning_ai %>% 
  filter(Timestamp=="pre_course") %>% 
  rename(name=me1) %>% 
  mutate(name = gsub("\\s+", "", name)) %>%
  left_join(students_index) %>% 
  mutate(
    participation_rate = NA,
    participation_rate_01 = NA,
    interaction_mean = NA,
    interaction_progress = NA,
    interaction_01 = NA,
    wordy_progress = NA,
    wordy_mean = NA,
    wordy_01 = NA
  )


df_en_AI <- 
  bind_rows(df_en_AI_pre_course,df_en_AI_post_course)



# REG
# self_efficacy Predictor for fearAI

m1 <- lm(fearAI_1 ~ self_efficacy + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)
m2 <- lm(fearAI_2 ~self_efficacy + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)
m3 <- lm(fearAI_3 ~  self_efficacy+ Post_course +
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)
m4 <- lm(fearAI ~  self_efficacy+ Post_course +
           participation_rate + interaction_mean + wordy_mean +
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = df_en_AI)
tab_model(m4, collapse.se = TRUE, show.ci = FALSE, digits = 4)



# self_image Predictor for fearAI

m1 <- lm(fearAI_1 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai) 

m2 <- lm(fearAI_2 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)

m3 <- lm(fearAI_3 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)

m0 <- lm(fearAI ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)
tab_model(m1,m2,m3,m0, show.se = TRUE, show.ci = FALSE, digits = 4)


# self_image Predictor for self_efficacy
m1 <- lm(self_efficacy_1 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)

m2 <- lm(self_efficacy_2 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)

m3 <- lm(self_efficacy_3 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)

m4 <- lm(self_efficacy_4 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)

m4 <- lm(self_efficacy_4 ~ self_image + Post_course+
           gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
         data = english_learning_ai)
tab_model(m1,m2, m3,m4, show.se = TRUE, show.ci = FALSE, digits = 4)

-------------------------------------------

  
  
  
  
  library(readxl)
leapAI_14032025 <- read_excel("C:/Users/Dan/Downloads/leapAI_14032025.xlsx")
df_en_AI <- read_excel("C:/Users/Dan/Downloads/df_en_AI.xlsx")

names(df_en_AI)
names(leapAI_14032025)

library(dplyr)
library(lubridate)

leapA <- leapAI_14032025 %>%
  mutate(uid = paste0("E", 10000 + match(name, unique(name)))
  ) %>% 
  select(Session_date,uid,Words_per_message,Messages_in_session,Valide_session
  ) %>% 
  arrange(uid, Session_date) %>%
  group_by(uid) %>%
  mutate(freq = row_number()) %>% ungroup()
#
leapB <- leapA %>% 
  # Ensure Session_date is in datetime format
  mutate(Session_date = ymd_hms(Session_date)) %>%
  # Arrange and assign session frequency per user
  arrange(uid, Session_date) %>%
  group_by(uid) %>%
  mutate(freq = row_number()) %>%
  ungroup() %>%
  # Add calendar week (ISO week number)
  mutate(calendar_week = isoweek(Session_date)) %>%
  # Add sequential numeric week starting from earliest session
  mutate(
    sequential_week = floor(as.numeric(difftime(Session_date, min(Session_date, na.rm = TRUE), units = "days")) / 7) + 1
  ) %>% 
  mutate(
    month = month(Session_date),  # extract month number
    semester = case_when(
      month >= 6 & month <= 9  ~ "summer",
      month >= 10              ~ "fall",
      month >= 1 & month <= 5  ~ "fall"  # assuming Jan–May also considered "fall"
    )
  ) %>%
  select(-month)  # optional: remove helper column


quantile(leapB$freq, probs = 0.99, na.rm = TRUE)


leapC <- leapB %>% 
  #filter(freq<64) %>% 
  select(uid, Valide_session , freq,calendar_week, 
         sequential_week, semester) %>% 
  rename(usage= Valide_session)

# Plot 1: Weeks 1 to 15
usage_early <- leapC %>%
  filter(sequential_week >= 1, sequential_week <= 15) %>%
  group_by(sequential_week) %>%
  summarise(usage_count = sum(usage, na.rm = TRUE), .groups = "drop")

# Plot 2: Weeks 23 onward (excluding vacation weeks 17–22)
usage_late <- leapC %>%
  filter(sequential_week >= 23) %>%
  group_by(sequential_week) %>%
  summarise(usage_count = sum(usage, na.rm = TRUE), .groups = "drop")




############ Cumulative usage calculation
#
usage_cumulative <- usage_early %>%
  arrange(sequential_week) %>%
  mutate(cumulative_usage = cumsum(usage_count))
#
usage_cumulative <- usage_late %>%
  arrange(sequential_week) %>%
  mutate(cumulative_usage = cumsum(usage_count))

# Plot cumulative usage
ggplot(usage_cumulative, aes(x = sequential_week, y = cumulative_usage)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue") +
  labs(
    title = "Cumulative App Usage Over Time",
    x = "Sequential Week",
    y = "Cumulative Valid Sessions"
  ) +
  theme_minimal()



leapC %>% 
  group_by(uid, semester) %>%
  summarise(total_usage = sum(usage, na.rm = TRUE), .groups = "drop") %>%
  group_by(semester) %>%
  summarise(
    avg_usage = mean(total_usage, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  filter(!is.na(semester))


# Step 1: Summarize average usage by CEFR level (excluding NAs)
avg_usage_by_cefr <- df_en_AI %>%
  rename(usage = participation_rate) %>%
  group_by(uid, english_CERF_level) %>%
  summarise(total_usage = sum(usage, na.rm = TRUE), .groups = "drop") %>%
  group_by(english_CERF_level) %>%
  summarise(
    avg_usage = mean(total_usage, na.rm = TRUE),
    n_students = n(),
    .groups = "drop"
  ) %>% 
  filter(!is.na(english_CERF_level),english_CERF_level != "Advanced_2")

ggplot(avg_usage_by_cefr, aes(x = english_CERF_level, y = avg_usage)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(avg_usage, 1)), vjust = -0.5, size = 4) +
  labs(
    title = "Average App Usage per Student by English CEFR Level",
    x = "CEFR Level",
    y = "Average Valid Sessions"
  ) +
  theme_minimal()





