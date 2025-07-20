
library(dplyr)
library(readxl)
english_learning_ai_cleanDF <- read_excel("Copy_of_english_learning_ai_cleanDF.xlsx")
english_learning_ai_cleanDF <- Copy_of_english_learning_ai_cleanDF
names(english_learning_ai)

english_learning_ai=english_learning_ai_cleanDF %>% 
  rename(
    fearAI_1=threat_challenge_categoryA_intimidating,
    fearAI_2=threat_challenge_categoryB_Unpleasant_feeling,
    fearAI_3=threat_challenge_categoryC_original_Uplifting_challenge,
    fearAI= threat_challenge
  ) %>% 
  mutate(self_efficacy = rowMeans(select(., self_efficacy_1:self_efficacy_4), na.rm = TRUE))

english_learning_ai$Post_course <- ifelse(english_learning_ai$Timestamp== "pre_course",0,1 )
english_learning_ai$Pre_course <- ifelse(english_learning_ai$Timestamp== "pre_course",1,0 )

english_learning_ai$english_CERF_level <- ifelse(english_learning_ai$english_CERF_level %in% c("Advanced_1","Advanced_2"),"Advanced",english_learning_ai$english_CERF_level)

rm(english_learning_ai_cleanDF)

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


############ t-tests self_image       ####

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



########## #  cor_matrix              ########
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






########### multiple regression                     ###########

self_image_vars
self_efficacy_vars
fearAI_cat_vars
threat_challenge_vars

df_self_image <- english_learning_ai %>% 
  mutate(self_image = rowMeans(select(., self_image_1:self_image_10), na.rm = TRUE))


library(sjPlot)

#           self_efficacy Predictor for fearAI      ----

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
sjPlot::tab_model(m1,m2,m3,m4, show.se = TRUE, show.ci = FALSE, digits = 4)

# sjPlot::tab_model(m1, m2, m3, m4, collapse.se = TRUE, show.ci = FALSE, digits = 4)



#           self_image Predictor for fearAI         ----

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
sjPlot::tab_model(m1,m2,m3,m0, show.se = TRUE, show.ci = FALSE, digits = 4)


#           self_image Predictor for self_efficacy  ----
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

sjPlot::tab_model(m1,m2, m3,m4, show.se = TRUE, show.ci = FALSE, digits = 4)


# ___________________ Leap AI SKY [leapAI_14032025] _______________________####


# SKY DF                                ----

library(readxl)
sky_dfB2 <- read_excel("C:/Users/Dan/Downloads/sky_dfB2.xlsx")
sky_dfB1 <- read_excel("C:/Users/Dan/Downloads/sky_dfB1.xlsx")
sky_df1 <- read_excel("C:/Users/Dan/Downloads/sky_df1.xlsx")
sky_df2 <- read_excel("C:/Users/Dan/Downloads/sky_df2.xlsx")
sky_df3 <- read_excel("C:/Users/Dan/Downloads/sky_df3.xlsx")

sky_dfB2 <- read_excel("sky_dfB2.xlsx")
sky_dfB1 <- read_excel("sky_dfB1.xlsx")
sky_df1 <- read_excel("sky_df1.xlsx")
sky_df2 <- read_excel("sky_df2.xlsx")
sky_df3 <- read_excel("sky_df3.xlsx")

sky_df <- rbind(sky_dfB2,sky_dfB1,sky_df1, sky_df2, sky_df3 )
rm(sky_dfB2,sky_dfB1,sky_df1, sky_df2, sky_df3 )


leapAI_14032025 <- sky_df %>% 
  distinct() %>% 
  rename(Words_per_message=words_mean_in_msg , 
         Messages_in_session=msgs_in_session ,
         Session_date=`Session Date`,
         name=Name) %>% 
  mutate(Valide_session = ifelse(!is.na(Words_per_message) | !is.na(Messages_in_session), 1, 0),)


names(leapAI_14032025)


##### creating uid index [Email_index]  ----

######### by Email

# survey
Email_survey <-  
  english_learning_ai %>% 
  select(Email) %>%
  distinct()

# sky
Email_sky <-  
  leapAI_14032025 %>% 
  select(Email) %>% 
  distinct()

# bind survey & sky
Email_index <- 
  inner_join(Email_survey,Email_sky ) %>%
  # Generate unique IDs starting from B10001
  mutate(uid = paste0("E", 10000 + match(Email, unique(Email)))) 

index_Email <- Email_index
  














#   CREATING VARs                       ----

leap_practice_0 <- 
  leapAI_14032025 %>% 
  rename(date =  Session_date) %>% 
  inner_join(Email_index)

any(is.na(leap_practice_0$uid))

# df Words_per_message                  ----

# 1. Words_per_message | 99%
wordy99 = quantile(leap_practice_0$Words_per_message, probs = 0.99, na.rm = TRUE)

# 2. Words_per_message | Median 
median_wordy_value=
  leap_practice_0 %>%
  # mutate(Words_per_message=ifelse(Words_per_message > wordy99,NA,Words_per_message)) %>%
  filter(!is.na(Words_per_message)) %>%
  group_by(uid) %>%   summarise(
    median=median(Words_per_message)) %>% summarise(
      median=median(median)) %>%
  pull(median)

# 3.  CREATING Words_per_message Vars [wordy_progress] [wordy_mean] [wordy_high_low]

# library(lubridate)
df_Words_per_message <- 
  leap_practice_0 %>% 
  rename(wordy=Words_per_message) %>% 
  # mutate(wordy=ifelse(wordy > wordy99,NA,wordy)) %>%
  filter(!is.na(wordy)) %>%
  
  group_by(uid) %>%
  summarise(
    wordy_mean=mean(wordy),
    wordy_high_low=ifelse(wordy_mean > median_wordy_value ,1,0),
    earliest_date = min(ymd_hms(date[!is.na(wordy)])),  # Earliest date with a value
    wordy_earliest = wordy[which.min(ymd_hms(date[!is.na(wordy)]))],  # wordy value at the earliest date
    wordy_highest = max(wordy),  # Highest wordy value
    wordy_progress = wordy_highest - wordy_earliest  # Gap between highest value and earliest value
  ) %>% ungroup() %>% 
  mutate(wordy_progress=ifelse(wordy_progress<0,0,wordy_progress)) %>% 
  select(uid,wordy_progress,wordy_mean,wordy_high_low)
leap_practice_A %>% count(wordy_high_low)

rm(wordy99,median_wordy_value)


# df Messages_in_session                ----

# 1. Messages_in_session | 99%
interaction99 = quantile(leap_practice_0$Messages_in_session, probs = 0.99, na.rm = TRUE)

# 2. Messages_in_session | Median 
median_interaction_value=
  leap_practice_0 %>% 
  # mutate(Messages_in_session=ifelse(Messages_in_session > interaction99,NA,Messages_in_session)) %>%
  filter(!is.na(Messages_in_session)) %>%
  group_by(uid) %>%   
  summarise(
    median=median(Messages_in_session)) %>% 
  summarise(
    median=median(median))%>%
  pull(median)

# 3.  CREATING Messages_in_session Vars [interaction_mean] [interaction_progress] [interaction_high_low]
df_Messages_in_session <- 
  leap_practice_0 %>% 
  # mutate(Messages_in_session=ifelse(Messages_in_session > interaction99,NA,Messages_in_session)) %>%
  rename(msg =Messages_in_session) %>% 
  filter(!is.na(msg)) %>%
  group_by(uid) %>%
  summarise(
    interaction_mean=mean(msg),
    interaction_median=median(msg),
    interaction_high_low=ifelse(interaction_mean > median_interaction_value ,1,0),
    earliest_date = min(ymd_hms(date[!is.na(msg)])),
    msg_earliest = msg[which.min(ymd_hms(date[!is.na(msg)]))],
    msg_highest = max(msg),
    interaction_progress = msg_highest - msg_earliest
  ) %>% select(uid,interaction_mean,interaction_progress,interaction_high_low) 
leap_practice_B %>% count(interaction_high_low)

rm(interaction99 ,median_interaction_value)

# df Valide_session                     ----

# 1. Valide_session | 99%
pct99_participation_value <- 
  leap_practice_0 %>% 
  group_by(uid) %>% 
  summarise(total = sum(Valide_session, na.rm = TRUE)) %>% 
  summarise(percentile_99 = quantile(total, 0.99, na.rm = TRUE)) %>%
  pull(percentile_99)


# 2. Valide_session | Median 
median_participation_value <- 
  leap_practice_0 %>% 
  group_by(uid) %>% 
  summarise(total = sum(Valide_session, na.rm = TRUE)) %>% ungroup() %>% 
  # filter(total<pct99_participation_value) %>% 
  summarise(median=median(total)) %>%
  pull(median)

# 3.  CREATING Valide_session Vars [participation_rate] [participation_high_low]
# library(lubridate)
df_Valide_session <- 
  leap_practice_0 %>% 
  group_by(uid) %>% 
  summarise(
    participation_rate=sum(Valide_session,na.rm = TRUE)
  ) %>% mutate(
    #    participation_rate=ifelse(participation_rate>pct99_participation_value,NA,participation_rate),
    participation_high_low=ifelse(participation_rate > median_participation_value,1,0)) %>% 
  filter(participation_rate>0)
leap_practice_C %>% count(participation_high_low)

rm(pct99_participation_value,median_participation_value)

#    combined df [leap_practice_achievements]  ----
leap_practice_achievements <- 
  left_join(df_Words_per_message,df_Messages_in_session) %>% 
  left_join(df_Valide_session) 

rm(df_Words_per_message,df_Messages_in_session,df_Valide_session) 

#    combined df [df_srvy_sky]                    ----
# "df_en_AI" = english_learning_ai +  leap_practice_achievements
names(df_en_AI)
df_en_AI <- read_excel("C:/Users/Dan/Downloads/df_en_AI.xlsx")

df_Ssurvey <- 
  english_learning_ai %>% 
  select(Timestamp, Post_course,Pre_course,
         gander_1male_2Female, Generation, sector_2, sector01,
         english_CERF_level, english_level_1Low_10High,
         self_image_1:self_efficacy_4,
         threat_challenge_5_original:self_efficacy,
         course_satisfaction,desire_to_continue, user_experience_level,
         Email) %>% left_join(Email_index)

df_Ssky <- Email_index %>% left_join(leap_practice_achievements)


df_srvy_sky <- df_Ssurvey %>% left_join(df_Ssky)

control_vars <- 
  df_en_AI %>% 
  filter(!is.na(uid) ) %>% 
  select(uid,gander_1male_2Female, Generation, sector_2, english_CERF_level) %>% 
  distinct()




# REG ..............................................................----




# NOT GOOD _ DONT USE THIS METHOD ----

SE_change <- df_en_AI %>% 
  group_by(Timestamp,uid) %>% 
  summarise(self_efficacy=mean(self_efficacy,na.rm=T)) %>% ungroup() %>% 
  filter(self_efficacy>0)  %>% 
  pivot_wider(
    id_cols = uid,                       # participant ID
    names_from = Timestamp,              # "pre_course", "post_course"
    values_from = self_efficacy          # or the actual name of your fear column
  ) %>%
  rename(
    self_efficacy_pre = pre_course,
    self_efficacy_post = post_course
  ) %>% 
  mutate(self_efficacy_change = self_efficacy_post - self_efficacy_pre)


df_regression <- SE_change %>%
  left_join(leap_practice_achievements, by = "uid") %>% 
  left_join(control_vars, by = "uid") %>% 
  filter(!is.na(self_efficacy_change))%>% 
  filter(self_efficacy_change !=(-4))

model1 <- lm(self_efficacy_change ~   
               participation_rate + wordy_mean + interaction_mean+
               gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
             data = df_regression)
summary(model1)
tab_model(model1, show.se = TRUE, show.ci = FALSE, digits = 4)


############# Normalization achievements    ----
library(tidyr)
library(scales)

min_max_scale <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

summary_stat <- leap_practice_achievements
summary_stat$wordy_min_max <- min_max_scale(summary_stat$wordy_mean)
summary_stat$interaction_min_max <- min_max_scale(summary_stat$interaction_mean)

summary_stat %>% 
  select(uid,
         participation_rate,
         interaction_mean, interaction_progress,interaction_progress_min_max,
         wordy_mean ,wordy_progress,wordy_progress_min_max) %>% 
  pivot_longer(!uid,names_to = "VAR", values_to = "Value") %>% 
  group_by(VAR) %>% 
  summarise(Mean=mean(Value,na.rm=T),
            SD=sd(Value,na.rm=T),
            Max=max(Value,na.rm=T),
            Min=min(Value,na.rm=T))

m11 <- lm(fearAI ~ participation_rate + interaction_mean + wordy_mean +
            gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
          data = df_en_AIpost_course)
summary(m11)
tab_model(m11, show.se = TRUE, show.ci = FALSE, digits = 4)

library(sjPlot)

df_en_AIpost_course$wordy <- scale(df_en_AIpost_course$wordy_mean)
df_en_AIpost_course$interaction <- scale(df_en_AIpost_course$interaction_mean)
df_en_AIpost_course$participation <- scale(df_en_AIpost_course$participation_rate)

m11 <- lm(fearAI ~ participation + interaction + wordy +
            gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
          data = df_en_AIpost_course)
summary(m11)
tab_model(m11, show.se = TRUE, show.ci = FALSE, digits = 4)


# Min-Max Normalization (Rescaling to 0–1)  -----

min_max_scale <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
df_en_AIpost_course$wordy_minmax <- min_max_scale(df_en_AIpost_course$wordy_mean)
df_en_AIpost_course$interaction_minmax <- min_max_scale(df_en_AIpost_course$interaction_mean)
df_en_AIpost_course$participation_minmax <- min_max_scale(df_en_AIpost_course$participation_rate)

m22 <- lm(fearAI ~ participation_minmax + interaction_minmax + wordy_minmax +
            gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
          data = df_en_AIpost_course)
summary(m22)
tab_model(m4, show.se = TRUE, show.ci = FALSE, digits = 4)










### reg  fearAI        ~  App_achievements     ----
names(leap_practice_achievements)

m1 <- lm(fearAI_1 ~ Post_course + 
           participation_rate + interaction_mean + wordy_mean +
           gander_1male_2Female + Generation + sector01 + english_CERF_level, 
         data = df_srvy_sky)

m2 <- lm(fearAI_2 ~  Post_course + 
           participation_rate + interaction_mean + wordy_mean +
           gander_1male_2Female + Generation + sector01 + english_CERF_level, 
         data = df_srvy_sky)

m3 <- lm(fearAI_3 ~  Post_course + 
           participation_rate + interaction_mean + wordy_mean +
           gander_1male_2Female + Generation + sector01 + english_CERF_level, 
         data = df_srvy_sky)

m123 <- lm(fearAI ~  Post_course + 
           participation_rate + interaction_mean + wordy_mean +
           gander_1male_2Female + Generation + sector01 + english_CERF_level, 
         data = df_srvy_sky)

sjPlot::tab_model(m1,m2, m3,m123, show.se = TRUE, show.ci = FALSE, digits = 4)
# sjPlot::tab_model(m4, collapse.se = TRUE, show.ci = FALSE, digits = 4)


### reg  self_efficacy ~  App_achievements     -----

m1 <- lm(self_efficacy_1 ~  Post_course + 
           participation_rate + interaction_mean + wordy_mean +
           gander_1male_2Female + Generation + sector01 + english_CERF_level, 
         data = df_srvy_sky)

m2 <- lm(self_efficacy_2 ~  Post_course + 
           participation_rate + interaction_mean + wordy_mean +
           gander_1male_2Female + Generation + sector01 + english_CERF_level, 
         data = df_srvy_sky)

m3 <- lm(self_efficacy_3 ~ Post_course + 
           participation_rate + interaction_mean + wordy_mean +
           gander_1male_2Female + Generation + sector01 + english_CERF_level, 
         data = df_srvy_sky)

m4 <- lm(self_efficacy_4 ~ Post_course + 
           participation_rate + interaction_mean + wordy_mean +
           gander_1male_2Female + Generation + sector01 + english_CERF_level, 
         data = df_srvy_sky)

m1234 <- lm(self_efficacy ~ Post_course + 
              participation_rate + interaction_mean + wordy_mean +
              gander_1male_2Female + Generation + sector01 + english_CERF_level, 
            data = df_srvy_sky)

sjPlot::tab_model(m1,m2, m3,m4,m1234, show.se = TRUE, show.ci = FALSE, digits = 4)
sjPlot::tab_model(m1, show.se = TRUE, show.ci = FALSE, digits = 4)



# linear model clustered ----

# Load necessary packages
library(lmtest)
library(sandwich)

# Fit the linear model
model <- lm(self_efficacy ~ Post_course + participation_rate + interaction_mean + wordy_mean +
              gander_1male_2Female + Generation + sector_2 + english_CERF_level, 
            data = df_en_AI)

# Show clustered standard errors by 'uid'
clustered_m1 <- coeftest(m1, vcov = vcovCL, cluster = ~ uid)
clustered_m2 <- coeftest(m2, vcov = vcovCL, cluster = ~ uid)
clustered_m3 <- coeftest(m3, vcov = vcovCL, cluster = ~ uid)
clustered_m4 <- coeftest(m3, vcov = vcovCL, cluster = ~ uid)
clustered_m1234 <- coeftest(m1234, vcov = vcovCL, cluster = ~ uid)

print(clustered_m1)

# DIS STAT ----

c1 <- english_learning_ai %>%select(Timestamp,english_level_1Low_10High) %>% 
  drop_na() %>% 
  mutate(Timestamp=ifelse(
    Timestamp=="post_course","B_post_course","A_pre_course"))%>% 
  group_by(Timestamp) %>% 
  summarise(Mean=mean(english_level_1Low_10High)) 

c2 <- english_learning_ai %>%select(Timestamp,course_satisfaction) %>% 
  drop_na() %>%
  summarise(Mean=mean(course_satisfaction)) %>% 
  mutate(Timestamp="course_satisfaction") %>% select(2,1)

c3 <- english_learning_ai %>%select(Timestamp,desire_to_continue) %>% 
  drop_na() %>%
  summarise(Mean=mean(desire_to_continue))%>% 
  mutate(Timestamp="desire_to_continue") %>% select(2,1)

c4 <- english_learning_ai %>%select(Timestamp,user_experience_level) %>% 
  drop_na() %>%
  summarise(Mean=mean(user_experience_level))%>% 
  mutate(Timestamp="user_experience_level") %>% select(2,1)

bind_rows(c1,c2,c3,c4) %>% kable() %>% kable_classic()














# DS ------

names(df_en_AI)

summary_data <- df_en_AI %>%
  select(uid,Timestamp,gander_1male_2Female, age,Generation,
         sector,sector_2,english_CERF_level)

write.csv(summary_data, "/cloud/project/summary_data.csv", row.names = FALSE)
write.csv(leap_practice_achievements, "/cloud/project/leap_practice_achievements.csv", row.names = FALSE)









