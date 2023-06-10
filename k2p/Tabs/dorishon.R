library(readxl)
drishon_opQs <- read_excel("C:/Users/Dan/Downloads/dorishon_openQs.xlsx",
                              sheet = "first_gan_hi_edu_openQs")
drishon_cleandata <- read_excel("C:/Users/Dan/Downloads/drishon_cleandata.xlsx")

dor <- drishon_opQs %>% 
  select(1,
         learning_what_wanted_nu,act_when_feel_busy_text,act_when_feel_busy_nu,satisfied_with_academic_achievements_nu,satisfied_with_academic_ability_skills_nu,aspiration_10_years_from_now_text,aspiration_10_years_from_now_num
         ) %>% 
  inner_join(drishon_cleandata)

dor$id <- sprintf("%03d",seq(1,339))

dor %>% 
  filter(aspiration_10_years_from_now_text != "אין",
         !is.na(aspiration_10_years_from_now_text)) %>% 
  group_by(aspiration_10_years_from_now_text) %>% 
  count() %>% 
  ggplot(aes(x="",y=n,fill=aspiration_10_years_from_now_text))+
  geom_bar(width = 1,stat="identity")

dor %>% 
  filter(aspiration_10_years_from_now_text != "אין",
         !is.na(aspiration_10_years_from_now_text)) %>% 
  group_by(aspiration_10_years_from_now_text) %>% 
  summarise(nn=n()) %>% 
  mutate(prc = nn/sum(nn)) %>% 
  mutate(prct = paste0(round(100 * nn/sum(nn), 0), "%")) %>% 
  ggplot(aes(x="",y=nn,fill=aspiration_10_years_from_now_text))+
  geom_bar(width = 1,stat="identity")+
  coord_polar("y", start=0)


dor01 <- 
  dor %>% 
  filter(aspiration_10_years_from_now_text != "אין",
         !is.na(aspiration_10_years_from_now_text)) %>% 
  group_by(aspiration_10_years_from_now_text) %>% 
  summarise(nn=n()) %>% 
  mutate(prc = nn/sum(nn)) %>% 
  mutate(prct = paste0(round(100 * nn/sum(nn), 0), "%"))

cor.test(dor$aspiration_10_years_from_now_num,
         dor$initi_av)
 
dor_cols <- dor[, c("mot_av" , "initi_av",
                    "act_when_feel_busy_nu", "aspiration_10_years_from_now_num")]

dor_cols$mot_av <- as.numeric(dor_cols$mot_av)
colnames(dor_cols) <- c("mot", "niti", "busy","asp")

dor_cols <- na.omit(dor_cols)

CC <- cor(dor_cols) %>% kable() %>% kable_classic()
cor.test(dor_cols)











