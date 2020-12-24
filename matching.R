library(tidyverse)
library(MatchIt)
library(tableone)
library(kableExtra)
# S   Table 1: Total Own Land Cultivated - Summer  ----

lso <- land_17_18_19 %>% 
  filter(total_land_cultivated_year>0,season=="Summer") %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(own=sum(total_ownland_cultivated)*0.0338)

lso <- lso %>% 
  mutate(after_1Y = year == 2018, 
         after_2Y = year == 2019, 
         own_sp = TreatmentControl == "Treatment")

lso_M <- filter(lso, year == 2017)

match.it <- matchit(own_sp ~ own , data = lso_M, method="nearest", ratio=3)

plot(match.it, type = 'jitter', interactive = FALSE)

df.match <- match.data(match.it)[3]

df.match <-inner_join (df.match,lso,by="household_questionnaire_id")


# after 1 year
df.match18 <- filter(df.match,year==2018)

pacman::p_load(tableone)
table18 <- CreateTableOne(vars = c('own'), 
                          data = df.match18, 
                          strata = 'TreatmentControl')

table188 <- print(table18, 
                  printToggle = FALSE, 
                  noSpaces = TRUE)

table188 [,1:3]%>%
  kable() %>%
  kable_styling(full_width = F, position = "left")


# after 2 year
# In both groups the numbers decrease


# RBS Table 1: Total Own Land Cultivated - Summer  ----   
lor <- Land_18_19%>% 
  filter(total_land_cultivated_year>0,season=="Summer") %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(ownland=total_ownland_cultivated*0.0338) 

lor <- lor %>% 
  mutate(after_1Y = year == 2019, 
         own_sp = TreatmentControl == "Treatment")

lor_M <- filter(lor, year == 2018)

match.it <- matchit(own_sp ~ ownland , data = lor_M, method="nearest", ratio=3)

plot(match.it, type = 'jitter', interactive = FALSE)

df.match <- match.data(match.it)
df.match <-select(df.match,3)

df.match <-inner_join (df.match,lor,by="household_questionnaire_id")


# after 1 year
df.match1 <- filter(df.match,year==2019)

pacman::p_load(tableone)
table1 <- CreateTableOne(vars = c('ownland'), 
                          data = df.match1, 
                          strata = 'TreatmentControl')

table11 <- print(table1, 
                  printToggle = FALSE, 
                  noSpaces = TRUE)

table11 [,1:3]%>%
  kable() %>%
  kable_styling(full_width = F, position = "left")

rm (match.it,df.match,df.match1,df.match2,table1,table11)
rm(lor,lor_M,lorCT)


# S   Table 2:  Gross Cropped Area  ----

C008109001:480 T103207002:400

lsc <- land_17_18_19%>% 
  filter(!household_questionnaire_id %in% c("C008109001", "T103207002")) %>%
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(gca=sum(total_land_cultivated)*0.0338)

lsc <- lsc %>% 
  mutate(after_1Y = year == 2018, 
         after_2Y = year == 2019, 
         own_sp = TreatmentControl == "Treatment")

lsc_M <- filter(lsc, year == 2017)

match.it <- matchit(own_sp ~ gca , data = lsc_M, method="nearest", ratio=3)

plot(match.it, type = 'jitter', interactive = FALSE)

df.match <- match.data(match.it)[3]

df.match <-inner_join (df.match,lsc,by="household_questionnaire_id")

# after 1 year
df.match1 <- filter(df.match,year==2018)

pacman::p_load(tableone)
table1 <- CreateTableOne(vars = c('gca'), 
                          data = df.match1, 
                          strata = 'TreatmentControl')

table11 <- print(table1, 
                  printToggle = FALSE, 
                  noSpaces = TRUE)

table11 [,1:3]%>%
  kable() %>%
  kable_styling(full_width = F, position = "left")

# after 2 year
df.match2 <- filter(df.match,year==2019)

pacman::p_load(tableone)
table2 <- CreateTableOne(vars = c('gca'), 
                         data = df.match2, 
                         strata = 'TreatmentControl')

table22 <- print(table2, 
                 printToggle = FALSE, 
                 noSpaces = TRUE)

table22 [,1:3]%>%
  kable() %>%
  kable_styling(full_width = F, position = "left")

rm (match.it,df.match,df.match1,df.match2,table1,table11,table2,table22)
rm(lsc,lsc_M)

# RBS Table 2:  Gross Cropped Area  ----
lgr <- Land_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(gca=sum(total_land_cultivated)*0.0338) 

lgr <- lgr %>% 
  mutate(after_1Y = year == 2019, 
         own_sp = TreatmentControl == "Treatment")

lgr_M <- filter(lgr, year == 2018)

match.it <- matchit(own_sp ~ gca , data = lgr_M, method="nearest", ratio=3)
df.match <- match.data(match.it)[3]
df.match <-inner_join (df.match,lgr,by="household_questionnaire_id")

df.match1 <- filter(df.match,year==2019)

pacman::p_load(tableone)
table1 <- CreateTableOne(vars = c('gca'),data = df.match1,strata = 'TreatmentControl')

table11 <- print(table1,printToggle = FALSE,noSpaces = TRUE)

table11 [,1:3]%>% kable() %>% kable_styling(full_width = F, position = "left")












#  S   Table 3: Cropping Intensity  ----
lsci <- land_17_18_19%>% filter(total_land_cultivated_year>0) %>%
  mutate(NEW_total_land_cult= case_when(TreatmentControl=="Control" & land_for_cultivation < total_land_cultivated ~ NA_real_,
                                        TRUE ~ total_land_cultivated)) %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(cult=sum(NEW_total_land_cult,na.rm = T),net=mean(land_for_cultivation))%>%
  mutate(crop_intensity=cult/net*100) 

lsci <- lsci %>% 
  mutate(after_1Y = year == 2018, 
         after_2Y = year == 2019, 
         own_sp = TreatmentControl == "Treatment")

lsci_M <- filter(lsci, year == 2017)

match.it <- matchit(own_sp ~ crop_intensity , data = lsci_M, method="nearest", ratio=3)
plot(match.it, type = 'jitter', interactive = FALSE)
df.match <- match.data(match.it)[3]
df.match <-inner_join (df.match,lsci,by="household_questionnaire_id")
# after 1 year
df.match1 <- filter(df.match,year==2018)
table1 <- CreateTableOne(vars = c('crop_intensity'),data = df.match1,strata = 'TreatmentControl')

table11 <- print(table1,printToggle = FALSE,noSpaces = TRUE)

table11 [,1:3]%>%kable() %>% kable_styling(full_width = F, position = "left")

# after 2 year
df.match2 <- filter(df.match,year==2019)

pacman::p_load(tableone)
table2 <- CreateTableOne(vars = c('crop_intensity'), 
                         data = df.match2, 
                         strata = 'TreatmentControl')

table22 <- print(table2, 
                 printToggle = FALSE, 
                 noSpaces = TRUE)

table22 [,1:3]%>%
  kable() %>%
  kable_styling(full_width = F, position = "left")


#  RBS Table 3: Cropping Intensity  ----
lrci <- Land_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  mutate(NEW_total_land_cult= case_when(
    TreatmentControl=="Control" & land_for_cultivation < total_land_cultivated ~ NA_integer_,
    TRUE ~ total_land_cultivated)) %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(cult=sum(NEW_total_land_cult,na.rm = T),net=mean(land_for_cultivation))%>%
  mutate(crop_intensity=cult/net*100) %>% filter(crop_intensity>=0)

lrci <- lrci %>% mutate(after_1Y = year == 2019,own_sp = TreatmentControl == "Treatment")

lrci_M <- filter(lrci, year == 2018)

match.it <- matchit(own_sp ~ crop_intensity , data = lrci_M, method="nearest", ratio=3)
df.match <- match.data(match.it)[3]
df.match <-inner_join (df.match,lrci,by="household_questionnaire_id")

df.match1 <- filter(df.match,year==2019)

table1 <- CreateTableOne(vars = c('crop_intensity'),data = df.match1,strata = 'TreatmentControl')

table11 <- print(table1,printToggle = FALSE,noSpaces = TRUE)

table11 [,1:3]%>% kable() %>% kable_styling(full_width = F, position = "left")


# total_litres_consumed_dieselkero -- S ----

litres_S <-  Procurement_17_18_19 %>%
  filter(!is.na(total_litres_consumed_dieselkero))%>%
  mutate(after_1Y = year == 2018, 
         after_2Y = year == 2019, 
         own_sp = TC == 1)

litres_S_M <- 
  select(filter(litres_S, year == 2017)
         ,c(household_questionnaire_id,at_which_price_did_you_buy_it,
            total_litres_consumed_dieselkero:own_sp))

tableBEFORE <- CreateTableOne(vars = 'total_litres_consumed_dieselkero', 
                          data = litres_S_M, 
                          strata = 'TreatmentControl')

match.it <- matchit(own_sp ~ total_litres_consumed_dieselkero , data = litres_S_M, method="nearest", ratio=2)
a <- summary(match.it)

plot(match.it, type = 'jitter', interactive = FALSE)

df.match <- match.data(match.it)[1]

df.match <-inner_join (df.match,litres_S,by="household_questionnaire_id")


# after 1 year
df.match18 <- filter(df.match,year==2018)

pacman::p_load(tableone)
table18 <- CreateTableOne(vars = c('total_litres_consumed_dieselkero'), 
                         data = df.match18, 
                         strata = 'TreatmentControl')

ggplot(data = df.match18, mapping = aes(x=TreatmentControl, 
                                        y=total_litres_consumed_dieselkero,
                                        fill=TreatmentControl)) +
  geom_boxplot() + theme_bw() +
  scale_fill_brewer(palette = "Accent")


########## lm
pro <- filter(df.match,year==2018)
fit <- lm(total_litres_consumed_dieselkero ~ TC, data=pro)
summary(fit)

# after 2 year
df.match19 <- filter(df.match,year==2019)

pacman::p_load(tableone)
table19 <- CreateTableOne(vars = c('total_litres_consumed_dieselkero'), 
                         data = df.match19, 
                         strata = 'TreatmentControl')


table199 <- print(table19, 
                printToggle = FALSE, 
                noSpaces = TRUE)

table199 [,1:3]%>%
  kable() %>%
  kable_styling()

rm (tableBEFORE,match.it,df.match,df.match18,df.match19,model1,table18,table199)
rm(litres_RBS,litres_S,litres_S_M,litres_rbs,litres_rbs_M)
# Irrigation Intensity -------------- S -------
irri_intens_S <- land_17_18_19 %>% filter(total_land_cultivated_year>0,season!="Annual") %>% 
  group_by(TreatmentControl,year,household_questionnaire_id) %>%
  summarise(gross_irri=sum(irrigated_out_of_tot_land_cult),gross_crop=sum(total_land_cultivated),ii=gross_irri/gross_crop*100) %>% 
  filter(ii>= 0,ii<=100) 

irri_intens_S_M <- irri_intens_S %>% 
  mutate(after_1Y = year == 2018, 
         after_2Y = year == 2019, 
         own_sp = TreatmentControl == "Treatment")

tableBEFORE <- CreateTableOne(vars = 'ii', 
                              data = irri_intens_S_M, 
                              strata = 'TreatmentControl')

irri_intens_S_M <- filter(irri_intens_S_M, year == 2017)

match.it <- matchit(own_sp ~ ii , data = irri_intens_S_M, method="nearest", ratio=1)
a <- summary(match.it)

plot(match.it, type = 'jitter', interactive = FALSE)

df.match <- match.data(match.it)[3]

df.match <-inner_join (df.match,irri_intens_S,by="household_questionnaire_id")


# after 1 year
df.match18 <- filter(df.match,year==2018)

pacman::p_load(tableone)
table18 <- CreateTableOne(vars = c('ii'), 
                          data = df.match18, 
                          strata = 'TreatmentControl')

ggplot(data = df.match18, mapping = aes(x=TreatmentControl, 
                                        y=ii,
                                        fill=TreatmentControl)) +
  geom_boxplot() + theme_bw() +
  scale_fill_brewer(palette = "Accent")


# after 2 year
df.match19 <- filter(df.match,year==2019)

pacman::p_load(tableone)
table19 <- CreateTableOne(vars = c('ii'), 
                          data = df.match19, 
                          strata = 'TreatmentControl')

# lm
x <- inner_join(irri_intens_S,Control_and_treatment_4_districts)
pro <- filter(x,year==2019)
fit <- lm(ii ~ TC, data=x)
summary(fit)

rm (tableBEFORE,match.it,df.match,df.match18,df.match19,fit)
rm(irri_intens_S,irri_intens_S_M)




# wem_liter_fuel_18_19  - p123_year----

litres_rbs <-  wem_liter_fuel_18_19 %>%
  filter(!is.na(p123_year))%>%
  mutate(after_1Y = year == 2019, 
         own_sp = TreatmentControl == "Treatment")


litres_rbs_M <- filter(litres_rbs, year == 2018) %>% select(-c(5:16))

tableBEFORE <- CreateTableOne(vars = 'p123_year', 
                              data = litres_rbs_M, 
                              strata = 'TreatmentControl')


match.it <- matchit(own_sp ~ p123_year , data = litres_rbs_M, method="nearest", ratio=2)
a <- summary(match.it)

plot(match.it, type = 'jitter', interactive = FALSE)

df.match <- match.data(match.it)[1]

df.match <-inner_join (df.match,litres_rbs,by="household_questionnaire_id")


# after 1 year
df.match19 <- filter(df.match,year==2019)

pacman::p_load(tableone)
table19 <- CreateTableOne(vars = c('p123_year'), 
                          data = df.match19, 
                          strata = 'TreatmentControl')

table199 <- print(table19, 
                  printToggle = FALSE, 
                  noSpaces = TRUE)

table199 [,1:3]%>%
  kable() %>%
  kable_styling()

ggplot(data = df.match18, mapping = aes(x=TreatmentControl, 
                                        y=p123_year,
                                        fill=TreatmentControl)) +
  geom_boxplot() + theme_bw() +
  scale_fill_brewer(palette = "Accent")







# S Table 4: Size of  the irrigated area----

lsi <- land_17_18_19%>% 
  filter(total_land_cultivated_year>0) %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(irrigated_land=sum(irrigated_out_of_tot_land_cult,na.rm = T)*0.0338)


# RBS Table 4: Size of  the irrigated area----

lsi <- Land_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(irrigated_land=sum(irrigated_out_of_tot_land_cult)*0.0338) 

lsi <- lsi %>% 
  mutate(after_1Y = year == 2019, 
         own_sp = TreatmentControl == "Treatment")

lsi_M <- filter(lsi, year == 2018)

match.it <- matchit(own_sp ~ irrigated_land , data = lsi_M, method="nearest", ratio=3)
plot(match.it, type = 'jitter', interactive = FALSE)
df.match <- match.data(match.it)[3]
df.match <-inner_join (df.match,lsi,by="household_questionnaire_id")
# after 1 year
df.match1 <- filter(df.match,year==2019)
table1 <- CreateTableOne(vars = c('irrigated_land'),data = df.match1,strata = 'TreatmentControl')

table11 <- print(table1,printToggle = FALSE,noSpaces = TRUE)

table11 [,1:3]%>%kable() %>% kable_styling(full_width = F, position = "left")




rm (match.it,df.match,df.match1,df.match2,table1,table11,table2,table22)
