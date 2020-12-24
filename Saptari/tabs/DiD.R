# total_litres_consumed_dieselkero    SAPTARI----
litres_S <-  Procurement_17_18_19 %>%
  filter(!is.na(total_litres_consumed_dieselkero))%>%
  mutate(after_1Y = year == 2018, 
         after_2Y = year == 2019, 
         own_sp = TC == 1)

ggplot(litres_S, aes(year, total_litres_consumed_dieselkero, color = TreatmentControl)) +
  stat_summary(geom = 'line') +
  theme_minimal()

model1 <-  lm(total_litres_consumed_dieselkero ~ after_1Y*own_sp, data = litres_S)
summary(model1)

model2 = lm(total_litres_consumed_dieselkero ~ after_2Y*own_sp, data = litres_S)
summary(model2)

ggplot(litres_S, aes(year, total_litres_consumed_dieselkero, color = own_sp)) +
  geom_jitter() +
  theme_minimal()+
  scale_x_continuous(breaks = c(2017,2018,2019))
  
# total_litres_consumed_dieselkero    RBS    ----
litres_RBS <-  Procurement_18_19 %>%
  filter(!is.na(total_litres_consumed_dieselkero),
         total_litres_consumed_dieselkero<10000)%>%
  mutate(after_1Y = year >= 2019, 
         own_sp = TC >= 1)

ggplot(litres_RBS, aes(year, total_litres_consumed_dieselkero, color = TreatmentControl)) +
  stat_summary(geom = 'line') +
  theme_minimal()

# TRUE and FALSE model

# Create two additional dummy variables to indicate before/after
# and treatment/control groups.


model = lm(total_litres_consumed_dieselkero ~ after_1Y*own_sp, data = litres_RBS)
summary(model)

ggplot(litres_RBS, aes(after_1Y, total_litres_consumed_dieselkero, color = own_sp)) +
  geom_jitter() +
  theme_minimal()

# 1 and 0 model
litres_RBS$aftr = as.numeric(dataf$year>=2019)
litres_RBS$sp = as.numeric(dataf$TC >= 1)

DiD=litres_RBS$aftr*litres_RBS$sp #Interaction term

fit=lm(total_litres_consumed_dieselkero~dataf$aftr+dataf$sp+DiD,data=litres_RBS)
summary(fit)

ggplot(litres_RBS, aes(aftr, total_litres_consumed_dieselkero, color = sp)) +
  geom_jitter() +
  theme_minimal()



# Irrigation Intensity -------------- SAPTARI----
irri_intens_S <- land_17_18_19 %>% filter(total_land_cultivated_year>0,season!="Annual") %>% 
  group_by(TreatmentControl,year,household_questionnaire_id) %>%
  summarise(gross_irri=sum(irrigated_out_of_tot_land_cult),gross_crop=sum(total_land_cultivated),ii=gross_irri/gross_crop*100) %>% 
  filter(ii>= 0,ii<=100) 

irri_intens_S <- irri_intens_S %>% 
  mutate(after_1Y = year == 2018, 
         after_2Y = year == 2019, 
         own_sp = TreatmentControl == "Treatment")

ggplot(irri_intens_S, aes(year, ii, color = TreatmentControl)) +
  stat_summary(geom = 'line') +
  theme_minimal()+
  scale_x_continuous(breaks = c(2017,2018,2019))

model1 <-  lm(ii ~ after_1Y*own_sp, data = irri_intens_S)
summary(model1)

model2 = lm(ii ~ after_2Y*own_sp, data = irri_intens_S)
summary(model2)

ggplot(irri_intens_S, aes(year, ii, color = own_sp)) +
  geom_jitter() +
  theme_minimal()+
  scale_x_continuous(breaks = c(2017,2018,2019))

#  wem_liter_fuel_18_19  - p123_year---------------------------
litres_rbs <-  wem_liter_fuel_18_19 %>%
  filter(!is.na(p123_year))%>%
  mutate(after_1Y = year == 2019, 
         own_sp = TC == 1)

ggplot(litres_rbs, aes(year, p123_year, color = TreatmentControl)) +
  stat_summary(geom = 'line') +
  theme_minimal()+
  scale_x_continuous(breaks = c(2018,2019))

ggplot(litres_rbs, aes(year, p123_year, color = own_sp)) +
  geom_jitter() +
  theme_minimal()+ 
  scale_x_continuous(breaks = c(2018,2019))

model1 <-  lm(p123_year ~ after_1Y*own_sp, data = litres_rbs)
summary(model1)
# -----------------------
  