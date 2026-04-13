library(dplyr)
library(haven)
library(tidyr)
library(ggplot2)
library("stringr") #"str_replace"
library(summarytools)

library(rstatix) # ttest "add_significance"
library(rempsyc) # ttest # nice_table
library(kableExtra )
library(tidyverse)


###### Cha.3 Determinants of Adoption  #### ####

df1 <-
  irrigation_BL_to_22 %>% 
  select(hh_id ,starts_with("drip"),
         flood_use_6y,flood_use_2021_22,flood_use_BL,
         ir_use_6y,ir_use_2021_22,ir_use_BL)

df2 <-
  a_source_irri %>%
  mutate(source_ramthal= ifelse(source_ramthal=="ramthal",1,0)
  ) %>%
  mutate(source_own_6y= ifelse(source_borwell %in% c("rain","ramthal"),0,1),
         source_canal_6y = ifelse(source_canal  %in% c("rain","ramthal"),0,1),
         source_pond_6y = ifelse(source_pond  %in% c("rain","ramthal"),0,1),
         source_schema_6y = ifelse(source_pond == 1| source_canal==1,1,0)) %>%
  select(hh_id,source_ramthal,source_own_6y,source_schema_6y) %>%
  left_join(
    BL_source_ir %>% select(hh_id,ir_source_own_bl,ir_source_schema_bl)
  ) %>% rename(
    source_own_bl = ir_source_own_bl  ,
    source_schema_bl = ir_source_schema_bl )

df3 <-
  rmtl_cntrl_vars %>%
  select(hh_id, in_project,
         hh_haed_age, hh_haed_gendar,
         hh_haed_edu_level, literate_hh_pct,
         public_assistance,govPnsin_scheme, official_assistance,
         bpl_card,caste_01,
         total_acre16,housing_str321,total_livestock,
         # dist_Km_boundary,Elevation,elevation_m,
         # cardinal_direction, zone,village
  ) %>%
  mutate(own_livestock_01=ifelse(total_livestock == 0,0,1)) # %>%
  # mutate(
  # zone=ifelse(zone =="i","I",zone),
  # zone=ifelse(zone =="ii","II",zone),
  # zone=ifelse(zone =="iii","III",zone),
  # zone=ifelse(zone =="iv","IV",zone),
  # )

df4 <-
  irrigation_BL_to_22 %>%
  select(hh_id,starts_with("drip"),starts_with("flood"),
         starts_with("ir"),-contains("method") ) %>%
  pivot_longer(
    -hh_id,
    names_to = c("Type", "Year"),
    names_pattern = "(drip|flood|ir)_use_(.*)",
    values_to = "use"
  ) %>% filter(!Year %in% c("BL","6y","2021_22")) %>%
  group_by(hh_id,Type) %>%
  summarise(usage_freq = sum(use, na.rm = T) ,.groups = "drop"
  ) %>%   pivot_wider(names_from = "Type",values_from ="usage_freq" ) %>%
  rename_with(~ paste0("total.Ys_", .x),-hh_id)


df5 <-
  rmtl_baseline2016 %>%
  select(hh_id,starts_with("F")) %>% 
  select(hh_id, contains("year"),-F3_year,-F4_year, -F12_year)%>%
  pivot_longer(-hh_id ,
               names_to = "income_type_f",
               values_to = "income22") %>% 
  mutate(income_type=
           ifelse(income_type_f %in% c("F1_year","F2_year","F9_year"),
                  "income_assistance", # Income by seasonal/  permanent  migrating OR Government pension or scheme
                  "income_NonCrop" ))  %>% 
  group_by(hh_id, income_type ) %>% 
  summarise(income_NonCrop = sum(income22,na.rm = T)/1000,.groups = "drop"
  ) %>% 
  mutate(income_NonCrop_01=ifelse(income_NonCrop == 0,0,1) 
  ) %>% select(-income_NonCrop) %>% 
  pivot_wider(names_from = income_type,
              values_from =income_NonCrop_01 )


  
# [I21]	Have you attended?  #I20 Have any presentations been made in your village on this project?
# Demonstration plots: [I24] Have you ever gone to visit them?  
# [I34]	Have you attended any of the trainings organized by the implementers of the project?
#
df6 <-
  rmtl_baseline2016 %>%
  select(hh_id,I21, I24,I34) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
  setNames(c("hh_id", 
             "attended_to_presentation",
             "visit_demo_plot",
             "attended_to_trainings"))

df_drip_bl <-
  hh_2022 %>% rename(in_project =in1_out0 ) %>%
  left_join(df1) %>%
  left_join(df2) %>%
  left_join(df3) %>%
  left_join(df4) %>%
  left_join(df5) %>%
  left_join(df6)

library(writexl)
write_xlsx(df_drip_bl, "C:/Users/Dan/Downloads/df_drip_bl_250326.xlsx")

df_project <- df_drip_bl %>%
  filter(in_project == 1) %>%
  mutate(total_acre16=ifelse(total_acre16 > 40, NA, total_acre16 ) ) %>%
  mutate(total_acre16=total_acre16/10,
         hh_haed_age=hh_haed_age/10)

# DS ----


df_project %>%
  group_by(drip_use_6y) %>%
  summarise(
    hh_haed_age = mean(hh_haed_age,na.rm=T)*10,
    hh_haed_gendar= mean(hh_haed_gendar,na.rm=T),
    literate_hh_pct= mean(literate_hh_pct,na.rm=T),
    source_schema_bl =mean(source_schema_bl,na.rm=T),
    source_own_bl =mean(source_own_bl,na.rm=T),
    attended_to_trainings =mean(attended_to_trainings,na.rm=T),
    own_livestock_01 =mean(own_livestock_01,na.rm=T),
    income_NonCrop =mean(income_NonCrop,na.rm=T),
    ir_use_BL =mean(ir_use_BL,na.rm=T),
    total_acre16=mean(total_acre16,na.rm=T)*10
  ) %>%
  pivot_longer(-drip_use_6y,names_to = "name",values_to = "val") %>%
  mutate(val= 
           ifelse(!name %in% c("hh_haed_age", "total_acre16"),
                  val*100, val) ) %>%
  pivot_wider(names_from =drip_use_6y,values_from = val) # %>%
  kableExtra::kable() %>%kableExtra::kable_minimal()


  
  
  
### REG ----



names(df_project)  

#  Model 1: Adoption (Ever used drip)
model_DI_6y <-
  lm(drip_use_6y ~ 
       hh_haed_age + hh_haed_gendar + literate_hh_pct +
       source_own_bl + source_schema_bl + 
       attended_to_trainings + own_livestock_01+
       income_NonCrop + ir_use_BL + total_acre16,
     data = df_project )
sjPlot::tab_model(model_DI ,show.se = T,digits = 4,show.stat  = F)

model_F_6y <-
  lm(flood_use_6y ~ 
       hh_haed_age + hh_haed_gendar + literate_hh_pct +
       source_own_bl + source_schema_bl + 
       attended_to_trainings + own_livestock_01+
       income_NonCrop + ir_use_BL + total_acre16,
     data = df_project )
sjPlot::tab_model(model_F ,show.se = T,digits = 4,show.stat  = F)



model_DI_6th <-
  lm(drip_use_2021_22 ~
       hh_haed_age + hh_haed_gendar + literate_hh_pct +
       source_own_bl + source_schema_bl + 
       attended_to_trainings + own_livestock_01+
       income_NonCrop + ir_use_BL + total_acre16,
     data = df_project )
sjPlot::tab_model(model_DI6 ,show.se = T,digits = 4,show.stat  = F)

model_F_6th <-
  lm(flood_use_2021_22 ~
       hh_haed_age + hh_haed_gendar + literate_hh_pct +
       source_own_bl + source_schema_bl + 
       attended_to_trainings + own_livestock_01+
       income_NonCrop + ir_use_BL + total_acre16,
     data = df_project )
sjPlot::tab_model(model_F6 ,show.se = T,digits = 4,show.stat  = F)


library(tidyr)
library(purrr)
library(broom)


df10 <- #assets_22 ##assets
  irrigation_BL_to_22 %>% 
  select(hh_id ,starts_with("drip")) %>% 
  pivot_longer(-hh_id ,names_to = "drip_yr",values_to = "use")%>% 
# pivot_longer(-hh_id ,names_to = "Asset",values_to = "Num_assets_22")
  left_join(hh_2022 %>% rename(in_project =in1_out0 )) %>%
  left_join(irrigation_BL_to_22 %>% 
              select(hh_id ,ir_use_BL) ) %>%
  left_join(df2) %>%
  left_join(df3) %>%
  left_join(df4) %>%
  left_join(df5) %>%
  left_join(df6) %>%
  filter(in_project == 1) %>%
  mutate(total_acre16=ifelse(total_acre16 > 40, NA, total_acre16 ) ) %>%
  mutate(total_acre16=total_acre16/10,
         hh_haed_age=hh_haed_age/10)







fml_assets

# 1) model formula inputs
fml <- use ~ hh_haed_age + hh_haed_gendar + literate_hh_pct +
  source_own_bl + source_schema_bl + 
  attended_to_trainings + own_livestock_01+
  income_NonCrop + ir_use_BL + total_acre16


# 2) Nest by status and fit 
fits <- df10 %>% # fits_assets_22
  group_by(drip_yr) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(fml, data = .x)),
    coefs = map(model, tidy),
    stats = map(model, glance))

# 3) Stacked outputs
inproj_with_fit <- # inproj_with_fit_assets
  fits %>% unnest(coefs) %>% 
  select(drip_yr,term, estimate, std.error, p.value) %>% ungroup()  %>% 
  pivot_longer(-c(drip_yr,term), names_to = "metric", values_to = "value") %>% 
  pivot_wider(names_from = drip_yr,values_from = value )
##
nr2 <- fits %>% unnest(stats) %>%  
  select(drip_yr,nobs, r.squared) %>% 
  rename(Num.Obs.=nobs,R2=r.squared ) %>% ungroup() %>% 
  pivot_longer(cols = c(`Num.Obs.`, R2),
               names_to = "term",values_to = "vl") %>%
  mutate(vl=round(vl, digits = 4)) %>% 
  pivot_wider(names_from = drip_yr,values_from = vl)


df_html <- inproj_with_fit %>% # df_html_assets
  mutate(across(-c(term, metric), ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  ))) %>% 
  select(-metric) %>% 
  rbind(nr2) %>% 
  select(term,drip_use_2017,drip_use_2018,drip_use_2019,drip_use_2020,drip_use_2021,drip_use_2021_22  )

df_html %>% kable() %>% kable_classic_2()

models_list <- list("Drip   16-22" = model_DI_6y,"Flood 16-22" = model_F_6y)
# models_list <- list("Drip   6th" = model_DI_6th,"Flood 6th" = model_F_6th)
P=
plot_summs(
  models_list ,
  coefs = c(
    "Age (Decades)" = "hh_haed_age" ,
    "Gender (Male)" = "hh_haed_gendar" , 
    "Literacy" = "literate_hh_pct", 
    "Own borewell" = "source_own_bl",
    "Gov. irrigation program" = "source_schema_bl" , 
    "Attended to trainings" = "attended_to_trainings"  ,
    "Own livestock" = "own_livestock_01" ,
    "External work income" = "income_NonCrop"  ,
    "Prior irrigation experience" = "ir_use_BL"  ,
    "Land holding (10 acres)" = "total_acre16"),
  legend.title = "", colors = c("#026bb9","#619847"),
  point.size = 1,line.size = c(.6, .6),
  inner_ci_level = NULL, point.shape = F)

P + ggtitle("2016-2022") +
  geom_text(aes(label = round(estimate,2) ),size=3,position = position_dodge(1.2)
  )

# P + ggtitle("2022") +geom_text(aes(label = round(estimate,2) ),size=3,position = position_dodge(1.2))



models_list <- fits %>% 
  filter( ! drip_yr %in% c("drip_use_BL","drip_use_6y","drip_use_2022" )) %>% 
  { setNames(.$model, .$drip_yr ) }

plot_summs(
  models_list ,
  coefs = c(
    "Age (Decades)" = "hh_haed_age" ,
    "Gender (Male)" = "hh_haed_gendar" , 
    "Literacy" = "literate_hh_pct", 
    "Own borewell" = "source_own_bl",
    "Gov. irrigation program" = "source_schema_bl" , 
    "Attended to trainings" = "attended_to_trainings"  ,
    "Own livestock" = "own_livestock_01" ,
    "External work income" = "income_NonCrop"  ,
    "Prior irrigation experience" = "ir_use_BL"  ,
    "Land holding (10 acres)" = "total_acre16"),
  colors = "Qual3", # "Qual1", "Qual2", "Qual3", "CUD", "CUD Bright",  "Rainbow"
  legend.title = "Models: \nProject \nYear ",
  point.size = 1,line.size = c(.6, .6),
  model.names = c("1st", "2nd","3rd", "4th", "5th","6th" ),
  inner_ci_level = NULL, point.shape = F)




























map_dfr(models_list, tidy, conf.int = TRUE, .id = "Model") %>% 
  filter(term != "(Intercept)") %>% 
  ggplot( aes(x = estimate, y = term , color = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray45",size=.5) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.5),
                  linewidth = .75,  # SE line thickness
                  fatten = 1)