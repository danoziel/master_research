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


###### Cha.3 Adopters characteristics  #### ####

df1 <- 
  irrigation_BL_to_22 %>% 
  select(hh_id,
         drip_use_6y,drip_use_2021_22,drip_use_BL,
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
         hh_haed_age, hh_haed_gendar,hh_haed_edu_level,literate_hh_pct,caste_01,
         official_assistance, bpl_card,
         total_acre16,housing_str321,
         dist_Km_boundary,Elevation,elevation_m,
         cardinal_direction, zone,village   
  ) %>% 
  left_join(df_income_NonCrop_bl_binary) %>% 
  mutate(hh_haed_edu_years=case_when(
    hh_haed_edu_level == 1 ~ 6,
    hh_haed_edu_level == 2 ~ 9,
    hh_haed_edu_level == 3 ~ 12,
    hh_haed_edu_level == 4 ~ 15,
    hh_haed_edu_level == 5 ~ 18,
    TRUE ~ 21
  ),zone=ifelse(zone =="i","I",zone),
  zone=ifelse(zone =="ii","II",zone),
  zone=ifelse(zone =="iii","III",zone),
  zone=ifelse(zone =="iv","IV",zone),
    )

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



df_drip_bl <- 
  hh_2022 %>% rename(in_project =in1_out0 ) %>% 
  left_join(df1) %>% 
  left_join(df2) %>% 
  left_join(df3)%>% 
  left_join(df4)

library(writexl)
write_xlsx(df_drip_bl, "C:/Users/Dan/Downloads/df_drip_bl.xlsx")


names(df_drip_bl)

df_overlap = 
  irrigation_BL_to_22 %>% 
  left_join(hh_2022) %>% filter(in1_out0 == 1) %>%
  select(hh_id,starts_with("drip"),starts_with("flood"),-ends_with("2022")) %>% 
  pivot_longer(-hh_id,names_to = "name",values_to = "vl") %>% 
  mutate(name= ifelse(name=="drip_use_2021_22","drip_use_2022",name))%>% 
  mutate(name= ifelse(name=="flood_use_2021_22","flood_use_2022",name)) %>% 
  separate(name,into = c("ir","yr"),sep="_use_") %>% 
  pivot_wider(names_from = "ir",values_from = "vl") %>% 
  mutate(drip_flood = ifelse(drip==1 & flood ==1,1,0 ))%>% 
  group_by(yr) %>% 
  summarise(dripPCT=mean(drip,na.rm=T)*100, 
            floodPCT=mean(flood,na.rm=T)*100,
            overlapPCT=mean(drip_flood,na.rm=T)*100
  )

df_overlap[c(8,1:7),] %>%   mutate(across(where(is.numeric), ~ round(.x, 2))) %>% 
  kable() %>% kable_paper()


df_project %>% 
  select(source_ramthal, # source_ramthal==1, n/384, n/562
         ir_use_BL,  # source_ramthal==1, n/78, n/868 
         source_own_bl, # source_own_bl==1,n/27,n/919
         source_schema_bl, 
         drip_use_6y,  # mutate(pct=ifelse(drip_use_6y == 1, n/291, n/655))
         flood_use_6y  # mutate(pct=ifelse(flood_use_6y == 1, n/178, n/751))
         ) %>% 
  count(drip_use_6y,ir_use_BL ) %>% 
  mutate(pct=ifelse(drip_use_6y == 1, n/291, n/655))






df_drip_bl %>% filter(in_project == 1) %>% 
  select(total.Ys_drip , total.Ys_flood) %>% 
  pivot_longer(everything(),names_to = "Type",values_to = "usage_freq") %>% 
  filter(usage_freq > 0) %>% 
  group_by(Type) %>%
  summarise(usage_freq = mean(usage_freq, na.rm = T) ,.groups = "drop"
  )

                 

df_3_DI <- 
  irrigation_BL_to_22 %>%
  left_join(df2 %>%  select(hh_id,source_ramthal)) %>% 
  right_join(rmtl_cntrl_vars %>% select(hh_id, in_project)) %>% 
  filter(in_project == 1) %>% 
  select(hh_id,source_ramthal,starts_with("drip"),starts_with("flood")
  ) %>% 
  select(starts_with("drip"),starts_with("flood"), 
         -ends_with("2021_22"),-ends_with("6y")
  ) %>%  pivot_longer(
    cols = everything(),
    names_to = c("Type", "Year"),
    names_pattern = "(drip|flood)_use_(.*)",
    values_to = "use"
  )%>%
  group_by(Type, Year) %>%
  summarise(adoption_rate = mean(use, na.rm = TRUE),.groups = "drop"
  )




df_ir_in <- 
  irrigation_BL_to_22 %>%
  left_join(df2 %>%  select(hh_id,source_ramthal)) %>% 
  right_join(rmtl_cntrl_vars %>% select(hh_id, in_project)) %>% 
  filter(in_project == 1) %>% 
  select(hh_id,source_ramthal,starts_with("drip"),starts_with("flood")
         ) %>% 
  select(starts_with("drip"),starts_with("flood"), 
         -ends_with("2021_22"),-ends_with("6y")
  ) %>%  pivot_longer(
    cols = everything(),
    names_to = c("Type", "Year"),
    names_pattern = "(drip|flood)_use_(.*)",
    values_to = "use"
  )%>%
  group_by(Type, Year) %>%
  summarise(adoption_rate = mean(use, na.rm = TRUE),.groups = "drop"
  )










df_ir_in_sr <- 
  irrigation_BL_to_22 %>%
  left_join(df2 %>%  select(hh_id,source_ramthal)) %>% 
  right_join(rmtl_cntrl_vars %>% select(hh_id, in_project)) %>% 
  filter(in_project == 1,
         source_ramthal==1) %>% 
  select(hh_id,source_ramthal,starts_with("drip"),starts_with("flood"), -ends_with("2021_22")
  )

df_ir_in_Fsr <- df_ir_in_sr %>%
  select(starts_with("drip"),starts_with("flood"), -ends_with("2021_22"),-ends_with("6y")
  ) %>%  pivot_longer(
    cols = everything(),
    names_to = c("Type", "Year"),
    names_pattern = "(drip|flood)_use_(.*)",
    values_to = "use"
  )%>%
  group_by(Type, Year) %>%
  summarise(adoption_rate = mean(use, na.rm = T)/2.463542,.groups = "drop"
  ) 




df_plot <- df_ir_in %>%
  select(starts_with("drip"),starts_with("flood"), 
         -ends_with("2021_22"),-ends_with("6y")
         ) %>%  pivot_longer(
    cols = everything(),
    names_to = c("Type", "Year"),
    names_pattern = "(drip|flood)_use_(.*)",
    values_to = "use"
  )%>%
  group_by(Type, Year) %>%
  summarise(adoption_rate = mean(use, na.rm = TRUE),.groups = "drop"
  ) %>%
  rbind(df_ir_in_Fsr%>% 
          mutate(
            Type = recode(Type,flood = "Flood. Ramthal source")) %>% 
          filter(Type != "drip")
        ) %>% 
  mutate(
    Type = recode(Type,drip  = "Drip",flood = "Flood"),
    Year = recode(Year,"BL"   = "Baseline","2017" = "1st\n2017","2018" = "2nd\n2018","2019" = "3rd\n2019","2020" = "4th\n2020","2021" = "5th\n2021","2022" = "6th\n2022"),
    Year = factor( Year,levels = c("Baseline","1st\n2017","2nd\n2018","3rd\n2019","4th\n2020","5th\n2021","6th\n2022"))
    ) 

drip_use_2018     138
flood_use_2018    44
overlapp 5
both  177

drip_use_2017     71
flood_use_2017    93
overlapp 0
both  167



  df_plot %>% 
  filter(Type != "Flood. Ramthal source" ) %>% 
  ggplot(aes(x = Year, y = adoption_rate, group = Type, color = Type)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  geom_text(
    aes(label = scales::percent(adoption_rate, accuracy = 1)),
    vjust = -1, size = 4, family = "serif",show.legend = F
    ) +
  scale_color_manual(values = c("Drip"  = "#4d85c6",
                                "Flood" = "#274e13ff"  #  "#93c47d", "Flood. Ramthal source" ="#38761d"
                                )) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, NA)
                     ) +
  labs(x = "",y = "Share of farmers (%)",color = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(text = element_text(family = "serif"),
        panel.grid = element_blank(),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
  )

ggsave("line_plot.png", p, bg = "transparent",
       width = 10, height = 3)


# DS 

df_drip_bl %>%
  filter(in_project == 1) %>% 
  mutate(total_acre16=ifelse(total_acre16 > 40, NA, total_acre16 ) ) %>% 
  summarise(
  DI.6Ys=mean(drip_use_6y,na.rm=T),
  DI.6th=mean(drip_use_2021_22,na.rm=T),
  Flood.6Ys=mean(flood_use_6y,na.rm=T),
  Ir.6Ys=mean(ir_use_6y,na.rm=T),
  Ramthal.as.Source=mean(source_ramthal,na.rm=T),
  ) %>% 
  pivot_longer(c(DI.6Ys:Ramthal.as.Source ),names_to = "name",values_to = "val") %>% 
  mutate(val=val*100) %>% 
  kableExtra::kable() %>%kableExtra::kable_minimal()

df_drip_bl %>%
  filter(in_project == 1) %>% 
  mutate(total_acre16=ifelse(total_acre16 > 40, NA, total_acre16 ) ) %>% 
  group_by(drip_use_6y) %>% 
  summarise(
    hh_haed_age = mean(hh_haed_age,na.rm=T),
      hh_haed_gendar= mean(hh_haed_gendar,na.rm=T),
      literate_hh_pct= mean(literate_hh_pct,na.rm=T),
      source_schema_bl =mean(source_schema_bl,na.rm=T),
      source_own_bl =mean(source_own_bl,na.rm=T),
      income_NonCrop =mean(income_NonCrop,na.rm=T),
      total_acre16=mean(total_acre16,na.rm=T)
  ) %>% 
  pivot_longer(-drip_use_6y,names_to = "name",values_to = "val") %>% 
  mutate(val= ifelse(!
    name %in% c("hh_haed_age", "total_acre16"),val*100,val) )%>% 
  pivot_wider(names_from =drip_use_6y,values_from = val ) %>% 
  kableExtra::kable() %>%kableExtra::kable_minimal()





# 2. Filter: Treatment group, limit to 20 acres, and remove missing land values
df_project <- df_drip_bl %>%
  filter(in_project == 1) %>% 
  mutate(total_acre16=ifelse(total_acre16 > 40, NA, total_acre16 ) ) %>% 
  mutate(total_acre16=total_acre16/10,
         hh_haed_age=hh_haed_age/10) %>% 
  left_join(df_302)


#  Model  FE zone: Adoption (Ever used drip)   ----
model1_feZ <- 
  lm(drip_use_6y ~ 
       hh_haed_age + hh_haed_gendar + literate_hh_pct +  
       caste_01 + housing_str321 + 
       source_schema_bl + source_own_bl + income_NonCrop+
       official_assistance + total_acre16 +  
       factor(zone),
     data = df_project )
sjPlot::tab_model(model1_feZ ,  show.se = T,digits = 4, show.stat  = F )

# plot FE coeff CI

tidy(model1_feZ, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate( Category = 
            ifelse(term %in% c("factor(zone)II","factor(zone)III","factor(zone)IV" )
                   , "B", "A") ) %>% 
  ggplot(aes(x = estimate, y = reorder(term, Category), color = Category)) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0, size = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray45", size = 0.5) +
  scale_color_manual(values = c("A" = "#3d85c6", "B" = "purple4")) +
  labs(title = "model  ",x = "Estimated Coefficient Value", y = NULL, caption = "Age and gender refer to the household head"
) +
  theme_minimal() + 
  theme(text = element_text(family = "serif"),
        legend.position = "none", panel.grid = element_blank())
# Notes:
# Age is divided by 10; coefficients reflect a 10-year increase.
# Land area is divided by 10; coefficients reflect a 10-acre increase.
# Age and gender refer to the household head









# 3 models ----

#  Model 1: Adoption (Ever used drip)
model_DI <- 
  lm(drip_use_6y ~ 
       hh_haed_age + hh_haed_gendar + literate_hh_pct + 
       ir_use_BL + 
       # source_schema_bl + source_own_bl + 
       income_NonCrop + total_acre16,
     data = df_project )
sjPlot::tab_model(model_DI ,show.se = T,digits = 4,show.stat  = F)

model_DI2 <- 
  lm(drip_use_2021_22 ~ 
       hh_haed_age + hh_haed_gendar + literate_hh_pct + 
       ir_use_BL + 
       # source_schema_bl + source_own_bl + 
       income_NonCrop + total_acre16,
     data = df_project )


#  Model 3: flood_use_6y   #134f5c
model_FI <- 
  lm(flood_use_6y ~ 
       hh_haed_age + hh_haed_gendar + literate_hh_pct + 
       ir_use_BL + 
       # source_schema_bl + source_own_bl + 
       income_NonCrop + total_acre16,
     data = df_project )

model_FI2 <- 
  lm(flood_use_2021_22 ~ 
       hh_haed_age + hh_haed_gendar + literate_hh_pct + 
       ir_use_BL + 
       # source_schema_bl + source_own_bl + 
       income_NonCrop + total_acre16,
     data = df_project )

sjPlot::tab_model(model_FI ,  show.se = T,digits = 4, show.stat  = F )

#  Model 3: source_ramthal  #4a86e8ff
model_SR <- 
  lm(source_ramthal ~ 
       hh_haed_age + hh_haed_gendar + literate_hh_pct + 
       ir_use_BL+ # source_schema_bl + source_own_bl + 
       income_NonCrop + total_acre16,
     data = df_project )
sjPlot::tab_model(model_SR ,  show.se = T,digits = 4, show.stat  = F )




# NON LINEAR CONNECTIONS ----

# total_acre16 non linear reg
df_project <- df_drip_bl %>%
  filter(in_project == 1, 
         total_acre16 < 40, 
         !is.na(total_acre16)) %>%
  mutate(land_decile = ntile(total_acre16, 10)) %>%  # Create 10 bins (deciles) 
  mutate(elev_decile = ntile(elevation_m, 10)) %>% 
  mutate(dist_decile = ntile(dist_Km_boundary, 10))

# Load necessary libraries
library(dplyr)
library(ggplot2)



# Calculate Mean Acres and Ever Used Rate (6y) for each bin
df_pA <- df_project %>%
  group_by(land_decile) %>%
  summarize(
    mean = mean(total_acre16, na.rm = TRUE),
    ever_used_rate = mean(drip_use_6y, na.rm = TRUE))

df_pE <- df_project %>%
  group_by(elev_decile) %>%
  summarize(
    mean = mean(elevation_m, na.rm = TRUE),
    ever_used_rate = mean(drip_use_6y, na.rm = TRUE)) %>% 
  filter(!is.na(elev_decile))

df_pD <- df_project %>%
  group_by(dist_decile) %>%
  summarize(
    mean = mean(dist_Km_boundary, na.rm = TRUE),
    ever_used_rate = mean(drip_use_6y, na.rm = TRUE)) %>% 
  filter(!is.na(dist_decile))



#  Create the line plot
df_pA %>% 
  ggplot(aes(x = land_decile, y = ever_used_rate)) +
  geom_line(color = "blue4", size = 1) + geom_point(color = "blue4", size = 3) +
  geom_text(aes(label = round(ever_used_rate, 2)), vjust = -1, size = 3.5) + # Add value labels
  scale_y_continuous(limits = c(0, 0.5)) +
  labs( title = "Drip Adoption Rate by Mean",
        subtitle = "Bins created as deciles (equal number of farms per point)",
        x = "Mean in Bin",y = "Ever Used Rate (6y)") +
  theme_minimal() + theme(text = element_text(family = "serif"),
                          panel.grid.minor = element_blank()) # Load necessary libraries




model4 <- 
  lm(drip_use_6y ~ 
       hh_haed_age + hh_haed_gendar + literate_hh_pct +  
       caste_01 + housing_str321 + source_old + official_assistance + 
       total_acre16 + I(total_acre16^2) + 
       dist_Km_boundary + I(dist_Km_boundary^2) +
       factor(zone),
     # factor(cardinal_direction), 
     # factor(village),
     data = df_project )
sjPlot::tab_model(model4 ,  show.se = T,digits = 5, show.stat  = F )



# PLOT REG COEFF + CI  ----
# 
library(dplyr)
library(ggplot2)
library(broom)


model_DI "#3d85c6"   drip_use_6y
model_FI "#6aa84f"   flood_use_6y
model_SR  "#7f6000"  source_ramthal 



library(purrr)

# Put models in a named list
model_list <- list("Drip Use" = model_DI,
                   "Flood use" = model_FI # , "Ramthal as source" = model_SR
                   )

model_list_2 <- list("Drip Use" = model_DI2, "Flood use" = model_FI2
                   )

# Tidy all of them at once and combine
all_models <- map_dfr(model_list_2, tidy, conf.int = TRUE, .id = "Model") %>% 
  mutate(CleanName = case_when(
    term == "hh_haed_age" ~ "Age (Decades)",
    term == "hh_haed_gendar" ~ "Gender (Male)",
    term == "literate_hh_pct" ~ "Literacy (%)",
    # term == "source_schema_bl" ~ "Gov. irrigation program",
    # term == "source_own_bl" ~ "Own borewell",
    term == "ir_use_BL" ~ "Prior irrigation experience",
    term == "income_NonCrop" ~ "External work income",
    term == "total_acre16" ~ "Land holding (10 acres)" ), 
    Model = factor(Model, levels = c("Ramthal as source","Flood use","Drip Use")),
    CleanName = 
      factor(CleanName, levels = c(
        "Land holding (10 acres)", 
        # "Own borewell", "Gov. irrigation program",
        "Prior irrigation experience","External work income",
        "Literacy (%)", "Gender (Male)", "Age (Decades)" ))
  ) %>% filter(term != "(Intercept)") 


p=
ggplot(all_models, aes(x = estimate, y = CleanName , color = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray45",size=.5) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.5),
                  linewidth = 2.5,  # SE line thickness
                  fatten = 1) +     # center point size
  labs(title = "Model ",x = "Estimated Coefficient Value", y = NULL) +
  theme_minimal(base_size = 48) + 
  theme(text = element_text(family = "serif"),legend.position = "right",
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
  ) + scale_color_manual(
    breaks = c("Ramthal as source","Flood use","Drip Use"),
    values = c("Drip Use" = "#4a86e8",
               "Flood use" ="#6aa84fcc" , # "#6aa84f",
               "Ramthal as source" = "#bf9000"))+
  guides(color = guide_legend(reverse = TRUE)) + xlim(-.2,.55)

ggsave("coef_plot.png", p, bg = "transparent",
       width = 32, height = 16)
# getwd()


cm <- c(
  "hh_haed_age"      = "Age (Decade)",
  "hh_haed_gendar"   = "Gender (Male)",
  "literate_hh_pct"  = "Literacy (%)",
  "source_schema_bl" = "Gov. irrigation program",
  "source_own_bl"    = "Own borewell",
  "income_NonCrop"   = "External work income",
  "total_acre16"     = "Land holding (10 acres)"
  )


modelsummary(model_list, coef_map = cm, stars = F, 
             statistic = c("std.error", "[{p.value}]"),
             gof_map = c("nobs", "r.squared"),
             title = "Outcoms: % of households using water in 2016-2022"
             )

modelsummary(model_list_6th, coef_map = cm, stars = F, 
             statistic = c("std.error", "[{p.value}]"),
             gof_map = c("nobs", "r.squared"),
             title = "Outcoms: % of households using water in 2021-22"
             )








# overlap data  ----

#| Used  source [Gov_supply] / infrastructure [Installed] / water [used] 

WI= a_rmtl_srvy22 %>% select(hh_id,mm4,mm5) %>% #| 🟦
left_join(a_sample[,1:2] )

WI5= WI %>% filter(mm5==1)

WIs=source_irri %>% #| 🟩 
filter(irri_source_num == 5) 

###### df WI5s
WI5s=inner_join( WI5[,c(1,4)] , WIs[,1] ) %>% mutate(overlap=1)
WI5s %>% count(farmers_hh) %>% mutate(grp=c(946,666 )) %>% mutate(n/grp)

# inside_ramthal  213 overlap [23%]
# outside_ramthal  48 overlap [7%]

ovl= a_rmtl_srvy22 %>% 
  select(hh_id,mm4,mm5) %>%    #| 🟦
left_join(source_irri) %>%   #| 🟩 
right_join(hh_irrigation) %>% #| 🟩 
left_join(a_sample[,1:2] )

ovl %>% count(farmers_hh,mm4,mm5,hh_irrigation)

ovl %>% count(farmers_hh,hh_irrigation,mm4,mm5)

ovl %>% count(farmers_hh,hh_drip, mm4,mm5 )

x=
  ovl %>% mutate(hh_6methods =ifelse(hh_6methods  %in% c("flood","furrows"),"furrows_flood",hh_6methods  )) %>% 
  count(farmers_hh,hh_6methods , irri_source  ) %>% 
  group_by(farmers_hh,hh_6methods) %>% 
  filter(!hh_6methods=="rain") %>%  
  group_by(farmers_hh)%>% mutate(ngrp=sum(n) ) %>% 
  mutate(prc=n/ngrp) %>% mutate_at(6,round,2) %>% 
  group_by(farmers_hh,hh_6methods) %>%  mutate(sum_method=sum(prc))
#


# mm2	gov projects but ramthal ----

# mm2		Is your land coming under such a government project? 
# 1	Ramrhal
# 2	Krishi Honda ( Farm pond)
# 3	Pradhan Mantri Krishi Sinchai Yojana(PMKSY)
# 4	Ganga kalyana
# 5	non

###### df Im_in_ramthal
rmtl_srvy22 %>% filter(in1_out0==1) %>% select(mm2_1,mm4) %>% mutate(mm214=mm2_1+mm4) %>% freq(mm2_1)
rmtl_srvy22 %>% filter(in1_out0==1)%>% select(mm2_1,mm4) %>% mutate(mm214=mm2_1+mm4) %>% freq(mm4)
rmtl_srvy22 %>% filter(in1_out0==1)%>% select(mm2_1,mm4) %>% mutate(mm214=mm2_1+mm4) %>% freq(mm214)

rmtl_srvy22 %>% filter(in1_out0==1) %>% freq(mm2)


rmtl_srvy22 %>% select(hh_id,in1_out0,mm2) %>% filter(in1_out0==1) %>% 
  separate_rows(mm2, sep = " ") %>%
  mutate(mm2 = trimws(mm2)) %>%   # clean any extra spaces
  filter(mm2 != "") %>% freq(mm2)
# 1	 61% Ramrhal  
# 2	 1%  Krishi Honda ( Farm pond)
# 4	 1%  Ganga kalyana
# 5	 37% non




#_____DF_Information _____________________ [info]             ----
#### knowledge about irrigation
# I3	Do you know any farmer in your village who uses it?
#### Demonstration plots
# I24	Have you ever gone to visit them?

info = 
  rmtl_baseline2016 %>% select(hh_id, I3 , I24) %>% 
  rename(know_frmr_uses_drip=I3, visit_demo_plot=I24)






--------------------------------------------------------------------------------
# Combine df [dt10] ----
--------------------------------------------------------------------------------
#
dt10 =
  rmtl_In_groups %>% 
  select(hh_id, infrstr_17_21, hh_drip_yrs_17_21, inf_drip_17_21) %>% 
  
  left_join(source_n_ir) %>%      # own_source_ir
  
  left_join(age_gndr_hh_head) %>%  # gndr # age
  left_join(caste) %>%             # caste_01 
  left_join(education16) %>%       # edu_hh_head_01
  left_join(economic16) %>%        # total_acre 
  left_join(info)                  # know_frmr_uses_drip

demography16=
  rmtl_In_groups %>% 
  select(hh_id, in1_out0 , south1_north0 , a5 ) %>% 
  left_join(age_gndr_hh_head) %>%  # gndr # age
  left_join(caste) 

# df10 2nd version 1.11.2025
# __________________________

dt10 <- rmtl_srvy22 %>% 
  select(hh_id, 
         m52,  # Have you attended any of the trainings ?
         m42   # Have you ever gone to visit it?
         ) %>% mutate(m42=ifelse(m42==-999,NA,m42)) %>% 
  rename(
    attended_trainings_22=m52,
    visit_demo_plot_22=m42
  ) %>%
  left_join(info) %>% # know_frmr_uses_drip visit_demo_plot
  rename(
    know_frmr_uses_drip_BL=know_frmr_uses_drip, 
    visit_demo_plot_BL=visit_demo_plot
         ) %>% # mutate(m42=ifelse(m42==-999,NA,m42)) %>% 
  left_join(rmtl_InOut) %>% rename(Elevation=elevation) %>% 
  filter(farmers_hh == "inside_ramthal") %>% 
  left_join(rmtl_con_vars) %>% 
  mutate(Income_sources = job_income_sourceS+govPnsin_scheme+rent_property) %>% 
  rename(
    Gendar=hh_haed_gendar,
    Age=hh_haed_age,
    Education=hh_haed_edu_level,
    Acre_Land=total_acre16,
    Caste=caste_01,
    Livestock=total_livestock,
    Farm_equipments=total_farm_equipments
  )

names(dt10)
summary(dt10)  


m_1 <- lm(drip_use ~ Gendar + Age + Education + Caste + Acre_Land + 
            Income_sources + Livestock + Farm_equipments,
          dt10)
summary(m_1)
sjPlot::tab_model(m_1, digits = 4, show.se = T)


m_2 <- lm(drip_use ~ 
            # know_frmr_uses_drip_BL + 
            # visit_demo_plot_BL +
            # attended_trainings_22 +
            visit_demo_plot_22,
          dt10)
sjPlot::tab_model(m_2, digits = 4, show.se = T)


dt10 %>%  group_by(drip_use) %>%  filter(!is.na(visit_demo_plot_22)) %>% 
  summarise(mean(visit_demo_plot_22,na.rm=T),n())



# DESCRIPTIVE STATISTICS

# destat
tapply(dt10$bpl_card, dt10$hh_drip_yrs_17_21, summary_stat)
tapply(dt10$bpl_card, dt10$infrstr_17_21, summary_stat)
#
tapply(dt10$official_assistance, dt10$hh_drip_yrs_17_21, summary_stat)
tapply(dt10$official_assistance, dt10$infrstr_17_21, summary_stat)
#
tapply(dt10$total_acre, dt10$hh_drip_yrs_17_21, summary_1_99)
tapply(dt10$total_acre, dt10$infrstr_17_21, summary_1_99)
#
tapply(dt10$total_plots, dt10$hh_drip_yrs_17_21, summary_stat)
tapply(dt10$total_plots, dt10$infrstr_17_21, summary_stat)
#
tapply(dt10$incomeK_2015, dt10$hh_drip_yrs_17_21, summary_stat)
tapply(dt10$incomeK_2015, dt10$infrstr_17_21, summary_stat)
##
##
dt10 %>% filter(is.na(caste_4321)) %>% count()
dt10 %>% group_by(hh_drip_yrs_17_21) %>% freq(caste_4321)
dt10 %>% group_by(infrstr_17_21) %>% freq(caste_4321)
##
## edu_hh_head_01  edu_hh_head educated_pct_hh
dt10 %>% group_by(hh_drip_yrs_17_21) %>% freq(edu_hh_head)
dt10 %>% group_by(hh_drip_yrs_17_21) %>% freq(educated_pct_hh)
tapply(dt10$educated_pct_hh, dt10$hh_drip_yrs_17_21, summary_stat)



##### COR   ####
my_data=dt10 %>% 
  select(
    gndr, age, caste_01, edu_hh_head_01, total_acre, own_source_ir ,know_frmr_uses_drip
  )%>%drop_na()
cor(my_data) %>% kable(format = "html", digits = 2) %>% kable_styling()


##### regs m1 infrstr_17_21      ----
 m11<-lm(infrstr_17_21 ~ 
          gndr + age + caste_01 + edu_hh_head_01  +
          total_acre16 +own_source_ir+know_frmr_uses_drip, dt10)
sjPlot::tab_model(m11, digits = 4, show.se = T)


##### regs m2 hh_drip_yrs_17_21  ----
m2 <- lm(hh_drip_yrs_17_21 ~ 
           gndr + age + caste_01 + edu_hh_head_01 + total_acre16, 
         dt10)

m3 <- lm(hh_drip_yrs_17_21 ~ gndr + age + caste_01 + edu_hh_head_01  +total_acre16 +
           own_source_ir+know_frmr_uses_drip, dt10)
summary(m2)
sjPlot::tab_model(m2, digits = 4, show.se = T)


##### regs m3 inf_drip_17_21     ----
m31<-lm(inf_drip_17_21 ~ 
          gndr + age + caste_01 + edu_hh_head_01  +
          total_acre +own_source_ir+know_frmr_uses_drip, dt10)
sjPlot::tab_model(m31, digits = 4, show.se = T)



--------------------------------------------------------------------------------
######|FIGs |---------------------------------------------------------- ----
library(ggplot2)
dev.off()

dt10 %>%
  group_by(total_acre_bin) %>%
  summarize(PCT = mean(infrstr_17_21),n()) %>% 
  ggplot(aes(x = total_acre_bin, y = PCT)) +
  geom_line() +geom_point() + theme_bw()+
  labs(x = "",y = "HH % infrastructure connection",title = "infrastructure connection/ total acre bins")

plot_mean_by_group <- function(data, x_var, y_var, group_var) {
  data %>%
    group_by({{group_var}}) %>%
    summarize(mean_value = mean({{y_var}}, na.rm = TRUE), count = n()) %>%
    ggplot(aes(x = {{group_var}}, y = mean_value)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    labs(x = "", y = "HH %", title = title)
}
color_bin <- c("infrstr_17_21" = "darkgreen", "inf_drip_17_21" = "darkblue")



#-----| TOTALAND |---------------------------------------------------------- ####

m1 <- lm(infrstr_17_21 ~ total_acre , dt10)
m1 <- lm(inf_drip_17_21 ~ total_acre , dt10)
summary(m1)
sjPlot::tab_model(m1, digits = 4, show.se = T)

title="infrstr_17_21/total_acre_bin"
plot_mean_by_group(dt10,total_acre_bin ,infrstr_17_21,total_acre_bin )

dt10 %>% select(infrstr_17_21,total_acre,total_acre_bin) %>% filter(!is.na(total_acre)) %>% group_by(total_acre_bin) %>%
  mutate(tb=mean(total_acre,na.rm = T)) %>% mutate_at(4,round,1) %>% mutate(tb=as.character(tb)) %>% 
  group_by(tb) %>%mutate(PCT = mean(infrstr_17_21)) %>% ungroup() %>% select(3,4,5 ) %>% distinct() %>%   arrange(total_acre_bin) %>%
  mutate(tb = factor(tb, levels = tb)
         ) %>% 
  ggplot(aes(x = tb, y = PCT)) +
  geom_line(aes(group = 1)) + 
  geom_point() +
  theme_bw() +
  labs(x = "Total Acre Bin")
  


title="total_acre_bin"
ta1=dt10 %>%group_by(total_acre_bin) %>%summarize(PCT = mean(infrstr_17_21)) %>% mutate(grp="infrstr_17_21")
ta2=dt10 %>%group_by(total_acre_bin) %>%summarize(PCT = mean(hh_drip_yrs_17_21)) %>% mutate(grp="hh_drip_yrs_17_21")
#ta3=dt10 %>%group_by(total_acre_bin) %>%summarize(PCT = mean(inf_drip_17_21,na.rm = T)) %>% mutate(grp="inf_drip_17_21")
rbind(ta1,ta2) %>% 
  ggplot(aes(x = total_acre_bin, y = PCT,colour =grp))+
  geom_line() +geom_point() + theme_bw()+ scale_colour_manual(values = color_bin)

dt10[,c(21,25)] %>% filter(total_acre_bin==1) %>% 
  summarise(min(total_acre16),max(total_acre16))


#-----| AGE      |---------------------------------------------------------- ####

m1 <- lm(infrstr_17_21 ~ age , dt10)
m1 <- lm(inf_drip_17_21 ~ age , dt10)
summary(m1)
sjPlot::tab_model(m1, digits = 4, show.se = T)

title="age_bin"
ta1=dt10 %>%group_by(age_bin) %>%summarize(PCT = mean(infrstr_17_21)) %>% mutate(grp="infrstr_17_21")
ta2=dt10 %>%group_by(age_bin) %>%summarize(PCT = mean(hh_drip_yrs_17_21)) %>% mutate(grp="hh_drip_yrs_17_21")
#ta3=dt10 %>%group_by(age_bin) %>%summarize(PCT = mean(inf_drip_17_21,na.rm = T)) %>% mutate(grp="inf_drip_17_21")
#
rbind(ta1,ta2) %>%  ggplot(aes(x = age_bin, y = PCT,colour =grp))+geom_line() +geom_point() + theme_bw() +labs (title = title)+ scale_colour_manual(values = color_bin)

#-----| EDUCATION|---------------------------------------------------------- ----

names(education16) 
# edu_hh_head 
# BIN: "edu_level_hh"  "high_edu_pct_hh"  "educated_pct_hh" 

summary_1_99(dt10$edu_hh_head)

# One LM only
m1 <- lm(infrstr_17_21 ~ edu_hh_head_01 , dt10)
summary(m1)
sjPlot::tab_model(m1, digits = 4, show.se = T)

title="edu_hh_head"
ta1=dt10 %>%group_by(edu_hh_head) %>%summarize(PCT = mean(infrstr_17_21)) %>% mutate(grp="infrstr_17_21")
ta3=dt10 %>%group_by(edu_hh_head) %>%summarize(PCT = mean(inf_drip_17_21,na.rm = T)) %>% mutate(grp="inf_drip_17_21")
rbind(ta1,ta3) %>% 
  ggplot(aes(x = edu_hh_head, y = PCT,colour =grp))+geom_line() +geom_point() + theme_bw() +labs (title = title)+ scale_colour_manual(values = color_bin)


#__________________________________________________________________________ ----
  
  
#
--------------------------------------------------------------------------------

# [ 1 ] Socioeconomic                        ----
# 1.1 Poverty                                ttests       ----

# bpl_card AND official_assistance
# B8	Does this household have a BPL ration card?
# B9	In the last 5 years, has the household received any assistance from the municipality/government/gram panchayat? (NOT including ration card)

pl16=
  rmtl_baseline2016 %>% 
  select(hh_id ,contains(c( "B8","B9"))) %>% 
  rename(bpl_card=B8,official_assistance=B9) %>% 
  filter(!is.na(bpl_card))

chisq.test(pl16$hh_drip_yrs_17_21,pl16$bpl_card)
chisq.test(pl16$hh_drip_yrs_17_21,pl16$official_assistance)

pl16 %>%  t_test(hh_drip_yrs_17_21~ bpl_card , detailed = T)
pl16 %>%  t_test(hh_drip_yrs_17_21~ official_assistance , detailed = T)

m1.1 <- lm(hh_drip_yrs_17_21~ bpl_card, pl16)
m1.1 <- lm(hh_drip_yrs_17_21~ official_assistance, pl16)

summary(m1.1)
sjPlot::tab_model(m1.1)
plot_model(m1.1, show.values = TRUE, value.offset = .1)




# 1.2 Social status                          plotbar      ----

######## castes category 


##
caste %>% filter(!is.na(caste_cat)) %>% 
  inner_join(rmtl_In_groups) %>% 
  group_by(hh_drip_yrs_17_21 ,caste_cat) %>% 
  count()%>% group_by(hh_drip_yrs_17_21) %>%  mutate(n/sum(n)
  ) %>% 
  ggplot() + aes(x = hh_drip_yrs_17_21, fill = factor(caste_cat)) +
  geom_bar(position = "fill")+ coord_flip()+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))



# 1.3 Education                              ttests       ----

#| edu_hh_level ⬇️

# ed16= edu_hh_level  %>% inner_join(rmtl_In_groups) 
ed16 %>% group_by(edu_hh_head ) %>% freq(hh_drip_yrs_17_21)

t1=ed16 %>% t_test(edu_hh_head ~ hh_drip_yrs_17_21 ,detailed = T)%>% select(4,2,3,10,12,13) %>% mutate(x="hh_drip_yrs_17_21")
t2=ed16 %>% t_test(prt_educated ~ hh_drip_yrs_17_21 ,detailed = T)%>% select(4,2,3,10,12,13)%>% mutate(x="hh_drip_yrs_17_21")

t3=ed16 %>% t_test(edu_hh_head ~ hh_irri_yrs_17_21  ,detailed = T)%>% select(4,2,3,10,12,13)%>% mutate(x="hh_irri_yrs_17_21")
t4=ed16 %>% t_test(prt_educated ~ hh_irri_yrs_17_21  ,detailed = T)%>% select(4,2,3,10,12,13)%>% mutate(x="hh_irri_yrs_17_21")

t1234=rbind(t1,t2,t3,t4)
nice_table(t1234)

# ed2016=rmtl_InOut_groups%>% inner_join(edu_hh_level)
t1=ed2016 %>% t_test(edu_hh_head ~ hh_drip_yrs_17_21 ,detailed = T)%>% select(4,2,3,10,12,13) %>% mutate(x="hh_drip_yrs_17_21")
t2=ed2016 %>% t_test(prt_educated ~ hh_drip_yrs_17_21 ,detailed = T)%>% select(4,2,3,10,12,13)%>% mutate(x="hh_drip_yrs_17_21")

t3=ed2016 %>% t_test(edu_hh_head ~ hh_irri_yrs_17_21  ,detailed = T)%>% select(4,2,3,10,12,13)%>% mutate(x="hh_irri_yrs_17_21")
t4=ed2016 %>% t_test(prt_educated ~ hh_irri_yrs_17_21  ,detailed = T)%>% select(4,2,3,10,12,13)%>% mutate(x="hh_irri_yrs_17_21")

t1234=rbind(t1,t2,t3,t4)
nice_table(t1234)

rm(t1,t2,t3,t4,t1234)


education10 =education %>% left_join(rmtl_In_groups[,1:4]) %>% 
  mutate(high_edu_hh_head = ifelse(edu_hh_head %in% c(5,6),1,0 )  )

m1=lm(infrstr_17_21 ~ edu_hh_head, education10)
m1=lm(infrstr_17_21 ~ edu_level_hh, education10)
m1=lm(infrstr_17_21 ~ edu_level_hh_CAT, education10)
m1=lm(infrstr_17_21 ~ high_edu_pct_hh, education10)
m1=lm(infrstr_17_21 ~ educated_pct_hh, education10)
m1=lm(infrstr_17_21 ~ edu_hh_head_01, education10)
m1=lm(infrstr_17_21 ~ high_edu_hh_head, education10)
summary(m1)

m1=lm(inf_drip_17_21 ~ edu_hh_head, education10)
m1=lm(inf_drip_17_21 ~ edu_level_hh, education10)
m1=lm(inf_drip_17_21 ~ edu_level_hh_CAT, education10)
m1=lm(inf_drip_17_21 ~ high_edu_pct_hh, education10)
m1=lm(inf_drip_17_21 ~ educated_pct_hh, education10)
m1=lm(inf_drip_17_21 ~ edu_hh_head_01, education10)
m1=lm(inf_drip_17_21 ~ high_edu_hh_head, education10)



# 1.4 Assets, income & revenue               ----
# land                                       ttest        ----
# total_acre
compute_summary(economic16$total_acre)
t2= economic16 %>% filter(total_acre<40.74) %>% 
  t_test(total_acre ~ hh_drip_yrs_17_21 ,detailed = T) %>% select(4,2,3,10,12,13)%>% mutate(x="hh_drip_yrs_17_21")

# total_plots
compute_summary(economic16$total_plots)
t3= economic16 %>%  filter(total_plots<7) %>% 
  t_test(total_plots ~ hh_drip_yrs_17_21 ,detailed = T) %>% select(4,2,3,10,12,13)%>% mutate(x="hh_drip_yrs_17_21")

###  income                                  ttest & lm   ----

# NET income earned by household members in the past 12 months.       				
# F1	Income sent by seasonal migrating household members	
# F2	Remittances (from permanent migrants)

# Do you earn any income from this source? (Y/N)	
rmtl_baseline2016 %>% select(contains(c("F1_s","F2_s"))) %>% freq()

# How much earned in past month? # How much earned in 2015?
f1_f2=rmtl_baseline2016 %>% select(contains(c("F1_","F2_")))
summary(f1_f2)

###### F3-F12 not include income from seasonal or permanent migrants.
# F3	Farming own or rented land
# F4	"Own livestock
# F5	"Own non-agricultural business
# F6	Salaried job
# F7	Casual work or daily labor by current household members
# F9	Government pension or scheme
# F10	Rent/lease of property or land (land, house, vehicle, electronic appliances, tractor, etc.)
# F11	Other jobs or activities not already mentioned
# F12	Total (Rs.)
# F13	According to what you indicated, your total HH income is Rs. [     ].
# F14	What is you income expectation in 2 years from now? (Rs.)		

f12_f13=
  rmtl_baseline2016 %>% 
  select(hh_id ,contains(c("F12_year","F13"))) %>% 
  mutate(bl_yr_income= ifelse(is.na(F12_year), F13,
                              ifelse(is.na(F13),F12_year, 
                                     pmax(F12_year,F13, na.rm = TRUE)))
  ) %>%  left_join(rmtl_In_groups)

# bl_yr_income
summary_1_99(f12_f13$bl_yr_income)
t4= f12_f13 %>% filter(bl_yr_income>7000 ,bl_yr_income<1000000) %>% 
  t_test(bl_yr_income ~ hh_drip_yrs_17_21 ,detailed = T) %>% 
  select(4,2,3,10,12,13)%>% mutate(x="hh_drip_yrs_17_21") %>% 
  mutate_at(2:3,round)


f3_f13=
  rmtl_baseline2016  %>% 
  select(hh_id,starts_with (c("F"))) %>% select(hh_id,contains(c("_s")))
f3_f13[f3_f13==2] <- 0
freq(f3_f13)

f3_13= f3_f13  %>% inner_join(rmtl_In_groups)

m1 <-
  lm(hh_drip_yrs_17_21 ~ F4_source+F5_source+F6_source+F7_source+F9_source,f3_13)
summary(m1)

# F7	Casual work or daily labor by current household members
t6=f3_13 %>% t_test(F7_source ~ hh_drip_yrs_17_21 ,detailed = T) %>% select(4,2,3,10,12,13)%>% mutate(x="hh_drip_yrs_17_21")

library(DT)

df=rbind(t2,t3,t4,t6) %>% mutate_at(2:6,round,2)
datatable(df)

# ASSETS                                     ttest        -----
e2016A=
  e2016 %>% mutate(total_assets=E6_1+E7_1+E8_1+E9_1+E10_1+E11_1+E12_1+E13_1+E15_1+E16_1+E18_1,
                   total_livestock=E6_1+E7_1+E8_1+E9_1,
                   total_farm_equipments=E10_1+E11_1+E12_1+E13_1) %>% inner_join(rmtl_In_groups)


e2016B= e2016 %>% select(-c(E20_1,E21_1))
e2016B[e2016B>0 & e2016B<1000] <- 1
e2016B=e2016B %>% 
  mutate(yn_assets=E6_1+E7_1+E8_1+E9_1+E10_1+E11_1+E12_1+E13_1+E15_1+E16_1+E18_1,
         yn_livestock=E6_1+E7_1+E8_1+E9_1,
         yn_farm_equipments=E10_1+E11_1+E12_1+E13_1) %>% inner_join(rmtl_In_groups)

ast2016= 
  
  model <- lm(hh_drip_yrs_17_21 ~ E6_1+E7_1+E8_1+E9_1+E10_1+E11_1+E12_1+E13_1+E15_1+E16_1+E18_1 , e2016B)
summary(model)


t05 <- assets22 %>% t_test(own_farm_equipments ~ farmers_hh , detailed = T) 
t06 <- assets22 %>% t_test(total_farm_equipments ~ farmers_hh , detailed = T) 


t_E <- rbind(t01,t03,t05,t02,t04,t06) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y. ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(t_E,title = c("Table E | Assats ಆಸ್","% Households own assats/ total assat household own" ),
           note = c("[E6-E21] How many of this item does the household currently own? (0 if none)","🟨" ))

# [ 2 ] Information                          lm           ----
#### knowledge about irrigation
# I2	Have you ever seen it working in a field?
# I3	Do you know any farmer in your village who uses it?
#### Demonstration plots
# I24	Have you ever gone to visit them?
#### training
# I34	Have you attended any of the trainings organized by the implementers of the project?

rmtl_baseline2016 %>%  select(contains(c("I2","I3","I24","I34")))
rmtl_baseline2016 %>%  select(hh_id, I2 , I3 , I24 , I34 ) %>% freq()

info <- 
  rmtl_baseline2016 %>%  
  select(hh_id, I2 , I3 , I24 , I34 ) %>%
  left_join(rmtl_In_groups[,1:4])

info  %>% t_test(I2   ~ hh_drip_yrs_17_21 ,detailed = T)%>% select(4,2,3,10,12,13) %>% mutate(x="hh_drip_yrs_17_21")
info  %>% t_test(I3   ~ hh_drip_yrs_17_21 ,detailed = T)%>% select(4,2,3,10,12,13) %>% mutate(x="hh_drip_yrs_17_21")
info  %>% t_test(I24   ~ hh_drip_yrs_17_21 ,detailed = T)%>% select(4,2,3,10,12,13) %>% mutate(x="hh_drip_yrs_17_21")
info  %>% t_test(I34   ~ hh_drip_yrs_17_21 ,detailed = T)%>% select(4,2,3,10,12,13) %>% mutate(x="hh_drip_yrs_17_21")



model <- lm(infrstr_17_21  ~ I2 + I3 + I24 + I34  , info)
model <- lm(hh_drip_yrs_17_21 ~ I2 + I3 + I24 + I34  , info)
model <- lm(inf_drip_17_21  ~ I2 + I3 + I24 + I34  , info)
summary(model)

model <- lm(infrstr_17_21  ~ I3 + I24  , info)
model <- lm(hh_drip_yrs_17_21 ~ I3 + I24  , info)
model <- lm(inf_drip_17_21  ~ I3 + I24   , info)
summary(model)

#
# [ ? ] source / irrigation                  DF           ----

# D20	Do you own the source of the irrigation?
D20=rmtl_baseline2016 %>% select(hh_id, starts_with("D20")) %>% 
  mutate(D20_7=as.numeric(D20_7))
D20[is.na(D20)] <- 0
D20 =D20 %>%  mutate(own_source_ir= rowSums(.[names(.)[2:10]], na.rm = T)) %>% 
  mutate(own_source_ir = ifelse(own_source_ir>0,1,0) ) %>% 
  select(hh_id,own_source_ir)

# D12	Has this plot been irrigated at least once during the last 5 years?
D12=rmtl_baseline2016 %>% select(hh_id, starts_with("D12_1")) %>% 
  mutate(D12_13=as.numeric(D12_13), D12_14=as.numeric(D12_14), D12_15=as.numeric(D12_15), D12_16=as.numeric(D12_16), D12_17=as.numeric(D12_17) )
D12[is.na(D12)] <- 0
D12 =D12 %>%  mutate(ir_before= rowSums(.[names(.)[2:10]], na.rm = T)) %>% 
  mutate(ir_before = ifelse(ir_before>0,1,0) ) %>% 
  select(hh_id,ir_before)

source_n_ir= inner_join(D20,D12)

# [ 3 ] Pipeline infrastructure status       ----
# 3.1 DIS's first use                        stat         ----

rmtl_srvy22 %>% 
  select(hh_id,contains("mw1c"),-mw1c_other,-mw1c__888)%>% filter(mw1c != "") %>% 
  select(-mw1c ) %>% 
  pivot_longer(-hh_id,names_to = "ans",values_to = "value") %>% 
  group_by(ans) %>% summarise(mean(value ))

vars_irri <- 
  rmtl_srvy22 %>% 
  select(farmers_hh,hh_id,
         l7_rank_1, mm4,mm5,mm9,
         mw4,  # MW4 : Are you still making use of the water from the project to irrigate your la
         mw1a, # If Yes, in which year did you first make use of the water?
         mw4a, # MW4A:If Yes, in what season?
         
         m59a  # M59a : Are you aware of the existence of a Water User Associations (WUA)?
  ) %>%
  left_join(rmtl_In_groups  [,1:4])
vars_irri$mw1a
vars_irri$mm9[vars_irri$mm9==-999] <- NA

infrstr_17_21

vars_irri %>% group_by(farmers_hh,mw4) %>% count() %>% group_by(farmers_hh) %>%  mutate(prt=n/sum(n)) %>% 
  ungroup() %>% mutate(prt = paste0(round(100 * prt, 0), "%"))

vars_irri %>% group_by(hh_irrigated ,mw4) %>% count() %>% group_by(hh_irrigated ) %>%  mutate(prt=n/sum(n)) %>% 
  ungroup() %>% mutate(prt = paste0(round(100 * prt, 0), "%"))


# 3.2 Maintenance                            ----
# 3.3 Damages                                lm           ----
###
# m35  What is the status of the main pipe coming into your land ?	# 1	Works | 2=Damaged
# m35b How long has the main pipes been damaged?		[months/years]
# m35c What is the status of the laterals?	1	Works, laid in the field | 2=OK, but in storage | 3=Damaged

rmtl_srvy22 %>% select(hh_id, contains("m35") )
m35=rmtl_srvy22 %>% select(hh_id, m35, m35b_month, m35b_year, m35c )
m35[m35==-999] <- NA
damage=m35 %>% 
  mutate(damage_yr= (m35b_month/12)+ m35b_year ) %>% 
  mutate(damage_yr=ifelse(m35==1,0,damage_yr)) %>% 
  mutate_at(6,round,2) %>% 
  mutate(damage_yr_bin = ntile(damage_yr, 5)) %>% 
  left_join(rmtl_In_groups[,1:4])

# infrstr_17_21 is erelevant the 0s are NAs in damage

damage %>% group_by(inf_drip_17_21) %>% summarise(mean(damage_yr,na.rm = T))
m1 <- lm(hh_drip_yrs_17_21~ damage_yr , damage)
m2 <- lm(inf_drip_17_21~ damage_yr , damage)
summary(m1)
sjPlot::tab_model(m2, digits = 4, show.se = T)


# [ 4 ] Location                             ----
# 4.1 Elevation                              ----
# 4.2 Village                                ----
# 4.3 North-center-south                     ----
# 4.4 Project border distance                ----
# 4.5 Operational irrigation zones           ----
# 4.6 Distance from tap/valve                ttest        ----


#--mm10--enough water
# MM10 : Has it ever happened to you that farmers 'before' you have used up a lot

#| mm10 - NO contribution!!!
#| 
rmtl_srvy22$mm10

rmtl_srvy22 %>% 
  select(farmers_hh,hh_id,starts_with("mm10") ) %>%
  filter(!is.na(mm10), farmers_hh=="inside_ramthal") %>%  
  count(mm10) %>% mutate(N=sum(n),pct=n/N)

mm10 <- 
  rmtl_srvy22 %>% select(farmers_hh,hh_id,starts_with("mm10") ) %>%
  filter(!is.na(mm10)) %>% 
  mutate(yes=1) %>% 
  pivot_wider(names_from = mm10, values_from = yes) %>% 
  pivot_longer(!c(farmers_hh,hh_id ), names_to = "mm10_ans", values_to = "yesno") %>% 
  mutate(yesno=ifelse(is.na(yesno),0,1))%>% left_join(irrigation_HH)  

ta=mm10 %>% group_by(hh_irrigated,mm10_ans) %>% summarise(mn=mean(yesno)) %>%
  ungroup() %>% mutate(mn = paste0(round(100 * mn, 0), "%"))

ta1=ta[6:10,2:3] %>% rename(use_irri_prt=mn)
ta0=ta[1:5,2:3] %>% rename(not_use_prt=mn)
inner_join(ta1,ta0) %>% kbl() %>% kable_styling()

# mm10%>% group_by(mm10_ans) %>% t_test(yesno~ irri01, detailed = T )


#--mw1c--Why didn't you use water?
#| mw1c - NO contribution!!!

rmtl_srvy22$mw1c

mw1c_why_no <- rmtl_srvy22 %>% select(farmers_hh,hh_id,starts_with("mw1c") ) %>%filter( mw1c != "" ) %>% 
  select(-mw1c__888 , -mw1c_other ,-mw1c) %>% 
  pivot_longer(!c(farmers_hh,hh_id ), names_to = "why_no_use", values_to = "yesno") %>% 
  left_join(irrigation_HH)

ta=mw1c_why_no %>% group_by(hh_irrigated,why_no_use ) %>% summarise(mn=mean(yesno)) %>%ungroup() %>% mutate(mn = paste0(round(100 * mn, 0), "%"))
ta1=ta[11:20,2:3] %>% rename(use_irri_prt=mn)
ta0=ta[1:10,2:3] %>% rename(not_use_prt=mn)
inner_join(ta1,ta0) [c(1:2,6,8:9),] %>% kbl() %>% kable_styling()

# mw1c_why_no  %>% group_by(why_no_use) %>%  t_test(yesno~ hh_irrigated , detailed = T )

# count hh is to make sure that significant t test rely on reasonable hh number
mw1c_why_no %>% group_by(why_no_use ,irri01 ) %>% summarise(sum(yesno))



# location_on_pipe
distance_from_filter <- 
  rmtl_srvy22 %>% 
  select(hh_id,mm4,mm9,
         mm10, # <labelled<double>[vars_irri$mm10]>: MM10 : Has it ever happened to you that farmers 'before' you have used up a lot
         mw1c_5, # "MW1C: I wanted to irrigate, but other farmers took all the water"         
         farmers_hh) %>% left_join(irrigation_HH) 

distance_from_filter %>% filter(mm9>-1 ) %>% group_by(hh_irrigated ) %>% summarise(mean(mm9))
distance_from_filter%>% filter(mm9>-1 ) %>% count(mm9) %>% as.data.frame()
distance_from_filter%>% filter(mm9>-1 , mm9<31 ) %>% group_by(hh_irrigated ) %>% summarise(mean(mm9))

t1 <- 
  distance_from_filter%>% filter(mm9<31 ) %>% 
  t_test(mm9  ~ hh_irrigated , detailed = T) %>% 
  rename(Not_irrigated=estimate1,Irrigated=estimate2,t=statistic ) %>% 
  select( Irrigated,Not_irrigated,estimate,conf.low,conf.high,t,df,p)

nice_table(t1,title = c("Table mm9A | Distance from valve", "Mean plot location as its distance from the valve"),
           note = c("[mm9] How many farmers are there between you and the valve/pipeline?","🟩" ))

# close_to_valve Yes or No
distance_from_filter_bins <- 
  distance_from_filter %>% filter(!is.na(mm9)) %>% 
  mutate(close_to_valveYN = ifelse(mm9==-999,NA, ifelse( mm9>=0 & mm9 < 5 ,1,0))) %>% #0-5 5+
  
  mutate(loc_on_pipe.5bin = case_when(
    mm9>=0 & mm9 < 5 ~ 1, # "0-5",
    mm9>= 5 & mm9 <11~ 2, #"05-10",
    mm9>= 11 & mm9 <21 ~ 3, #"10-20",
    mm9>= 21 & mm9 <31 ~ 4, #"20-30",
    mm9>= 31 & mm9 <51 ~ 5, #"30-50",
    TRUE ~ NA)) %>% 
  mutate(close1.far2.very3=case_when( 
    mm9>=0 & mm9< 4 ~ 1, #"close"
    mm9>=4 & mm9< 8 ~ 2, #"far",
    mm9>=8 & mm9<51 ~ 3, #"very_far",
    TRUE ~ NA))

distance_from_filter_bins %>% group_by(hh_irrigated ) %>% summarise(sum=sum(close_to_valveYN ),n=n()) %>% mutate(sum/n)
distance_from_filter_bins %>% filter(mm9<31 ) %>% group_by(hh_irrigated ) %>% summarise(sum=sum(close_to_valveYN ),n=n()) %>% mutate(sum/n)
distance_from_filter_bins %>% count(hh_irrigated, close_to_valveYN )%>%group_by(hh_irrigated) %>% summarise(n/sum(n))

# Fig. freq
rmtl_srvy22 %>%
  select(farmers_hh, hh_id,mm9,mm10) %>% mutate(mm10=as.numeric(mm10)) %>% 
  left_join(rmtl_In_groups) %>% filter(mm9>=0, !is.na(hh_drip_yrs_17_21)) %>% # rm -999
  count(hh_drip_yrs_17_21,mm9) %>% 
  group_by(hh_drip_yrs_17_21) %>%mutate(pct=n/sum(n)) %>% 
  group_by(mm9) %>% mutate(sum_pct=sum(pct)) %>% mutate(percent=pct/sum_pct) %>% 
  mutate(hh_irrigated=ifelse(hh_drip_yrs_17_21==1,"Irrigation","0irrigation")) %>%
  mutate_at(6,round,2) %>% mutate(percent=percent*100) %>% 
  ggplot(aes(x = mm9, y = percent, fill = hh_irrigated)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#b4b4b4ff", "#03bfffff")) +
  labs(title = "", x = "No. farmers before you", y = "Share of farmers (%)") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentage
  scale_x_continuous(breaks = seq(0, 14, by = 3)) +  # Set x-axis range up to 20 and display all integer values
  theme_minimal() +
  theme(text = element_text(family = "Serif"),  # Set font to Serif
        legend.position = "bottom")  




#__________________________________________________________________________----


##### essantials                         ----


attach(rmtl_baseline2016)
detach(rmtl_baseline2016)

summary_1_99(economic16$total_acre)
quantile(economic16$total_acre, 0.99, na.rm = TRUE)

# BINS
age_gndr <- age_gndr %>%
  mutate(age_cat = cut(age, breaks = c(0, 19, 29, 39, 49, 59, 100), 
                       labels = c(1, 2, 3, 4, 5, 6),
                       right = FALSE, include.lowest = TRUE)) %>% 
  mutate(age_bin = ntile(age, 5))


# Function to compute summary statistics
summary_1_99 <- function(x) {
  # Remove NAs for calculations and round to 2 decimal places
  summary <- c(
    Count = length(x),
    Mean = round(mean(x, na.rm = TRUE), 2),
    Median = round(median(x, na.rm = TRUE), 2),
    sd = round(sd(x, na.rm = TRUE), 2),
    p = round(quantile(x, 0.01, na.rm = TRUE), 2),
    P = round(quantile(x, 0.99, na.rm = TRUE), 2),
    Min = round(min(x, na.rm = TRUE), 2),
    Max = round(max(x, na.rm = TRUE), 2)
  )
  
  return(summary)
}
summary_stat <- function(x) {
  # Remove NAs for calculations and round to 2 decimal places
  summary <- c(
    Count = length(x),
    Mean = round(mean(x, na.rm = TRUE), 2),
    Median = round(median(x, na.rm = TRUE), 2),
    sd = round(sd(x, na.rm = TRUE), 2)
  )
  
  return(summary)
}

# group by for summry function(x)
tapply(income16$bl_yr_income, income16$hh_drip_yrs_17_21, compute_summary)

# group by as variable
group_var <- yield22_acre$your_grouping_variable

# Apply the function to each group
summary_stats <- tapply(yield22_acre$kg_per_acre, group_var, compute_summary)

# Convert the result to a data frame
summary_df <- as.data.frame(do.call(rbind, summary_stats))


attr(a_rmtl_srvy22$l7_rank_3, "labels")

flat_vector <- unlist(L48[,-1], use.names = FALSE)
table(flat_vector, useNA = "always") 

# remove columns only NA or empty
select(where(~!all(.x=="")))
select(where(~!all(is.na(.x))))

# LM
#m_edu <- lm(hh_irrigated ~ edu_hh_head + edu_hh + hh_literacyPrt , social_bl16)
m_edu <- lm(hh_irrigated ~ hh_literacyPrt+edu_hh_head , social_bl16)
summary(m_edu)
sjPlot::tab_model(m1,show.se = TRUE,
                  pred.labels = c("(Intercept)",  "% literacy in HH","HH head education level"),
                  dv.labels = "`",
                  string.se = "__Std. Err__",
                  string.pred = "Predictors",
                  string.est = "Coef.",
                  string.ci = "CI (95%)",
                  string.p = "p",
                  p.style = "numeric",
                  col.order = c("est","se","ci","p")
)


# regression with fixed effects
model <- lm(hh_irrigated ~ factor(District) + X, data = social_bl16)

broom::tidy(m_edu) %>%mutate(p.value = round(p.value, 3)) %>% 
  kbl() %>% kable_minimal()

#nice_lm
model1 <- lm(mpg ~ cyl + wt * hp, mtcars)
model2 <- lm(qsec ~ disp + drat * carb, mtcars)
mods <- nice_lm(list(model1, model2), standardize = TRUE)
mods
nice_table(mods, highlight = TRUE)


# write.csv
write.csv(rmtl_In_groups, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_In_groups.csv", row.names=FALSE)
write.csv(rmtl_InOut_groups, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_InOut_groups.csv", row.names=FALSE)
write.csv(rmtl_16_18_22_sample, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_16_18_22_sample.csv", row.names=FALSE)

