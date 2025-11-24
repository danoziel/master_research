 
library(dplyr)
library(tidyr)
library(tidyverse)

library(kableExtra )

library(RColorBrewer)
display.brewer.pal(n = 11, name = 'Dark2')
my_colors <- brewer.pal(8, "Dark2") # Paired, Set1



#custom theme to format ----
mp_theme=theme_bw()+
  theme(
    panel.grid.major.y =element_blank(),
    panel.grid.minor=element_blank(),
    axis.line=element_line(),
    
    text=element_text(family="serif"),
    legend.title=element_blank(), 
    axis.text=element_text(size=14),
    axis.title=element_text(size=10),
    legend.text = element_text(size = 12))


#__________________________  land holding  _______________________________ ----



# ) DF
df_land <- a_plots_size %>% 
  filter(!plotStatus %in% c("1","6")) %>% 
  group_by(hh_id) %>% 
  summarise(land_holding_2022=sum(acres,na.rm = T)) %>% 
  left_join(rmtl_cntrl_vars) %>% 
  filter(land_holding_2022 < 40 )

# ) DESC STAT
df_land %>% 
  group_by(in_project) %>%
  summarise(land_holding=mean(land_holding_2022,na.rm=T)) %>% 
  ungroup()

# 1) model formula 
fml_land <- land_holding_2022 ~ in_project + dist_Km_boundary  +
  hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 +
  housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property +
  total_livestock + total_farm_equipments

# 2) Nest by status and fit
fits_land <- df_land %>% nest() %>%
  mutate(model = map(data, ~ lm(fml_land, data = .x)),
         coefs = map(model, tidy),
         stats = map(model, glance))

# 3) Stacked outputs
coef_tbl_land <- fits_land %>% unnest(coefs) %>% 
  filter(term == "in_project") %>%
  select(estimate, std.error, p.value) %>% ungroup()
##
fit_stats_land <- fits_land %>% unnest(stats) %>%  
  select(nobs, r.squared) %>% 
  rename(Num.Obs.=nobs,R2=r.squared ) %>% ungroup()

# 4) Join with model summary stats
inproj_with_fit_land <- coef_tbl_land %>% cbind(fit_stats_land )

# 5) Bild df for control group 
control_mean <- df_land %>% 
  group_by(in_project) %>%
  summarise(land_holding=mean(land_holding_2022,na.rm=T)) %>% ungroup() %>% 
  filter(in_project == 0) %>% 
  select(-in_project) %>% rename(control_mean=land_holding)

# 6) Create YES rows
twoRows <- 
  tribble( ~metric,~land_holding,
           "Control vars", "Yes",
           "Dist. boundary", "Yes",
           "Baseline value","Yes")

# ML Outcomes Table ...................................................

df_html_land <- 
  inproj_with_fit_land %>% cbind(control_mean ) %>% 
  pivot_longer(estimate:control_mean, 
               names_to = "metric", values_to = "land_holding") %>% 
  mutate(across(-metric, ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  ))) %>% 
  rbind(twoRows)

library(kableExtra)
df_html_land[c(1:3,7:8,4:6),] %>% 
  kable("html", caption = "Land holding",align = "c") %>%
  kable_classic( full_width = F) 



# PLOT ----
library(jtools)
library(ggplot2)

models_list_land <- 
  fits_land %>% { setNames(.$model, .$status) } # names become model names in the legend

m1_plot <- 
  plot_summs(models_list_land ,coefs = c("In Project" = "in_project"),
             model.names = names(models_list_land),
             inner_ci_level = NULL, point.shape = F) + 
  labs(x = "Land Holding in Acre", y = NULL) +
  xlim(-1, 2) 

m1_plot + mp_theme


#__________________________ INPUTS  _______________________________  ----
# costs of  [__].   Season wise

costesA <- rmtl_srvy22 %>% 
  select(hh_id, 
         contains ("l70"),  # irrigation equipment
         contains ("l71"),  # Mechanization
         contains ("l72"),  # Fuel
         contains ("l73"))  # Labor

costesB <- costesA %>% 
  pivot_longer(-hh_id,names_to = "observation",values_to = "inputs_Rs" ) %>% 
  separate(observation, into = c("inputs", "season"), sep = "_") %>% 
  mutate(
    inputs = case_when(
      inputs=="l70" ~ "irriEquipment",
      inputs=="l71" ~ "mechanization",
      inputs=="l73" ~ "labor",
      TRUE ~ "fuel")
  ) %>% 
  filter(!is.na(inputs_Rs)) %>% 
  mutate(inputs_Rs=ifelse(inputs_Rs == -999,NA,inputs_Rs), 
         season=ifelse(season == "rab","rabi",season))


#  [ 
library(readr)
a_plots_crop <- 
  read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/a_plots_crop.csv")

inputs_season_2022 <- a_plots_crop %>% 
  count(hh_id,season,plotID) %>% 
  left_join(
    a_plots_size %>% select(hh_id, plotID, acres)
  ) %>% 
  group_by(hh_id,season) %>% 
  summarise(sum_acre=sum(acres,na.rm = T),.groups = "drop") %>% 
  left_join(costesB,by=c("hh_id","season")) %>% 
  filter(!is.na( inputs ))

inputs_Y_2022 <- inputs_season_2022 %>% 
  # filter(season != "KHA22") %>%
  filter(season != "kha") %>% 
  group_by(hh_id,inputs) %>% 
  summarise(sum_acre=sum(sum_acre),inputs_Rs=sum(inputs_Rs)) %>%
  ungroup() %>% 
  mutate(inputs_per_acre=inputs_Rs/sum_acre )

#   ]


# desc stat 2022 ----
inputs_season_2022 %>% left_join(hh_2022) %>% 
  mutate(season=ifelse(season == "rabi","Rabi","Kharif")) %>% 
  mutate(inputs_per_acre=inputs_Rs/sum_acre ) %>%  
  group_by(season,inputs,farmers_hh) %>% 
  summarise(inputs_per_acre=mean(inputs_per_acre,na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = farmers_hh,values_from = inputs_per_acre) %>% 
  mutate(gap_pct=(inside_ramthal - outside_ramthal)/inside_ramthal)

inputs_Y_2022 %>% left_join(hh_2022) %>% 
  group_by(inputs,farmers_hh) %>% 
  summarise(inputs_per_acre=mean(inputs_per_acre,na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = farmers_hh,values_from = inputs_per_acre) %>% 
  mutate(gap_pct=(inside_ramthal - outside_ramthal)/inside_ramthal)

# code for plot
inputs_Y_2022 %>% left_join(hh_2022) %>% 
  group_by(inputs,farmers_hh) %>% 
  summarise(inputs_per_acre=mean(inputs_per_acre,na.rm = T)) %>% 
  ungroup()



# inputs Baseline 
inputs_2015 <- rmtl_baseline2016 %>% 
  select(hh_id,D49_2015:D60_2014) %>% select(hh_id , contains("2015")) %>% 
  pivot_longer(-hh_id,names_to = "inputs", values_to = "inputs_Rs" ) %>% 
  separate(inputs, into = c("inputs", "year"), sep = "_") %>% 
  select(-year) %>% 
  mutate(
    inputs = case_when(
      inputs=="D58" ~ "irriEquipment",
      inputs=="D59" ~ "mechanization",
      inputs=="D60" ~ "labor",
      TRUE ~ "Else")
  ) %>% filter(inputs != "Else" ) %>% 
  group_by(hh_id ) %>% # Create total_inputs to remove HH with NA inputs
  mutate(total_inputs=sum(inputs_Rs,na.rm = T)) %>% ungroup() %>% 
  filter(total_inputs !=0)
inputs_2015$inputs_Rs[is.na(inputs_2015$inputs_Rs)] <- 0
inputs_2015[,1] %>% distinct()


cult_acre_2015 <- 
  BL_plotCrop  %>% select( hh_id,plotID,season) %>% distinct() %>% 
  left_join(bl6_plotAcre %>% rename(plotID=plot_num)) %>% 
  filter(season != "rabi_2014_15") %>% 
  group_by(hh_id) %>% 
  summarise(acre_2015=sum(plot_acre))

inputs_BL <- 
  inner_join(inputs_2015,cult_acre_2015) %>% 
  mutate(inputs_per_acre_BL=inputs_Rs/acre_2015 ) %>% 
  select(hh_id,inputs,inputs_per_acre_BL)


# REG  ----

inputs_22 <- inputs_Y_2022 %>% 
  select(hh_id, inputs, inputs_per_acre) %>% 
  filter(inputs != "fuel") %>% 
  left_join(inputs_BL) %>% 
  left_join(rmtl_con_vars)

library(tidyr)
library(purrr)
library(broom)

# 1) model formula inputs
# NO BASLINE "YIELD %" VALUES IN THIS REG

fml_inputs <- inputs_per_acre ~ in_project + dist_Km_boundary  + inputs_per_acre_BL+
  hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 +
  housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property +
  total_livestock + total_farm_equipments

# 2) Nest by status and fit
fits_inputs_22 <- inputs_22 %>%
  group_by(inputs) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(fml_inputs, data = .x)),
    coefs = map(model, tidy),
    stats = map(model, glance))

# 3) Stacked outputs
coef_inputs <- fits_inputs_22 %>% unnest(coefs) %>% 
  filter(term == "in_project") %>%
  select(inputs, estimate, std.error, p.value) %>% ungroup()
##
stats_inputs <- fits_inputs_22 %>% unnest(stats) %>%  
  select(inputs,nobs, r.squared) %>% 
  rename(Num.Obs.=nobs,R2=r.squared ) %>% ungroup()

# 4) Join with model summary stats
inproj_with_fit_inputs <- coef_inputs %>%
  left_join(stats_inputs,by = "inputs") %>% ungroup()

twoRows <- 
  tribble(~metric, ~irriEquipment, ~labor, ~mechanization,
          "Control vars", "Yes","Yes", "Yes",
          "Dist. boundary", "Yes","Yes", "Yes",
          "Baseline value", "Yes","Yes", "Yes")

control_mean <- 
  inputs_22 %>% group_by(in_project,inputs) %>% 
  summarise(
    inputs_per_acre =  mean(inputs_per_acre,na.rm=T)
  ) %>% ungroup() %>% 
  filter(in_project == 0) %>% 
  pivot_wider(names_from = "inputs", 
              values_from = "inputs_per_acre" ) %>% 
  rename(metric=in_project)
control_mean$metric[control_mean$metric==0] <- "control_mean"


df_html_inputs <- inproj_with_fit_inputs %>%
  pivot_longer(-inputs, 
               names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = inputs, values_from = value
  ) %>% 
  rbind(control_mean) %>% 
  mutate(across(-metric, ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  ))) %>% 
  rbind(twoRows) %>% 
  rename_with(~ c("Irrigation", "Labor", "Mechanization"), .cols = 2:4)

# reg table
library(kableExtra)
df_html_inputs[c(1:3,7:9,4:6),] %>% 
  kable("html", caption = "Annual inputs for 2022 (in ₹ INR)",align = "c") %>%
  kable_classic( full_width = F) 


# PLOT ----------------------------------
library(jtools)
models_list <- fits_inputs_22 %>% 
  # filter(status == "Lost_in_harvest") %>% 
  { setNames(.$model, .$inputs) }    # names become model names in the legend

m1_plot <- 
  plot_summs(models_list ,coefs = c("In Project" = "in_project"),
             model.names = c("Irrigation", "Labor", "Mechanization"),
             inner_ci_level = NULL, point.shape = F) + 
  labs(x = "Amount  in INR", y = NULL) +
  xlim(-1100, 100) 

m1_plot + mp_theme



#__________________________  CROP SELLING  ___________________________ ----

# L56	Season Level	Who did you sell crop to?  

# 1	Market (specify market name)
# 2	APMC (specify)
# 3	Other trader/wholesaler
# 4	Government society/NGO
# 5	Private company (specify name)
# -888	Other (specify)


L56_crop_sell <- 
  rmtl_srvy22 %>% 
  select(farmers_hh,hh_id, starts_with( "l56")) %>% 
  select(farmers_hh,hh_id,ends_with(c("1","_2","3","4","5"))) %>% 
  pivot_longer(-c(farmers_hh,hh_id),names_to = "Q",values_to = "ans" ) %>% 
  filter(ans == 1) %>%    
  separate(Q, into = c("part1", "season", "ans_num"), sep = "_"
  ) %>% 
  group_by(farmers_hh,season) %>% 
  mutate( unique_hhid = n_distinct(hh_id)
  ) %>% 
  group_by(unique_hhid ,farmers_hh,season,ans_num) %>% 
  summarise(SUM=sum(ans)) %>% 
  mutate(pct = SUM/unique_hhid 
  ) %>% 
  group_by(farmers_hh,ans_num) %>% 
  summarise(pct=mean(pct)) %>% ungroup() 


# desc stat ----
library(ggplot2)
library(treemapify)
.................................................................
# To whom is the crop sold  

L56_crop_sell %>% 
  mutate(
    Crop_sale_to = case_when(
      ans_num=="1" ~ "Market",
      ans_num=="2" ~ "APMC",
      ans_num=="3" ~ "Other trader",
      ans_num=="4" ~ "Gov society/NGO",
      TRUE ~ "Private company")
  ) %>% 
  mutate_at(3,round,3) %>% 
  ggplot(
    aes(area = pct, fill = Crop_sale_to, label = paste0(Crop_sale_to, "\n", pct*100, "%"))) +
  geom_treemap(color = "white",show.legend = T) +
  geom_treemap_text(colour = "black", 
                    place = "centre", grow = TRUE, 
                    family = "serif",reflow = TRUE,
                    size = .75) +
  facet_wrap(~farmers_hh) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 12, base_family = "serif") +
  labs(
    title = "Where the crop is sold",
    subtitle = "Percentage of produce sold in each market type"
  )

.................................................................
# Where is the crop sold?  

L56_sell_to <- 
  rmtl_srvy22 %>% 
  select(farmers_hh,hh_id, starts_with( c("l56_a","l56_b"  )  )) %>% 
  pivot_longer(-c(farmers_hh,hh_id),names_to = "Q",values_to = "ans" )%>%    
  separate(Q, into = c("part1", "ab", "season"), sep = "_"
  ) %>% 
  mutate(ans=ifelse(ans %in% c("none","0","-999","Middle Man","Pink","Shops"),NA, ans )) %>% 
  mutate(
    ans_clean = case_when(
      str_starts(ans, "Hun") ~ "Hungund",
      str_starts(ans, "Hub") ~ "Hubli",
      str_starts(ans, "Bag") ~ "Bagalkot",
      str_starts(ans, "Ami") ~ "Aminagad",
      str_starts(ans, "I") ~ "Ilkal",
      str_starts(ans, "l") ~ "Ilkal",
      str_starts(ans, "Kar") ~ "Karadi",
      str_starts(ans, "No") ~ NA,
      TRUE ~ ans)
  ) %>% 
  filter(!is.na(ans_clean)) %>% 
  select(farmers_hh,hh_id,ab,ans_clean) %>% distinct() %>% 
  count(farmers_hh,ab,ans_clean
  ) %>%  
  group_by(farmers_hh,ab) %>% 
  mutate(N=sum(n),pct = n/N) %>% 
  mutate(MtMc=ifelse(pct<0.04,"Else",ans_clean)
  ) %>% 
  group_by(farmers_hh,ab,MtMc) %>% 
  summarise (pct = sum(pct)*100) %>% ungroup()%>% 
  mutate_at(4,round,0)


# table MARKET  .....................................
L56_sell_to %>% 
  filter(ab=="a",farmers_hh =="inside_ramthal") %>% 
  rename(Market=MtMc,pct_IN=pct) %>% 
  select(Market,pct_IN) %>% 
  full_join(
    L56_sell_to %>% 
      filter(ab=="a",farmers_hh !="inside_ramthal") %>% 
      rename(Market=MtMc,pct_OUT=pct) %>% 
      select(Market,pct_OUT)
  ) %>% 
  arrange(desc(pct_IN)) %>% 
  kbl() %>% kable_styling()

# table APMC  ........................................
L56_sell_to %>% 
  filter(ab=="b",farmers_hh =="inside_ramthal") %>% 
  rename(APMC=MtMc,pct_IN=pct) %>% 
  select(APMC,pct_IN) %>% 
  full_join(
    L56_sell_to %>% 
      filter(ab=="b",farmers_hh !="inside_ramthal") %>% 
      rename(APMC=MtMc,pct_OUT=pct) %>% 
      select(APMC,pct_OUT)
  ) %>% 
  arrange(desc(pct_IN)) %>% 
  kbl() %>% kable_styling()




#__________________________  Revenue  ____________________________________ ----
# L78	Total revenue? [season-crop]

library(readr)
a_plots_revenue <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/a_plots_revenue.csv")
View(a_plots_revenue)

size_acre <- a_plots_size %>% select(hh_id,plotID,acres) %>% filter(!is.na(acres))

revenue_22B =
  a_plots_revenue %>% filter(plotRevenue>999) %>% 
  left_join(size_acre) %>% 
  mutate(season=ifelse(season=="rabi","Rabi","Kharif")) %>% 
  group_by(season,hh_id) %>% 
  summarise(revenue=sum(plotRevenue,na.rm = T),
            acres = sum(acres, na.rm = T)) %>% ungroup() %>%  # filter(hh_id==100014)
  mutate(revenue_per_acre=revenue/acres/1000) %>% 
  left_join(rmtl_con_vars)

revenue_22B %>% 
  group_by(season,in_project) %>% summarise(mean(revenue_per_acre,na.rm = T),n())

# ka # Kharif - full reg 
# kb # Kharif - reg without "dist_Km_boundary"
# kc # Kharif - only "revenue_per_acre ~ in_project"
# ra rb rc for Rabi

ka <- lm( revenue_per_acre ~ in_project + dist_Km_boundary  +
            hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 +
            housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property +
            total_livestock + total_farm_equipments,
          data = revenue_22B  %>% filter(season =="Kharif") )

sjPlot::tab_model(ka ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)
sjPlot::tab_model(kb ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)
sjPlot::tab_model(kc ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)
sjPlot::tab_model(ra ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)
sjPlot::tab_model(rb ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)
sjPlot::tab_model(rc ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)



library(tidyr)
library(purrr)
library(broom)

# REG    ----
# 1) DF [revenue_22]

revenue_22 =
  a_plots_revenue %>% filter(plotRevenue>999) %>% 
  left_join(size_acre) %>% 
  filter(season !="kha") %>% 
  group_by(season,hh_id) %>% 
  summarise(revenue=sum(plotRevenue,na.rm = T),
            acres = sum(acres, na.rm = T)) %>% ungroup() %>%  # filter(hh_id==100014)
  mutate(revenue_per_acre=revenue/acres/1000) %>% 
  left_join(rmtl_con_vars)

revenue_22 %>% 
  group_by(season,in_project) %>% summarise(mean(revenue_per_acre,na.rm = T),n())

# 1) model formula 
# NO BASLINE VALUES IN THIS REG
fml_reve <- revenue_per_acre ~ in_project + dist_Km_boundary  +
  hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 +
  housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property +
  total_livestock + total_farm_equipments

# 2) Nest by status and fit
fits_reve <- revenue_22 %>%
  group_by(season) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(fml_reve, data = .x)),
    coefs = map(model, tidy),
    stats = map(model, glance))

# 3) Stacked outputs
coef_reve <- fits_reve %>% unnest(coefs) %>% filter(term == "in_project") %>%
  select(season, estimate, std.error, p.value) %>% ungroup()
##
stats_reve <- fits_reve %>% unnest(stats) %>%  select(season,nobs, r.squared) %>% 
  rename(Num.Obs.=nobs,R2=r.squared ) %>% ungroup()
##
inproj_reve <- coef_reve %>% left_join(stats_reve,by = "season")

twoRows <-  tribble(~metric,  ~KHA22, ~rabi, "Control vars", "Yes", "Yes", "Dist. boundary", "Yes", "Yes", "Baseline value", "No","No")

revenue_22 %>% 
  group_by(season,in_project) %>% 
  summarise(reve= mean(revenue_per_acre,na.rm = T)) %>% 
  filter(in_project == 0) %>% rename(metric=in_project)

control_mean <- 
  revenue_22 %>% 
  group_by(season,in_project) %>% 
  summarise(reve= mean(revenue_per_acre,na.rm = T)) %>% 
  filter(in_project == 0) %>% pivot_wider(names_from = season, values_from = reve
  ) %>% rename(metric=in_project)
control_mean$metric[control_mean$metric==0] <- "control_mean"


df_html_reve <- inproj_reve %>%
  pivot_longer(-season, names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = season, values_from = value
  ) %>% 
  rbind(control_mean) %>% 
  mutate(across(-metric, ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  ))) %>% 
  rbind(twoRows)

# reg table
library(kableExtra)
df_html_reve[c(1:3,7:9,4:6),] %>% 
  kable("html", caption = "Revenue per acre",align = "c") %>%
  kable_classic( full_width = F) 

# PLOT   ----
library(jtools)
models_list <- fits_reve %>%  { setNames(.$model, .$season) }    

m1_plot <- plot_summs(models_list ,coefs = c("In Project" = "in_project"),
             model.names = c("Kharif","Rabi"),
             inner_ci_level = NULL, point.shape = F) + 
  labs(x = "Revenue per Acre (In thousands Rs.)", y = NULL) +
  xlim(-4, 2) 

m1_plot + mp_theme


#__________________________  Income   ____________________________________ ----

# F1	Income sent by seasonal migrating household members
# F2	Remittances (from permanent migrants)
# F3 f3	 [Calculate from cultivation module]
# f3_1   Rented land
# F4	   Own livestock (Net profit)
# F5	Own non-agricultural business (Net profit)
# F6	Salaried job
# F7	Casual work or daily labor by current household members
# F9	Government pension or scheme
# F10	Rent/lease of property or land (land, house, vehicle, electronic appliances, tractor, etc.)
# F11	   Other jobs or activities not already mentioned
# F12	Total (Rs.) [sum F1-F11]
# F13	   According to what you indicated, your total HH income is Rs. [     ].

F_2022 <- rmtl_srvy22 %>% select(hh_id, starts_with("f") ) 
names(F_2022)

F_2022 <- rmtl_srvy22 %>% 
  select(hh_id, starts_with("f") ) %>% 
  select(hh_id:f2_amt,f5:f10_amt,f12) %>% 
  pivot_longer(-hh_id ,
               names_to = "income_type",
               values_to = "income22") %>% 
  mutate(income_type = ifelse(income_type=="f12","f12_amt",income_type))


rmtl_baseline2016 %>% select(hh_id,starts_with("F"))
# library(stringr)
F_2015 <- rmtl_baseline2016 %>% right_join(hh_2022) %>% 
  select(hh_id,starts_with("F")) %>% 
  select( -ends_with("month"),
          -starts_with(c("farmer","F3","F4","F11","F13","F14")) )%>% 
  pivot_longer(-hh_id ,
               names_to = "income_type",
               values_to = "income15")%>% 
  mutate( 
    income_type = str_to_lower(income_type),  # make all lowercase 
    income_type = case_when(
      str_detect(income_type, "_year$") ~ str_replace(income_type, "_year$", "_amt"),
      str_detect(income_type, "_source$") ~ str_replace(income_type, "_source$", ""),
      TRUE ~ income_type) )%>% 
  mutate( 
    income15 = case_when(
      income15 == 0 ~ NA,
      income15 == 2 ~ 0 ,
      TRUE ~ income15) )




# desc stat
income <- F_2022 %>% 
  left_join(F_2015) %>%
  mutate(income22=ifelse(income22==-999,NA,income22)) %>% 
  filter(!(is.na(income22) & is.na(income15))) %>%  
  mutate(income22= ifelse(income22 > 1, income22/1000, income22),
         income15= ifelse(income15 > 1, income15/1000, income15) ) %>% 
  left_join(rmtl_cntrl_vars) 
income$income22[is.na(income$income22)] <- 0
income$income15[is.na(income$income15)] <- 0



income_summary <- income %>%
  group_by(income_type, in_project) %>%
  summarise(Mean = mean(income22, na.rm = TRUE), .groups = "drop") %>%
  mutate(in_project=ifelse(in_project==1,"IN","OUT")) 

control_mean <- income_summary %>%
  mutate(
    Mean = if_else(Mean < 1, round(Mean ,3), round(Mean))
  ) %>% 
  pivot_wider(names_from = "income_type", values_from = "Mean" ) %>% 
  filter(in_project =="OUT") %>% 
  mutate(metric="control_mean") %>% 
  select("metric",
    "f1", "f2", "f5", "f6", "f7", "f9", "f10",
    "f1_amt", "f2_amt", "f5_amt", "f6_amt",
    "f7_amt", "f9_amt", "f10_amt", "f12_amt"
  )

income_summary_tbl <- income_summary %>%
  mutate(
    Mean = if_else(Mean < 1, round(Mean * 100,1), round(Mean))
  ) %>% 
  pivot_wider(names_from = "in_project", values_from = "Mean" ) %>% 
  mutate(income_type = factor(
    income_type, levels = c("f1", "f2", "f5", "f6", "f7", "f9", "f10",
                            "f1_amt", "f2_amt", "f5_amt", "f6_amt",
                            "f7_amt", "f9_amt", "f10_amt", "f12_amt"))
    ) %>%
  arrange(income_type)%>% 
  select(income_type, IN,OUT)

income_summary_tbl[1:7,] %>% kbl() %>% kable_styling()
income_summary_tbl[8:15,] %>% kbl() %>% kable_styling()


# REG  ----

library(tidyr)
# library(purrr)
# library(broom)

# 1) model formula inputs # 2) Nest by status and fit 
fits_income <- income %>%
  group_by(income_type) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(
        income22 ~ in_project + dist_Km_boundary + income15 +
        hh_haed_age + hh_haed_gendar + hh_haed_edu_level + 
        total_acre16 + housing_str321 + 
        livestock_dairy + Bullock + Tractor + Plough + 
        Thresher + Seed_drill + Motorcycle + Fridge, 
        data = .x)),
    coefs = map(model, broom::tidy),
    stats = map(model, broom::glance))

# 3) Stacked outputs # 4) Join with model summary stats
inproj_with_fit_income <- 
  fits_income %>% unnest(coefs) %>% 
  filter(term == "in_project") %>% 
  left_join(
    fits_income %>% unnest(stats) %>% select(income_type,nobs, r.squared)
  )%>% ungroup() %>% 
  select(income_type, estimate, std.error, p.value,nobs, r.squared) %>% 
  rename(Num.Obs.=nobs,R2=r.squared ) %>% 
  pivot_longer(-income_type, names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = income_type, values_from = value ) %>% 
  select("metric",
         "f1", "f2", "f5", "f6", "f7", "f9", "f10",
         "f1_amt", "f2_amt", "f5_amt", "f6_amt",
         "f7_amt", "f9_amt", "f10_amt", "f12_amt"
  )


# reg table ----
df_html_income <- inproj_with_fit_income %>%
  rbind(control_mean) %>% 
  mutate(across(-metric, ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  )))

# library(kableExtra)
df_html_income[,1:8] %>% 
  kable("html", caption = "% of household ",align = "c") %>%
  kable_classic( full_width = F) 

df_html_income %>% select(metric, f1_amt:f12_amt) %>% 
  kable("html", caption = "Amount income in K Rs. ",align = "c") %>%
  kable_classic( full_width = F) 


# PLOT ----------------------------------
library(jtools)
models_listA <- fits_income[ c(1:5,7,9),] %>% { setNames(.$model, .$income_type) }
models_listB <- fits_income[-c(1:5,7,9),] %>% { setNames(.$model, .$income_type) }

m1_plot <- plot_summs( models_listA ,coefs = c("In Project" = "in_project"),
  model.names = c("Income 1", "Income 2","Income 3", "Income 4", "Income 5", "Income 6", "Income 7     " ),
  inner_ci_level = NULL, point.shape = F,colors = my_colors) + 
  labs(x = "% of HH to Incom type", y = NULL) +
  xlim(-.12, .12) 
m1_plot + mp_theme

m2_plot <- plot_summs( models_listB ,coefs = c("In Project" = "in_project"),
                       model.names = c("Income 1", "Income 2","Income 3", "Income 4", "Income 5", "Income 6", "Income 7","Income 2022" ),
                       inner_ci_level = NULL, point.shape = F,colors = my_colors) + 
  labs(x = "Income amount (In thousands Rs.)", y = NULL)
m2_plot + mp_theme


#__________________________ ASSET  __________________________       ----

# How many of this item does the household currently own? (0 if none)

# LIVESTOCK # [E6Cows E7Bullock E8Buffaloes E9Goats&sheep]
# FARM EQUIPMENT # [E10Tractor E11Plough E12Thresher E13Seed drill] 
# VEHICLES # [E15Cycles E16Motorcycles E17Cars] 
# HH ITEMS # [E18Fridge # E19	Television]

### REMOVE [E14 JCB]
### skip E20Gold E21Silver


# assets_22 .........
vars_e01 <- rmtl_srvy22 %>% select(hh_id,starts_with("e"),-e21) %>% 
  rename_with(~ c(
    "Cows","Bullock","Buffaloes","Goats_sheep",       # LIVESTOCK
    "Tractor","Plough","Thresher","Seed_drill","JCB", # FARM EQUIPMENT 
    "Cycles","Motorcycles","Cars",                    # VEHICLES
    "Fridge","TV"), .cols = 2:15) %>%                 # HH ITEMS
  select(-JCB)
vars_e01[is.na(vars_e01)] <- 0

# vars_e01 %>% count(Buffaloes)
# vars_e01$Buffaloes=ifelse(vars_e01$Buffaloes>9,NA,vars_e01$Buffaloes)

assets_22 <-  vars_e01 %>%  
  mutate(
    Livestock= Cows+Bullock+Buffaloes+Goats_sheep,
    Farm_equipments=Tractor+Plough+Thresher+Seed_drill,
    Vehicles= Cycles+Motorcycles+Cars,
    HH_items= Fridge+TV,
    Total_assets= Livestock+Farm_equipments+Vehicles+HH_items) %>% 
  filter(Total_assets>0) %>% 
  select(hh_id,Livestock:Total_assets) %>% 
  pivot_longer(-hh_id ,names_to = "Asset",values_to = "Num_assets_22")

# assets_15 .........
vars_e01_BL <- 
  rmtl_baseline2016 %>% select(hh_id,starts_with("E")) %>% 
  select(hh_id, ends_with("_1"),-c(E20_1:E11_to_E13_1)) %>% 
  rename_with(~ c(
    "Cows","Bullock","Buffaloes","Goats_sheep",
    "Tractor","Plough","Thresher","Seed_drill","JCB",
    "Cycles","Motorcycles","Cars",
    "Fridge","TV"), .cols = 2:15)%>%
  select(-JCB)
vars_e01_BL[is.na(vars_e01_BL)] <- 0

assets_15 <-  vars_e01_BL %>% 
  mutate(
         Livestock_BL= Cows+Bullock+Buffaloes+Goats_sheep,
         Farm_equipments_BL=Tractor+Plough+Thresher+Seed_drill,
         Vehicles_BL= Cycles+Motorcycles+Cars,
         HH_items_BL= Fridge+TV,
         Total_assets_BL= Livestock_BL + Farm_equipments_BL + 
           Vehicles_BL + HH_items_BL) %>% 
  filter(Total_assets_BL>0) %>% 
  select(hh_id,Livestock_BL:Total_assets_BL) 

assets_15 <-  vars_e01_BL %>% 
  mutate(
    Livestock= Cows+Bullock+Buffaloes+Goats_sheep,
    Farm_equipments=Tractor+Plough+Thresher+Seed_drill,
    Vehicles= Cycles+Motorcycles+Cars,
    HH_items= Fridge+TV,
    Total_assets= Livestock+Farm_equipments+Vehicles+HH_items) %>% 
  filter(Total_assets>0) %>% 
  select(hh_id,Livestock:Total_assets) %>% 
  pivot_longer(-hh_id ,names_to = "Asset",values_to = "Num_assets_BL")

# desc stat
assets <- assets_22 %>% 
  inner_join(assets_15) %>% 
  left_join(rmtl_con_vars) 

assets %>% group_by(Asset,in_project) %>% 
  summarise(Mean = mean(Num_assets_22,na.rm=T)) %>% 
  mutate(in_project=ifelse(in_project==1,"IN","OUT")) %>%
  pivot_wider (names_from = "in_project", values_from = "Mean") %>% 
  mutate((IN - OUT)/IN)


# REG  ----

library(tidyr)
library(purrr)
library(broom)

# 1) model formula inputs
fml_assets <- Num_assets_22 ~ in_project + dist_Km_boundary  + Num_assets_BL+
  hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 +
  housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property

# 2) Nest by status and fit 
fits_assets_22 <- assets %>%
  group_by(Asset) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(fml_assets, data = .x)),
    coefs = map(model, tidy),
    stats = map(model, glance))

# 3) Stacked outputs
coef_assets <- fits_assets_22 %>% unnest(coefs) %>% 
  filter(term == "in_project") %>%
  select(Asset, estimate, std.error, p.value) %>% ungroup()
##
stats_assets <- fits_assets_22 %>% unnest(stats) %>%  
  select(Asset,nobs, r.squared) %>% 
  rename(Num.Obs.=nobs,R2=r.squared ) %>% ungroup()

# 4) Join with model summary stats
inproj_with_fit_assets <- coef_assets %>%
  left_join(stats_assets,by = "Asset") %>% ungroup() %>%
  pivot_longer(-Asset, 
               names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = Asset, values_from = value )


# reg table df I
twoRows <- tribble(
  ~metric, ~Livestock, ~Farm_equipments, ~Vehicles, ~HH_items, ~Total_assets,
  "Control vars", "Yes","Yes", "Yes","Yes", "Yes",
  "Dist. boundary", "Yes","Yes", "Yes","Yes", "Yes",
  "Baseline value", "Yes","Yes", "Yes","Yes", "Yes")

# reg table df II
control_mean <- assets %>% 
  filter(in_project==0) %>% 
  group_by(Asset) %>% 
  summarise(Mean = mean(Num_assets_22,na.rm=T)) %>% 
  pivot_wider (names_from = "Asset", values_from = "Mean") %>% 
  mutate(metric="control_mean") %>% 
  select( metric ,Livestock ,Farm_equipments, Vehicles, HH_items, 
          Total_assets)
  

# reg table ----
df_html_assets <- inproj_with_fit_assets %>%
  rbind(control_mean) %>% 
  mutate(across(-metric, ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  ))) %>% 
  rbind(twoRows) %>% 
  rename_with(~ c("Farming","Vehicles", "Electronics", "Total"), .cols = 3:6)

library(kableExtra)
df_html_assets[c(1:3,7:9,4:6),] %>% 
  kable("html", caption = "Total number of asset household own",align = "c") %>%
  kable_classic( full_width = F) 

# PLOT ----------------------------------
library(jtools)
models_list <- fits_assets_22 %>% { setNames(.$model, .$Asset) }

m1_plot <- plot_summs(
  models_list ,coefs = c("In Project" = "in_project"),
  model.names = c("Livestock", "Farming","Vehicles", "Electronics", "Total" ),
  inner_ci_level = NULL, point.shape = F) + 
  labs(x = "Number of Items", y = NULL) +
  xlim(-.8, 2.8) 

m1_plot + mp_theme

















#__________________________ Migration  __________________________       ----


rmtl_baseline2016 %>% select(hh_id, C1)%>% 
  left_join(
    rmtl_midline2018 %>% select(hh_id, c1_exist)) %>% 
  left_join (
    rmtl_srvy22 %>% select(hh_id,r1)) %>% 
  rename(BL=C1,S18=c1_exist,s22=r1)


### migration 2022 ###
attr(rmtl_srvy22$r3, "labels")
# r1 How many household members live in this house?
# r26_ Since 2016, has [member's name] migrated from the village for work for a period of 6 months or more?
rmtl_srvy22 %>% select(hh_id, farmers_hh, r1 )
rmtl_srvy22 %>% select(hh_id, farmers_hh, starts_with("r3_"))

migration22=
  rmtl_srvy22 %>% 
  select(hh_id, starts_with("r26_")) %>% 
  pivot_longer(-hh_id,
               names_to = "mmbr", values_to = "migrated") %>% 
  filter(!is.na(migrated)) %>% 
  group_by(hh_id) %>% 
  summarise(migrated22=sum(migrated)) %>% 
  left_join(rmtl_srvy22 %>% select(hh_id, r1) ) %>% 
  rename(n22=r1) %>% 
  mutate(migrated_prc_22= migrated22/n22) 


### migration 2018 ###
# C1	How many household members live in this house?
# PREVIOUS HOUSEHOLD MEMBERS: C27	How many people, who previously lived in the household in the past 10 years now live elsewhere?
migration18 =
  rmtl_midline2018 %>% select(hh_id, c1_exist, c27) %>% 
  filter(c1_exist>0) %>% 
  rename(n18=c1_exist, migrated18=c27) %>% 
  mutate(migrated_prc_18= migrated18/n18) 

### migration BL 2015 ###
# C1	How many household members live in this house?
# C27	How many people, who previously lived in the household in the past 10 years now live elsewhere?
# C18_	Where do they reside most of the time during the rest of the year?

migration15 =
  rmtl_baseline2016 %>% select(hh_id, C1,C27) %>% 
  filter(C1>0)%>% 
  rename(n15=C1, migrated15=C27) %>% 
  mutate(migrated_prc_15= migrated15/n15) 


# DESC STST ----
migration =
  migration22 %>% 
  left_join(migration18) %>% 
  left_join(migration15) %>%
  left_join(rmtl_InOut) %>% 
  left_join(rmtl_cntrl_vars) 

"fraction members of a HH who migrates"

migration %>% group_by(in_project) %>% 
  summarise(pct_migration_2022 = mean(migrated_prc_22,na.rm=T),
            pct_migration_2018 = mean(migrated_prc_18,na.rm=T))


# REG ----
m1 <-  lm(migrated_prc_22  ~ in_project +
            dist_Km_boundary + migrated_prc_15 +
            hh_haed_age + hh_haed_gendar + hh_haed_edu_level + 
            total_acre16 + housing_str321 + 
            job_income_sourceS + govPnsin_scheme + rent_property+
            livestock_dairy + Bullock + Tractor + Plough + 
            Thresher + Seed_drill + Motorcycle + Fridge, 
          migration)
sjPlot::tab_model(m1 ,  show.se = T,digits = 5, show.stat  = F )

m2 <-  lm(migrated_prc_22  ~ in_project, migration)
summary(m2)

m3 <-  lm(migrated_prc_18  ~ in_project +
            dist_Km_boundary + migrated_prc_15 +
            hh_haed_age + hh_haed_gendar + hh_haed_edu_level + 
            total_acre16 + housing_str321 + 
            job_income_sourceS + govPnsin_scheme + rent_property+
            livestock_dairy + Bullock + Tractor + Plough + 
            Thresher + Seed_drill + Motorcycle + Fridge, 
          migration)
sjPlot::tab_model(m3 ,  show.se = T,digits = 5, show.stat  = F )

m4 <-  lm(migrated_prc_18  ~ in_project, migration)
summary(m4)




# PLOT ---- 
fml1 <- migrated_prc_22  ~ in_project +dist_Km_boundary + migrated_prc_15 +
  hh_haed_age + hh_haed_gendar + hh_haed_edu_level + 
  total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+
  livestock_dairy + Bullock + Tractor + Plough + Thresher + Seed_drill + Motorcycle + Fridge

fml3 <- migrated_prc_18  ~ in_project +dist_Km_boundary + migrated_prc_15 +
  hh_haed_age + hh_haed_gendar + hh_haed_edu_level + 
  total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+
  livestock_dairy + Bullock + Tractor + Plough + Thresher + Seed_drill + Motorcycle + Fridge

fits <- migration %>% nest() %>%
  mutate(model = map(data, ~ lm(fml1 #fml3
                                , data = .x)),
         coefs = map(model, tidy),
         stats = map(model, glance))

models_list <- 
  fits %>% { setNames(.$model, .$status) } 
#           ignore Warning message

m1_plot <- plot_summs(models_list ,coefs = c("In Project" = "in_project"),
           model.names = names(models_list),
           inner_ci_level = NULL, point.shape = F) + 
  labs(title = "Fraction members of a HH who migrates",
    x = "% of imigration (per HH)", y = NULL) +
  xlim(-0.05, 0.15) 
m1_plot + mp_theme


#__________________________ education  __________________________       ----

# r7	Are they literate?
# r8	What is their educational level? (0=NOT literate)
# r11 	Is the institution public or private?
# r12		What are the annual tuition fees?
# The HH head is the same as in 2016, so there is no need to examine it

# literatecy22
# r7	Are they literate?

literatecy22= 
  rmtl_srvy22 %>% select(hh_id,starts_with("r7" ) ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "literate") %>% 
  group_by(hh_id) %>% 
  summarise(literatecy_22=mean(literate,na.rm=T)) %>% 
  mutate(literatecy_22 = ifelse(literatecy_22<0,NA,literatecy_22)) # transfer NaN to NA
  
  

# literatecy15
# C6 Are they literate

literatecy15= 
  rmtl_baseline2016 %>% select(hh_id,starts_with("c6" ) ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "literate") %>% 
  group_by(hh_id) %>% 
  summarise(literatecy_15=mean(literate,na.rm=T)) %>% 
  mutate(literatecy_15 = ifelse(literatecy_15<0,NA,literatecy_15))




# edu_level22
# r8	What is their educational level? 

edu_level22 = 
  rmtl_srvy22 %>% select(hh_id, starts_with("r8"), -ends_with("_bin") ) %>% 
  select(-contains("new")) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "edu_level") %>% 
  group_by(hh_id) %>% 
  summarise(edu_level_22=mean(edu_level,na.rm=T)) %>% 
  mutate(edu_level_22 = ifelse(edu_level_22<0,NA,edu_level_22)) # transfer NaN to NA

# edu_level15
# C7 educational level

edu_level15 = 
  rmtl_baseline2016 %>% select(hh_id, starts_with("c7"), -ends_with("_bin") ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "edu_level") %>% 
  group_by(hh_id) %>% 
  summarise(edu_level_15=mean(edu_level,na.rm=T)) %>% 
  mutate(edu_level_15 = ifelse(edu_level_15<0,NA,edu_level_15)) # transfer NaN to NA



# edu_level_2nd_gen_22
r8_22 =  
  rmtl_srvy22 %>% select(hh_id, starts_with("r8"), -ends_with("_bin") ) %>% 
  select(-contains("new")) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "edu_level")
r8_22$id_member <- sub("^r8_(\\d{1,2})","r_\\1",r8_22$id_member )


r6_22 = # R6 What is their relationship to the head of household?
  rmtl_srvy22 %>% select(hh_id,starts_with("r6" ) ) %>% 
  pivot_longer(
    !hh_id, names_to = "id_member", values_to = "relation_HH_head") 
r6_22$id_member <- sub("^r6_(\\d{1,2})","r_\\1",r6_22$id_member )

edu_level_2nd_gen_22 <- 
  left_join (r8_22,r6_22) %>% 
  mutate(edu_gen_2nd=ifelse(
    relation_HH_head %in% c(3,9,11:13),edu_level,NA) ) %>% 
  group_by(hh_id) %>% 
  summarise(edu_gen2_22=mean(edu_gen_2nd,na.rm=T)) %>% 
  mutate(edu_gen2_22 = ifelse(edu_gen2_22<0,NA,edu_gen2_22)) # transfer NaN to NA


# edu_level_2nd_gen_15

# C7 educational level
# C5 elationship to the head of household

c7_15 <- 
  rmtl_baseline2016 %>% select(hh_id, starts_with("c7"), -ends_with("_bin") ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "edu_level")
c7_15$id_member <- sub("^C7_(\\d{1,2})","C_\\1",c7_15$id_member )

C5_15 = # C5 elationship to the head of household
  rmtl_baseline2016 %>% select(hh_id,starts_with("c5" ), -contains("os") ) %>% 
  pivot_longer(
    !hh_id, names_to = "id_member", values_to = "relation_HH_head") 
C5_15$id_member <- sub("^C5_(\\d{1,2})","C_\\1",C5_15$id_member )

edu_level_2nd_gen_15 <- 
  left_join (c7_15,C5_15) %>% 
  mutate(edu_gen_2nd=ifelse(
    relation_HH_head %in% c(3,9,11:13),edu_level,NA) ) %>% 
  group_by(hh_id) %>% 
  summarise(edu_gen2_15=mean(edu_gen_2nd,na.rm=T)) %>% 
  mutate(edu_gen2_15 = ifelse(edu_gen2_15<0,NA,edu_gen2_15)) # transfer NaN to NA




# private school 2022
# R11 	Is the institution public or private?
edu_private_22 = 
  rmtl_srvy22 %>% select(hh_id,starts_with("r11" ) ) %>% 
  pivot_longer(
    !hh_id, names_to = "id_member", values_to = "institut")%>% 
  mutate(institut = ifelse(is.na (institut),0,institut),
         privet=ifelse(institut==2,1,0),
         public=ifelse(institut==1,1,0)) %>%
  group_by(hh_id) %>% 
  summarise(n_privet=sum(privet), n_public=sum(public)) %>% 
  mutate(privet_22 = ifelse(n_privet > 0, 1,
                  ifelse(n_public > 0, 0, NA)))


# private school 2015
# C10	Is the institution public or private?

edu_private_15 = 
  rmtl_baseline2016 %>% select(hh_id,starts_with("C10" ) ) %>% 
  pivot_longer(
    !hh_id, names_to = "id_member", values_to = "institut")%>% 
  mutate(institut = ifelse(!institut %in% c(1,2) ,0,institut),
         privet=ifelse(institut==2,1,0),
         public=ifelse(institut==1,1,0)) %>%
  group_by(hh_id) %>% 
  summarise(n_privet=sum(privet), n_public=sum(public)) %>% 
  mutate(privet_15 = ifelse(n_privet > 0, 1,
                         ifelse(n_public > 0, 0, NA)))
  
  
  
  
  
# tuition 2022
# R12		What are the annual tuition fees?
tuition22 =
  rmtl_srvy22 %>% select(hh_id,starts_with("r12" ) ) %>% 
  pivot_longer(
    !hh_id, names_to = "id_member", values_to = "tuition") %>% 
  mutate(n_mm= ifelse(tuition >= 0, 1, NA )) %>% 
  group_by(hh_id) %>% 
  summarise(tuitionHH= sum(tuition,na.rm = T),n=sum(n_mm,na.rm = T)) %>% 
  mutate(tuition_hhm_22=tuitionHH/n) %>% 
  mutate(tuition_hhm_22 = ifelse(tuition_hhm_22<0,NA,tuition_hhm_22)) # transfer NaN to NA

# tuition 2015
# C11	What are the annual tuition fees?
  
tuition15 = 
  rmtl_baseline2016 %>% select(hh_id,starts_with("C11" ) ) %>% 
  pivot_longer(
    !hh_id, names_to = "id_member", values_to = "tuition") %>% 
  mutate(tuition= ifelse(tuition < 0, NA, tuition )) %>% 
  mutate(n_mm= ifelse(tuition >= 0, 1, NA )) %>% 
  group_by(hh_id) %>% 
  summarise(tuitionHH= sum(tuition,na.rm = T),n=sum(n_mm,na.rm = T)) %>% 
  mutate(tuition_hhm_15=tuitionHH/n) %>% 
  mutate(tuition_hhm_15 = ifelse(tuition_hhm_15<0,NA,tuition_hhm_15)) # transfer NaN to NA



education1522 <- 
  literatecy22 %>% left_join(literatecy15) %>% 
  left_join(edu_level22) %>% left_join(edu_level15) %>% 
  left_join(edu_level_2nd_gen_22) %>% left_join(edu_level_2nd_gen_15) %>% 
  left_join(edu_private_22 %>% select(hh_id,privet_22)) %>% 
  left_join(edu_private_15 %>% select(hh_id,privet_15)) %>% 
  left_join(tuition22 %>% select(hh_id,tuition_hhm_22))%>% 
  left_join(tuition15 %>% select(hh_id,tuition_hhm_15))

education1522



#__________________________ __________________________       ----


# INCOME                                   ----

vars_f01 <- rmtl_srvy22 %>% select(farmers_hh, hh_id,starts_with("f")) 
names(vars_f01)
names(vars_f01) <- c("farmers_hh","hh_id",
                     "Migrating_household_income_hh",
                     "Migrating_household_income_amt",
                     "Remittance_hh","Remittance_amt",
                     "revenue12_amt",
                     "lease_land_hh","lease_land_amt",
                     "Own_livestock_hh","Own_livestock_net_profit",
                     "non_agri_business_hh","non_agri_business_net_profit",
                     "Salaried_job_hh","Salaried_job_amt",
                     "Casual_work_hh","Casual_work_amt",
                     "Gov_pension_scheme_hh","Gov_pension_scheme_amt",
                     "Rent_property_hh","Rent_property_amt",
                     "Other_activities_hh","Other_activities_amt",
                     "income_2021","sum_income12months" 
) 
vars_f02=vars_f01
vars_f02[is.na(vars_f02)] <- 0
vars_f02[vars_f02==-999] <- NA

vars_f02$Migrating_household_income_amt=ifelse(vars_f02$Migrating_household_income_amt==6,NA,vars_f02$Migrating_household_income_amt)

vars_f02$Remittance_amt=ifelse(vars_f02$Remittance_amt<1999,NA,vars_f02$Remittance_amt)

vars_f02$sum_income12months=ifelse(vars_f02$sum_income12months<9999,NA,vars_f02$sum_income12months)

income_21_22 = 
  vars_f02 %>% 
  mutate(
    income_2022_withoutRevenue=sum_income12months-revenue12_amt,
    income_2022_withRevenue=sum_income12months,
    total_income_sources_2022=Migrating_household_income_hh+
                        Remittance_hh+lease_land_hh+ #Own_livestock_hh+
                        non_agri_business_hh+Salaried_job_hh+Casual_work_hh+Gov_pension_scheme_hh+
                        Rent_property_hh+Other_activities_hh,
    external_income=Migrating_household_income_amt+ Remittance_amt+Gov_pension_scheme_amt ) %>% 
  mutate(external_income=ifelse( is.na(external_income),0,external_income))

rm(vars_f01,vars_f02)


# F13	What is the annual household income in 2021? (Rs.)
# F12	Total income NOT A QUESTION Software will sum F1-F11

income_21_22 %>% group_by(farmers_hh) %>% summarise( mean(income_2021,na.rm = T))
income_21_22 %>% group_by(farmers_hh) %>% summarize( mean(income_2022_withoutRevenue,na.rm = T))
income_21_22 %>% group_by(farmers_hh) %>% summarize( mean(income_2022_withRevenue,na.rm = T))
income_21_22 %>% group_by(farmers_hh) %>% summarize( mean(total_income_sources_2022,na.rm = T))

t01 <- income_21_22 %>% t_test(income_2022_withoutRevenue ~ farmers_hh , detailed = T) 
t02 <- income_21_22 %>% t_test(income_2022_withRevenue ~ farmers_hh , detailed = T) 
t03 <- income_21_22 %>% t_test(income_2021 ~ farmers_hh , detailed = T) 
t04 <- income_21_22 %>% t_test(total_income_sources_2022 ~ farmers_hh , detailed = T) 

t_F <- rbind(t01,t02,t03,t04) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y. ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 

t_F$.y.[t_F$.y.=="income_2022_withoutRevenue"] <- "2022 without crop revenue"
t_F$.y.[t_F$.y.=="income_2022_withRevenue"] <- "2022 with crop Revenue"
t_F$.y.[t_F$.y.=="income_2021"] <- "2021 with crop Revenue"
t_F$.y.[t_F$.y.=="total_income_sources_2022"] <- "total income sources 2022 "

nice_table(t_F,title = c("Table F | Income ಆದಾಯ   " ,"Household income from the farm and from sources other than the farm" ),
           note = c("", "income_2021| F13	What is the annual household income in 2021? (Rs.)",
                    "income_2022_withoutRevenue| F13 minus F3(revenue12_amt)",
                    "income_2022_withRevenue| F12	Total income NOT A QUESTION Software will sum F1-F11",
                    "num_income_sources_2022| count F1-F11","🟨" ))

# External income
library(dplyr)

income_21_22 %>%
  select(hh_id, farmers_hh, Migrating_household_income_hh, Remittance_hh, Gov_pension_scheme_hh) %>%
  group_by(farmers_hh) %>%
  summarise(
    n = n(),
    migration = sprintf("%.1f%%", sum(Migrating_household_income_hh) / n * 100),
    remittances = sprintf("%.1f%%", sum(Remittance_hh) / n * 100),
    pensions_or_subsidie = sprintf("%.1f%%", sum(Gov_pension_scheme_hh) / n * 100)
  ) %>% kbl() %>% kable_material()




summary(income_21_22$Gov_pension_scheme_amt)


t05 <- income_21_22 %>% t_test(external_income ~ farmers_hh , detailed = T) 
t06 <- income_21_22 %>% t_test(Gov_pension_scheme_amt ~ farmers_hh , detailed = T) 

t_F129 <- rbind(t05,t06) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y. ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p)
t_F129$.y.[t_F129$.y.=="external_income"] <- "External income"
t_F129$.y.[t_F129$.y.=="Gov_pension_scheme_amt"] <- "Pension/subsidies [Gov/NGO]"

nice_table(t_F129,title = c("Table F1.2.9 | External income" ,"family assistance, Pensions and subsidies - Government/Non-Government" ),
           note = c("", "[F1]Income sent by seasonal migrating household members" , 
                    "[F2]Remittances (from permanent migrants)" , 
                    "[F9]Government pension or scheme" ,"🟨" ))


#education      ----
#   
# r7	Are they literate?
# r8	What is their educational level? (0=NOT literate)

# The HH head is the same as in 2016, so there is no need to examine it

r7= 
  rmtl_srvy22 %>% select(hh_id,starts_with("r7" ) ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "literate") %>% 
  filter(!is.na(literate))
r7$id_member <- sub("^r7_(\\d{1,2})","r_\\1",r7$id_member )

# r8= 
#   rmtl_srvy22 %>% select(hh_id, starts_with("r8"), -ends_with("_bin") ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "edu_level")
r8$id_member <- sub("^r8_(\\d{1,2})","r_\\1",r8$id_member )

attr(rmtl_srvy22$r8_1, "labels")

edu22 = 
  left_join(r7,r8)%>%
  mutate(edu_level=ifelse(is.na(edu_level),0 ,edu_level )) %>% 
  mutate(high_edu_level=ifelse(edu_level %in% c(4:6),1 ,0 )) %>% 
rm(r7,r8)

education122 =
  edu22 %>% 
  group_by(hh_id) %>% 
  summarise(
    n_hhm= n(),
    literate_pct_hh=mean(literate),    
    edu_hh_level_hh= mean(edu_level),
    high_edu_pct_hh= mean(high_edu_level)
  ) %>%  ungroup() %>% 
  mutate_at(3:5,round,2) %>% left_join(hh_2022)


t01 <- education122 %>% t_test(literate_pct_hh  ~ farmers_hh , detailed = T) 
t02 <- education122 %>% t_test(edu_hh_level_hh ~ farmers_hh , detailed = T) 
t03 <- education122 %>% t_test(high_edu_pct_hh ~ farmers_hh , detailed = T) 

t_R8 <- rbind(t01,t02,t03) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y. ,Ramthal,Outside_Ramthal,t,df,p,conf.low,conf.high)

nice_table(t_R8,title = c("Table R8 | Education","Education among HH members" ),
           note = c("","[r7] Are they literate?",
                    "[r8] What is their educational level? (0=NOT literate)"))






# SOCIAL CAPITAL    ----


social_22= rmtl_srvy22 %>% select(hh_id, farmers_hh, starts_with("h"))
attr(rmtl_srvy22$h1, "labels") # 1=dissatisfied|2|3|4|5=Satisfied
attr(rmtl_srvy22$h2, "labels") # 1=important|2|3|4=Not


h1h2_h18h23 <- 
  social_22 %>% 
  select(hh_id, farmers_hh,h1:h2,h18:h23 ) %>% 
  group_by(farmers_hh) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  select(-hh_id) %>% 
  pivot_longer(!farmers_hh, names_to = "POV", values_to = "count") %>% 
  pivot_wider(names_from = farmers_hh, values_from = count) 



# 	From the following list, rank the top 3 most important factors 
# H3 to a determining agricultural success
# H4 determining whether a farmer will have water for irrigation

# H3
h3_common_factor=
  social_22 %>% select(hh_id, farmers_hh, starts_with("h3") )%>% 
  group_by(farmers_hh) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  select(-c(hh_id,h3__777)) %>% 
  pivot_longer(!farmers_hh, names_to = "POV", values_to = "count") %>% 
  pivot_wider(names_from = farmers_hh, values_from = count) %>% 
  mutate(inside_ramthal=inside_ramthal/946,outside_ramthal=outside_ramthal/666) %>% 
  mutate_at(2:3,round,2) %>% arrange(desc(inside_ramthal ))
  
h3_factors <- 
  social_22 %>% 
  select(hh_id, farmers_hh, h3 )%>% 
  separate(h3, into = c("rnk1" ,"rnk2", "rnk3"), sep = " ") 

h3_rnk1=
  h3_factors %>% count(farmers_hh,rnk1) %>% 
  mutate(prt=ifelse(farmers_hh=="inside_ramthal",n/946,n/666 )) %>% 
  select(-n) %>% filter(!rnk1 %in% c("","-777")) %>% 
  mutate_at(3,round,2) %>% 
  pivot_wider(names_from = farmers_hh, values_from = prt)
  
h3_rnk2=
  h3_factors %>% count(farmers_hh,rnk2) %>% 
  filter(!is.na(rnk2) ) %>%
  mutate(prt=ifelse(farmers_hh=="inside_ramthal",n/946,n/666 )) %>% 
  select(-n) %>%  
  mutate_at(3,round,2) %>% 
  pivot_wider(names_from = farmers_hh, values_from = prt)

h3_rank1 = h3_rnk1[1,2:3] %>% mutate(Factors ="h3_rank1 | Skill_n_knowledge" )
h3_rank2 = h3_rnk2[1,2:3] %>% mutate(Factors ="h3_rank2 | Access_to_gov_schemes" )


# H4
h4_common_factor=
  social_22 %>% select(hh_id, farmers_hh, starts_with("h4") )%>% 
  group_by(farmers_hh) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  select(-c(hh_id,h4__777)) %>% 
  pivot_longer(!farmers_hh, names_to = "POV", values_to = "count") %>% 
  pivot_wider(names_from = farmers_hh, values_from = count) %>% 
  mutate(inside_ramthal=inside_ramthal/946,outside_ramthal=outside_ramthal/666) %>% 
  mutate_at(2:3,round,2) %>% arrange(desc(inside_ramthal ))

h4_factors <- 
  social_22 %>% 
  select(hh_id, farmers_hh, h4 )%>% 
  separate(h4, into = c("rnk1" ,"rnk2", "rnk3"), sep = " ") 

h4_rnk1=
  h4_factors %>% count(farmers_hh,rnk1) %>% 
  mutate(prt=ifelse(farmers_hh=="inside_ramthal",n/946,n/666 )) %>% 
  select(-n) %>% filter(!rnk1 %in% c("","-777")) %>% 
  mutate_at(3,round,2) %>% 
  pivot_wider(names_from = farmers_hh, values_from = prt)

h4_rnk2=
  h4_factors %>% count(farmers_hh,rnk2) %>% 
  filter(!is.na(rnk2) ) %>%
  mutate(prt=ifelse(farmers_hh=="inside_ramthal",n/946,n/666 )) %>% 
  select(-n) %>%  
  mutate_at(3,round,2) %>% 
  pivot_wider(names_from = farmers_hh, values_from = prt)

h4_rank1 = h4_rnk1[1,2:3] %>% mutate(Factors ="h4_rank1 | Skill_n_knowledge" )
h4_rank2 = h4_rnk2[1,2:3] %>% mutate(Factors ="h4_rank2 | Access_to_gov_schemes" )



####### graph

# h3_# h4
h3_H4_ranks <- 
  rbind(h3_rank1,h3_rank2,h4_rank1,h4_rank2) %>% 
  select(Factors,inside_ramthal,outside_ramthal)

h3_H4_ranks %>% mutate(POV=c("H3.1","H3.2","H4.1","H4.2" )) %>%
  select(POV,inside_ramthal,outside_ramthal) %>% 
  kbl() %>% kable_material()
  

Factors_of_agricultural_success
Factors_of_water_availability

# H1:H2
h1h2_h18h23[1:2,] %>% kbl() %>% kable_material()
h1h2_h18h23[1:2,] %>% select(-POV) %>% kbl() %>% kable_material()

#H18:H23
h1h2_h18h23[3:8,] %>% kbl() %>% kable_material()
h1h2_h18h23[3:8,] %>% select(-POV) %>% kbl() %>% kable_material()







######################    essantials    ----

# Function to compute summary statistics
compute_summary <- function(x) {
  c(
    Mean = mean(x),
    Median = median(x),
    sd = sd(x),
    Q25 = quantile(x, 0.25),
    Q75 = quantile(x, 0.75),
    P9= quantile(x, 0.9),
    P95= quantile(x, 0.95),
    Min = min(x),
    Max = max(x)
  )
}
compute_summary_1_99 <- function(x) {
  c(
    Count = n(),
    Mean = mean(x),
    Median = median(x),
    sd = sd(x),
    P1 = quantile(x, 0.01),
    P99 = quantile(x, 0.99),
    Min = min(x),
    Max = max(x)
  )
}

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

# write.csv
write.csv(rmtl_In_groups, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_In_groups.csv", row.names=FALSE)
write.csv(rmtl_InOut_groups, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_InOut_groups.csv", row.names=FALSE)
write.csv(rmtl_baseline2016, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_baseline2016.csv", row.names=FALSE)





















