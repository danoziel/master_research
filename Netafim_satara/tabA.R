
library(readxl)
library(dplyr)
library(tidyr)
library(kableExtra)
#| SC = Sugar Cane
#| df12_sugarcane  Combined database of the first survey and the updated survey
#| df2_sugarcane  Database of ONLY updated survey


# import DFs ------------------------------------------------------------ ----
# obs 11.11.24 - 10.12.24
SugarcaneDF_11Nov2024_10Dec2024 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/NETAFIM_2024/Flood_-_Drip_Survey_Satara_2024_-_all_versions_-_labels_-_2025-03-26-07-55-40.xlsx")

DF_1A <- SugarcaneDF_11Nov2024_10Dec2024 %>% 
  rename(Mobile="Mobile number of the farmer") %>%
  mutate(
  uid = paste0("A", 10000 + match(Mobile, unique(Mobile))))
  
  
# obs 11.12.24 onward
Sugarcane_survey_24_25 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/NETAFIM_2024/Sugarcane_survey_24_25_-_all_versions.xlsx")

DF_1B <- Sugarcane_survey_24_25 %>% 
  rename(Mobile = farmer_mobilenumber) %>% 
  mutate(uid = paste0("B", 20000 + match(Mobile, unique(Mobile)))
         ) %>% 
  mutate(
    ir_method = ifelse(ir_method == "drip","Drip","Flood")  ) %>% 
  mutate(
    install_drip_year = ifelse(
      install_drip_year %in% 1:9,
      2024 - install_drip_year,
      install_drip_year
    )
  ) %>% 
  mutate(
    ir_source = case_when(
      ir_source == "borewell" ~ "Borewell",
      ir_source == "openwell" ~ "Open well",
      ir_source =="pond" ~ "Pond",
      ir_source =="river" ~ "Canal/ River/Dam",
      TRUE ~ ir_source
    )) %>% 
  mutate(
    ir_source_own_or_public = case_when(
      ir_source_own_or_public == "own" ~ "My own",
      ir_source_own_or_public == "pblic" ~ "Public",
      TRUE ~ ir_source_own_or_public
    )
  )


# clean the DFs  -------------------------------------------------------- ----

names(SugarcaneDF_11Nov2024_10Dec2024) # 30 Obs

class(DF_1B$plots_sugarcane)

DF_A <- DF_1A %>% 
  rename(
         acre_sugarcane= "How many acres of sugarcane do you cultivate?", # [8]
         ir_method = "What irrigation method you use in your sugarcane plot?", # [20]
         ir_source = "What is the main irrigation source are you dependent on?", # [22]
         ir_source_own_or_public ="Is it your own or public?", # [24]                                                                                            
         
         irri_times_month = "How many times a month you water your plot?...25", # [25] 
         irri_times_week= "How many times a week you activate the  irrigation for the sugarcane?" , # [26]
         irri_hours = "For how many hours every time?...27", # [27]
         irri_days_between_ir = "What is the average number of days between irrigations?...28", # [28]
         
         insecticides_yn = "Do you use insecticides for the sugarcane plot?...46", # [46]                                             
         fungi_yn = "Do you use fungicides for the sugarcane plot?", # 66
         herbicides_use =  "How do you perform weed control?", # [93]
         
         sold_ton = "How many tons of sugarcane did you sold last year?", # [101]                                                                    
         revenue = "What was the total revenue? (in Rs.)...102",                                                                           
         expenses_total= "What are the expenses on a sugarcane plot? (in Rs.)", # [106]                                                                 
         expenses_labor ="How much of it is for labor? (in Rs.)...107", # [107] 

         ) %>% 
  mutate(plots_sugarcane= NA_real_,
         fertilze = NA_character_,

         ) %>% 
  select(uid,
         acre_sugarcane, plots_sugarcane,
         ir_method,ir_source,ir_source_own_or_public,
         irri_times_month, irri_times_week, irri_hours, irri_days_between_ir,
         insecticides_yn, fungi_yn, fertilze,herbicides_use,
         
         sold_ton, revenue, expenses_total, expenses_labor,

         start,Mobile) 

# --- --- --- --- --- --- 
# updated survey
# names(Sugarcane_survey_24_25)

DF_B <- DF_1B %>% 
  select(uid,
         acre_sugarcane, plots_sugarcane,
         ir_method,ir_source,ir_source_own_or_public,
         irri_times_month, irri_times_week, irri_hours, irri_days_between_ir,
         
         insecticides_yn, fungi_yn, fertilze, herbicides_use,
         sold_ton, revenue, expenses_total, expenses_labor,
         
         start,Mobile)

#               df12_sugarcane -- --- --- --- --- --- ---- 
df12_sugarcane <- rbind(DF_A,DF_B) %>% 
  mutate(ir_source_own_or_public=
           ifelse(ir_source %in% c("Canal/ River/Dam","Pond"), "Public",ir_source_own_or_public))



#               df2_sugarcane --- --- --- --- --- --- ---- 
df2_sugarcane <- DF_1B %>% 
  select(uid,ir_method,
         
         crop_2_acre:crop_2_drip_acre,
         install_drip_year,
         drip_more_less,
         
         job_biz_income,job_biz_less,
         first_drip_system,drip_total_year,
         
         "flood_before_after_drip","irri_months_drip_flood",
         irri_days_drip_flood,"irri_hours_drip_flood",
         
         recommendation,showe_practice,
         
         starts_with("why_drip"), # [37]- [44]
         
         plan_install_drip,                
         "plan_install_drip_date",
         "plan_install_drip_prevent_jobBiz",
         "plan_install_drip_why_not_jobBiz",
         plan_install_drip_why_not,
         
         contains("problm1"), # [80]-[85]                          
         "drip_problm2", # [87]-[90]
         
         start,Mobile) 

library(writexl)
write_xlsx(df2_sugarcane, "C:/Users/Dan/OneDrive - mail.tau.ac.il/NETAFIM_2024/df2_sugarcane.xlsx")
write_xlsx(df12_sugarcane, "C:/Users/Dan/OneDrive - mail.tau.ac.il/NETAFIM_2024/df12_sugarcane.xlsx")

# rm(DF_1A,DF_1B)
# rm(DF_A,DF_B)
# rm(SugarcaneDF_11Nov2024_10Dec2024,SugarcaneDF_11Dec2024_26Mar2025)


### functionS ###                                     ----
plot_ir_method_01 <- function(data, fill_var, plot_title, fill_colors) {
  
  ggplot(data, aes(x = ir_method, y = percent, fill = .data[[fill_var]])) +
    geom_col(width = 0.6) +
    labs(title = plot_title, x = "", y = "%", fill = "") +
    theme_minimal(base_size = 14, base_family = "serif") +
    scale_fill_manual(values = fill_colors)
}


# Summary Statistics ----------------------------------------------------- ----

#     Drip or Flood freq  .................................................----
# Number of Households by Irrigation Methods

#### PLOT
df12_sugarcane %>% 
  count(ir_method) %>% 
  ggplot(aes(x = ir_method, y = n, fill = ir_method)) + 
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  geom_text(aes(label = n ),
            position = position_stack(vjust = 0.85),
            color = "black", size = 4) +
  # scale_fill_manual(values = method_colors) +
  scale_fill_manual(values = c("Drip" = "dodgerblue3", "Flood" = "gray")) +
  labs(title = "Number of Households \nby Irrigation Methods",
       x = "", y = "No. of HH", fill = "") +
  theme_minimal(base_family = "serif")+
  theme(legend.position = "none")

  

#     sugarcane acre and plots  .......................................... ----
#### TABLE 
sc_plot <- 
  df12_sugarcane %>%
  group_by(ir_method) %>%
  summarise(
    n = n(),
    acre_mean = mean(acre_sugarcane, na.rm = TRUE),
    acre_sd = sd(acre_sugarcane, na.rm = TRUE),
    acre_max = max(acre_sugarcane, na.rm = TRUE),
    plots_mean = mean(plots_sugarcane, na.rm = TRUE),
    plots_sd = sd(plots_sugarcane, na.rm = TRUE),
    plots_max = max(plots_sugarcane, na.rm = TRUE))

sc_plot %>%kable() %>% kable_minimal()

#### PLOT
ci_summary <- df12_sugarcane %>%
  select(ir_method, acre_sugarcane, plots_sugarcane) %>% 
  rename(`Total Sugarcane Acre`= acre_sugarcane,
         `Total Sugarcane Plots`= plots_sugarcane) %>% 
  pivot_longer(
    cols = -ir_method, names_to = "variable", values_to = "value") %>%
  group_by(ir_method, variable) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = sum(!is.na(value)),
    se = sd / sqrt(n),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se,
    .groups = "drop") 


ggplot(ci_summary, aes(x = ir_method, y = mean, fill = ir_method)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    color = "black"
  ) +
  geom_text(
    aes(label = paste0(round(mean, 2))), 
    vjust = 9,
    size = 4  ) +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_manual(values = c("Drip" = "dodgerblue3", "Flood" = "gray")) +
  labs(title = "Sugarcane Plots with 95% Confidence Intervals", 
       x = "", y = "", fill = "") +
  theme_minimal(base_family = "serif")+ 
  theme(legend.position = "none")


install.packages("gridExtra")


#     irrigation source .................................................. ----
#### PLOT
water_source <- 
  df12_sugarcane %>%
  select(ir_method, "ir_source", "ir_source_own_or_public") %>%
  pivot_longer(-ir_method, names_to = "variable", values_to = "value") %>%
  group_by(ir_method, variable, value) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  group_by(ir_method, variable) %>%
  mutate(freq_percent = round(100 * count / sum(count), 1)) %>%
  arrange(ir_method, variable, desc(freq_percent))


##### [ir_source]
ir_source_colors <- c("skyblue", "skyblue2", "skyblue3", "skyblue4" )
water_source %>%
  filter(variable == "ir_source") %>%
  ggplot(aes(x = ir_method, y = freq_percent, fill = value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(freq_percent), "%")),
    position = position_stack(vjust = 0.5),color = "black", size = 4) +
  scale_fill_manual(values = ir_source_colors) +
  labs(title="Water Source for Irrigation ",x="", y="% of HH") +
  theme_minimal(base_family = "serif")


##### [ir_source_own_or_public]
OPsource_colors <- c("lightblue3", "lightblue2" )
water_source %>%
  filter(variable == "ir_source_own_or_public") %>%
  ggplot(aes(x = ir_method, y = freq_percent, fill = value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(freq_percent), "%")),
            position = position_stack(vjust = 0.5),color = "black", size = 4) +
  scale_fill_manual(values = OPsource_colors) +
  labs(title = "Water Source | Own or Public",
       x = "",y = "% of HH" , fill = "" ) +
  theme_minimal(base_family = "serif")

df12_sugarcane %>%
  select(ir_method, "ir_source_own_or_public")

#     irrigation times  .................................................. ----

irrigation_pattern <- 
  df12_sugarcane %>% 
  select(ir_method,
         irri_times_month,irri_times_week,irri_hours,irri_days_between_ir) %>% 
  mutate(irri_days_between_ir=
           ifelse(ir_method=="Drip" & irri_days_between_ir>7,NA,irri_days_between_ir)  ) %>%
  pivot_longer(
    cols = -ir_method, names_to = "variable", values_to = "value") %>%
  group_by(variable,ir_method) %>%
   summarise(mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),n = sum(!is.na(value)),se = sd / sqrt(n),
    ci_lower = mean - 1.96 * se,ci_upper = mean + 1.96 * se, .groups = "drop") %>% 
  filter(!is.na(sd))

pattern_drip <- irrigation_pattern %>% filter(ir_method=="Drip")
pattern_flood <- irrigation_pattern %>% filter(ir_method=="Flood")


# Define the order and new labels
ordered_vars <- c("irri_times_month","irri_times_week", "irri_hours", "irri_days_between_ir")
new_labels <- c("irri_times_week" = "Drip | Times per Week",
                "irri_times_month"= "Flood | Times per Month",    
                "irri_hours" = "Hours per Irrigation",
                "irri_days_between_ir" = "Days Between Irrigation")

# pattern_drip %>%
pattern_flood %>% 
  ggplot(aes(x = factor(variable, levels = ordered_vars), y = mean, fill = variable)) +
  geom_bar(stat = "identity", width = 0.6, 
 #          fill = "dodgerblue2") +
            fill = "gray") +
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width = 0.2,color = "black") +
  geom_text(aes(label = round(mean, 2)),  position = position_stack(vjust = 0.5),
            color = "black", size = 4) +
  scale_x_discrete(labels = new_labels) +
  labs(title = "Irrigation Pattern",x = "", y = "") +
  theme_minimal(base_family = "serif") +
  theme(legend.position = "none")




# flood_in_drip_plot  ----
# flood_before_after_drip

flood_in_drip_plot <- df2_sugarcane %>% 
  select(flood_before_after_drip,irri_months_drip_flood,
         irri_days_drip_flood,irri_hours_drip_flood)
  
flood_in_drip_plot %>% count(flood_before_after_drip)

tibble::tribble(~Farm, ~flood_before_after_drip, ~n,
               "Drip farm",  "flood_before_drip",16,"Drip farm","Drip only",1) %>%
  mutate(freq_percent = n / sum(n) * 100) %>% 
  ggplot(aes(x = Farm, y = freq_percent, fill = flood_before_after_drip)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = paste0(round(freq_percent), "%")),
            position = position_stack(vjust = 0.5),color = "white", size = 4) +
  scale_fill_manual(values = c("flood_before_drip" = "dodgerblue4", "Drip only" = "dodgerblue2")) +
  labs(title = "Flooding in addition to dripping",x="",y="" , fill = "" ) +
  theme_minimal(base_family = "serif") + 
  theme(axis.text = element_blank(),axis.ticks = element_blank(),panel.grid = element_blank())


#flood freq
# irri_months_drip_flood	How many months in total do you flood?
# irri_days_drip_flood	How many days in a month?
# irri_hours_drip_flood	How many hours each day?


flood_in_drip_plot %>%   select(-flood_before_after_drip) %>% 
  mutate(irri_days_drip_flood=
           ifelse(irri_days_drip_flood==11,NA,irri_days_drip_flood)) %>%
  rename("Times per Month"= irri_days_drip_flood ,
         "Hours per Irrigation"= irri_hours_drip_flood,
         "Month in a Year" = irri_months_drip_flood) %>% 
  pivot_longer(cols = everything(),
               names_to = "variable", values_to = "value",values_drop_na = T) %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),n = sum(!is.na(value)),se = sd / sqrt(n),
            ci_lower = mean - 1.96 * se,ci_upper = mean + 1.96 * se, .groups = "drop") %>% 
  mutate(variable = factor(variable, levels = c("Times per Month","Hours per Irrigation","Month in a Year"))) %>% 
  
  ggplot(aes(x = variable, y = mean, fill = variable)) +
  geom_bar(stat = "identity", width = 0.6,fill="dodgerblue2") +
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width = 0.2,color = "black") +
  geom_text(aes(label = round(mean, 2)),  position = position_stack(vjust = 0.5),
            color = "black", size = 4) +
  labs(title = "Flooding in drip plot",x = "", y = "") +
  theme_minimal(base_family = "serif")








#     SC treat  ...........................................................----

treat <- df12_sugarcane %>%
  select(uid, start,ir_method,insecticides_yn, fungi_yn, fertilze) %>% 
  mutate(start=as.Date(start)) %>% 
  mutate(fertilze = if_else(start < as.Date("2025-01-28"), NA_character_, fertilze)) %>% 
  arrange(fertilze) %>% 
  select(ir_method ,insecticides_yn ,fungi_yn ,fertilze)
treat$fungi_yn[treat$fungi_yn=="1"] <- "Yes"
treat$fungi_yn[treat$fungi_yn=="0"] <- "No"
treat$insecticides_yn[treat$insecticides_yn=="1"] <- "Yes"
treat$insecticides_yn[treat$insecticides_yn=="0"] <- "No"

treat1 <- treat %>% 
  pivot_longer(cols = -ir_method, names_to = "variable", values_to = "value") %>%
  count(variable,ir_method,value) %>% 
  filter(!is.na(value)) %>% 
  group_by(variable,ir_method) %>% mutate(freq_percent=n/sum(n)*100) 
treat1$variable[treat1$variable=="fungi_yn"] <- "Use Fungicides"
treat1$variable[treat1$variable=="insecticides_yn"] <- "Use Insecticides"


# Plot for "fertilze"
treat1 %>%filter(variable == "fertilze") %>%
  mutate(value = factor(value, levels = c("fertigation", "manual_spread","both"))) %>% 
  ggplot(aes(x = ir_method, y = freq_percent, fill = value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(freq_percent), "%")),
            position = position_stack(vjust = 0.5),color = "black", size = 4) +
  labs(title = "Fertilization method",x = "",y = "% of HH" , fill = "" ) +
  theme_minimal(base_family = "serif") + 
  scale_fill_manual(
    values = c("manual_spread" = "pink3","fertigation" = "pink1", "both" = "pink4"),    
    labels = c( "Fertigation","Manual spread","Both" ))


# Plot for "fungi_yn" "insecticides_yn
treat1 %>%filter(variable != "fertilze") %>%
  mutate(value = factor(value, levels = c("Yes","No"))) %>% 
  ggplot(aes(x = ir_method, y = freq_percent, fill = value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(freq_percent), "%")),
            position = position_stack(vjust = 0.5),color = "black", size = 4) +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "",x = "",y = "% of HH" , fill = "" ) +
  theme_minimal(base_family = "serif") + 
  scale_fill_manual(values = c("Yes" = "pink2","No" = "pink3"))





#     other crop ..........................................................----
# "What crop you cultivate other then sugarcane?"                                                                        
other_crops_0 <- df2_sugarcane %>% 
  select(uid,ir_method,
         crop_2_acre,crop_2,crop_2_drip, crop_2_drip_acre )

# df2_sugarcane$crop_2other

# 1
other_crops_01 <- df2_sugarcane %>%
  select(uid, ir_method, starts_with("crop_2"))
other_crops_01$`crop_2/mango`[other_crops_01$crop_2other=="Mongo"] <- 1
other_crops_01$`crop_2/wheat`[other_crops_01$crop_2other=="Wheet"] <- 1
other_crops_01$`crop_2/wheat`[other_crops_01$crop_2other=="Wheet Mais and onions and cow milk area"] <- 1
other_crops_01$`crop_2/maize`[other_crops_01$crop_2other=="Wheet Mais and onions and cow milk area"] <- 1
other_crops_01$`crop_2/onion`[other_crops_01$crop_2other=="Wheet Mais and onions and cow milk area"] <- 1

# df2_sugarcane %>% count(`crop_2/wheat`)
# other_crops_01 %>% count(`crop_2/wheat`)

# 2
other_crops_1 <- other_crops_01 %>% 
  select(uid,ir_method, starts_with("crop_2")) %>% 
  rename(
    `crop_2/Soybean`=`crop_2/Soy_bean`,
    `crop_2/Limebean`=`crop_2/Lime_bean`,
    `crop_2_drip/Soybean`=`crop_2_drip/Soy_bean`,
    `crop_2_drip/Limebean`=`crop_2_drip/Lime_bean`,
  ) %>% 
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), 0, as.numeric(.x)))) %>% 
  select(where(~ !all(.x == 0)))
#  3
crop_other= other_crops_1 %>% 
  select(ir_method,contains("2/")) %>% 
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), 0, as.numeric(.x)))) %>% 
  pivot_longer(
    cols = -ir_method, names_to = "variable", values_to = "value") %>%
  count(ir_method,variable,value) %>% 
  mutate(variable = ifelse(value == 0, paste0(variable, "_no"), paste0(variable, "_cult")))
#  4
crop_drip= other_crops_1 %>% 
  select(ir_method,contains("drip/")) %>% 
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), 0, as.numeric(.x)))) %>% 
  pivot_longer(
    cols =-ir_method, names_to = "variable", values_to = "value") %>%
  count(ir_method,variable,value) %>% filter(value != 0)%>% 
  mutate(variable = paste0(variable, "_drip"))
#  5
crop_NOdrip <- rbind(crop_other,crop_drip) %>% 
  mutate(variable = sub(".*/", "", variable)) %>% 
  mutate(crop_name = str_remove(variable, "_(cult|drip|no)$")) %>%
  group_by(ir_method,crop_name) %>%
  summarise(
    cult_sum = sum(n[str_detect(variable, "_cult")]),
    drip_sum = sum(n[str_detect(variable, "_drip")])
  ) %>%
  mutate(n = cult_sum - drip_sum) %>%
  mutate(n = ifelse(n < 0, abs(n), n)) %>% 
  select(ir_method,crop_name, n)%>% rename(variable=crop_name) %>% 
  mutate(variable = paste0(variable, "_NOdrip"))
# 6
crop_n <- other_crops_1 %>% count(ir_method) %>% rename(N=n)

# DF
library(stringr)

crop_bind <- rbind(crop_other,crop_drip) %>% 
  mutate(variable = sub(".*/", "", variable)) %>% 
  select(ir_method,variable,n) %>% 
  rbind(crop_NOdrip) %>% 
  separate(variable, into = c("crop", "crop_status"), sep = "_") %>% 
  left_join(crop_n) %>% 
  filter(crop_status %in% c("drip","NOdrip" ) ) %>% 
  mutate(freq_percent = round(100 * n/N, 0)) %>%  ungroup() %>% 
  select(ir_method,crop,crop_status,freq_percent) %>% 
  mutate(crop = str_to_title(crop))

library(writexl)
write_xlsx(crop_bind, "C:/Users/Dan/OneDrive - mail.tau.ac.il/NETAFIM_2024/crop_bind.xlsx")


# PLOT
crop_bind %>% 
  mutate(crop_status =ifelse(crop_status =="drip","Drip Irrigated","Cutivated")) %>% 
  ggplot(aes(x = ir_method, y = freq_percent, fill = crop_status)) +
  geom_col() +
  facet_wrap(~ crop) +
  labs(title = "Crops other than Sugarcane",x="",y = "% of HH ",fill = "") +
  theme_minimal(base_size = 14, base_family = "serif") +
  scale_fill_manual(values = c("Drip Irrigated" = "dodgerblue2", "Cutivated" = "darkolivegreen3"))

# PLOT 2nd version
crop_bind %>% 
  mutate(crop_status =ifelse(crop_status =="drip","ir drip","ir")) %>% 
  mutate(crop_ir = paste(crop, ir_method, sep = "\n")) %>%
  ggplot(aes(x = crop_ir, y = freq_percent, fill = crop_status)) +
  geom_col() +
  scale_fill_manual(values = c("ir drip" = "dodgerblue3", "ir" = "gray"))


crop_bind %>% count(crop)

p1_crop <- crop_bind %>% 
  filter(crop %in% c("Ginger","Wheat","Soybean")) %>% 
  mutate(crop_status =ifelse(crop_status =="drip","Drip Irrigated","Cutivated")) %>% 
  ggplot(aes(x = ir_method, y = freq_percent, fill = crop_status)) +
  geom_col() +facet_wrap(~ crop) +
  labs(title = "Crops other than Sugarcane",x="",y = "% of HH ",fill = "") +
  theme_minimal(base_size = 14, base_family = "serif") +
  scale_fill_manual(
    values=c("Drip Irrigated" = "dodgerblue2", 
             "Cutivated" = "darkolivegreen3"))+
  theme(legend.position = "none")

p2_crop <- crop_bind %>% 
  filter(crop %in% c("Onion","Mango","Maize" )) %>% 
  mutate(crop_status =ifelse(crop_status =="drip","Drip Irrigated","Cutivated")) %>% 
  ggplot(aes(x = ir_method, y = freq_percent, fill = crop_status)) +
  geom_col() +facet_wrap(~ crop) +
  labs(title = "Crops other than Sugarcane",x="",y = "% of HH ",fill = "") +
  theme_minimal(base_size = 14, base_family = "serif") +
  scale_fill_manual(
    values=c("Drip Irrigated" = "dodgerblue2", 
             "Cutivated" = "darkolivegreen3"))+
  theme(legend.position = "none")

p3_crop <- crop_bind %>% 
  filter(crop %in% c("Other" )) %>% 
  mutate(crop_status =ifelse(crop_status =="drip","Drip Irrigated","Cutivated")) %>% 
  ggplot(aes(x = ir_method, y = freq_percent, fill = crop_status)) +
  geom_col() +facet_wrap(~ crop) +
  labs(title = "Crops other than Sugarcane",x="",y = "% of HH ",fill = "") +
  theme_minimal(base_size = 14, base_family = "serif") +
  scale_fill_manual(
    values=c("Drip Irrigated" = "dodgerblue2", 
             "Cutivated" = "darkolivegreen3"))





#     economy    ......................................................... ----

economy_1 <- df12_sugarcane %>% 
  select(ir_method, acre_sugarcane ,
         sold_ton, revenue, expenses_total, expenses_labor) %>% 
  mutate(
    across(where(is.numeric), ~na_if(.x, 0)),
    sold_ton = ifelse(sold_ton >= 1 & sold_ton <= 9, sold_ton * 10, sold_ton),
    revenue = ifelse(revenue >= 1 & revenue <= 100, revenue * 10000, revenue),
    expenses_total = ifelse(expenses_total >= 1 & expenses_total <= 100, expenses_total * 10000, expenses_total),
    expenses_labor = ifelse(expenses_labor >= 1 & expenses_labor <= 100, expenses_labor * 10000, expenses_labor),
    across(c(revenue, expenses_total, expenses_labor), ~ .x / 1000))

economy_2 <- economy_1 %>%
  mutate(
    sold_ton_per_acre = sold_ton / acre_sugarcane,
    revenue_per_acre = revenue / acre_sugarcane,
    expenses_total_per_acre = expenses_total / acre_sugarcane,
    expenses_labor_per_acre = expenses_labor / acre_sugarcane
  ) %>% 
  select(ir_method,sold_ton_per_acre, revenue_per_acre, expenses_total_per_acre, expenses_labor_per_acre)

economy <- economy_2 %>%
  pivot_longer(
    cols = -ir_method, names_to = "variable", values_to = "value") %>%
  group_by(variable,ir_method) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),n = sum(!is.na(value)),se = sd / sqrt(n),
            ci_lower = mean - 1.96 * se,ci_upper = mean + 1.96 * se, 
            .groups = "drop")

library(writexl)
write_xlsx(economy, "C:/Users/Dan/OneDrive - mail.tau.ac.il/NETAFIM_2024/economy.xlsx")



# Plot 1: Sold Production (Ton)
economy %>% filter(variable=="sold_ton_per_acre") %>% 
  ggplot(aes(x = ir_method, y = mean, fill = ir_method)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width = 0.2,color = "black") +
  labs(title = "Production Sold per Acre",x = "",y="Crop (Ton)") +
  theme_minimal(base_size = 14, base_family = "serif") +
  scale_fill_manual(values = c("Drip" = "dodgerblue3", "Flood" = "gray")) +
  theme(legend.position = "none")

# Plot 2: expenses
economy %>%
  filter(variable %in% c("expenses_total_per_acre", "expenses_labor_per_acre")) %>%
  mutate(variable = recode(variable, expenses_total_per_acre = "Total Expenses", 
                           expenses_labor_per_acre = "Labor Expenses")) %>%
  ggplot(aes(x = ir_method, y = mean, fill = ir_method)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.2,color="black") +
  geom_text(aes(label = paste0(round(mean))),vjust=7,size = 4) +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_manual(values = c("Drip" = "dodgerblue3", "Flood" = "gray")) +
  labs(title = "Expenses per Acre", x = "", y ="Thousands ₹") +
  theme_minimal(base_family = "serif")+ 
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0, 37), breaks = seq(0, 35, by = 10))


# Plot 3: Revenue
economy %>% filter(variable=="revenue_per_acre") %>% 
  ggplot(aes(x = ir_method, y = mean, fill = ir_method)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width = 0.2,color = "black") +
  labs(title = "Revenue per Acre",x = "",y="Thousands ₹") +
  theme_minimal(base_size = 14, base_family = "serif") +
  scale_fill_manual(values = c("Drip" = "dodgerblue3", "Flood" = "gray")) +
  theme(legend.position = "none")



#      Opinion drip_more_less .................................................. ----
# tibble::tribble( ~ir_method, ~drip_more_less, ~n,
#                  "Drip","less", 50,"Drip",  "more", 2,
#                  "flood", "less", 40,"flood", "more", 20) %>%
#   group_by(ir_method) %>% mutate(percent = n / sum(n) * 100)

df2_sugarcane %>% 
  filter(!is.na(drip_more_less)) %>% 
  count(ir_method,drip_more_less) %>% 
  mutate(drip_more_less=ifelse(drip_more_less=="less","Less","More") ) %>% 
  group_by(ir_method) %>% mutate(freq_percent=n/sum(n)*100) %>% ungroup() %>% 
  ggplot(aes(x = ir_method, y = freq_percent, fill = drip_more_less)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(freq_percent), "%")),
            position = position_stack(vjust = 0.5),color = "white", size = 4) +
  scale_fill_manual(values = c("Less" = "dodgerblue4", "More" = "dodgerblue2")) +
  labs(title = "Opinion | Does drip cause more or less agricultural work? Apart from deploying the pipes",x = "",y = "% of HH" , fill = "" ) +
  theme_minimal(base_family = "serif")

df2_sugarcane %>% 
  filter(!is.na(drip_more_less)) %>% 
  count(ir_method,drip_more_less) %>% 
  mutate(drip_more_less=ifelse(drip_more_less=="less","Less","More") ) %>% 
  group_by(ir_method) %>% mutate(freq_percent=n/sum(n)*100) %>% ungroup()  %>% 
    ggplot(aes(x = ir_method, y = freq_percent, fill = drip_more_less)) +
  geom_bar(stat = "identity") +     coord_flip() +
  geom_text(aes(label = paste0(round(freq_percent), "%\n", drip_more_less)),
            position = position_stack(vjust = 0.5),color = "white", size = 4) +
  scale_fill_manual(values = c("Less" = "dodgerblue4", "More" = "dodgerblue2")) +
  labs(title = "Opinion | Does drip cause more or less agricultural work? \nApart from deploying the pipes",
       x = "",y = "% of HH" , fill = "" ) +
  theme_minimal(base_family = "serif") + 
  theme(axis.ticks = element_blank(),panel.grid = element_blank(),
    legend.position = "none")



#    first_drip_system ....................................................... ----

df2_sugarcane %>% 
  filter(!is.na(first_drip_system)) %>% 
  count(first_drip_system) %>% 
  mutate(freq_percent=n/sum(n)*100,
         first_drip_system=ifelse(first_drip_system==1,"Yes","No")) %>% 
  ggplot(aes(x = 1, y = freq_percent, fill = first_drip_system)) +
  geom_bar(stat = "identity") +     
  coord_flip() + # horazonal
  geom_text(aes(label = paste0(round(freq_percent), "%\n", first_drip_system)),
            position = position_stack(vjust = 0.5),color = "white", size = 4) +
  scale_fill_manual(values = c("Yes" = "dodgerblue4", "No" = "dodgerblue2")) +
  labs(title = "Is this the first drip system you use? \n(Farms with drip only)",
       x = "",y = "% of HH" , fill = "" ) +
  theme_minimal(base_family = "serif") + 
  theme(axis.text = element_blank(),axis.ticks = element_blank(),panel.grid = element_blank(),
    legend.position = "none")


#   plan_install_drip  -----
df2_sugarcane %>% 
  filter(!is.na(plan_install_drip)) %>% 
  count(plan_install_drip) %>% 
  mutate(freq_percent=n/sum(n)*100,
         plan_install_drip=ifelse(plan_install_drip==1,"Yes","No")) %>% 
  ggplot(aes(x = 1, y = freq_percent, fill = plan_install_drip)) +
  geom_bar(stat = "identity") +     coord_flip() +
  geom_text(aes(label = paste0(round(freq_percent), "%\n", plan_install_drip)),
            position = position_stack(vjust = 0.5),color = "white", size = 4) +
  scale_fill_manual(values = c("Yes" = "dodgerblue4", "No" = "dodgerblue2")) +
  labs(title = "Have plan to install drip \n(Farms without drip)",
       x = "",y = "% of HH" , fill = "" ) +
  theme_minimal(base_family = "serif") + 
  theme(axis.text = element_blank(),axis.ticks = element_blank(),panel.grid = element_blank(),
        legend.position = "none")




#      plan_install_drip_why_not -----
#      For what reasons you don't want drip?
plan <- 
  df2_sugarcane %>% select(plan_install_drip_why_not) %>% 
  filter(!is.na(plan_install_drip_why_not))

plan$no_plan_drip[plan$plan_install_drip_why_not %in% c(
  "Money issues","Expenses","Money")] <- "Lack of Capital"
plan$no_plan_drip[plan$plan_install_drip_why_not %in% c(
  "Dam water is near by","Dam water near to","River is close and he have only one acres")] <- "I Have Water"

plan_not <- plan %>% 
  count(no_plan_drip) %>% 
  mutate(freq_percent=n/sum(n)*100) 
plan_not_N <- sum(plan_not$n)

plan_not %>% 
  ggplot(aes(x = factor(no_plan_drip), y = freq_percent)) +
  geom_bar(stat = "identity", width = 0.6, fill = "dodgerblue4") +
  geom_text(aes(label = paste0(round(freq_percent), "%")),
            vjust = 1.5, color = "white", size = 4) +
  labs(
    title = paste0("For what reasons you don't want drip? [n=",plan_not_N,"]"),
    x = "", y = "% of HH") +
  theme_minimal(base_family = "serif") +
  theme(legend.position = "none")



#      install_drip_year ..................................................----
df2_sugarcane %>%
  select(uid, install_drip_year) %>%
  filter(!is.na(install_drip_year)) %>%
  count(install_drip_year) %>%
  mutate(freq_percent = round(100 * n / sum(n), 0)) %>%
  ggplot(aes(x = factor(install_drip_year), y = freq_percent, fill = factor(install_drip_year))) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(freq_percent, "%")),
            vjust = 1.15, color = "black", size = 4) +
  labs(title = "Drip System Installation Year", x = "",y = "% of HH") +
  theme_minimal(base_family = "serif")+
  theme(legend.position = "none")


#  NO PLOT    job_biz_income , job_biz_less ................................... ----
job_biz <- df2_sugarcane %>%
  select(uid,ir_method, 
         job_biz_income, 
         job_biz_less,
         plan_install_drip_prevent_jobBiz,
         plan_install_drip_why_not_jobBiz) 

# tibble::tribble( ~ir_method, ~job_biz_income,
#                  "Drip",1,"Drip",1,"Drip",1,"Drip",0,
#                  "flood",0,"flood",0,"flood",1,"flood",1) %>%
#   group_by(ir_method) %>% summarise(mean=mean(job_biz_income ))

# Do you have income from a salary job or non-farming business in addition to selling crops?

job_biz %>% filter(!is.na(job_biz_income)) %>% 
  group_by(ir_method) %>% summarise(freq_percent=mean(job_biz_income )*100) %>% 
  
  ggplot(aes(x = ir_method, y = freq_percent,fill = ir_method)) +
  geom_bar(stat = "identity") +     coord_flip() +
  geom_text(aes(label = paste0(round(freq_percent), "%")),
            position = position_stack(vjust = 0.5),color = "black", size = 4) +
  labs(title = "Do you have income from a salary job or non-farming business in addition to selling crops?",
       subtitle = "fraction of farmers said YES",
       x = "",y = ""  ) +
  scale_fill_manual(values = c("Drip" = "dodgerblue3", "Flood" = "gray")) +
  theme_minimal(base_family = "serif") + 
  theme(axis.ticks = element_blank(),panel.grid = element_blank(),
        legend.position = "none")



# Are you holding the job/business because drip causes LESS work?
# ${job_biz_income} = '1'
job_biz %>% filter(!is.na(job_biz_less)) %>% 
  group_by(ir_method) %>% summarise(freq_percent=mean(job_biz_less )*100) %>% 
  ggplot(aes(x = ir_method, y = freq_percent,fill = ir_method)) +
  geom_bar(stat = "identity") +     coord_flip() +
  geom_text(aes(label = paste0(round(freq_percent), "%")),
            position = position_stack(vjust = 0.5),color = "black", size = 4) +
  labs(title = "Are you holding the job/business because drip causes LESS work?",x = "",y = ""  ) +
  scale_fill_manual(values = c("Drip" = "dodgerblue3", "Flood" = "gray")) +
  theme_minimal(base_family = "serif") + 
  theme(axis.ticks = element_blank(),panel.grid = element_blank(),
        legend.position = "none")


# Are you concerned that drip will prevent you from holding a job/business?
# #         ir_method==Flood & ${job_biz_income} = '1' 
# #         and plan_install_drip} = '1'  

job_biz %>% filter(!is.na(plan_install_drip_prevent_jobBiz)) %>% 
  summarise(Yes=mean(plan_install_drip_prevent_jobBiz ))

job_biz %>% filter(!is.na(plan_install_drip_prevent_jobBiz)) %>% 
  group_by(ir_method) %>% summarise(freq_percent=mean(plan_install_drip_prevent_jobBiz )*100) %>% 
  
  ggplot(aes(x = ir_method, y = freq_percent,fill = ir_method)) +
  geom_bar(stat = "identity") +     coord_flip() +
  geom_text(aes(label = paste0(round(freq_percent), "%\n")),
            position = position_stack(vjust = 0.5),color = "black", size = 4) +
  labs(title = "# Are you concerned that drip will prevent you from holding a job/business?",x = "",y = ""  ) +
  scale_fill_manual(values = c("Drip" = "dodgerblue3", "Flood" = "gray")) +
  theme_minimal(base_family = "serif") + 
  theme(axis.ticks = element_blank(),panel.grid = element_blank(),
        legend.position = "none")



#	Do you think using drip will prevent you from holding your job/business?
# #         ir_method==Flood & ${job_biz_income} = '1' 
# #         ${plan_install_drip} = '0' 

job_biz %>% 
  filter(!is.na(plan_install_drip_why_not_jobBiz)) %>% 
  summarise(Yes=mean(plan_install_drip_why_not_jobBiz ))


tibble::tribble( ~plan_install_drip_why_not_jobBiz,
                 1,1,1,0,0,0,1,0,1,1,0,0,0,0,0,0,0) %>%
  summarise(Yes=mean(plan_install_drip_why_not_jobBiz ))



#      irri_months_drip_flood ----

ir_drip_also_flood <- 
  df2_sugarcane %>% 
  select(uid,flood_before_after_drip,irri_months_drip_flood,
         irri_days_drip_flood,irri_hours_drip_flood) %>% 
  filter(!is.na(flood_before_after_drip)) %>% 
  mutate(flood_days_year=irri_months_drip_flood*irri_days_drip_flood ) %>% 
  count(flood_days_year) %>% 
  mutate(freq_percent=n/sum(n)*100) %>% 
  filter(flood_days_year!= 121)

ir_drip_also_flood %>% 
  ggplot(aes(x = factor(flood_days_year), y = freq_percent)) +
  geom_bar(stat = "identity", width = 0.6, fill = "dodgerblue4") +
  geom_text(aes(label = paste0(round(freq_percent), "%")),
            vjust = 1.5, color = "white", size = 4) +
  labs(title = "Flood Days a Year in Drip-Irrigated Plot", x = "", y = "% of HH") +
  theme_minimal(base_family = "serif")

#      starts_with("why_drip")   ----

why_drip<-
  df2_sugarcane %>% 
  filter(!is.na(why_drip)) %>% 
  select(starts_with("why_drip"),
         -why_drip,-`why_drip/other`, -`why_drip/hold_job_biz`)
why_drip_N <- as.numeric(count(why_drip))

why_drip %>% 
  pivot_longer(
    cols =everything(), names_to = "variable", values_to = "value") %>%
  count(variable,value) %>% 
  mutate(variable = sub(".*/", "", variable),
         freq_percent=(n/why_drip_N*100) ) %>% 
  filter(value != 0) %>% 
  ggplot(aes(x = factor(variable), y = freq_percent)) +
  geom_bar(stat = "identity", width = 0.6, fill = "dodgerblue4") +
  geom_text(aes(label = paste0(round(freq_percent), "%")),
            vjust = 1.5, color = "white", size = 4) +
  labs(
    title = paste0("What are the reasons you adopted drip irrigation? [n=",why_drip_N,"]"),
    x = "", y = "% of HH") +
  theme_minimal(base_family = "serif")



























#      problm1 ----
problm1 <- df2_sugarcane %>% 
  select(contains("problm1")) %>% filter(!is.na(drip_problm1 ))

# imply old text observation
problm1[,1:2] %>%  filter(is.na(`drip_problm1/no_problem` ))%>% 
  count(drip_problm1)

problm1$`drip_problm1/no_problem`[problm1$drip_problm1 %in% c(
  "No", "No problem","No issue")] <- 1
problm1 %>% count(`drip_problm1/no_problem`)

problm1$`drip_problm1/rats`[problm1$drip_problm1 %in% c(
  "Rat spoiling","Rat damage","Rat damge","Rat destroying drip")] <- 1
problm1 %>% count(`drip_problm1/rats`)

problm1$`drip_problm1/low_pressure`[problm1$drip_problm1 %in% c(
  "Pressure drip","End of drip line low prey")] <- 1
problm1 %>% count(`drip_problm1/low_pressure`)

problm1$`drip_problm1/water_lock`[problm1$drip_problm1 %in% c(
  "After 2 years water locked in drip",
  "After one year getting lock",
  "Drop lock issue","Water lock",
  "Within 2 years local drop damage")] <- 1
problm1 %>% count(`drip_problm1/water_lock`)









# draft ----


mutate(
  plots_sugarcane= NA_real_,
  drip_more_less =NA_character_,
  job_biz_less = NA_real_, 
  job_biz_income = NA_real_,
  install_drip_year=NA_real_
) %>% 

