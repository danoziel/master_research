
#| 🟡BASELINE 2016 # `rmtl_baseline2016` df from `df16.R` script
#| 🟠MIDELINE 2018 # `rmtl_midline2018` df from `df18.R` script
#| 🟣MIDELINE 2022 # `rmtl_srvy22` df
Ramthal_Karnataka_Cleaned_Data <- read_dta("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/Ramthal_Karnataka_submissions/Ramthal_Karnataka_Cleaned_Data.dta")
rmtl_srvy22 <- Ramthal_Karnataka_Cleaned_Data %>% rename(hh_id=id)
rm(Ramthal_Karnataka_Cleaned_Data)

library(dplyr)
library(haven)
library(tidyr)
library(stringr) #"str_replace"
library(rstatix) # ttest "add_significance"
library(rempsyc) # ttest # nice_table
library(kableExtra )


# DF [full_seasons] ----
full_seasons <- 
  rmtl_InOut %>% select(hh_id,in1_out0,farmers_hh)%>% 
  mutate(kharif_2021=1,rabi_2021_22=1,kharif_2022=1) %>% 
  pivot_longer(!c(farmers_hh,hh_id,in1_out0) , 
               names_to = "season", values_to = "count") %>% 
  select(-"count")




# DF's | [cult_hh]  [hh_2022]  [rmtl_In_groups] [rmtl_InOut_groups] ----

cult_hh <- a_plots_crop[,1] %>% distinct() %>% mutate(hh_cult_2022=1)

# rmtl_InOut ----

mm4_mm5 <- 
  rmtl_srvy22 %>% select(hh_id,mm4,mm5,farmers_hh) 

ir_yr_21 <- 
  irrigation_HH %>% # df in this script below
  rename(hh_irri_2021=hh_irrigated,
         hh_drip_2021= hh_drip,
         hh_6m_2021= hh_6methods)

ir_yr_17_21 <- # rbind dfs in part1_WaterUsage.R script
  rbind(ir18_2017,ir22_2018_2020,ir22_2021) %>% 
  group_by(farmers_hh,hh_id) %>% 
  summarise(hh_irri_yrs_17_21=sum(hh_irri) , hh_drip_yrs_17_21=sum(hh_drip)) %>% 
  mutate(hh_irri_yrs_17_21=ifelse(hh_irri_yrs_17_21==0,0,1), hh_drip_yrs_17_21=ifelse(hh_drip_yrs_17_21==0,0,1) ) %>% ungroup()

rmtl_InOut_groups <- 
  hh_2022 %>%
  left_join(mm4_mm5) %>%
  left_join(ir_yr_21) %>% 
  left_join(ir_yr_17_21) %>%
  left_join(geo_rmtl)

# rmtl_InOut DF
rmtl_InOut <- 
  rmtl_InOut_groups %>% 
  mutate(drip_use= ifelse(is.na(mm5),0,mm5),
         drip_use=drip_use+hh_drip_yrs_17_21,
         drip_use= ifelse(drip_use==2,1,drip_use)
  )%>% 
  mutate(ir_use= ifelse(is.na(mm5),0,mm5),
         ir_use=ir_use+hh_irri_yrs_17_21,
         ir_use= ifelse(ir_use==2,1,ir_use)
  ) %>%   
  mutate(hh_drip_2021_22=ifelse(hh_6m_2021 =="drip",1,0)) %>% 
  mutate(hh_ir_2021_22=ifelse(hh_6m_2021 =="rain",0,1)
  )  %>% 
  rename(hh_6m_2021_22=hh_6m_2021) # %>%   select(-c(infrstr_17_21, waterIR_17_21) )






# Irrigation source a_source_irri ( from L 'cultivation') ----
#   L7 rank irrigation source
# What irrigation source are you dependent on? (Rank according to the degree of importance)
# 
attr(rmtl_srvy22$l7_rank_1, "labels")
# value    label
# 2        Tank/ farm pond
# 3        Open well
# 4        Borewell
# 5        Government water supply source (other than canal)
# 6        Rainfed
# -888     Other (specify) == canal[7]

# DF [hh_2022] N sample to water source
hh_2022 <- 
  rmtl_srvy22 %>% select(hh_id,farmers_hh) %>% 
  group_by(farmers_hh) %>%  
  mutate(N_in.out=n(),in_project=ifelse(farmers_hh=="inside_ramthal",1,0)) %>% ungroup()


# DF [L7_source_irri]
L7_source_irri <- 
  rmtl_srvy22 %>%select(hh_id,starts_with("l7_"))%>%  
  mutate(l7_rank_1 = as.numeric (l7_rank_1),
         l7_rank_2 = as.numeric(l7_rank_2),
         l7_rank_3 = as.numeric(l7_rank_3)
         ) %>%
  # Set "canal" as 7 and combine it into "l7_rank_1 2 3"
  mutate(l7_rank_1=ifelse(l7_rank_1=="-888", 7 ,l7_rank_1),
         l7_rank_2=ifelse(l7_rank_2=="-888",7,l7_rank_2),
         l7_rank_3=ifelse(l7_rank_3=="-888",7,l7_rank_3)
         )


a_source_irri <- 
  L7_source_irri %>%
  mutate(source_ramthal = case_when(
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . == 5) ~ "ramthal",
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . == 4) ~ "borwell",
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . %in% c(2,3,7)) ~ "pond_Owell_canal",
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . == 6)      ~ "rain",
    TRUE ~ NA_character_)
  ) %>% 
  mutate(source_borwell = case_when(
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . == 4) ~ "borwell",
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . == 5) ~ "ramthal",
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . %in% c(2,3,7)) ~ "pond_Owell_canal",
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . == 6)      ~ "rain",
    TRUE ~ NA_character_)
  ) %>% 
  mutate(source_pond = case_when(
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . %in% c(2,3)) ~ "pond_Owell",
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . == 7) ~ "canal",
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . == 4) ~ "borwell",
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . == 5) ~ "ramthal",
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . == 6)      ~ "rain",
    TRUE ~ NA_character_)
  ) %>% 
  mutate(source_canal = case_when(
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . == 7) ~ "canal",
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . == 4) ~ "borwell",
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . == 5) ~ "ramthal",
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . %in% c(2,3)) ~ "pond_Owell",
    if_any(c(l7_rank_1, l7_rank_2, l7_rank_3), ~ . == 6)      ~ "rain",
    TRUE ~ NA_character_)
  ) %>% 
  mutate(source_rank1 = case_when(
    l7_rank_1 == 2 ~ "pond",
    l7_rank_1 == 3 ~ "openwell",
    l7_rank_1 == 4 ~ "borwell",
    l7_rank_1 == 5 ~ "ramthal",
    l7_rank_1 == 6 ~ "rain",
    l7_rank_1 == 7 ~ "canal",
    TRUE ~ NA_character_)) %>% 
  select(hh_id,source_rank1,source_ramthal, source_borwell, source_canal,source_pond)
    
    






#  

#    plots_VARS----
plots_VARS=
  a_rmtl_srvy22 [
    ,c(1,grep(
      "^plot_[1-9]|plot_survey|l_plot_status_|plot_size|^plot_village_[1-9]",names(a_rmtl_srvy22)),#prev plots
      grep("kharif_pre_cultivated|rabi_pre_cultivated|KHA22_pre_cultivated",names(a_rmtl_srvy22)),#prev plots
      grep("l30|l31_hissa|l31_survey|l31_name",names(a_rmtl_srvy22)), #new plots
      grep("l35",names(a_rmtl_srvy22)), #new plots size
      grep("kharif_new_cultivated|rabi_new_cultivated|KHA22_new_cultivated",names(a_rmtl_srvy22)), #new plots
      grep("l13",names(a_rmtl_srvy22)), # Why does your household no longer own the plot?                  
      grep("l20",names(a_rmtl_srvy22)), # Current Operation Status multiple choice                 
      grep("l25",names(a_rmtl_srvy22)), # When did you start cultivating this plot?                  
      grep("l28",names(a_rmtl_srvy22)), # Why was it left fallow?                  
      grep("l32",names(a_rmtl_srvy22)) # How did you come to own this land? NEW PLOT
    )]



# a_plots_size [ plots_size|survey-hissa|plotStatus ] ----

#prev plots 
plotPrev_01=
  rmtl_srvy22[,c(1,grep("^plot_[1-9]|plot_survey|l_plot_status_|plot_size|^plot_village_[1-9]",names(rmtl_srvy22)) )]

library(tidyr)
plotPrev_01 <- as.data.frame(lapply(plotPrev_01, as.character), stringsAsFactors = FALSE)
plotPrev_02 <- plotPrev_01%>% pivot_longer(cols = -hh_id,names_to = c("observation"))

plotPrev_02$observation <- sub("^plot_(\\d{1,2})","plotSrvyHissa_\\1", plotPrev_02$observation) 
plotPrev_02$observation <- sub("^l_plot_status_(\\d{1,2})","plotStatus_\\1", plotPrev_02$observation) 
plotPrev_02$observation <- sub("^plot_village_(\\d{1,2})","plotVillage_\\1", plotPrev_02$observation) 
plotPrev_02$observation <- sub("^plot_survey_(\\d{1,2})","plotSrvy_\\1", plotPrev_02$observation) 
plotPrev_02$observation <- sub("^plot_size_acre_(\\d{1,2})","acre_\\1", plotPrev_02$observation) 
plotPrev_02$observation <- sub("^plot_size_guntas_(\\d{1,2})","guntas_\\1", plotPrev_02$observation) 

library(tidyr)
plotPrev_03 <- plotPrev_02 %>% separate(observation, into = c("vars", "plotID"), sep = "_")
plotPrev_03 <-plotPrev_03 %>% pivot_wider(names_from = vars , values_from = value)

plotPrev_03$plotID <- sprintf("%02d", as.numeric(plotPrev_03$plotID))
plotPrev_03$plotID <- sub("^(\\d{1,2})","plot_\\1",  plotPrev_03$plotID) 

#  column order
col_order_plot <- c("hh_id","plotID","plotSrvy","plotSrvyHissa","acre","guntas",
                    "plotStatus","plotVillage")
plotPrev_04 <- plotPrev_03[, col_order_plot]

plotPrev_04[plotPrev_04=="NA"] <- NA
plotPrev_04[is.na(plotPrev_04)] <- ""

plotPrev_04 <- 
  plotPrev_04[!apply(plotPrev_04[, !colnames(plotPrev_04) %in% c("hh_id", "plotID")] == "", 1, all), ]

rm(plotPrev_01,plotPrev_02,plotPrev_03)

#new plots 
plotNew_01= 
  a_rmtl_srvy22[,c(1,grep("l31_hissa|l31_survey|l31_name",names(a_rmtl_srvy22)),# rm l30= count new plot
                   grep("l35",names(a_rmtl_srvy22)))]

plotNew_01 <- as.data.frame(lapply(plotNew_01, as.character), stringsAsFactors = FALSE)
plotNew_02 <- plotNew_01%>% pivot_longer(cols = -hh_id,names_to = c("observation"))

plotNew_02$observation <- sub("^l31_hissa_(\\d{1,2})","plotSrvyHissa_\\1", plotNew_02$observation) 
plotNew_02$observation <- sub("^l31_survey_(\\d{1,2})","plotSrvy_\\1", plotNew_02$observation) 
plotNew_02$observation <- sub("^l31_name_(\\d{1,2})","plotVillage_\\1", plotNew_02$observation) 
plotNew_02$observation <- sub("^l35_acre_(\\d{1,2})","acre_\\1", plotNew_02$observation) 
plotNew_02$observation <- sub("^l35_guntha_(\\d{1,2})","guntas_\\1", plotNew_02$observation) 

plotNew_03 <- plotNew_02 %>% separate(observation, into = c("vars", "plotID"), sep = "_")
plotNew_03 <-plotNew_03 %>% pivot_wider(names_from = vars , values_from = value)

plotNew_035 <- plotNew_03 %>%
  mutate(plotID=as.numeric(plotID)) %>% 
  mutate(plotID=plotID+10)

plotNew_035$plotID <- sub("^(\\d{1,2})","plot_\\1",  plotNew_035$plotID) 
plotNew_035[is.na(plotNew_035)] <- ""

plotNew_04 <- 
  plotNew_035[!apply(plotNew_035[, !colnames(plotNew_035) %in% c("hh_id", "plotID")] == "", 1, all), ]

plotNew_04$plotStatus <- "new"
plotNew_04 <- plotNew_04[, col_order_plot]

rm(plotNew_01, plotNew_02,plotNew_03,plotNew_035)

# plotPrev + plotNew = plots_size 

plotPrevNew_04 <- rbind(plotPrev_04,plotNew_04)

# plotPrevNew_04$guntas[plotPrevNew_04$guntas <= 0] <- ""
# plotPrevNew_04$acre[plotPrevNew_04$acre <= 0] <- ""

plotPrevNew_04$acre <- as.numeric(plotPrevNew_04$acre)
plotPrevNew_04$guntas <- as.numeric(plotPrevNew_04$guntas)
# plotPrevNew_04$guntas [plotPrevNew_04$guntas== -999] <- 0

plots_size <- 
  plotPrevNew_04 %>%
  mutate(guntas_acre=guntas*0.025) %>% 
  mutate(acres = coalesce(guntas_acre, 0) + coalesce(acre, 0)) %>% 
  select(-(c(acre,guntas,guntas_acre)))
plots_size$acres[plots_size$acres == 0] <- NA

a_plots_size = plots_size





  
# a_irri_rain_method [ irrigated|rainfed|method ] season-plot-crop ----

# L48		How often was the crop irrigated manually? # 1=Always|2=Sometime|3=Never/Rain-fed 
# L48a	What is the method of irrigation?
  
# Checking whether "other" contains values
L48_other <- a_rmtl_srvy22 [,c(1,grep("l48",names(a_rmtl_srvy22)))] %>% select(contains("other"))
flat_vector <- unlist(L48_other, use.names = FALSE)
table(flat_vector, useNA = "always")
rm(L48_other) 

#Removing YEARS and "other" columns.
L48 <- rmtl_srvy22 %>% select(1, matches("l48"), -starts_with("l48_y"), -starts_with("l48a_y"), -contains("other"))

# Checking if its same as L48
# Lp_48 <- L48 %>% select(1, matches("l48_prev_"), matches("l48_new_"),matches("l48a_prev_"),matches("l48a_new_"))

# labels
attr(L48$l48_prev_kha_1_1, "labels")
attr(L48$l48a_prev_kha_1_1, "labels")
# L48		How often was the crop irrigated manually? # 1=Always|2=Sometime|3=Never/Rain-fed 
# L48a	What is the method of irrigation?
#       Flood     Furrows     Drip      Sprinkler   Manual     Hose 
#       1         2           3         4           5          6 

L48 <- rmtl_srvy22 %>% select(1, matches("l48"), -starts_with("l48_y"), -starts_with("l48a_y"), -contains("other"))
L48 <- as.data.frame(lapply(L48, as.character), stringsAsFactors = FALSE)
L48 <- L48 %>% pivot_longer(cols = -hh_id,names_to = "observation",  values_to = "values")

L1_48 <- L48 %>% separate(observation, into = c("L48","prevnew", "season", "plotID","cropIrri"), sep = "_")

L2_48 <- L1_48 %>% pivot_wider(names_from = L48, values_from = values) %>% 
  rename(irri_freq = l48, irri_method = l48a)

L2_48$plotID <- as.numeric(L2_48$plotID )
L2_48$plotID <- ifelse(L2_48$prevnew=="new", L2_48$plotID+10,L2_48$plotID)
L2_48$plotID <- sprintf("%02d", as.numeric(L2_48$plotID))

L2_48$plot_crop <- paste0(L2_48$plotID, "_", L2_48$cropIrri)

L2_48$plotID <- sub("^(\\d{1,2})","plot_\\1",  L2_48$plotID) 
L2_48$cropIrri <- sub("^(\\d{1,2})","cropIrri_\\1",L2_48$cropIrri) 

# 10012024 ml48
ml48=L2_48 %>% mutate(irri_method= ifelse(irri_freq ==3,0,irri_method  ) ) %>% 
  filter( !is.na(irri_method))
ml48$irri_method_num <- ml48$irri_method
ml48$irri_method[ml48$irri_method==1] <- "flood" 
ml48$irri_method[ml48$irri_method==2] <- "furrows"
ml48$irri_method[ml48$irri_method==3] <- "drip"
ml48$irri_method[ml48$irri_method==4] <- "sprinkler"
ml48$irri_method[ml48$irri_method %in% c(5,6) ] <- "hose"
ml48$irri_method[ml48$irri_method==0] <- "rain"
ml48$hh_id=as.numeric(ml48$hh_id)
hh_2022 <-  rmtl_srvy22 %>% select(hh_id,farmers_hh)
a_irri_rain_method = ml48  %>% left_join(hh_2022)
a_irri_rain_method$season[a_irri_rain_method$season== "KHA22"] <- "kharif_2022"
a_irri_rain_method$season[a_irri_rain_method$season== "kha"] <- "kharif_2021"
a_irri_rain_method$season[a_irri_rain_method$season== "rab"] <- "rabi_2021_22"
a_irri_rain_method$season[a_irri_rain_method$season== "rabi"] <- "rabi_2021_22"

                                        
                    
rm( L48,L1_48,L2_48,L3_48)

# irrigation 2021 kha+rab                 ----     
irrigation_HH <- 
  a_irri_rain_method %>%
  select( hh_id ,irri_method) %>% distinct() %>% 
  group_by(hh_id)  %>%
  mutate(hh_6methods = ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>%
  ungroup() %>% select(hh_id,hh_6methods) %>% distinct() %>% 
  mutate(hh_irri=ifelse(hh_6methods=="rain",0,1),
         hh_drip=ifelse(hh_6methods=="drip",1,0)) %>% 
  left_join(rmtl_16_18_22_sample)

irrigation_HH %>% rename(in_project = sample ) %>% 
  count(in_project , hh_drip) %>% 
  filter(!is.na(in_project)) %>% 
  group_by(in_project ) %>% mutate(N=sum(n),n/N)









# irrigation 2021-22 kha+rab+KHA22                      ---- 

irrigation_HH_k21_r2122_k22 <- 
  a_irri_rain_method %>%  select( hh_id ,irri_method) %>% distinct() %>% 
  group_by(hh_id
           )  %>%
  mutate(hh_6methods = ####### DRIP prioritize 
           ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) 
         ) %>%
  mutate(hh_6methods_flood_prioritize = ####### FLOOD prioritize 
           ifelse("flood" %in% irri_method , "flood", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "hose"), "hose",ifelse(any(irri_method  == "drip"), "drip",ifelse(any(irri_method  == "sprinkler"), "sprinkler","rain"))))) 
         ) %>% ungroup()
  
ir_drip_HH_krk <- # DRIP prioritize  # krk =kha+rab+KHA22
  irrigation_HH_k21_r2122_k22 %>%  
  select(hh_id,hh_6methods) %>% distinct() %>% 
  mutate(krk_hh_irri=ifelse(hh_6methods=="rain",0,1),
         krk_hh_drip=ifelse(hh_6methods=="drip",1,0)) %>% 
  rename(krk_hh_6methods=hh_6methods) %>% left_join(hh_2022)

ir_flood_HH_krk <- # FLOOD prioritize  # krk =kha+rab+KHA22
  irrigation_HH_k21_r2122_k22 %>%  
  select(hh_id,hh_6methods_flood_prioritize) %>% distinct() %>% 
  mutate(krk_hh_irri=ifelse(hh_6methods_flood_prioritize=="rain",0,1),
         krk_hh_flood=ifelse(hh_6methods_flood_prioritize %in% c("flood", "furrows","hose" ),1,0)) %>% 
  left_join(hh_2022)

# ir YEARS ----
rmtl_srvy22 %>% select(hh_id, starts_with("l48_y"), starts_with("l48a_y"))
L39y <- rmtl_srvy22 %>% select(hh_id, starts_with("l39b_y_")) # What crops were planted in [ 2018 2019 2020 ]?
attr(L39y$l39b_y_20_1, "labels")

# ir methods 2018-19-20 ----
rmtl_srvy22 %>% select(hh_id, starts_with("l48a_y"))

# check if there are "others"
rmtl_srvy22 %>% select(hh_id, starts_with("l48a_y"),-contains("other")) %>% 
  pivot_longer(cols = -hh_id,names_to = "observation",  values_to = "values") %>% 
  count(values) # NO "others"

ir22_2018_2020 <-
  rmtl_srvy22 %>% select(farmers_hh,hh_id, starts_with("l48a_y"),-contains("other")) %>% 
  pivot_longer(cols = -c(farmers_hh,hh_id),names_to = "observation",  values_to = "irri_method") %>% 
  separate(observation, into = c("L48a","y" ,"year_ir","crop15"), sep = "_") %>% 
  mutate(year_ir =as.numeric(year_ir),year_ir=year_ir+2000) %>% 
  mutate(irri_method=ifelse(is.na(irri_method),0,irri_method)) %>% 
  group_by(hh_id,year_ir) %>% 
  mutate(
    hh_6methods = ifelse(3 %in% irri_method , "drip", 
                  ifelse(any(irri_method  == 2), "furrows",
                  ifelse(any(irri_method  == 1), "flood",
                  ifelse(any(irri_method  == 4), "sprinkler",
                  ifelse(any(irri_method  == 6), "hose","rain"))))) ) %>%  #  filter(hh_id %in% c(103257,105832))
  select(farmers_hh,hh_id,hh_6methods,year_ir) %>% distinct() %>% ungroup() %>% 
  mutate(hh_irri=ifelse(hh_6methods=="rain",0,1),
         hh_drip=ifelse(hh_6methods=="drip",1,0)) %>% 
  select(year_ir,everything())

ir22_2018_2020 %>%  count(farmers_hh,year_ir,hh_6methods) %>% mutate(prt=ifelse(farmers_hh=="inside_ramthal",n/946,n/666))




# YIELD		 a_total_yield        ----					
# L49	 What was the total yield [int]	# [Season-Crop]	

attr(rmtl_srvy22$l49_prev_kha_unit_1_1 , "labels") 
##  1=Kilograms  ##       2=Bags     ##  3=Quintal  ##  4=Ton   ## 
##      1 Kg     ##  l49_XX_XX_bag_  ##    100Kg    ##  1000Kg  ##

names(L49plot)
L49 <- 
  rmtl_srvy22 %>% select(farmers_hh,hh_id,starts_with("l49_" )) %>% 
  
  #filter(hh_id %in% c("100001","100019") ) %>%  
 # select(where(~!all(is.na(.x)))) %>% 
  
  pivot_longer(cols = -c(farmers_hh ,hh_id),names_to = c("observation"))
L49A <- L49 %>% separate(observation, into = c("L" ,"pn","season", "unitbag", "plot", "crop"), sep = "_")

L49b=L49A %>% 
  mutate(plotID=ifelse(is.na(crop) ,unitbag, plot), 
         Unitbag=ifelse(is.na(crop) , NA ,unitbag ), 
         crop_number=ifelse(is.na(crop) , plot ,crop )) %>% 
  select(-c(L,plot,unitbag,crop))

L49c <- L49b %>%mutate(plotID=as.numeric(plotID), plotID=ifelse(pn=="new",plotID+10,plotID)  )
L49c$plotID <- sprintf("%02d", L49c$plotID)
L49c$plotID <- sub("^(\\d{1,2})","plot_\\1",  L49c$plotID) 
L49c$crop_number <- sub("^(\\d{1,2})","crop\\1",  L49c$crop_number) 

L49c$season[L49c$season=="rab"] <- "rabi_2021_22"
L49c$season[L49c$season=="kha"] <- "kharif_2021"
L49c$season[L49c$season=="KHA22"] <- "kharif_2022"

L49c$Unitbag[is.na(L49c$Unitbag)] <- "total_yield"

L49d <- L49c %>% pivot_wider(names_from = Unitbag, values_from = value)

a_total_yield <- L49d %>% 
  mutate(kg_crop = 
           ifelse(unit == 3,total_yield*100,
           ifelse(unit == 4,total_yield*1000,
           ifelse(unit == 2,total_yield*bag, 
                  total_yield))) 
         ) %>% 
  filter(!is.na(total_yield)) %>% 
  select(farmers_hh,hh_id,season,plotID,crop_number,kg_crop) 

# most of the 0's are for annual crops
a_total_yield$kg_crop[a_total_yield$kg_crop == 0] <- NA
  

write.csv(a_total_yield, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/a_total_yield.csv", row.names=FALSE)


# YIELD Sold Kept Lost    [yield_prt]         ----					
# How much of the yield was [%]	# [percentage at Season-Crop]
# [L52] Sold # [L53] Kept for HH consumption # [L54] Lost in post-harves

names(L54)

# [L52] Sold
L52= rmtl_srvy22 %>% select(hh_id, farmers_hh, starts_with("L52"), -starts_with("l52_stored") ) %>% 
  pivot_longer(cols = -c(farmers_hh ,hh_id),names_to = c("observation"))
L52A <- L52 %>% separate(observation, into = c("L" ,"pn","season", "plot", "crop"), sep = "_") %>% 
  filter(!is.na(value) )
L52b <- L52A %>% rename(prt_sold=value) %>% select(-L)

# [L52] _stored
L52_stored = rmtl_srvy22 %>% select(hh_id, farmers_hh, starts_with("l52_stored")) %>% 
  pivot_longer(cols = -c(farmers_hh ,hh_id),names_to = c("observation"))
L52A_stored <- L52_stored %>% separate(observation, into = c("L","stored","pn","season", "plot", "crop"), sep = "_") %>% 
  filter(!is.na(value) )
L52b_stored <- L52A_stored %>% rename(prt_stored=value) %>% select(-stored)%>% select(-L)

# [L53] Kept
L53= rmtl_srvy22 %>% select(hh_id, farmers_hh, starts_with("L53")) %>% 
  pivot_longer(cols = -c(farmers_hh ,hh_id),names_to = c("observation"))
L53A <- L53 %>% separate(observation, into = c("L" ,"pn","season", "plot", "crop"), sep = "_") %>% 
  filter(!is.na(value) )
L53b <- L53A %>% rename(prt_consum=value) %>% select(-L)

# [L54] Lost
L54= rmtl_srvy22 %>% select(hh_id, farmers_hh, starts_with("L54")) %>% 
  pivot_longer(cols = -c(farmers_hh ,hh_id),names_to = c("observation"))
L54A <- L54 %>% separate(observation, into = c("L" ,"pn","season", "plot", "crop"), sep = "_") %>% 
  filter(!is.na(value) )
L54b <- L54A %>% rename(prt_lost=value ) %>% select(-L)

yield_prt = full_join(L52b,L52b_stored) %>% 
  full_join(L53b) %>% 
  full_join(L54b) %>%
  mutate(plotID=as.numeric(plot), 
         plotID=ifelse(pn=="new",plotID+10,plotID)  )

yield_prt$plotID <- sprintf("%02d", yield_prt$plotID)
yield_prt$plotID <- sub("^(\\d{1,2})","plot_\\1",  yield_prt$plotID) 
yield_prt$crop_number <- sub("^(\\d{1,2})","crop\\1",  yield_prt$crop) 

yield_prt$season[yield_prt$season=="rab"] <- "rabi_2021_22"
yield_prt$season[yield_prt$season=="kha"] <- "kharif_2021"
yield_prt$season[yield_prt$season=="KHA22"] <- "kharif_2022"

yield_prt <- yield_prt %>% 
  select(hh_id, farmers_hh, season, plotID, crop_number, prt_sold, prt_stored, prt_consum, prt_lost)




##### REVENUE a_plots_revenue -----
# l78_reven  
# reven crop plot season
# l78_reven_prev_PLOT_CROP1/CROP2
reve_crop_prev <- rmtl_srvy22[,c(1,grep("l78_reven_prev_",names(rmtl_srvy22)) )]

#  reve_crop_prev 

re01_pre <- reve_crop_prev %>% pivot_longer(cols = -hh_id,names_to = "observation",  values_to = "plotRevenue")

re01_pre$observation <- sub("^l78_reven_prev_kha_","kha_", re01_pre$observation)
re01_pre$observation <- sub("^l78_reven_prev_rab_","rabi_", re01_pre$observation)
re01_pre$observation <- sub("^l78_reven_prev_KHA22_","KHA22_", re01_pre$observation)

re02_pre <- re01_pre %>% separate(observation, into = c("season", "plotID","cropReve"), sep = "_")

re02_pre$plotID <- sprintf("%02d", as.numeric(re02_pre$plotID))
re02_pre$plotID <- sub("^(\\d{1,2})","plot_\\1",  re02_pre$plotID) 
re02_pre$cropReve <- sub("^(\\d{1,2})","cropReve_\\1",re02_pre$cropReve) 

re03_pre <- re02_pre %>% filter(plotRevenue>0)
re04_pre <- re03_pre # 1,513 HH

#  reve_crop_new 
reve_crop_new <-  rmtl_srvy22[,c(1,grep("l78_reven_new_",names(rmtl_srvy22)) )]
re01_new <- reve_crop_new%>% pivot_longer(cols = -hh_id,names_to = "observation",  values_to = "plotRevenue")

re01_new$observation <- sub("^l78_reven_new_kha_","kha_", re01_new$observation)
re01_new$observation <- sub("^l78_reven_new_rab_","rabi_", re01_new$observation)
re01_new$observation <- sub("^l78_reven_new_KHA22_","KHA22_", re01_new$observation)

re02_new <- re01_new %>% separate(observation, into = c("season", "plotID","cropReve"), sep = "_")

re03_new <- re02_new %>%
  mutate(plotID=as.numeric(plotID)) %>% 
  mutate(plotID=plotID+10)

re03_new$plotID <- sub("^(\\d{1,2})","plot_\\1",re03_new$plotID)
re03_new$cropReve <- sub("^(\\d{1,2})","cropReve_\\1",re03_new$cropReve)

re04_new <- re03_new%>% filter(plotRevenue>0)


#  a_plots_revenue 

# bind [re04_pre + re04_new]
a_plots_revenue <- rbind(re04_pre,re04_new)

a_plots_revenue <- 
  a_plots_revenue %>%
  mutate(plot_crop = paste0(gsub("[^0-9]", "", plotID), "_", gsub("[^0-9]", "", cropReve)))


#######  CROP plots_crop_2022 (former: a_plots_crop) ###### ----
# L39a	Crop-Plot-Season 21-22 ----
# L39a	Crop-Plot-Season	kharif 2022 # Rabi 2021/2022 # Kharif 2021
# Perennial/biseasonal crops will be listed in Kharif

L39_prev <- rmtl_srvy22 [,c(1,grep("l39_prev_",names(rmtl_srvy22)))] 

# L39_prev
C1_pre=L39_prev[,c(1, grep("^l39_prev_\\d+_\\d+_\\d+$", names(L39_prev)))] # Eliminate Pattern like "l39_prev_3__888_6"  "l39_prev_3_other_6" "l39_prev_kha_7" 

C2_pre <- C1_pre%>% pivot_longer(-hh_id,names_to = "observation", values_to = "values")
C2_pre$observation <- sub("^l39_prev_1","kha",C2_pre$observation) 
C2_pre$observation <- sub("^l39_prev_2","rabi",C2_pre$observation) 
C2_pre$observation <- sub("^l39_prev_3","KHA22",C2_pre$observation) 

C3_pre <- separate(C2_pre, observation, into = c("season", "crop", "plotID"), sep = "_")
C3_pre$plotID <- as.numeric(C3_pre$plotID)
C3_pre$plotID <- sprintf("%02d",(C3_pre$plotID))
C3_pre$plotID <- sub("^(\\d{1,2})","plot_\\1",C3_pre$plotID) 

C4_pre <- C3_pre %>% filter(values>0)

your_data <- C4_pre %>%
  group_by(hh_id,season,plotID) %>%
  mutate(row_number = row_number())


# pre other

Co1_pre=L39_prev[,c(1, grep("^l39_prev_\\d+_other", names(L39_prev)) )] # Eliminate Pattern like "l39_prev_3__888_6"  "l39_prev_3_other_6" "l39_prev_kha_7" 
Co2_pre <- Co1_pre%>% pivot_longer(-hh_id,names_to = "observation", values_to = "values")
Co2_pre$observation <- sub("^l39_prev_1","kha",Co2_pre$observation) 
Co2_pre$observation <- sub("^l39_prev_2","rabi",Co2_pre$observation) 
Co2_pre$observation <- sub("^l39_prev_3","KHA22",Co2_pre$observation) 

Co3_pre <- separate(Co2_pre, observation, into = c("season", "other", "plotID"), sep = "_")
Co3_pre <- Co3_pre %>% rename(crop=values)

Co3_pre$plotID <- as.numeric(Co3_pre$plotID)

Co3_pre$plotID <- sprintf("%02d",(Co3_pre$plotID))
Co3_pre$plotID <- sub("^(\\d{1,2})","plot_\\1",Co3_pre$plotID) 

Co4_pre <- Co3_pre %>% 
  filter(crop != "") %>% mutate(values="1") %>% 
  select(hh_id,season,crop,plotID,values )


# L39_new
L39_new  <- rmtl_srvy22 [,c(1,grep("l39_new",names(rmtl_srvy22)))]
C1_new=L39_new[,c(1, grep("^l39_new_\\d+_\\d+_\\d+$", names(L39_new)))] # Eliminate Pattern like "l39_prev_3__888_6"  "l39_prev_3_other_6" "l39_prev_kha_7" 

C2_new <- C1_new%>% pivot_longer(-hh_id,names_to = "observation", values_to = "values")
C2_new$observation <- sub("^l39_new_1","kha",C2_new$observation) 
C2_new$observation <- sub("^l39_new_2","rabi",C2_new$observation) 
C2_new$observation <- sub("^l39_new_3","KHA22",C2_new$observation) 

C3_new <- C2_new %>% separate(observation,into=c("season","crop","plotID"), sep = "_")

C4_new <- C3_new %>%
  mutate(plotID=as.numeric(plotID)) %>% 
  mutate(plotID=plotID+10)
C4_new$plotID <- sub("^(\\d{1,2})","plot_\\1",C4_new$plotID)

C4_new <- C4_new%>% filter(values>0)

# new other

Co1_new=L39_new[,c(1, grep("^l39_new_\\d+_other", names(L39_new)) )] # Eliminate Pattern like "l39_prev_3__888_6"  "l39_prev_3_other_6" "l39_prev_kha_7" 
Co2_new <- Co1_new%>% pivot_longer(-hh_id,names_to = "observation", values_to = "values")
Co2_new$observation <- sub("^l39_new_1","kha",Co2_new$observation) 
Co2_new$observation <- sub("^l39_new_2","rabi",Co2_new$observation) 
Co2_new$observation <- sub("^l39_new_3","KHA22",Co2_new$observation) 

Co3_new <- separate(Co2_new, observation, into = c("season", "other", "plotID"), sep = "_")
Co3_new <- Co3_new %>% rename(crop=values)

Co4_new <- Co3_new %>%
  mutate(plotID=as.numeric(plotID)) %>% 
  mutate(plotID=plotID+10)
Co4_new$plotID <- sub("^(\\d{1,2})","plot_\\1",Co4_new$plotID)

Co4_new <- Co4_new %>% 
  filter(crop != "") %>% mutate(values="1") %>% 
  select(hh_id,season,crop,plotID,values )


rm(L39_prev, your_data, C4_pre, C3_pre, C2_pre, C1_pre, 
   Co1_pre, Co2_pre, Co3_pre, Co4_pre,
   L39_new, C1_new, C2_new, C3_new, C4_new,
   Co1_new, Co2_new, Co3_new, Co4_new
)

# crop_plot= rbind()

crop_plot= rbind(C4_pre,Co4_pre,C4_new,Co4_new)

crop_plot$crop [crop_plot$crop == "Ajwain"] <- 99
crop_plot$crop[crop_plot$crop == "Ajjawain"] <- 99

crop_plot$crop[crop_plot$crop == "Cucumber"] <- 90
crop_plot$crop[crop_plot$crop == "Coriander seeds"] <- 96
crop_plot$crop[crop_plot$crop =="Fodder for Cattle"] <-91
crop_plot$crop[crop_plot$crop =="mulberry leaves"] <-92
crop_plot$crop[crop_plot$crop =="Neem tree"] <-93
crop_plot$crop[crop_plot$crop =="palm tree"] <-94
crop_plot$crop[crop_plot$crop =="Sandal and Neem trees"] <-95

library(readr)
list_crop <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/list_crop.xlsx")


# Add the crop_type column using ifelse
list_crop$crop_type <- ifelse(list_crop$crop_name %in% c(
  "Beans", "Capsicum", "Cauliflower", "Chillies", "Cowpea",
  "Field beans", "Finger_millet", "Foxtail_millet", "Gherkins",
  "Gourds", "Ladyfinger", "Peas", "Potato", "Raddish","Tomato"), 
  "Seasonal",
  ifelse(list_crop$crop_name %in% c(
    "Bengal_gram", "Black_gram", "Cotton", "Greengram","Sorghum_jowar",
    "Sunflower", "Groundnut", "Horsegram", "Linseed", "Maize", "Paddy", 
    "Pearl.millet_bajra", "Soyabean", "Toor", "Wheat", "Ajwain",
    "Coriander_seeds","Cucumber","Onions","Safflower","Sesame"
    ),
    "Annual",
    ifelse(list_crop$crop_name %in% c(
      "Castor", "Cocoa", "Fig", "Guava", "Jackfruit", "Papaya", "Pineapple", 
      "Pomegranate", "Sapota","Sugarcane","Coconut", "Lime","Neem_tree",
      "Sandal_and_Neem_trees","Palm_tree"
      ),
      "Perennial",
      "Seasonal")))                     

write.csv(list_crop, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/list_crop.csv", row.names=FALSE)

crop_plot_1 <-
  crop_plot %>%
  rename(crop_code=crop) %>% mutate(crop_code=as.integer(crop_code) ) %>% 
  left_join(list_crop) %>%
  select(hh_id, season, plotID, crop_common,crop_name    )

crop_plot_2 <- crop_plot_1 %>%
   group_by(hh_id,season,plotID) %>%
   mutate(crop_number = row_number()) %>% ungroup()

crop_plot_3 <- crop_plot_2 %>%
   mutate(plot_crop = paste0(gsub("[^0-9]", "", plotID),
                             "_", gsub("[^0-9]", "", crop_number))) # %>% 
  mutate(crop_code=as.numeric(crop_code) ) 

crop_plot_3$crop_number <- sub("^(\\d{1,2})","crop\\1",  crop_plot_3$crop_number) 

crop_plot_3$season[crop_plot_3$season=="kha"]   <- "kharif_2021"
crop_plot_3$season[crop_plot_3$season=="rabi"]  <- "rabi_2021_22"
crop_plot_3$season[crop_plot_3$season=="KHA22"] <- "kharif_2022"


crop_plot_3 %>% count(season)
crop_plot_3[,1:2] %>% distinct() %>% count(season)


crop_plot_4 = crop_plot_3
crop_plot_4 %>% select(1,season)  %>% distinct() %>% count(season)
crop_plot_4 %>% count(plotID)
crop_plot_4 %>% count(hh_id ) # 1578 HH

plots_crop_2022 <-  crop_plot_4 %>% 
  left_join(a_plots_size %>% select(hh_id,plotID,acres)) 
mean(plots_crop_2022$acres) # check if there is NAs

rm(crop_plot, crop_plot_1, crop_plot_2, crop_plot_3,
   crop_plot_4,crop_plot_4A,crop_plot_4B
)

plots_crop_2022








# 4 missing in a_plots_crop to combine a_total_yield
a_total_yield[,2:6] %>% full_join(plots_crop_2022) %>% filter(is.na(crop_name   ))

plots_crop_yield_2022 <- 
  a_total_yield[,2:6] %>% full_join(plots_crop_2022) %>% 
  fill(crop_code, crop_name, common_n_family, .direction = "down") %>%   
  mutate(plot_crop = paste0(substr(plotID, 6, 7), 
                            "_", 
                            substr(crop_number, 5, 5)))



# L39_2021  ----



# crop2021$l39_prev_1   :l39_prev_1_other_10 %>% 
#   summarise_at(vars(l39b_y_18_10:l39b_y_18__888), sum, na.rm = TRUE)



crop2021 <- L39_a [,c(1,grep("^l39_(new_|prev_)[1-9]",names(L39_a)))] # rm l39_prev_kha_1 & l39_new_kha_1
crop2021 <- crop2021[,!grepl("^l39_(new_|prev_)[1-9]__888", names(crop2021))] # rm l39_prev_2__888_4 l39_new_2__888_4

library(tidyr)
l3999 <- 
  crop2021%>% 
  pivot_longer(-id,names_to = "crop", values_to = "n_HH") %>% 
  filter(n_HH>0)

# l3999$plot need to resulve the "other"
l3999$plot <- sub("^l39_prev_\\d+_\\d+_(\\d)","plot_\\1", l3999$crop) #prev plots 1-10
l3999$plot <- sub("^l39_new_\\d+_\\d+_(\\d)","plot_1\\1", l3999$plot) #new plots 11-21

l3999$l39_crop <- sub("^l39_(new_|prev_)\\d+_(\\d+)_.+", "\\2", l3999$crop)
l3999$l39_crop <-ifelse(l3999$n_HH != 1,l3999$n_HH,l3999$l39_crop)
l3999$l39_crop[l3999$l39_crop == "Ajjawain"] <- "Ajwain"

# l3999$season <- sub(".*_1_\\d+_\\d+$", "kha", l3999$crop)
l3999$season <- sub("^l39_(new_|prev_)1_.*", "kha", l3999$crop)
l3999$season <- sub("^l39_(new_|prev_)2_.*", "rab", l3999$season)
l3999$season <- sub("^l39_(new_|prev_)3_.*", "KHA22",l3999$season)


HH_num_per_crop_2021 <-  l3999 %>%rename(HH_id=id) %>% mutate(HH_id=as.character(HH_id) ) %>%  inner_join(a_sample)


# CROP LIST crops 2021 L39_2021 -----

L39_2021 <- 
  l3999 %>% select(id, season ,l39_crop ) %>% 
  mutate(l39_crop=ifelse(l39_crop == "Ajjawain", "Ajwain", l39_crop )) %>% 
  mutate(crop_yr=ifelse(l39_crop=="Ajwain",99,l39_crop )) %>% 
  filter(season != "KHA22")%>% 
  
  select(id,crop_yr ) %>% distinct()%>% 
  count(crop_yr, name = "n_HH_2021") 

L39_2021$crop_yr[L39_2021$crop_yr == "Cucumber"] <- 90
L39_2021$crop_yr[L39_2021$crop_yr == "Coriander seeds"] <- 96
L39_2021$crop_yr[L39_2021$crop_yr =="Fodder for Cattle"] <-91
L39_2021$crop_yr[L39_2021$crop_yr =="mulberry leaves"] <-92
L39_2021$crop_yr[L39_2021$crop_yr =="Neem tree"] <-93
L39_2021$crop_yr[L39_2021$crop_yr =="palm tree"] <-94
L39_2021$crop_yr[L39_2021$crop_yr =="Sandal and Neem trees"] <-95


# L39b	Crop-Year	2018 2019 2020 ----
# L39b	Crop-Year	What crops were planted in [ year ]?
L39b_y_2018=a_rmtl_srvy22[,c(1,grep("l39b_y_18",names(a_rmtl_srvy22)))] 
L39b_y_2019=a_rmtl_srvy22[,c(1,grep("l39b_y_19",names(a_rmtl_srvy22)))] 
L39b_y_2020=a_rmtl_srvy22[,c(1,grep("l39b_y_20",names(a_rmtl_srvy22)))] 

library(haven)
library(tidyr)
L39b_y_18A <- 
  L39b_y_2018 %>%
  summarise_at(vars(l39b_y_18_10:l39b_y_18__888), sum, na.rm = TRUE) %>% 
  pivot_longer(everything(),names_to = "crop", values_to = "n_HH_2018") %>% 
  mutate(crop=ifelse(crop=="l39b_y_18__888","l39b_y_18_Ajwain",crop))
L39b_y_18A$crop <- sub("^l39b_y_18_", "l39b_", L39b_y_18A$crop)

L39b_y_19A <- 
  L39b_y_2019 %>%
  summarise_at(vars(l39b_y_19_10:l39b_y_19__888), sum, na.rm = TRUE) %>% 
  pivot_longer(everything(),names_to = "crop", values_to = "n_HH_2019") %>% 
  mutate(crop=ifelse(crop=="l39b_y_19__888","l39b_y_19_Ajwain",crop))
L39b_y_19A$crop <- sub("^l39b_y_19_", "l39b_", L39b_y_19A$crop)

L39b_y_20A <- 
  L39b_y_2020 %>% 
  summarise_at(vars(l39b_y_20_10:l39b_y_20__888), sum, na.rm = TRUE)%>% 
  pivot_longer(everything(),names_to = "crop", values_to = "n_HH_2020")%>% 
  mutate(crop=ifelse(crop=="l39b_y_20__888","l39b_y_20_Ajwain",crop))
L39b_y_20A$crop <- sub("^l39b_y_20_", "l39b_", L39b_y_20A$crop)

# full_join lists crops_freq_2018_2021 

# L39b_y_18_19_20
L39b_y_18_19_20 <- 
  full_join(L39b_y_18A, L39b_y_19A) %>% full_join(L39b_y_20A) %>% 
  mutate(col_to_rm_zero=n_HH_2018+n_HH_2019+n_HH_2020) %>% 
  filter(col_to_rm_zero>0) %>% 
  mutate(crop_yr=ifelse(crop=="l39b_Ajwain","l39b_99",crop )) 

L39b_y_18_19_20$crop_yr <- sub("^l39b_", "", L39b_y_18_19_20$crop_yr)
L39b_y_18_19_20 <- select(L39b_y_18_19_20, crop_yr, n_HH_2018, n_HH_2019, n_HH_2020)

# crops_freq_2018_2021
crops_freq_2018_2021 <- full_join(L39b_y_18_19_20,L39_2021)
crops_freq_2018_2021[is.na(crops_freq_2018_2021)] <- 0

library(readxl)
list_crop <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/list_crop.xlsx")
list_cropA <- list_crop %>% mutate(crop_code= as.character(crop_code)) %>% rename(crop_yr=crop_code)

crops_freq_2018_2021 <-left_join(crops_freq_2018_2021,list_cropA )
# crop_family	crop_common	common_n_family	common_n_family_other


library(treemap)
treemap(crops_freq_2018_2021,index=c("crop_family", "crop_name"),
        vSize="n_HH_2021",
        vColor = "color_cn",
        type = 'color',position.legend	 = "none",title = "",)


treemap(crops_freq_2018_2021,index=c("crop_family","crop_name"),
        vSize="n_HH_2018",vColor="n_HH_2018",
        type="value",position.legend	 = "none",title = "",)




# L80_L90 adopted OR stopped growing----
# Since 2016, have you adopted any of the above crops?
# Since 2016, have you stopped growing one of the above crops?
#|-----------------------------------------------------------|
#| The numbers seem exaggerated and illogical                |
#| a thorough examination must be carried out or thrown away |
#|-----------------------------------------------------------|
L80_L90=a_rmtl_srvy22 %>% select(id,l81:l87,l89:l96)

L80 <- L80_L90 %>%
  summarise_at(vars(l81:l87), sum, na.rm = TRUE)
names(L80) <- c('Sunflower','Tomato','Onion','Flower Crops','Chillies','Leafy vegetables','Toor/Redgram/PegionPea')
L80 <- L80 %>% 
  pivot_longer(everything(),names_to = "crop_adopted", values_to = "n_HH") 



























###### LABOR ----
# L74 Labor 
# How much paid labor and family labor, in percentages, per season?
L74 <- a_rmtl_srvy22 %>% select(id,starts_with("L74"))

# L76 Labor days 
# How many days did they put in a season, on average?
# l76_prev_SEASON_hh_?_PLOT_MMBR
L76 <- a_rmtl_srvy22 %>% select(id,starts_with("L76"))

#kharif 2021 plot 1
L76Kha2021_p1 <- L76 %>% select(id,starts_with("l76_prev_1_hh_1_1_"),starts_with("l76_prev_1_hh_2_1_"))

#kharif 2021
L76Kha2021 <- L76 %>% select(id,starts_with("l76_prev_1_hh_") )
L76Kha2021$mean <- rowMeans(L76Kha2021[, -1], na.rm = TRUE)
L76Kha2021 %>% summarise(Mean= mean(mean, na.rm = TRUE))

#rabi 2021
L76rab2021 <- L76 %>% select(id,starts_with("l76_prev_2_hh_") )
L76rab2021$mean <- rowMeans(L76rab2021[, -1], na.rm = TRUE)
L76rab2021 %>% summarise(Mean= mean(mean, na.rm = TRUE))

#kharif 2022
L76Kha2022 <- L76 %>% select(id,starts_with("l76_prev_3_hh_") )
L76Kha2022$mean <- rowMeans(L76Kha2022[, -1], na.rm = TRUE)
L76Kha2022 %>% summarise(Mean= mean(mean, na.rm = TRUE))

names(L76)










#    pre_cultivated PC ----
PC= a_rmtl_srvy22[,c(1,grep("kharif_pre_cultivated|rabi_pre_cultivated|KHA22_pre_cultivated",names(a_rmtl_srvy22))  )]
pc_01 <- as.data.frame(lapply(PC, as.character), stringsAsFactors = FALSE)
pc_02 <- pc_01%>% pivot_longer(cols = -hh_id,names_to = c("observation"))

pc_02$observation <- sub("^kharif_pre_cultivated_(\\d{1,2})","kharifCult_\\1", pc_02$observation) 
pc_02$observation <- sub("^rabi_pre_cultivated_(\\d{1,2})","rabiCult_\\1", pc_02$observation) 
pc_02$observation <- sub("^KHA22_pre_cultivated_(\\d{1,2})","KHA22Cult_\\1", pc_02$observation) 

pc_03<- pc_02 %>% separate(observation, into = c("vars", "plotID"), sep = "_")
pc_03$plotID <- sprintf("%02d", as.numeric(pc_03$plotID))
pc_03$plotID <- sub("^(\\d{1,2})","plot_\\1",  pc_03$plotID) 

PCN_01=  # new_cultivated
  a_rmtl_srvy22[,c(1,grep("l31_hissa|l31_survey|l31_name",names(a_rmtl_srvy22)),# rm l30= count new plot
                   grep("l35",names(a_rmtl_srvy22)),
                   grep("kharif_new_cultivated|rabi_new_cultivated|KHA22_new_cultivated",names(a_rmtl_srvy22)) )]

######################    essantials    ----

attr(a_rmtl_srvy22$l7_rank_3, "labels")

flat_vector <- unlist(L48[,-1], use.names = FALSE)
table(flat_vector, useNA = "always") 

# remove columns only NA or empty
select(where(~!all(.x=="")))
select(where(~!all(is.na(.x))))

# write.csv
write.csv(plots_crop_2022, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/plots_crop_2022.csv", row.names=FALSE)
write.csv(rmtl_InOut_groups, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_InOut_groups.csv", row.names=FALSE)
write.csv(rmtl_baseline2016, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_baseline2016.csv", row.names=FALSE)
