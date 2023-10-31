library(dplyr)
library(haven)

# a_plots DB ----
a_plots=
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

plot_size_vers <- a_rmtl_srvy22[,c(grep("acre|guntas",names(a_rmtl_srvy22)) )]

# Variables to check what they represent: ----
# what the diff "plot_size_acre_1" AND "plot_acre_1"
# 1  l46_y_18_acre_7
# 2  plot_acre_guntas_for_round 
# 3  plot_acre_guntas_div 
# 4  plot_acre_guntas #   plot_acre_sum

A=a_rmtl_srvy22 %>%
  select(plot_size_acre_1,plot_size_acre_2,plot_size_acre_3,
         plot_acre_sum,plot_acre_guntas_for_round,plot_acre_guntas_div,
         plot_acre_guntas,
         plot_acre_1,plot_acre_2,plot_acre_3,id)

# aggregate the data ----
# test ----
B1__plot <- a_plots [,c(1,grep("plot_[1-3]|plot_size_acre_[1-3]|l_plot_status_[1-3]|^plot_village_[1-3]",names(a_plots)) )]
B1__plot <- as.data.frame(lapply(B1__plot, as.character), stringsAsFactors = FALSE)
B1__plot=B1__plot[1:10,]

library(tidyr)
df1 <- B1__plot%>% pivot_longer(cols = -id,names_to = c("observation"))

# -----

#prev plots ----
plotPrev_01=
  a_plots[,c(1,grep("^plot_[1-9]|plot_survey|l_plot_status_|plot_size|^plot_village_[1-9]",names(a_plots)),
             grep("kharif_pre_cultivated|rabi_pre_cultivated|KHA22_pre_cultivated",names(a_plots))  )]

library(tidyr)
plotPrev_01 <- as.data.frame(lapply(plotPrev_01, as.character), stringsAsFactors = FALSE)
plotPrev_02 <- plotPrev_01%>% pivot_longer(cols = -id,names_to = c("observation"))

names(plotPrev_01)
plotPrev_02$observation <- sub("^plot_(\\d{1,2})","plotSrvyHissa_\\1", plotPrev_02$observation) 
plotPrev_02$observation <- sub("^l_plot_status_(\\d{1,2})","plotStatus_\\1", plotPrev_02$observation) 
plotPrev_02$observation <- sub("^plot_village_(\\d{1,2})","plotVillage_\\1", plotPrev_02$observation) 
plotPrev_02$observation <- sub("^plot_survey_(\\d{1,2})","plotSrvy_\\1", plotPrev_02$observation) 
plotPrev_02$observation <- sub("^plot_size_acre_(\\d{1,2})","acre_\\1", plotPrev_02$observation) 
plotPrev_02$observation <- sub("^plot_size_guntas_(\\d{1,2})","guntas_\\1", plotPrev_02$observation) 
plotPrev_02$observation <- sub("^kharif_pre_cultivated_(\\d{1,2})","kharifCult_\\1", plotPrev_02$observation) 
plotPrev_02$observation <- sub("^rabi_pre_cultivated_(\\d{1,2})","rabiCult_\\1", plotPrev_02$observation) 
plotPrev_02$observation <- sub("^KHA22_pre_cultivated_(\\d{1,2})","KHA22Cult_\\1", plotPrev_02$observation) 

library(tidyr)
plotPrev_03 <- plotPrev_02 %>% separate(observation, into = c("vars", "plotID"), sep = "_")
plotPrev_03 <-plotPrev_03 %>% pivot_wider(names_from = vars , values_from = value)

plotPrev_03$plotID <- sprintf("%02d", as.numeric(plotPrev_03$plotID))
plotPrev_03$plotID <- sub("^(\\d{1,2})","plot_\\1",  plotPrev_03$plotID) 

#  column order
col_order_plot <- c("id","plotID","plotSrvy","plotSrvyHissa","acre","guntas",
                   "plotStatus","plotVillage","kharifCult","rabiCult","KHA22Cult")
plotPrev_04 <- plotPrev_03[, col_order_plot]

plotPrev_04[is.na(plotPrev_04)] <- ""

plotPrev_04 <- 
  plotPrev_04[!apply(plotPrev_04[, !colnames(plotPrev_04) %in% c("id", "plotID")] == "", 1, all), ]




#new plots ----
plotNew_01= 
  a_plots[,c(1,grep("l31_hissa|l31_survey|l31_name",names(a_plots)),# rm l30= count new plot
             grep("l35",names(a_plots)),
             grep("kharif_new_cultivated|rabi_new_cultivated|KHA22_new_cultivated",names(a_plots)) )]

plotNew_01 <- as.data.frame(lapply(plotNew_01, as.character), stringsAsFactors = FALSE)
plotNew_02 <- plotNew_01%>% pivot_longer(cols = -id,names_to = c("observation"))

names(plotPrev_01)
names(plotPrev_04)

plotNew_02$observation <- sub("^l31_hissa_(\\d{1,2})","plotSrvyHissa_\\1", plotNew_02$observation) 
plotNew_02$observation <- sub("^l31_survey_(\\d{1,2})","plotSrvy_\\1", plotNew_02$observation) 
plotNew_02$observation <- sub("^l31_name_(\\d{1,2})","plotVillage_\\1", plotNew_02$observation) 
plotNew_02$observation <- sub("^l35_acre_(\\d{1,2})","acre_\\1", plotNew_02$observation) 
plotNew_02$observation <- sub("^l35_guntha_(\\d{1,2})","guntas_\\1", plotNew_02$observation) 
plotNew_02$observation <- sub("^kharif_new_cultivated_(\\d{1,2})","kharifCult_\\1", plotNew_02$observation) 
plotNew_02$observation <- sub("^rabi_new_cultivated_(\\d{1,2})","rabiCult_\\1", plotNew_02$observation) 
plotNew_02$observation <- sub("^KHA22_new_cultivated_(\\d{1,2})","KHA22Cult_\\1", plotNew_02$observation) 

names(plotNew_03)

plotNew_03 <- plotNew_02 %>% separate(observation, into = c("vars", "plotID"), sep = "_")
plotNew_03 <-plotNew_03 %>% pivot_wider(names_from = vars , values_from = value)

plotNew_035 <-plotNew_03
plotNew_035$plotID <- as.numeric(plotNew_035$plotID)

plotNew_035 <- plotNew_035 %>%
  mutate(plotID=as.numeric(plotID)) %>% 
  mutate(plotID=plotID+10,
         plotID=as.character(plotID))

names(plotNew_035)
names(plotPrev_04)

plotNew_035$plotID <- sub("^(\\d{1,2})","plot_\\1",  plotNew_035$plotID) 
plotNew_035[is.na(plotNew_035)] <- ""

plotNew_04 <- 
  plotNew_035[!apply(plotNew_035[, !colnames(plotNew_035) %in% c("id", "plotID")] == "", 1, all), ]

plotNew_04$plotStatus <- "new"
plotNew_04 <- plotNew_04[, col_order_plot]

#101382  100814

# plotPrev_04 plotNew_04 ----

plotPrevNew_04 <- 
  rbind(plotPrev_04,plotNew_04)

plotPrevNew_04$acre <- as.numeric(plotPrevNew_04$acre)
plotPrevNew_04$guntas <- as.numeric(plotPrevNew_04$guntas)


# total_plots ----

plotsN <- a_rmtl_srvy22[, c(1 ,grep("l_plot_status_", names(a_rmtl_srvy22)))]
plotsN[plotsN==1 ] <- NA #remove 1 [Sold/disposed]
plotsN[plotsN==6 ] <- NA #remove 6 [Refuse / Plot Not Exisit]
plotsN$total_prev_plots <- rowSums(!is.na(plotsN[, -1]))

L30 <- a_rmtl_srvy22 %>% select(id,l30)

total_plots1=
  plotsN[,c(1,12)] %>%
  left_join(L30) %>%
  rename(new_plot=l30) %>% 
  mutate(total_hh_plots=total_prev_plots +new_plot)

total_plots2=
  total_plots1 %>% 
  rename(id_srvy=id,new_plot=l30) %>% 
  mutate(id_srvy=as.character(id_srvy)) %>%
  left_join(a_sample)
rm(L30,plotsN)

total_plots2 %>% 
  mutate(mm5=ifelse(is.na(mm5),"2.no system install",
                    ifelse(mm5==0,"1.2 didnt use","1.1 use water"))) %>% 
  group_by(mm5) %>% summarise(total_plots=mean(total_hh_plots,na.rm = TRUE) )


total_plots_normalized=
  total_plots[,c(1,4,6)] %>% filter(!is.na(total_hh_plots)) %>% 
  group_by(mm5,total_hh_plots) %>%
  count(total_hh_plots) %>%
  group_by(total_hh_plots) %>%
  mutate(Percent = n / sum(n) * 100)

total_plots_normalized %>% 
  mutate(mm5=ifelse(is.na(mm5),"2.no system install",
                    ifelse(mm5==0,"1.2 didnt use","1.1 use water"))) %>% 
    filter(total_hh_plots<9) %>% 
  ggplot(aes(x = factor(total_hh_plots), y = Percent, fill = factor(mm5))) +
  geom_bar(stat = "identity") +
  labs(title = "Total HH plots", x = "total_hh_plots", y = " ") +
  scale_fill_discrete(name = "mm5") +
  scale_fill_manual(values=c("skyblue", "#E69F00","gray"))+  
    theme_ipsum()+
    scale_y_continuous(labels = scales::percent_format(scale = 1))

# plot_size ----

plot_size=a_plots[,c(1,grep("^plot_[1-9]|plot_survey|l_plot_status|plot_size",names(a_plots)),
                     grep("l31_hissa|l31_survey",names(a_plots))#new plots
                     )]

# P1prev,  plot#1 prev
P1size =plot_size %>% select(id, ends_with("1"))

P1crop =a_rmtl_srvy22 %>% select(id,l39_prev_kha_1,l39_prev_rab_1,l39_prev_KHA22_1)

P1reve =  a_rmtl_srvy22[,c(1,grep(
  "l78_reven_prev_kha_1_|l78_reven_prev_KHA22_1_|l78_reven_prev_rab_1_",names(a_rmtl_srvy22))
  )]

library(purrr)
dfs_list <- list(P1size, P1crop, P1reve, total_plots1)
P1prev <- reduce(dfs_list, full_join, by = "id")

P1_reven_acre01 <- P1prev %>%
  
  mutate(plot_size_acre_1= ifelse( plot_size_acre_1==0,1,plot_size_acre_1)) %>% 

  mutate(plot_size_acre_1=as.numeric(plot_size_acre_1)) %>% 
  mutate(
    l78_reven_prev_kha_1 = rowSums(select(., starts_with("l78_reven_prev_kha_1")), na.rm = TRUE),
    l78_reven_prev_rab_1 = rowSums(select(., starts_with("l78_reven_prev_rab_1_")), na.rm = TRUE),
    l78_reven_prev_KHA22_1 = rowSums(select(., starts_with("l78_reven_prev_KHA22_1_")), na.rm = TRUE)
         ) %>% 
  mutate(
    reven_acre_kha21_rab21=
      (l78_reven_prev_kha_1+l78_reven_prev_rab_1)/plot_size_acre_1,
    reven_acre_kha21_rab21_KHA22=
      (l78_reven_prev_kha_1+l78_reven_prev_rab_1+l78_reven_prev_KHA22_1)/plot_size_acre_1
  ) 

P1_reven_acre02 <- 
  P1_reven_acre01 %>% filter(reven_acre_kha21_rab21>100 ) %>% 
  select(id,plot_size_acre_1,reven_acre_kha21_rab21) %>% 
  mutate(id_srvy=as.character(id)) %>% 
  left_join(a_sample[,1:3] ) %>% 
  mutate(water_usage= ifelse(mm5==1,"UW",ifelse(mm5==0,"NUW",mm5)),
         FG= ifelse(is.na(mm5),"NIS", ifelse(mm5==1,"UW","NUW")),
         Irri_systm= ifelse(mm4==1,"IS","NIS"))


TP01 <-
  P1_reven_acre02  %>% group_by(water_usage) %>%get_summary_stats(reven_acre_kha21_rab21, type = "mean_sd")
P1_reven_acre02  %>% group_by(Irri_systm) %>%get_summary_stats(reven_acre_kha21_rab21, type = "mean_sd")

library(rstatix)
TP02 <- 
  P1_reven_acre02 %>% 
  t_test(reven_acre_kha21_rab21 ~ water_usage, detailed = F) %>%
  add_significance()

library(rempsyc)
nice_table(TP01)
nice_table(TP02[c(1,6:8)])


  







# L74 Labor ----
# How much paid labor and family labor, in percentages, per season?
L74 <- a_rmtl_srvy22 %>% select(id,starts_with("L74"))

# L76 Labor days ----
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

# L78	season-crop	Total revenue? ----

zed=
a_rmtl_srvy22 %>% select(id, starts_with("l78_reven_prev_kha"))
  
plot_crop_season=
  a_rmtl_srvy22 %>% select(id, starts_with("l39_prev_K"),starts_with("l39_prev_rab"))

plot_acre=
  a_plots %>% select(starts_with("plot_size_acre"))



# L39a	Crop-Plot-Season 21-22 ----
# L39a	Crop-Plot-Season	kharif 2022 # Rabi 2021/2022 # Kharif 2021
# Perennial/biseasonal crops will be listed in Kharif

L39_prev <- a_rmtl_srvy22 [,c(1,grep("l39_prev_",names(a_rmtl_srvy22)))] 
L39_new  <- a_rmtl_srvy22 [,c(1,grep("l39_new",names(a_rmtl_srvy22)))]
L39_a <- full_join(L39_prev,L39_new)

#L39_a %>% select(id,l39_prev_kha_1,l39_prev_kha_2 ) %>%separate_rows(l39_prev_kha_1, sep = " ") %>% distinct()

# crop2021$l39_prev_1   :l39_prev_1_other_10 %>% 
#   summarise_at(vars(l39b_y_18_10:l39b_y_18__888), sum, na.rm = TRUE)
  
# l39_prev_kha=L39a [,c(1,grep("^l39_prev_kha_[1-9]",names(L39a)))]

# l399_9=L39a [,c(1,grep("l39_prev_kha_1|l39_prev_kha_2",names(L39a)))]
# l399_9=l399_9[c(1:5,30:35),1:3]

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

# list crops 2021 L39_2021 -----

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

# full_join lists crops_freq_2018_2021 ----

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

treemap(
  crops_freq_2018_2021,
  index=c("crop_family", "crop_name"),
  vSize="n_HH_2021",
  vColor="n_HH_2021",
  type="index",
#  type="value",
  title = "",
)




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




                   

# Treemap ----

https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/proportions.html
https://yjunechoe.github.io/posts/2020-06-30-treemap-with-ggplot/
  
library(highcharter)
library(treemap)

https://zhiyang.netlify.app/post/tree


dat <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")
dat[,c("revenue_category", "franchise","revenue") ]
dat2 <- dat

dat2 %>% group_by(revenue_category) %>% summarise(n())

treemap(
  dat2,
  index=c("revenue_category", "franchise"),
  vSize="revenue",
  vColor="revenue",
  type="value",
  title = "",
  title.legend = "",
  position.legend	 = "none",
)

treemap(
  dat2,
  index=c("revenue_category", "franchise"),
  vSize="revenue",
  vColor="revenue",
  type="index",
)







