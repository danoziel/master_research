library(tidyverse)
library(dplyr)
#  CULTIVATION_short 
A <- rbind(2,3,4) %>%   select(where(~!all(is.na(.x))))


# IRRIGATION   ----		
# L7		What irrigation source are you dependent on? (Rank according to the degree of importance)

ans <- data.frame(
  Grp = c ("control","control","control","control","control","control",
           "treatment","treatment","treatment","treatment","treatment","treatment"),
  op = c (2,3,4,5,-888,NA,
          2,3,4,5,-888,NA), 
  source = c("Tank","Open well","Borewell","Government water supply","Other (Rain)","non",
             "Tank","Open well","Borewell","Government water supply","Other (Rain)","non"))

dtl7_rank_1 <- Adt3 %>%select(id,Grp,starts_with("l7_"))

l7_rank_1 <- Adt %>%select(id,Grp,starts_with("l7_")) %>% bind_rows(dtl7_rank_1)%>%
  count(Grp,l7_rank_1) %>% 
  rename(op=l7_rank_1,n_rank_1=n) %>% 
  full_join(ans) %>% 
  arrange(desc(Grp))

l7_rank <- Adt %>%select(id,Grp,starts_with("l7_")) %>% bind_rows(dtl7_rank_1)%>%
  count(Grp,l7_rank_2) %>%
  rename(op=l7_rank_2,n_rank_2=n) %>% 
  full_join(l7_rank_1) %>% 
  arrange(desc(Grp) ) %>% 
  group_by(Grp) %>%
  mutate(sumA=sum(n_rank_1,na.rm = T)) %>% 
  mutate(rank_1_freq_in_group = paste0(round(100 * n_rank_1/sumA, 0), "%"),
         rank_2_freq_in_group = paste0(round(100 * n_rank_2/sumA, 0), "%")) %>% 
  mutate(rank_1_freq_in_group=ifelse(n_rank_1=="NA%",NA,rank_1_freq_in_group),
         rank_2_freq_in_group=ifelse(n_rank_2=="NA%",NA,rank_2_freq_in_group)) %>% 
  select(Grp,source,n_rank_1,rank_1_freq_in_group,n_rank_2,rank_2_freq_in_group)

# CROP         ----

# L39 	What crop are planted on this plot? 
#       Perennial/biseasonal crops will be listed in Kharif

# L39a [Crop-Plot-Season] 
CR3 <- Adt3 %>% filter(id %in% c(1000709 ,39305302 ,39305501)) %>% 
  select(id,list2016, survy2022,starts_with("l39_") ) %>% 

         
CR3 <- Adt3 %>% 
  select(id,list2016, survy2022,mm5,# l39_ _ _1 - l39_ _ _9 l39_new_kha_1
         l39_prev_kha_1,l39_prev_rab_1,l39_prev_sum_1,
         l39_prev_kha_2,l39_prev_rab_2,l39_prev_sum_2,
         l39_prev_kha_3,l39_prev_rab_3,l39_prev_sum_3,
         l39_prev_kha_4,l39_prev_rab_4,l39_prev_sum_4,
         l39_prev_kha_5,l39_prev_rab_5,l39_prev_sum_5,
         l39_prev_kha_6,l39_prev_rab_6,l39_prev_sum_6,
         l39_prev_kha_7,l39_prev_rab_7,l39_prev_sum_7,
         l39_prev_kha_8,l39_prev_rab_8,l39_prev_sum_8,
         l39_prev_kha_9,l39_prev_rab_9,l39_prev_sum_9,
         
# L39b	What crops were planted in [ Crop-Year ]?
         l39b_y_20,l39b_y_19,l39b_y_18)

V <- CR3 %>%
  filter(mm5==0,l39_prev_kha_1=="",l39_prev_rab_1=="",l39_prev_sum_1=="",
         l39_prev_kha_2=="",l39_prev_rab_2=="",l39_prev_sum_2=="")

# L48	How often was the crop irrigated manually?

# L48a		What is the method of irrigation?
#          L39a Crop-Plot-Season
#          L39b Crop

IM3 <- Adt3 %>%  #l48_prev/new_season*3_plot*9_crop*3
  select(id,Im_in_out,in_out,mm5,
         l48_prev_kha_1_1,l48_prev_kha_1_2,l48_prev_kha_1_3,
         l48_prev_kha_2_1,l48_prev_kha_2_2,l48_prev_kha_2_3,
         # l48_y_year18/19/20_crop*7/6/6
         l48_y_18_1,l48_y_18_2,
         l48_y_19_1,l48_y_19_2,
         l48_y_20_1,l48_y_20_2)

# L39 What crop # L48	How often # L48a	What method #
V <- Adt3 %>%  
  select(id,Im_in_out,in_out,mm5,
         l39_prev_kha_1,# L39 What crop
         l48_prev_kha_1_1,l48_prev_kha_1_2,  # L48	How often
         l48a_prev_kha_1_1,l48a_prev_kha_1_2,# L48a	What method
         
         l39b_y_18,l39b_y_19,l39b_y_20,# L39 What crop
         l48_y_18_1,# L48	How often
         l48_y_19_1,# L48	How often
         l48_y_20_1,# L48	How often
         l48a_y_18_1,# L48a	What method
         l48a_y_19_1,# L48a	What method
         l48a_y_20_1)# L48a	What method
         
         
  
# L41		Have you used this cultivation method?  [Crop level]

L41_3 <- Adt3 %>%
  select(id,Im_in_out,in_out,mm5,
         l41_1_1, # Mulching
         l41_2_1, # Trellising הדליה
         l41_3_1, # Furrow תלם.תעלות
         l41_4_1, # Line sowing
         l41_5_1, # Fertigation
         
         l41_1_8) # crop 8


# ??? L41a Which are the most common cultivation method you used 3 years before 2021/2022? (2018/2019)
#    single question

Adt3 %>%
  select(starts_with("l41")) # 
  # Mulching
  # Trellesing
  # Furrow
  # Line sowing
  # Fertigation  
  
# YIELD        ----
#   
# L49		What was the total yield
#   Crop-Plot-Season	for Qs: L39a
L49_3$mm5 <- haven::as_factor(L49_3$mm5 )

L49_3 <- Adt3 %>%
  select(id,list2016, survy2022,mm5,starts_with("l49_")) 

V <- L49_3 %>%
  filter(l49_prev_kha_1_1==0 | is.na(l49_prev_kha_1_1),
         is.na(l49_prev_kha_1_2),
         is.na(l49_prev_kha_2_1),
         is.na(l49_prev_kha_3_1),
         
         is.na(l49_prev_rab_2_1),
         is.na(l49_prev_kha_4_1),
         is.na(l49_prev_rab_3_1),
         
         is.na(l49_prev_rab_1_1),
         is.na(l49_new_sum_1_1),
         
         is.na(l49_prev_sum_1_1))



#   L49a Was the total yeild greater or lesser than expected

L49a_3 <- Adt3$mw9_rabi_2017 %>% select(id,list2016, survy2022,mm5,starts_with("l49a_")) 
L49a_4 <- Adt4 %>% select(id,list2016, survy2022,mm5,starts_with("l49a_")) 

#   L50 If the total yield was lesser than you expected, what was the reason for crop loss? (multiple choice)
#        l50_prev_kha_plot_crop # l50_prev_kha_option_plot_crop

L50_3 <- Adt3 %>% select(id,list2016, survy2022,starts_with("l50")) 
L50_4 <- Adt4 %>% select(id,list2016, survy2022,starts_with("l50"))
L50_4[L50_4==""] <- NA
L50_4[L50_4==0] <- NA
L50_4 <- na.omit (L50_4)


#   L51	If the total yield was greater than you expected, what was the reason for crop surplus? (multiple choice - 2 max)

L <- Adt3 %>% select(id,list2016, survy2022,starts_with("l51")) 


# How much of the yield was ________ (Answer in %)  [Season-Crop]	----
# L52		Sold
# L53		Kept for HH consumption
# L54		Lost in post-harvest

L3 <- Adt3 %>% select(id,list2016, survy2022,starts_with("l53"))
L[L==""] <- NA
L[L==0] <- NA
L <- na.omit (L)
library(tidyr)

# L52		Sold
L2 <- Adt3 %>% select(id,list2016, survy2022,starts_with("l52")) %>%
  pivot_longer(
    cols = `l52_prev_kha_1_1`:`l52_stored_new_sum_8_4`, 
    names_to = "Sold",
    values_to = "SoldPCT") %>% 
  filter(SoldPCT>0)

# L53		Kept for HH consumption
L3   <- Adt3 %>% select(id,list2016, survy2022,starts_with("l53")) %>%
  pivot_longer(
    cols = `l53_prev_kha_1_1`:`l53_new_sum_8_4`, 
    names_to = "consumption",
    values_to = "consPCT") %>% 
  filter(consPCT>0)

# L54		Lost in post-harvest
L4 <- Adt3 %>% select(id,list2016, survy2022,starts_with("l54")) %>% 
  pivot_longer(
    cols = `l54_prev_kha_1_1`:`l54_new_sum_8_4`, 
    names_to = "Lost",
    values_to = "LostPCT") %>% 
  filter(LostPCT>0)

#----
# Since 2016, have you adopted any of the above crops? # If YES →  L46
# 
# L81		Sunflower
L <- jan %>% select(id,starts_with("l81"))

# L82		Tomato
# L83		Onion
# L84		Flower Crops
# L85		chili
# L86		Leafy vegetables
# L87		Tur /Red gram /Pegion Pea  


# Since 2016, have you stopped growing one of the above crops?  
#   If Continued to Grow→  L46  
#
# L89		Sorghum (jowar)
L <- Adt3 %>% select(id,list2016, survy2022,starts_with("L89"))
  
# L90		Pearl millet (bajra)
# L91		Groundnut
# L92		Bengal gram /Chick Pea
# L93		Greengram
# L94		Horsegram
# L95		Black gram
# L96		Cotton



# L47		What factors influenced your decision to cultivate this crop? (Mark all that apply)

# INPUTS       ----
# Season Level	costs of  [ ____ ] 
# L70		irrigation equipment
L1 <- Adt3 %>% select(id,list2016, survy2022,starts_with("L70"),
                      starts_with("L71"),starts_with("L72"),starts_with("L73"))
L2 <- Adt4 %>% select(id,list2016, survy2022,starts_with("L70"),
                      starts_with("L71"),starts_with("L72"),starts_with("L73"))
L3 <- Adt1101 %>% select(id,list2016, survy2022,starts_with("L70"),
                         starts_with("L71"),starts_with("L72"),starts_with("L73"))
L4 <- Adt1401 %>% select(id,list2016, survy2022,starts_with("L70"),
                         starts_with("L71"),starts_with("L72"),starts_with("L73"))
L5 <- Adt1801 %>% select(id,list2016, survy2022,starts_with("L70"),
                         starts_with("L71"),starts_with("L72"),starts_with("L73"))

ADT <- rbind(L1,L2,L3,L4,L5)

ADT %>%  pivot_longer( # בדיקת סטאטוס ערכים חסרים
  cols = c(`l70_kha`: `l70_sum`), 
  names_to = "seasom", 
  values_to = "irri_equ"
)%>%
  filter(!is.na(irri_equ)) %>%
  select(1) %>% distinct() #כל החקלאים דיווחו לפחות ערך אחד 

ADT %>%  pivot_longer( # האם הערכים סבירים
  cols = c(`l70_kha`: `l70_sum`), 
  names_to = "seasom", 
  values_to = "irri_equ"
)%>%
  filter(!is.na(irri_equ),irri_equ>0) %>% 
  group_by(seasom,list2016) %>% 
  summarise(mean(irri_equ),median(irri_equ),min(irri_equ),max(irri_equ),n())


# L71		Mechanization 

ADT %>%  # check NAs status
  pivot_longer( 
  cols = c(`l71_kha`,`l71_rab`, `l71_sum`), 
  names_to = "seasom", 
  values_to = "irri_equ"
)%>% 
  # filter(!is.na(irri_equ)) %>%
  # select(1) %>% distinct() #כל החקלאים דיווחו לפחות ערך אחד 

  # האם הערכים סבירים?
  filter(!is.na(irri_equ),irri_equ>0) %>% 
  group_by(seasom,list2016) %>% 
  summarise(mean(irri_equ),median(irri_equ),min(irri_equ),max(irri_equ),n())

# L72		Fuel
ADT %>%  # check NAs status
  pivot_longer( 
    cols = c(`l72_kha`,`l72_rab`, `l72_sum`), 
    names_to = "seasom", 
    values_to = "irri_equ"
  )%>% 
   filter(!is.na(irri_equ)) %>%
   select(1) %>% distinct() #כל החקלאים דיווחו לפחות ערך אחד 
  
  # האם הערכים סבירים?
  filter(!is.na(irri_equ),irri_equ>0) %>% 
  group_by(seasom,list2016) %>% 
  summarise(mean(irri_equ),median(irri_equ),min(irri_equ),max(irri_equ),n())



# L73		Labor <- 
  ADT %>%  # check NAs status
    pivot_longer( 
      cols = c(`l73_kha`,`l73_rab`, `l73_sum`), 
      names_to = "seasom", 
      values_to = "irri_equ"
    )%>% 
    filter(!is.na(irri_equ)) %>%
    select(1) %>% distinct() #כל החקלאים דיווחו לפחות ערך אחד 
  
  # האם הערכים סבירים?
  filter(!is.na(irri_equ),irri_equ>0) %>% 
    group_by(seasom,list2016) %>% 
    summarise(mean(irri_equ),median(irri_equ),min(irri_equ),max(irri_equ),n())


# LABOR       ----
#   L74 How much do you use paid labor and how much is family labor, per season? Answer in percentages [Season-Plot]
L1 <- Adt3 %>% select(id,list2016, survy2022,starts_with("L74"),starts_with("L73"))

ADT <- rbind(L1,L5)
  

#   L76	How many days did they put in a season, on average? [Season-Plot]
L1 <- Adt3 %>% 
  select(id,list2016,survy2022,
         starts_with("L74"),starts_with("L76")) %>% 
  select(where(~!all(is.na(.x))))

# CROP SELLING   L56 L56a ----

#   L56  Who did you sell crop to?  Mark all that apply [Season Level]
#   L56a  Have you ever received assistance to sell your crops? if yes, from whom? [single question]
L1 <- Adt3 %>% 
  select(id,list2016,survy2022,starts_with("L56")) %>% 
  select(-c(5:10,12,16:21,27:32,34 )) %>% 
  filter(l56_kha == "", l56_rab == "")

# SEEDS         L57 L58   ----
#   L57		Name the seed [season-crop]
#   L58		Is it normal or improved seeds?

L1 <- Adt3 %>% 
  select(id,list2016,survy2022,starts_with("L57"),starts_with("L58")) %>% 
  filter(is.na(l58_kha_1), is.na(l58_rab_1) )

# REVENUE     L78  ----
#   L78 Total revenue? [season-crop]

L1 <- Adt3 %>% 
  select(id,list2016,survy2022,starts_with("L78")) %>% 
  select(where(~!all(is.na(.x)))) %>% 
  filter(is.na(l78_reven_prev_kha_1_1), is.na(l78_reven_prev_kha_2_1),
         is.na(l78_reven_prev_sum_1_1), is.na(l78_reven_prev_sum_1_2),
         is.na(l78_reven_prev_rab_1_1), is.na(l78_reven_prev_rab_1_2) )

# PLOTS        ----
#   What the status of the plot?
L1 <- Adt3 %>% select(id,list2016, survy2022,contains("l_plot_status_") ) %>% 
  mutate(l_plot_status_10=NA)
L2 <- Adt4 %>% select(id,list2016, survy2022,contains("l_plot_status_") )%>% 
  mutate(l_plot_status_9=NA,l_plot_status_10=NA)
L3 <- Adt1101 %>% select(id,list2016, survy2022,contains("l_plot_status_") )
L4 <- Adt1401 %>% select(id,list2016, survy2022,contains("l_plot_status_") )
L5 <- Adt1801 %>% select(id,list2016, survy2022,contains("l_plot_status_") )

ADT <- rbind(L1,L2,L3,L4,L5)
ADT %>% 
  pivot_longer(
    cols = `l_plot_status_1`:`l_plot_status_10`, 
    names_to = "plot_num",
    values_to = "plot_status") %>% 
  filter(!is.na(plot_status) ) %>% 
  count(id) %>% 
  summarise(mean(n))

#   L13		Why does your household no longer own the plot?
L1 <- Adt3 %>% select(id,list2016, survy2022,starts_with("l13") ) 

#   L20		Current Operation Status (multiple choice)
L1 <- Adt3 %>% select(id,list2016, survy2022,starts_with("l20") ) 

#   L25		When did you start cultivating this plot? 
L1 <- Adt3 %>% select(id,list2016, survy2022,starts_with("l25") ) %>% 
  select(where(~!all(is.na(.x))))

#   L28		Why was it left fallow?
L1 <- Adt3 %>% select(id,list2016, survy2022,starts_with("l28") ) %>% 
  select(where(~!all(is.na(.x))))

#   L29		Have you gained/received new lands since 2018?
L1 <- Adt3 %>% select(id,list2016, survy2022,starts_with("l29") ) %>% 
  select(where(~!all(is.na(.x))))

#   L30		How many NEW plots?
L1 <- Adt3 %>% select(id,list2016, survy2022,starts_with("l30") ) %>% 
  select(where(~!all(is.na(.x))))
#   L31		[ Village ] [ Survey nu ] [ Hissa number ] [ acre ]  
L1 <- Adt3 %>% select(id,list2016, survy2022,starts_with("l31") ) %>% 
  select(where(~!all(is.na(.x))))
#   L32		 How did you come to own this land? select one 
L1 <- Adt3 %>% select(id,list2016, survy2022,starts_with("l32") ) %>% 
  select(where(~!all(is.na(.x))))
