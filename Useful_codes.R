write.csv(days_use_hh, file = "C:/Users/Dan/Documents/R/Rautahat.Bara.Sarlahi/data/Agriculture_18_19.csv", row.names=FALSE)
write.csv(wem_liter_fuel_and_time_to_irrigate_1_ha, file = "C:/Users/Dan/Documents/master_research/DATAs/data_saptari/wem_liter_fuel_and_time_to_irrigate_1_ha.csv", row.names=FALSE)
write.csv(banihatti_tidy, file = "C:/Users/Dan/Documents/banihatti_tidy.csv", row.names=FALSE)
write.csv(H1557_2018_m6_m19, file = "C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/bH1557_2018_m6_m19.csv", row.names=FALSE)

library(foreign)
write.dta(spip_nepal_time_energy, "spip_nepal_time_energy.dta") 

# Save the data in different vector ----
women_weight <- genderweight %>%
  filter(group == "F") %>%
  pull(weight)

# t test ----
res <- 
  t.test(total_litres_consumed_dieselkero ~ TreatmentControl, data = df.match)

#regression ----
ggplot(income.data, aes(x=income, y=happiness))+ geom_point()

income.happiness.lm <- lm(happiness ~ income, data = income.data)

summary(income.happiness.lm)

#remove the "-"
ifmr_hissa_16_18A$hissa_srvy_no <- gsub("-$","",ifmr_hissa_16_18A$hissa_srvy_no )


#  NA----
#in full dataset - dplyr
rowwise() %>% 

drop_na(irri_for_season,season_of_crop)%>% library(tidyverse)

#replace NA to 0
x$land_cult [is.na(x$land_cult)] <- 0 

#replace NaN to Na, x=column
mutate(x= sub(NaN, NA, x)) %>%
  
#renam column-----
R_intensity_Baseline %>% 
  rename(new = old) #renam column old TO new

names(crop0) <- c('ID', 'svy', 'hisa', 'crop1','crop2')


# copy column----
R.Agriculture_Baseline_2018_ <- R.Agriculture_Baseline_2018_ %>%
  mutate( name_of_crop_detail = name_of_crop ) #(copy=original)
#subset----
Treats_Lands <- subset(R.Lands_Endline_EPC_2019_,  TC == 1) 
#Controlling number of decimal digits----
mutate(across(is.numeric, round, 2)) 


# freq by % symbol ----
mutate(freq = paste0(round(100 * total_land_cultivated/nca, 0), "%"))

mutate(freq = paste0(round(100 * n/sum(n), 0), "%"))

mutate(label_percent()(x))
 
#replace 2 to 1 ----
#replace 2 to 1 
Data$TC[Data$TC == 2] <- 1 

R_Lands_I_Baseline_2018_[6,3]<- 40

# digits after point
coords$print <- as.numeric((coords$`2`),digits=8)

# column 1-nth
df$ID <- seq.int(nrow(df))
> df1$consecutive_numbers<-1:nrow(df1)
mutate (observation = 1:n())
add_column(Column_After = "After",.after = "A") 


#from rows to column ----
Q1 <- spread(Q1, oslosp, freq) 

#short to long
library(rstatix)
library(ggpubr)

mydata.long <- tse %>% 
  pivot_longer(-gender, names_to = "variables", values_to = "value")

#split column to two - ifelse----
at_btselem <- at_btselem %>% mutate(total_events_located_IL  = ifelse(location == 1, "1",NA)) %>%
  mutate(total_events_located_yesha  = ifelse(location == 2, "1",NA)) 
#location- old, total_events_located_IL-new

# convert first letter to uppercase----
library(stringr)
village_list$village <- str_to_title(village_list$village) 

#round ----
mutate_at(3,round,2) # (column ,round ,digits)

colMeans(land_Treats)

mutate(avm_self = rowMeans(.[names(.)[7:8]], na.rm = T),#mean per row - by defined columns
       
       #-----  frequency in percentage   ----    
       R_Aquaculture_Baseline_2018_ %>% filter(!is.na(practice_aquaculture)) %>%
         group_by(practice_aquaculture,TreatmentControl) %>%summarise(n=n()) %>% 
         mutate(freq = n / sum(n)) #Percentage per sub group
       
       R_Aquaculture_Baseline_2018_ %>% filter(!is.na(practice_aquaculture)) %>%
         count(practice_aquaculture,TreatmentControl) %>%mutate(freq = n / sum(n))#Percentage per group
       
       #   R_Aquaculture_Baseline_2018_ %>%filter(!is.na(practice_aquaculture)) %>%
       #     group_by(practice_aquaculture)%>%summarise(n=n()) %>%mutate(freq = n / sum(n))
       #                                 _||_
       #                                 \  /
       #                                  \/
       R_Aquaculture_Baseline_2018_ %>%filter(!is.na(practice_aquaculture)) %>%
         count(practice_aquaculture) %>%mutate(freq = n / sum(n))
       
# dates---------------------------------
library(data.table)
peace_index$date <-  as.Date(peace_index$date, "%Y-%m-%d")
  
date <- seq(as.IDate("1994-01-01"), as.IDate("2019-12-31"), 1))
dt <- data.table(date=date)


summarise_at(c("harvest_KG_talya100","harvest_KG_CONTROL"), sum, na.rm = TRUE)
       
peace_index_17_18 <- peace_index_17_18 %>%
  mutate(political_spectrum=case_when(
    party %in% c(2,3,4,5,7,9,10)~1,
    party %in% c(14:24,27:30)~4))

library(extrafont)
theme_minimal() +
  theme(text = element_text(family = "Georgia"))

# from: 1899-12-31 09:00:00 TO 9:00
water01$`Irrigation Start Time` <- format(water01$`Irrigation Start Time`, format="%H:%M")

# add_column in dplyr----
df %>% add_column(new_col = 0)

https://www.listendata.com/2017/03/if-else-in-r.html


#map----
library(leaflet)
library(rgdal)
library(dplyr)

p_lon <- 29.9018696
p_lat <- 76.0084641

leaflet() %>%
  setView(lng = p_lat, lat = p_lon, zoom = 13.5) %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addCircles(
    data = kharif_2020_rice,
    radius = sqrt(10^kharif_2020_rice$plot_acre) *10,
    color = "#008B00",
    fillColor = "#008B00",
    fillOpacity = 0.2,
    popup = paste0(
      "<kharif_2020_rice>Farmer Name: </kharif_2020_rice>", kharif_2020_rice$farmer_name, "<br>",
      "<kharif_2020_rice>Plot Size (acre): </kharif_2020_rice>", kharif_2020_rice$plot_acre, "<br>",
      "<kharif_2020_rice>Farmer ID: </kharif_2020_rice>", kharif_2020_rice$farmer_id, "<br>"
    ))


## tab_model for two models ----
tab_model(model11,model13,digits=3,p.style="stars", show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("Saptari", "Rautahat Bara Sarlahi"))

# Barplot with error bars----
# Standard deviation, Standard error or Confidence Interval ----
https://www.r-graph-gallery.com/4-barplot-with-error-bar.html


# ----nasapower--------------------------------------------

26.734950834599527, 85.9272131400479

library(nasapower)
ag_d <- get_power(
  community = "SSE",
  lonlat = c(85.92, 26.73),
  pars = c( "KT","CLRSKY_SFC_SW_DWN","ALLSKY_SFC_SW_DWN", "PRECTOT"),
  dates =c( "2017-06-02","2019-12-16"),
  temporal_average = "DAILY"
)


# Regular Expression Syntax: -------
http://www.endmemo.com/r/gsub.php

\\d  Digit, 0,1,2 ... 9
\\D  Not Digit
\\s  Space
\\S  Not Space
\\w  Word
\\W  Not Word
\\t  Tab
\\n  New line
 ^   Beginning of the string
 $   End of the string
 \   Escape special characters, e.g. \\ is "\", \+ is "+"

  |   Alternation match. e.g. /(e|d)n/ matches "en" and "dn"
  •   Any character, except \n or line terminator
[ab]  a or b
[^ab] Any character except a and b
[0-9] All Digit
[A-Z] All uppercase A to Z letters
[a-z] All lowercase a to z letters
[A-z] All Uppercase and lowercase a to z letters
  i+  i at least one time
  i*  i zero or more times
  i?  i zero or 1 time
i{n}  i occurs n times in sequence

i{n1,n2}   i occurs n1 - n2 times in sequence
i{n1,n2}?  non greedy match, see above example
i{n,}      i occures >= n times

[:alnum:] Alphanumeric characters: [:alpha:] and [:digit:]
[:alpha:] Alphabetic characters: [:lower:] and [:upper:]
[:blank:] Blank characters: e.g. space, tab
[:cntrl:] Control characters
[:digit:] Digits: 0 1 2 3 4 5 6 7 8 9
[:graph:] Graphical characters: [:alnum:] and [:punct:]
[:lower:] Lower-case letters in the current locale
[:print:] Printable characters: [:alnum:], [:punct:] and space
[:punct:] Punctuation character: ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
[:space:] Space characters: tab, newline, vertical tab, form feed, carriage return, space
[:upper:] Upper-case letters in the current locale
[:xdigit:] Hexadecimal digits: 0 1 2 3 4 5 6 7 8 9 A B C D E F a b c d e f


D24_[] <- lapply(D24_, function(x) if(is.numeric(x)) 
  as.character(x) else x)



