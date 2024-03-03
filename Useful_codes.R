# IMPORT csv dta files ----

write.csv(L7_source_irri, file ="C:/Users/Dan/Documents/master_research/DATAs/data_master/data_saptari/Master_HH_N.csv", row.names=FALSE)


write.csv(cp_prob, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/crop_pattern_16_18_22.csv", row.names=FALSE)

library(foreign)
write.dta(pump_type.csv, "C:/Users/Dan/Documents/master_research/DATAs/data_master/data_saptari/pump_type.dta")
library(haven)
write_dta(peace_index, "C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_df_16102023/peace_index.dta")

# read_dta
baseline_rmtl_2016 <- read_dta("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/baseline_survey_2016/CMF_RAMTHAL_IRRIGATION_18 Aug 2016 - cleaned.dta")


# IMPORT from google drive ----

install.packages("googlesheets4")
library(googlesheets4)
gs4_auth()

# Check if you have a valid token
gs4_has_token()
# [1] TRUE

# Get the Google Sheets file
sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1pNh0CnNdTBDoneDt0rG_apaLZNI4iQVuzIl9Cms2pgk/edit#gid=1024016564")

# Import data from the first sheet
data <- read_sheet(sheet, sheet = 1)

#### If Stata file (.dta) stored in Google Drive
library(haven)

# file link from the drive
# https://drive.google.com/file/d/10yPJXnxAxLprwaU7SNtYUzMDw0mSuz4A/view?usp=share_link

file_id <- "10yPJXnxAxLprwaU7SNtYUzMDw0mSuz4A"
file_path <- sprintf("https://drive.google.com/uc?export=download&id=%s", file_id)
baseline_rmtl_2016 <- read_dta(file_path)


library(rempsyc)
stats_num=d_1 %>% group_by(HH_project) %>% get_summary_stats(total_plots, type = "mean_sd")


# Save the data in different vector ----
women_weight <- genderweight %>%
  filter(group == "F") %>%
  pull(weight)

# arrange ----
arrange(desc(Grp))

# summary_stats ----
library(rstatix)
DF %>% group_by(HH_project) %>%
  get_summary_stats(income_2016, type = "mean_sd")# %>% mutate_at(4:5,round) # (column ,round ,digits)


# t_test t.test ----


# t_test
DF01 <- DF %>% t_test(income_2016 ~ HH_project, detailed = F) %>% add_significance()
DF02 <- vars_inc %>% t_test(income_2022 ~ farmers_hh, detailed = T) %>% add_significance()

library(rempsyc)
nice_table(DF02 )
nice_table(DF01[c(6:9)])

# t.test
library(broom)
t01 <- t.test(income_2022 ~ farmers_hh, data = vars_inc)
t01 <- tidy(t01, conf.int = TRUE)
nice_table(t01)


t.test.results <- nice_t_test(
  data = mtcars,
  response = names(mtcars)[1:6],
  group = "am",
  warning = FALSE
)
t.test.results




edu1=baseline_RMTL[,c(1,grep("^C5_[1-9]|^C7_[1-9]",names(baseline_RMTL)))] %>% select(-ends_with("bin"))%>% mutate(edu_head_hh=NA ) 




#regression ----

ggplot(data, aes(x=income, y=happiness))+ geom_point()

ggplot(DF, aes(x=total_acres, y=total_plots)) +
  geom_point(shape=18, color="green4")+
  geom_smooth(method=lm, se=FALSE, color="brown4")+ theme_minimal()

model <- lm(total_acres ~ total_plots, DF)
summary( model )

library(sjPlot)
tab_model(model, show.se = TRUE)

https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html

#remove the "-"
ifmr_hissa_16_18A$hissa_srvy_no <- gsub("-$","",ifmr_hissa_16_18A$hissa_srvy_no )


#  NA----
#in full dataset - dplyr
rowwise() %>% 

drop_na(irri_for_season,season_of_crop)%>% library(tidyverse)

# remove columns only NA or empty
select(where(~!all(.x=="")))
select(where(~!all(is.na(.x))))

#replace value to another value ----

#replace NA to 0
x$land_cult [is.na(x$land_cult)] <- 0 
CR3 [CR3 ==""] <- NA 

#replace NaN to Na, x=column
mutate(x= sub(NaN, NA, x)) %>%

#replace 2 to 1 
Data$TC[Data$TC == 2] <- 1 

R_Lands_I_Baseline_2018_[6,3]<- 40


#renam column-----
R_intensity_Baseline %>% 
  rename(new = old) #renam column old TO new

names(crop) <- c('ID', 'svy', 'hisa', 'crop1','crop2')


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
 


# digits after point
coords$print <- as.numeric((coords$`2`),digits=8)

# column 1-nth
df$ID <- seq.int(nrow(df))
 df1$consecutive_numbers<-1:nrow(df1)
mutate (observation = 1:n())
add_column(Column_After = "After",.after = "A") 


#from rows to column ----


#short to long
library(rstatix)
library(ggpubr)

mydata.long <- tse %>% 
  pivot_longer(-gender, names_to = "variables", values_to = "value")

   pivot_wider(names_from = D12_, values_from = n) 
  
#ifelse----
at_btselem <- at_btselem %>% 
  mutate(total_events_located_IL  = ifelse(location == 1, "1",NA))

df <-data.frame(Name = c("Tom","Mary","Tim","Chris") )
# if name starting with T
# if name include i
# if name ends with s

ifelse(grepl('^T', df$Name), 'YES', 'NO')
ifelse(grepl('i', df$Name), 'YES', 'NO')
ifelse(grepl('s$', df$Name), 'YES', 'NO')

#location- old, total_events_located_IL-new

# convert first letter to uppercase----
library(stringr)
village_list$village <- str_to_title(village_list$village) 

#round ----
mutate_at(3,round,2) # (column ,round ,digits)

colMeans(land_Treats)

#  Get the column number in R given the column name [duplicate]
match("column_name",names(df))


mutate(avm_self = rowMeans(.[names(.)[7:8]], na.rm = T),#mean per row - by defined columns

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

# remove columns that contain only NAs ----
subset(df, id == "101069") %>% 
  # remove columns that contain only NAs
  select(-which(sapply(., function(x) all(is.na(x))))) %>% 
  # remove columns that contain only empty cells
  mutate_all(as.character) %>% # Convert all columns to character type
  select_if(~ !all(is.null(.)) & !all(. == ""))

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

# table for regrassin ----
## tab_model for two models ----
tab_model(model11,model13,digits=3,p.style="stars", show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("Saptari", "Rautahat Bara Sarlahi"))

# Barplot with error bars----
# Standard deviation, Standard error or Confidence Interval ----
https://www.r-graph-gallery.com/4-barplot-with-error-bar.html
# GGPLOT ----
# scatterplot ----
yield22_acre %>% filter(season=="kharif_2021") %>% 
  ggplot(aes(x=kg_per_acre, y=kg_season)) + 
  geom_point()+
  scale_x_continuous(limits=c(0, 500)) +
  scale_y_continuous( limits=c(0, 4450))



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





#colorblind-friendly palette ----

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
scale_colour_manual(values=cbPalette)