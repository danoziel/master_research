library(haven)
Ramthal_Karnataka_Cleaned_Data <- read_dta("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/Ramthal_Karnataka_submissions/Ramthal_Karnataka_Cleaned_Data.dta")
Ramthal_Karnataka_Cleaned <- Ramthal_Karnataka_Cleaned_Data %>% left_join(ramtal_groups)
rm(Ramthal_Karnataka_Cleaned_Data,ramtal_groups)

library(tidyverse)
water_mid22 = Ramthal_Karnataka_Cleaned_Data %>% select (id,in1_out0,starts_with("m"))


# usefull code ----
# remove columns that contain only NAs
x %>% 
  select(-which(sapply(., function(x) all(is.na(x)))))
# remove columns that contain only empty cells
x %>%
  mutate_all(as.character) %>%  # Convert all columns to character type
  select_if(~ !all(is.null(.)) & !all(. == ""))

# adding character "plot_" to digit
df$PLOT <- ifelse(grepl("^plot_\\d$", df$PLOT), sub("^plot_(\\d)$", "plot_0\\1", df$PLOT), df$PLOT)

# Using sub to replace the digits before the last underscore
# with the digits after the last underscore
names(L48_prev) <- sub("(\\d+)_(\\d)$", "\\2_\\1", names(L48_prev))

# crosstab ----
crosstab= Ramthal_Karnataka_Cleaned [,c("id","in_out","mm2_1")] %>%
  mutate(in_out=ifelse(in_out=="Inside","IN","OUT"),self_raport_2022=ifelse(mm2_1=="Im_inside","IN","OUT")) %>% 
  mutate(in_out=case_when(is.na(in_out) ~ self_raport_2022,TRUE ~ in_out)) # NAs

library(sjPlot)
sjPlot::tab_xtab(var.row = crosstab$in_out , 
                 var.col = crosstab$self_raport_2022, 
                 title = "Farmers' houshold. In & Out the project Baseline list | Survey2022 self-report",
                 var.labels = c("Baseline list", "_______Self-report_______"),
                 show.row.prc = TRUE,
                 digits = F,
#                 show.exp = TRUE,
                 tdcol.n = "black",
 #                tdcol.expected = "gray",
                 tdcol.row = "#339933",
                 emph.total = T,
                 emph.color = "#f8f8f8",
                 show.legend = T,
                 show.summary = T)
rm(crosstab)

water_mid22 <- midline_rmtl_2022 %>% 
  select(id,a5, starts_with("m")) %>% 
  inner_join(trt_ctrl) %>% rename(sr2022=self_raport_2022)
rm(midline_rmtl_2022)

#
# Water usage ----

#   L7 rank irrigation source    ====
# What irrigation source are you dependent on? (Rank according to the degree of importance)

L7 <- 
  Ramthal_Karnataka_Cleaned_Data %>%select(id,starts_with("l7_"))%>%  
  mutate_all(as.character) %>% 
  pivot_longer(
    cols = -id, 
    names_to = "L7_source_rank",
    values_to = "L7_source_type") %>% 
  group_by()
  count()





# mm4 # infrastructure installed
water_mid22 %>% group_by(in_out,mm4) %>% summarise(n=n()) %>% mutate(sum(n))

# mm4 mm5
water_mid22 %>% group_by(in_out,mm4,mm5) %>% summarise(n=n()) %>% mutate(sum(n))
water_mid22 %>% group_by(in_out,mm5) %>% summarise(n=n()) %>% mutate(sum(n))

#,mm4,mm5,mw2 Have you used it in [ _____ ]?
water_mid22 %>%group_by(in_out,mm4,mm5,mw2) %>% summarise(n=n()) 

 #mm4,mm5,mw1a If Yes, in which year did you first make use of the water?
water_mid22 %>% group_by(in_out,mm4,mm5,mw1a) %>% summarise(n=n())

#mm4,mm5,mw1b,#In what season did you use it in that year?
water_mid22 %>% group_by(in_out,mm4,mm5,mw1b) %>% summarise(n=n())

#mm4,mm5,mw4, # Are you still making use of the water from the project to irrigate your land?
water_mid22 %>% group_by(in_out,mm4,mm5,mw4) %>% summarise(n=n())

#mm4,mm5 mw4b,#If No or Sometimes- What was the last year you use of the water?
water_mid22 %>% group_by(in_out,mm4,mm5,mw4b) %>% summarise(n=n())

#mm4,mm5  mw12,# Typically, in your experience, when water is provided in a particular year, in which month does it start?
water_mid22 %>% group_by(in_out,mm4,mm5,mw12) %>% summarise(n=n())

#mm4,mm5mw13,# Typically, in your experience, when water is provided in a particular year, in which month does it end for the year?
water_mid22 %>% group_by(in_out,mm4,mm5,mw13) %>% summarise(n=n())

#mm4,mm5 mw14) %>% # Typically, in your experience, during the period water is provided, how often is it provided?
water_mid22 %>% group_by(in_out,mm4,mm5,mw14) %>% summarise(n=n())

#mm4,mm5 m20_ 2022-2017 did you ever make use kharif_2022
water_mid22 %>% group_by(in_out,mm4,mm5,m20_kharif_2022) %>% summarise(n=n())
water_mid22 %>% group_by(in_out,mm4,mm5,m20_rabi_2022) %>% summarise(n=n())

