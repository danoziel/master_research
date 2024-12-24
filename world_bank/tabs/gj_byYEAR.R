#|_____________________________________________________________________________
#|
#|          Distribution of Adoption Properties BY YEARS 2005-2013
#|          
#| ANALSYS  X = year_Registration
#|_____________________________________________________________________________

library(dplyr)
library(ggplot2)

----------------------------------
  gj1B # Total 441,700 HH 
  gj1  # Total 242,110 HH    
---------------------------------

# % of MIS Registrations ------------------------------------------------------
yr <- gj1 %>%
  count(mistype,year_Registration) %>% 
  group_by(mistype) %>% 
  mutate(N=sum(n),pct=n/N) %>% ungroup()

yrB <- gj1B %>% 
  count(mistype,year_Registration) %>% 
  group_by(mistype) %>% 
  mutate(N=sum(n),pct=n/N)%>% ungroup()

ggplot(
  #  yr,
  yrB, 
  aes(x = year_Registration, y = pct,color = mistype, group = mistype)) +
  geom_line(size = 1) + 
  scale_x_continuous(breaks = seq(2005, 2013, 1)) +
  scale_color_manual(values=c("Drip"= "blue4", "Sprinkler"= "royalblue1")) +  
  labs(title = "Percent of MIS Registrations over the Years", 
       x = "Registrations Year", 
       y = "Percent of Registrations") +
  theme_classic()

# Caste ----------------------------------------------------------------------

mis_type=
  gj1 %>%select(regno,mistype) %>% filter(mistype != "")


clr_caste <- c("#5F6EA0",  "#5F9EA0" )

gj1 %>% filter(!is.na(caste)) %>% 
  count(year_Registration,caste) %>%   
  group_by(caste) %>% 
  mutate(sum(n),prt = n / sum(n) * 100) %>% 
  ggplot(aes(year_Registration, prt, color = caste)) +
  geom_line(size=1.5)+theme_light() + 
  scale_x_continuous(breaks=seq(2005,2014,1))+
  scale_colour_manual(values=clr_caste)

CD=gj1 %>% filter(!is.na(caste)) %>% 
  inner_join (mis_type) %>% filter(mistype=="Drip")

CS=gj1 %>% filter(!is.na(caste)) %>% 
  inner_join (mis_type) %>% filter(mistype=="Sprinkler")

CD %>% count(year_Registration,caste) %>% 
  group_by(caste) %>% mutate(sum(n),prt = n / sum(n) * 100) %>% ggplot(aes(year_Registration, prt, color = caste)) + geom_line(size=1)+theme_light() + scale_x_continuous(breaks=seq(2005,2014,1))+scale_colour_manual(values=clr_caste) +
  ggtitle("Drip-irrigated farms")

CS %>% count(year_Registration,caste) %>% 
  group_by(caste) %>% mutate(sum(n),prt = n / sum(n) * 100) %>% ggplot(aes(year_Registration, prt, color = caste)) + geom_line(size=1)+theme_light() + scale_x_continuous(breaks=seq(2005,2014,1))+scale_colour_manual(values=clr_caste) +
  ggtitle("Sprinkler-irrigated farms")




# Loan -----------------------------------------------------------------------
freq(gj1$Loan)

loan_year=
  gj1 %>%filter(!is.na(Loan)) %>% count(year_Registration,Loan) %>%
  group_by(year_Registration) %>% 
  mutate(pct=n/sum(n)*100) %>% mutate_at(4,round) %>% filter(Loan==1) %>% 
  ungroup() %>% mutate(Loan="year_wise")

loan_farmers=
  gj1 %>%filter(!is.na(Loan)) %>% count(year_Registration,Loan) %>%
  group_by(Loan) %>% 
  mutate(pct=n/sum(n)*100) %>% mutate_at(4,round) %>% filter(Loan==1) %>% 
  ungroup() %>% mutate(Loan="HHs_wise")

loan=
  gj1 %>%filter(!is.na(Loan)) %>% count(year_Registration,Loan) %>%
  group_by(year_Registration) %>% 
  mutate(pct_year_wise=n/sum(n)*100) %>% ungroup(
  ) %>% 
  group_by(Loan) %>% 
  mutate(pct_hh_wise=n/sum(n)*100) %>%ungroup() %>%  
  mutate_at(4:5,round) %>% filter(Loan==1) %>% 
  select(year_Registration,pct_year_wise, pct_hh_wise
  ) %>% pivot_longer(
    !year_Registration, names_to = "wise", values_to = "pct")


clr_loan <- c("gold2",  "gold4" )
ggplot(loan,aes(x=year_Registration, y=pct, color = wise)) +
  geom_line(size=1)+
  ylim(0,70)+scale_x_continuous(breaks=seq(2005,2014,1))+
  scale_colour_manual(values=clr_loan)+
  ggtitle("% HH who took loan") +theme_bw()


# Loan BY MIS ................................................................

mis_type=gj1 %>%select(regno,mistype) %>% filter(mistype != "")

# mistype=="Drip
LD=gj1 %>% filter(!is.na(Loan)) %>% inner_join (mis_type) %>% 
  filter(mistype=="Drip") %>% 
  count(year_Registration,Loan) %>%group_by(year_Registration) %>% mutate(pct_year_wise=n/sum(n)*100) %>% ungroup() %>% 
  group_by(Loan) %>% mutate(pct_hh_wise=n/sum(n)*100) %>%ungroup() %>% filter(Loan==1) %>% 
  select(year_Registration,pct_year_wise, pct_hh_wise) %>% pivot_longer(!year_Registration, names_to = "wise", values_to = "pct")

LD %>%
  ggplot(aes(year_Registration, pct, color = wise)) + geom_line(size=1)+theme_light()+scale_colour_manual(values=clr_loan) +
  ylim(0,70)+ #xlim(2005,2013)+ scale_x_continuous(breaks=seq(2005,2014,1))+
  ggtitle("Drip-irrigated farms")


# mistype=="Sprinkler
LS=
  gj1 %>% filter(!is.na(Loan)) %>% inner_join (mis_type) %>% 
  filter(mistype=="Sprinkler") %>% 
  count(year_Registration,Loan) %>%group_by(year_Registration) %>% mutate(pct_year_wise=n/sum(n)*100) %>% ungroup() %>% 
  group_by(Loan) %>% mutate(pct_hh_wise=n/sum(n)*100) %>%ungroup() %>%filter(Loan==1) %>% 
  select(year_Registration,pct_year_wise, pct_hh_wise) 

LS %>%
  # add_row(year_Registration = 2011, pct_year_wise = 0, pct_hh_wise = 0) %>%
  # add_row(year_Registration = 2012, pct_year_wise = 0, pct_hh_wise = 0) %>%
  # add_row(year_Registration = 2013, pct_year_wise = 0, pct_hh_wise = 0) %>% 
  pivot_longer(!year_Registration, names_to = "wise", values_to = "pct") %>% 
  ggplot(aes(year_Registration, pct, color = wise)) + geom_line(size=1)+theme_light()+scale_colour_manual(values=clr_loan) +
  ylim(0,70)+xlim(2005,2013)+ #scale_x_continuous(breaks=seq(2005,2014,1))+
  ggtitle("Sprinkler-irrigated farms")


  
  
  
  
  
# Total land & Irrigated Land [In Ha] -------------------------------------------

summary_1_99(gj1$TotalLand)
summary_1_99(gj1$misarea)
summary_1_99(gj1$mi_pct_land)

land11=
  gj1 %>% select(regno, year_Registration,TotalLand, misarea, mi_pct_land ) %>% 
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56 ) %>% 
  group_by(year_Registration) %>% 
  summarise(
    total_land=mean(TotalLand),
    irrigated_land=mean(misarea),
    pct_irrigated_land=mean(mi_pct_land)*100) %>% 
  mutate_at(4,round) %>% 
  pivot_longer(!year_Registration, names_to = "land", values_to = "size") %>% 
  mutate(mistype="All farm")

#   Fig ----
colour3 <- c("royalblue1", "brown4")
land1 %>% filter(land != "pct_irrigated_land" ) %>% 
  ggplot(aes(year_Registration, size, color = land)) +
  geom_line(size=1.5)+theme_light() +
  scale_x_continuous(breaks=seq(2005,2014,1))+
  scale_colour_manual(values = colour3)

# Total land & Irrigated Land [In Ha] -----------------------------------------

land2=
  gj1 %>% select(regno, year_Registration,TotalLand, misarea, mi_pct_land,mistype ) %>%
  filter(mistype %in% c("Drip","Sprinkler")) %>% 
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56 ) %>% 
  group_by(mistype,year_Registration) %>% 
  summarise(
    land_holding=mean(TotalLand),
    land_irrigated=mean(misarea),
    pct_irrigated_land=mean(mi_pct_land)*100) %>% mutate_at(5,round) %>% 
  ungroup() %>% 
  pivot_longer(!c(mistype,year_Registration), names_to = "land", values_to = "size") %>% 
  unite(land_mistype, land, mistype, sep = "_", remove = FALSE)

#   Fig ----
clr4 <- c("brown3","brown2" , "royalblue4" ,"royalblue1")
land2 %>% filter(land != "pct_irrigated_land" ) %>% 
  ggplot(aes(year_Registration, size, color = land_mistype)) +
  geom_line(size=1)  +
  theme_light() +
  scale_x_continuous(breaks = seq(2005, 2014, 1)) +
  scale_colour_manual(values = clr4) 



# share of irrigated land [In %]  ----

#__Fig___share of irrigated land [In %] 
land1 %>% filter(land == "pct_irrigated_land" ) %>% 
  ggplot(aes(year_Registration,size)) +
  geom_line(color="royalblue4", size=1)+theme_light() +
  scale_x_continuous(breaks=seq(2005,2014,1))

#__Fig___share of irrigated land [In %] 
clr5 <- c("royalblue4"  ,"royalblue1")
land2 %>% filter(land == "pct_irrigated_land" ) %>% 
  ggplot(aes(year_Registration, size, color = land_mistype)) +
  geom_line(size=1)  +
  theme_light() +
  scale_x_continuous(breaks = seq(2005, 2014, 1)) +
  scale_colour_manual(values = clr5) 


# ______________________________________________________________________________
# first to adopt        ----
# ______________________________________________________________________________



# first to adopt + date_gap ----

date_rank=
  gj1 %>% 
  select(regno, RegistrationDate,villageSI )%>%
  # right_join(gj_village1) %>% group_by(farmervillage) %>% 
  group_by(villageSI) %>% 
  arrange(RegistrationDate) %>%
  mutate(rank_date_adopters = row_number(),
         days_gap = c(NA, diff(RegistrationDate))) %>% ungroup()

#__Fig___No. weeks between RegistrationDate farmers_______________________________________________
date_gap <- 
  date_rank_villageWISE %>% group_by(rank_date_adopters) %>% filter(rank_date_adopters!=1) %>%
  summarise(days_gap=mean(days_gap)) %>%
  mutate(week_gap=days_gap/7) %>% 
  mutate(accumulated_week_gap = cumsum(week_gap))

date_gap %>% ggplot(aes(rank_date_adopters ,week_gap)) +
  geom_line(color="#69b3a2", size=1)+theme_light()+
  xlim(2,30)# +
scale_x_continuous(breaks=seq(2,20,2))



# accumulated_week_gap ----
#__Fig_________________________________________________
library(patchwork) # To display 2 charts together
library(hrbrthemes)

# A few constants
coeff <- 5
weeks <- "#69b3a2"
acc_weeks <- rgb(0.2, 0.6, 0.9, 1)


date_gap %>% filter(rank_date_adopters<30) %>%  
  ggplot( aes(x=rank_date_adopters)) +
  geom_line( aes(y=week_gap), size=1, color=weeks) + 
  geom_line( aes(y=accumulated_week_gap / coeff), size=1, color=acc_weeks) +
  scale_y_continuous(
    name = "Weeks Gap",
    sec.axis = sec_axis(~.*coeff, name="Accumulated Weeks Gap")
  )  + 
  theme_ipsum() +
  theme(axis.title.y = element_text(color = weeks, size=13),
        axis.title.y.right = element_text(color = acc_weeks, size=13))+
  ggtitle("Total weeks between registration date of first 20 farmers")
scale_x_continuous(breaks=seq(2,30,2))




-------------------------------------------------------------------------------
  
# essantials ..............................................................----


summary_1_99 <- function(x) {
  c(
    Count = sum(!is.na(x)),
    Mean = format(mean(x, na.rm = TRUE), digits = 6),
    Median = format(median(x, na.rm = TRUE), digits = 6),
    p = format(quantile(x, 0.01, na.rm = TRUE), digits = 6),
    P = format(quantile(x, 0.99, na.rm = TRUE), digits = 6),
    Min = format(min(x, na.rm = TRUE), digits = 6),
    Max = format(max(x, na.rm = TRUE), digits = 6)
  )
}

compute_summary <- function(x) {
  c(
    Count = sum(!is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    p = quantile(x, 0.01, na.rm = TRUE),
    P = quantile(x, 0.99, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
}

# Function to create plot
plot_line_1color <- function(data, x_var, y_var) {
  ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_line(color = "#69b3a2", size = 1.5) +
    labs(x = x_var, y = y_var, title = paste("The", y_var, "by", x_var)) +
    theme_light()+
    scale_x_continuous(breaks=seq(2,20,2))
}

write.csv(gj1, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/world_bank/gj1.csv", row.names=FALSE)


