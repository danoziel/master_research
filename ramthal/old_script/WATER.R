
Adt3
Adt4
A <- rbind(2,3,4)
View(A)

# PAR PARTICIPATION IN THE PROJECT ----
PAR2 <- Adt2 %>%select(id,Im_in_out,in_out,
                      mm1,#Have you heard of any government irrigation project in your area?
                      mm2,# Is your land coming under such a government project?
                      mm4,# Has any infrastructure (e.g. piping) been installed in your land at any time in the past?
                      mm5,#Have you ever (even once) made any use of this water for irrigating your land?
                      m3,#Are you aware of any advantages of drip irrigation over other irrigation methods?
                      m4,#Who is deciding which land and farmers are ALLOWED TO TAKE PART in this drip irrigation project?
                      m5,#On what basis are they deciding what/who is included or not in the project?
                      m6,#		What proportion of the households in your village have ever received water from the project?
                      m7,#		Do you think the income of those farmers covered by the project improved? 
                      m8,	#By roughly how much? 
                      mm9,#How many farmers are there between you and the valve/pipeline?
                      mm10)#Has it ever happened to you that farmers "before" you have used up a lot of the water from the pipe, so you did not have enough?

PAR3 <- Adt3 %>%select(id,Im_in_out,in_out,mm1, mm2, mm4, mm5, m3, m4,m5,m6,m7,m8,mm9,mm10)                      
PAR4 <- Adt4 %>%select(id,Im_in_out,in_out,mm1, mm2, mm4, mm5, m3, m4,m5,m6,m7,m8,mm9,mm10)                      

PAR <- rbind(PAR2,PAR3,PAR4)                      
                      

# LV Land value  ----
LV2 <- 
  Adt2%>%
  select(id,Im_in_out,in_out,
         #non ramthal
         m11a	,#	 LEASE your land, how much? (Rs. per Acre per year)
         m11b,#  estimate  LEASE  inside
         m12,# sell  your land, how much? (Rs. per Acre)
         m13,#  estimate  BUY  inside
         
         # ramthal
         m14a,#	 LEASE your land, how much? (Rs. per Acre per year)
         m14b, #  estimate  LEASE  inside
         m17,# sell  your land, how much? (Rs. per Acre)
         m18,#  estimate  BUY  outside
         #  non ramthal +ramthal
         m19) #spent by the government and the company per acre

LV3 <- Adt3%>%select(id,Im_in_out,in_out,m11a	,m11b, m12, m13,m14a, m14b, m17, m18, m19)
LV4 <- Adt4%>%select(id,Im_in_out,in_out,m11a	,m11b, m12, m13,m14a, m14b, m17, m18, m19)

LV <- rbind(LV2,LV3,LV4) %>% arrange(in_out) 
View(LV)
LV %>% filter(m11a>0) %>% summarise(mean(m11a),median(m11a))
LV %>% filter(m11b>0) %>% summarise(mean(m11b),median(m11b))
LV %>% filter(m12>0) %>% summarise(mean(m12),median(m12))
LV %>% filter(m13>0) %>% summarise(mean(m13),median(m13))
LV %>% filter(m14a>0) %>% summarise(mean(m14a),median(m14a))
LV %>% filter(m14b>0) %>% summarise(mean(m14b),median(m14b))
LV %>% filter(m17>0) %>% summarise(mean(m17),median(m17))
LV %>% filter(m18>0) %>% summarise(mean(m18),median(m18))



# UW Use of Water from the Project ----

UW3 <- Adt3 %>%select(id,Im_in_out,in_out,mm5,
                       mw2,# Have you used it in [ _____ ]?
                       mw1a,# If Yes, in which year did you first make use of the water? 
                       mw1b,#In what season did you use it in that year?
                       mw1c,#If No, Why?
                       mw4, # Are you still making use of the water from the project to irrigate your land?
                       mw4a,# If Yes, in what season?
                       mw4b,#If No or Sometimes- What was the last year you use of the water?
                       mw5,# How many years in total did you ever make use of the water for irrigation during Kharif?
                       mw6,#How many years in total did you ever make use of the water for irrigation during Rabi?
                     m10,# Of all the people who are covered by the project, how many still make use of it? (in percent) 
                       mw12,#
                       mw13,#
                       mw14)#
 

UW33 <- Adt3$mw9_rabi_2017 %>%select(id,Im_in_out,in_out,mm5,
 #m20, # Can you indicate in which years and seasons you ever make use of the water for irrigation?
 m20_kharif_2017,m20_kharif_2018,m20_kharif_2019,m20_kharif_2020, m20_kharif_2021,m20_kharif_2022,
 m20_rabi_2017,m20_rabi_2018,m20_rabi_2019,m20_rabi_2020, m20_rabi_2021,m20_rabi_2022,

  #mw7 In Kharif, when you did NOT irrigate, what are the reasons? 
 mw7_kharif_2017,mw7_kharif_2018,mw7_kharif_2019,mw7_kharif_2020, mw7_kharif_2021,mw7_kharif_2022,
 
  #mw8 In Kharif seasons that you DID irrigate, did you notice any effect on your crop?
 mw8_kharif_2017,mw8_kharif_2018,mw8_kharif_2019,mw8_kharif_2020, mw8_kharif_2021,mw8_kharif_2022,
 
 #mw9_rabi_2017, # In Rabi, when you did NOT irrigate, what are the reasons? 
 mw9_rabi_2018 ,mw9_rabi_2019,mw9_rabi_2020, mw9_rabi_2021,mw9_rabi_2022,
 
 #mw10 In Rabi seasons that you DID irrigate, did you notice any effect on your crop?
 mw10_rabi_2017,mw10_rabi_2018,mw10_rabi_2019,mw10_rabi_2020, mw10_rabi_2021,mw10_rabi_2022)
 

# DS Damaged irrigation system ----
 
DS3 <- 
   Adt3 %>%  
   mutate(m35b_year=m35b_year*12,m35b=m35b_month+m35b_year) %>%
   select(id,Im_in_out,in_out,mm5,
          m35,  # What is the status of the main pipe coming into your land ?
          m35a, # What caused the damage?
          m35b, # How long has the main pipes been damaged?
          m35c, # What is the status of the laterals?
          m36, # did you contact someone to fix ?
          m37) # did they help you and fix it?
          
DS4 <-Adt4 %>% 
  mutate(m35b_year=m35b_year*12,m35b=m35b_month+m35b_year) %>%
  select(id,Im_in_out,in_out,mm5,m35,m35a,m35b,m35c, m36,m37)
  
DS <- rbind(DS3,DS4) %>% 
   mutate(m35=ifelse(m35==1,"Works",ifelse(m35==2,"Damaged",NA))) %>% 
   mutate(m35c=ifelse(m35c==1,"Works, laid in the field",
                   ifelse(m35c==2,"OK, but in storage","Damaged")))

DS %>% filter(m35b>0) %>% summarise(mean(m35b),median(m35b))


# RE Recommendations ----

RE3<- 
  Adt3 %>% 
  select(id,Im_in_out,in_out,
         mw34, #R Did you ever receive any advise or recommendations from the irrigation company?
         m34,  #R If Yes, Did you follow the irrigation company's recommendations?
         m34a, #R If no, why?
         m40c,  #B Are you aware of a drip irrigation project demonstration plot?
         m42,   #B Have you ever gone to visit it?
         m43,   #B Why have you not gone to visit them?	
         m46,   #B What was your impression of the cultivation in that plot?
         m47,   #B Which crops were cultivated there? multiple answers
         m48,   #B How do you compare the productivity there to your own?
         m50a)  #B What prevents you from following the same practices like in the demo plot?

RE4<- Adt4 %>% select(id,Im_in_out,in_out,mw34,m34,m34a,m40c,m42,m43,m46,m47,m48,m50a)

RE <- rbind(RE3,RE4) 

# TR Trainings ----

TR2<- 
  Adt2 %>% 
  select(id,Im_in_out,in_out,#B
         m51,
         m52,
         m53,
         m54,
         m55,
         m56)

TR3<- Adt3 %>% select(id,Im_in_out,in_out, m51,m52,m53,m54,m55,m56)	
TR4<- Adt4 %>% select(id,Im_in_out,in_out, m51,m52,m53,m54,m55,m56)	

TE <- rbind(TR2,TR3,TR4)

# WUA Water User Associations (WUA) ----

WUA3<- Adt3 %>%  # B
  select(id,Im_in_out,in_out,mm5,
         m59a,
         m62,
         m62a,
         m62b,
         m62c)

WUA4<- Adt4 %>% select(id,Im_in_out,mm5,in_out,m59a,m62,m62a,m62b,m62c)

WUA <- rbind(WUA3,WUA4)

# WTP Willingness to pay ----
#ramthal + non ramthal

WTP3<- Adt3 %>%
  select(id,Im_in_out,in_out,mm5,
         m67,
         m68)

WTP4<- Adt4 %>% select(id,Im_in_out,mm5,in_out,m67, m68)

WTP <- rbind(WTP3,WTP4)

# MAI Maintenance ----
#ramthal

MAI2<- 
  Adt2 %>%
  select(id,Im_in_out,in_out,
         m69,
         m70,
         m72,
         m73,
         m74,
         m75_a,
         m75_b,
         m75_c,
         m75_d,
         m75_e,
         m75_f)

MAI3<-Adt3 %>% select(id,Im_in_out,in_out,m69,m70,m72,m73,m74,m75_a,m75_b,m75_c,m75_d,m75_e, m75_f)
MAI4<-Adt4 %>% select(id,Im_in_out,in_out,m69,m70,m72,m73,m74,m75_a,m75_b,m75_c,m75_d,m75_e, m75_f)

MAI <- rbind(MAI2, MAI3, MAI4)






## combinations ----