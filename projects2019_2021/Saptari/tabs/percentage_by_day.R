
# T210701004  (18) | only monsoon+winter 17-18 | Saptari
# T109902002  (10) | winter_2019_2020          | Sapteri
# E0104705010 (22) | Summer_2017_2018	         | Sapteri
# A0110402001 (39) | 2017 missing              | Sapteri


A_days <- water01_SEASONs %>% 
  filter(! HH %in% c("T210701004", "T109902002", "E0104705010","A0110402001") ) %>% 
  group_by(HH,date) %>%
  summarise(hr= sum(Hours)) %>%
  add_column(irri = "A") %>% 
  select(1,2,4)

A_days_list <- A_days[,2] %>%   distinct() %>% arrange(date)

A_days_start_end <- A_days %>% 
  full_join (A_days_list) %>% 
  summarise(start=min(date),end=max(date)) %>% 
  mutate(total_days=end-start)
# NEW add 23/10/2023 ----

A_days <- water01 %>% 
  filter(! HH %in% c("T210701004", "T109902002", "E0104705010","A0110402001") ) %>% 
  group_by(HH,date) %>%
  summarise(hr= sum(Hours)) %>%
  add_column(irri = "A") %>% 
  select(1,2,4)

A_days_list <- A_days[,2] %>%   distinct() %>% arrange(date)

A_days_start_end <- A_days %>% 
  full_join (A_days_list) %>% 
  summarise(start=min(date),end=max(date)) %>% 
  mutate(start=as.Date(start),
         end=as.Date(end)) %>% 
  mutate(last_day="2019-12-16") %>%   mutate(last_day=as.Date(last_day)) %>% 
  mutate(total_days=end-start,till_last_day=last_day-start) %>% 
  filter(total_days>18)

A_days_start_end %>% summarise(min(total_days),mean(total_days),max(total_days))
A_days_start_end %>% summarise(min(start),max(start))

# ----

HH_list <- water01 %>% select(1,26) %>%   distinct()

# 50-53 HH - yes no irrigation ----
# 1 - A_days_start_end 
A0110402001 <- 
  A_days %>% 
  filter(HH=="A0110402001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-02-14",date <= "2019-11-01") %>% 
  mutate(A0110402001=ifelse(is.na(irri),"no", "yes")) 
A0110402001 <- A0110402001[,c(2,4)]

# 2 - A_days_start_end 
A104507035 <- 
  A_days %>% 
  filter(HH=="A104507035") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-02",date <= "2019-12-11") %>% 
  mutate(A104507035=ifelse(is.na(irri),"no", "yes")) 
A104507035 <- A104507035[,c(2,4)]

A1 <- full_join(A0110402001,A104507035) %>% arrange(date)

# 3 - A_days_start_end 
E0104705010 <- 
  A_days %>% 
  filter(HH=="E0104705010") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-12-19",date <= "2018-03-29") %>% 
  mutate(E0104705010=ifelse(is.na(irri),"no", "yes")) 
E0104705010 <- E0104705010[,c(2,4)]

A1 <- full_join(A1,E0104705010)

# 4 - A_days_start_end 
T100503002	 <- 
  A_days %>% 
  filter(HH=="T100503002") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-07-02",date <= "2019-09-12") %>% 
  mutate(T100503002=ifelse(is.na(irri),"no", "yes")) 
T100503002 <- T100503002[,c(2,4)]

A1 <- full_join(A1,T100503002)


# 5 - A_days_start_end 
T102007001	 <- 
  A_days %>% 
  filter(HH=="T102007001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-29",date <= "2019-10-14") %>% 
  mutate(T102007001=ifelse(is.na(irri),"no", "yes")) 
T102007001 <- T102007001[,c(2,4)]

A1 <- full_join(A1,T102007001)

# 6 - A_days_start_end 
T103204001	 <- 
  A_days %>% 
  filter(HH=="T103204001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-03",date <= "2019-12-16") %>% 
  mutate(T103204001=ifelse(is.na(irri),"no", "yes")) 
T103204001 <- T103204001[,c(2,4)]

A1 <- full_join(A1,T103204001)

# 7 - A_days_start_end 
T104209001			 <- 
  A_days %>% 
  filter(HH=="T104209001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-07-17",date <= "2019-12-06") %>% 
  mutate(T104209001=ifelse(is.na(irri),"no", "yes")) 
T104209001 <- T104209001[,c(2,4)]

A1 <- full_join(A1,T104209001)

# 8 - A_days_start_end 
T106605002					 <- 
  A_days %>% 
  filter(HH=="T106605002") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-05",date <= "2019-12-12") %>% 
  mutate(T106605002=ifelse(is.na(irri),"no", "yes")) 
T106605002 <- T106605002[,c(2,4)]

A1 <- full_join(A1,T106605002)

# 9 - A_days_start_end 
T109203001							 <- 
  A_days %>% 
  filter(HH=="T109203001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-02",date <= "2019-12-09") %>% 
  mutate(T109203001=ifelse(is.na(irri),"no", "yes")) 
T109203001 <- T109203001[,c(2,4)]

A1 <- full_join(A1,T109203001)

# 10 - A_days_start_end 
T109205004	 <- 
  A_days %>% 
  filter(HH=="T109205004") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-07-18",date <= "2019-10-08") %>% 
  mutate(T109205004=ifelse(is.na(irri),"no", "yes")) 
T109205004 <- T109205004[,c(2,4)]

A1 <- full_join(A1,T109205004)

# 11 - A_days_start_end 
T109902001	 <- 
  A_days %>% 
  filter(HH=="T109902001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-02",date <= "2019-09-07") %>% 
  mutate(T109902001=ifelse(is.na(irri),"no", "yes")) 
T109902001 <- T109902001[,c(2,4)]

A1 <- full_join(A1,T109902001)

# 12 - A_days_start_end 
T109902002			 <- 
  A_days %>% 
  filter(HH=="T109902002") %>%
  full_join (A_days_list) %>% 
  filter(date >="2019-10-16",date <= "2019-12-08") %>% 
  mutate(T109902002=ifelse(is.na(irri),"no", "yes")) 
T109902002 <- T109902002[,c(2,4)]

A1 <- full_join(A1,T109902002)

# 13 - A_days_start_end 
T110505001					 <- 
  A_days %>% 
  filter(HH=="T110505001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-03",date <= "2019-09-11") %>% 
  mutate(T110505001=ifelse(is.na(irri),"no", "yes")) 
T110505001 <- T110505001[,c(2,4)]

A1 <- full_join(A1,T110505001)

# 14 - A_days_start_end 
T200103002		 <- 
  A_days %>% 
  filter(HH=="T200103002") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-02",date <= "2019-04-27") %>% 
  mutate(T200103002=ifelse(is.na(irri),"no", "yes")) 
T200103002 <- T200103002[,c(2,4)]

A1 <- full_join(A1,T200103002)

# 15 - A_days_start_end 
T203901002				 <- 
  A_days %>% 
  filter(HH=="T203901002") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-07-16",date <= "2019-11-02") %>% 
  mutate(T203901002=ifelse(is.na(irri),"no", "yes")) 
T203901002 <- T203901002[,c(2,4)]

A1 <- full_join(A1,T203901002)

# 16 - A_days_start_end 
T203908001						 <- 
  A_days %>% 
  filter(HH=="T203908001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-02",date <= "2019-12-08") %>% 
  mutate(T203908001=ifelse(is.na(irri),"no", "yes")) 
T203908001 <- T203908001[,c(2,4)]

A1 <- full_join(A1,T203908001)

# 17 - A_days_start_end 
T205105003						 <- 
  A_days %>% 
  filter(HH=="T205105003") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-06",date <= "2019-12-04") %>% 
  mutate(T205105003=ifelse(is.na(irri),"no", "yes")) 
T205105003 <- T205105003[,c(2,4)]

A1 <- full_join(A1,T205105003)

# 18 - A_days_start_end 
T210101002						 <- 
  A_days %>% 
  filter(HH=="T210101002") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-22",date <= "2019-11-11") %>% 
  mutate(T210101002=ifelse(is.na(irri),"no", "yes")) 
T210101002 <- T210101002[,c(2,4)]

A1 <- full_join(A1,T210101002)

# 19 - A_days_start_end 
T210408001						 <- 
  A_days %>% 
  filter(HH=="T210408001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-02",date <= "2019-12-09") %>% 
  mutate(T210408001=ifelse(is.na(irri),"no", "yes")) 
T210408001 <- T210408001[,c(2,4)]

A1 <- full_join(A1,T210408001)

# 20 - A_days_start_end 
T210701004						 <- 
  A_days %>% 
  filter(HH=="T210701004") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-10",date <= "2017-12-29") %>% 
  mutate(T210701004=ifelse(is.na(irri),"no", "yes")) 
T210701004 <- T210701004[,c(2,4)]

A1 <- full_join(A1,T210701004)

# 21 - A_days_start_end 
T210708002						 <- 
  A_days %>% 
  filter(HH=="T210708002") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-02",date <= "2019-12-16") %>% 
  mutate(T210708002=ifelse(is.na(irri),"no", "yes")) 
T210708002 <- T210708002[,c(2,4)]

A1 <- full_join(A1,T210708002)

# 22 - A_days_start_end 
T210709001						 <- 
  A_days %>% 
  filter(HH=="T210709001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-04",date <= "2019-11-10") %>% 
  mutate(T210709001=ifelse(is.na(irri),"no", "yes")) 
T210709001 <- T210709001[,c(2,4)]

A1 <- full_join(A1,T210709001)

# 23 - A_days_start_end 
T210709003						 <- 
  A_days %>% 
  filter(HH=="T210709003") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-03",date <= "2019-12-16") %>% 
  mutate(T210709003=ifelse(is.na(irri),"no", "yes")) 
T210709003 <- T210709003[,c(2,4)]

A1 <- full_join(A1,T210709003)

# 24 - A_days_start_end 
T210904001						 <- 
  A_days %>% 
  filter(HH=="T210904001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-02",date <= "2019-11-27") %>% 
  mutate(T210904001=ifelse(is.na(irri),"no", "yes")) 
T210904001 <- T210904001[,c(2,4)]

A1 <- full_join(A1,T210904001)

# 25 - A_days_start_end 
T300307001						 <- 
  A_days %>% 
  filter(HH=="T300307001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-24",date <= "2019-12-03") %>% 
  mutate(T300307001=ifelse(is.na(irri),"no", "yes")) 
T300307001 <- T300307001[,c(2,4)]

A1 <- full_join(A1,T300307001)

# 26 - A_days_start_end 
T300406089						 <- 
  A_days %>% 
  filter(HH=="T300406089") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-21",date <= "2019-10-27") %>% 
  mutate(T300406089=ifelse(is.na(irri),"no", "yes")) 
T300406089 <- T300406089[,c(2,4)]

A1 <- full_join(A1,T300406089)

# 27 - A_days_start_end 
T300508099						 <- 
  A_days %>% 
  filter(HH=="T300508099") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-08-21",date <= "2019-12-07") %>% 
  mutate(T300508099=ifelse(is.na(irri),"no", "yes")) 
T300508099 <- T300508099[,c(2,4)]

A1 <- full_join(A1,T300508099)

# 28 - A_days_start_end 
T300608006						 <- 
  A_days %>% 
  filter(HH=="T300608006") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-27",date <= "2019-08-21") %>% 
  mutate(T300608006=ifelse(is.na(irri),"no", "yes")) 
T300608006 <- T300608006[,c(2,4)]

A1 <- full_join(A1,T300608006)

# 29 - A_days_start_end 
T300608033 <- 
  A_days %>% 
  filter(HH=="T300608033") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-22",date <= "2019-08-19") %>% 
  mutate(T300608033=ifelse(is.na(irri),"no", "yes")) 
T300608033 <- T300608033[,c(2,4)]

A1 <- full_join(A1,T300608033)

# 30 - A_days_start_end 
T300901091 <- 
  A_days %>% 
  filter(HH=="T300901091") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-22",date <= "2019-08-25") %>% 
  mutate(T300901091=ifelse(is.na(irri),"no", "yes")) 
T300901091 <- T300901091[,c(2,4)]

A1 <- full_join(A1,T300901091)

# 31 - A_days_start_end 
T300901113 <- 
  A_days %>% 
  filter(HH=="T300901113") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-22",date <= "2019-04-01") %>% 
  mutate(T300901113=ifelse(is.na(irri),"no", "yes")) 
T300901113 <- T300901113[,c(2,4)]

A1 <- full_join(A1,T300901113)

# 32 - A_days_start_end 
T301911010 <- 
  A_days %>% 
  filter(HH=="T301911010") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-08-19",date <= "2019-04-27") %>% 
  mutate(T301911010=ifelse(is.na(irri),"no", "yes")) 
T301911010 <- T301911010[,c(2,4)]

A1 <- full_join(A1,T301911010)

# 33 - A_days_start_end 
T302602009 <- 
  A_days %>% 
  filter(HH=="T302602009") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-23",date <= "2019-09-21") %>% 
  mutate(T302602009=ifelse(is.na(irri),"no", "yes")) 
T302602009 <- T302602009[,c(2,4)]

A1 <- full_join(A1,T302602009)

# 34 - A_days_start_end 
T302603034 <- 
  A_days %>% 
  filter(HH=="T302603034") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-29",date <= "2019-03-20") %>% 
  mutate(T302603034=ifelse(is.na(irri),"no", "yes")) 
T302603034 <- T302603034[,c(2,4)]

A1 <- full_join(A1,T302603034)

# 35 - A_days_start_end 
T302806050 <- 
  A_days %>% 
  filter(HH=="T302806050") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-22",date <= "2019-11-23") %>% 
  mutate(T302806050=ifelse(is.na(irri),"no", "yes")) 
T302806050 <- T302806050[,c(2,4)]

A1 <- full_join(A1,T302806050)

# 36 - A_days_start_end 
T303007001 <- 
  A_days %>% 
  filter(HH=="T303007001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-24",date <= "2019-08-19") %>% 
  mutate(T303007001=ifelse(is.na(irri),"no", "yes")) 
T303007001 <- T303007001[,c(2,4)]

A1 <- full_join(A1,T303007001)

# 37 - A_days_start_end 
T304802030		 <- 
  A_days %>% 
  filter(HH=="T304802030") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-07-06",date <= "2019-09-20") %>% 
  mutate(T304802030=ifelse(is.na(irri),"no", "yes")) 
T304802030 <- T304802030[,c(2,4)]

A1 <- full_join(A1,T304802030)

# 38 - A_days_start_end 
T304802122		 <- 
  A_days %>% 
  filter(HH=="T304802122") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-22",date <= "2019-10-31") %>% 
  mutate(T304802122=ifelse(is.na(irri),"no", "yes")) 
T304802122 <- T304802122[,c(2,4)]

A1 <- full_join(A1,T304802122)

# 39 - A_days_start_end 
T305001120		 <- 
  A_days %>% 
  filter(HH=="T305001120") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-22",date <= "2019-11-02") %>% 
  mutate(T305001120=ifelse(is.na(irri),"no", "yes")) 
T305001120 <- T305001120[,c(2,4)]

A1 <- full_join(A1,T305001120)

# 40 - A_days_start_end 
T305519001		 <- 
  A_days %>% 
  filter(HH=="T305519001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-22",date <= "2019-10-09") %>% 
  mutate(T305519001=ifelse(is.na(irri),"no", "yes")) 
T305519001 <- T305519001[,c(2,4)]

A1 <- full_join(A1,T305519001)

# 41 - A_days_start_end 
T305602003		 <- 
  A_days %>% 
  filter(HH=="T305602003") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-24",date <= "2019-09-18") %>% 
  mutate(T305602003=ifelse(is.na(irri),"no", "yes")) 
T305602003 <- T305602003[,c(2,4)]

A1 <- full_join(A1,T305602003)

# 42 - A_days_start_end 
T305607002		 <- 
  A_days %>% 
  filter(HH=="T305607002") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-22",date <= "2019-11-28") %>% 
  mutate(T305607002=ifelse(is.na(irri),"no", "yes")) 
T305607002 <- T305607002[,c(2,4)]

A1 <- full_join(A1,T305607002)

# 43 - A_days_start_end 
T305607004		 <- 
  A_days %>% 
  filter(HH=="T305607004") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-23",date <= "2019-09-12") %>% 
  mutate(T305607004=ifelse(is.na(irri),"no", "yes")) 
T305607004 <- T305607004[,c(2,4)]

A1 <- full_join(A1,T305607004)

# 44 - A_days_start_end 
T305607005			 <- 
  A_days %>% 
  filter(HH=="T305607005") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-27",date <= "2019-09-19") %>% 
  mutate(T305607005	=ifelse(is.na(irri),"no", "yes")) 
T305607005	 <- T305607005	[,c(2,4)]

A1 <- full_join(A1,T305607005	)

# 45 - A_days_start_end 
T306004001 <- 
  A_days %>% 
  filter(HH=="T306004001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-22",date <= "2019-10-11") %>% 
  mutate(T306004001	=ifelse(is.na(irri),"no", "yes")) 
T306004001	 <- T306004001	[,c(2,4)]

A1 <- full_join(A1,T306004001	)

# 46 - A_days_start_end 
T306102001 <- 
  A_days %>% 
  filter(HH=="T306102001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-24",date <= "2019-12-02") %>% 
  mutate(T306102001	=ifelse(is.na(irri),"no", "yes")) 
T306102001 <- T306102001	[,c(2,4)]

A1 <- full_join(A1,T306102001	)

# 47 - A_days_start_end 
T306102002 <- 
  A_days %>% 
  filter(HH=="T306102002") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-24",date <= "2019-12-06") %>% 
  mutate(T306102002	=ifelse(is.na(irri),"no", "yes")) 
T306102002 <- T306102002	[,c(2,4)]

A1 <- full_join(A1,T306102002	)

# 48 - A_days_start_end 
T308703001 <- 
  A_days %>% 
  filter(HH=="T308703001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-23",date <= "2019-11-26") %>% 
  mutate(T308703001	=ifelse(is.na(irri),"no", "yes")) 
T308703001 <- T308703001	[,c(2,4)]

A1 <- full_join(A1,T308703001	)

# 49 - A_days_start_end 
T308705001 <- 
  A_days %>% 
  filter(HH=="T308705001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-24",date <= "2019-09-07") %>% 
  mutate(T308705001	=ifelse(is.na(irri),"no", "yes")) 
T308705001 <- T308705001	[,c(2,4)]

A1 <- full_join(A1,T308705001	)

# 50 - A_days_start_end 
T308707002 <- 
  A_days %>% 
  filter(HH=="T308707002") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-21",date <= "2019-12-03") %>% 
  mutate(T308707002	=ifelse(is.na(irri),"no", "yes")) 
T308707002 <- T308707002	[,c(2,4)]

A1 <- full_join(A1,T308707002	)

# 51 - A_days_start_end 
T309306012 <- 
  A_days %>% 
  filter(HH=="T309306012") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-23",date <= "2019-11-30") %>% 
  mutate(T309306012	=ifelse(is.na(irri),"no", "yes")) 
T309306012 <- T309306012	[,c(2,4)]

A1 <- full_join(A1,T309306012	)

# 52 - A_days_start_end 
T309708020 <- 
  A_days %>% 
  filter(HH=="T309708020") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-06-22",date <= "2018-07-10") %>% 
  mutate(T309708020	=ifelse(is.na(irri),"no", "yes")) 
T309708020 <- T309708020	[,c(2,4)]

A1 <- full_join(A1,T309708020	)

# 53 - A_days_start_end 
T309800000 <- 
  A_days %>% 
  filter(HH=="T309800000") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-12-21",date <= "2019-11-30") %>% 
  mutate(T309800000	=ifelse(is.na(irri),"no", "yes")) 
T309800000 <- T309800000	[,c(2,4)]

A1 <- full_join(A1,T309800000	)

# 54 - A_days_start_end 
T309900000 <- 
  A_days %>% 
  filter(HH=="T309900000") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-12-21",date <= "2019-11-30") %>% 
  mutate(T309900000	=ifelse(is.na(irri),"no", "yes")) 
T309900000 <- T309900000	[,c(2,4)]

A1 <- full_join(A1,T309900000	)

# rm  50-53 HH  --------------------

rm(A104507035,A0110402001,E0104705010,T100503002,T102007001,T103204001,T104209001,T106605002,
   T109203001,T109205004,T109902001,T109902002,T110505001,T200103002,T203901002,T203908001,
   T205105003,T210101002,T210408001,T210701004,T210708002,T210709001,T210709003,T210904001,
   T300307001,T300406089,T300508099,T300608006,T300608033,T300901091,T300901113,T301911010,
   T302602009,T302603034,T302806050,T303007001,T304802030,T304802122,T305001120,T305519001,
   T305602003,T305607002,T305607004,T305607005	,T306004001,T306102001,T306102002,T308703001,
   T308705001,T308707002,T309306012,T309708020,T309800000,T309900000
)
# A1_seasons  + omit 4 HH-----------------

A1_seasons <- A1 %>%
  select(-c("T210701004", "T109902002", "E0104705010","A0110402001") ) %>% 
  mutate(season=case_when(
    date >= "2017-06-02" & date <= "2017-09-30" ~ "Monsoon_2017_2018",
    date >= "2017-10-01" & date <= "2018-01-31" ~ "Winter_2017_2018",
    date >= "2018-02-01" & date <= "2018-05-31" ~ "Summer_2017_2018",
    
    date >= "2018-06-01" & date <= "2018-09-30" ~ "Monsoon_2018_2019",
    date >= "2018-10-01" & date <= "2019-01-31" ~ "Winter_2018_2019",
    date >= "2019-02-01" & date <= "2019-05-31" ~ "Summer_2018_2019",
    
    date >= "2019-06-01" & date <= "2019-09-30" ~ "Monsoon_2019_2020",
    date >= "2019-10-01" & date <= "2019-12-16" ~ "Winter_2019_2020"))

# T210701004  (18) | only monsoon+winter 17-18 | Saptari
# T109902002  (10) | winter_2019_2020          | Sapteri
# E0104705010 (22) | Summer_2017_2018	         | Sapteri
# A0110402001 (39) | 2017 missing              | Sapteri

# A1_days_HH totalHH----
A1_days_HH <- A1_seasons %>% 
  rowwise() %>% 
  mutate(sumVar = across(c(A104507035:),~ifelse(. %in% c("yes", "no"),1,0)) %>% sum) %>% 
  mutate(irri = across(c(A104507035:T309900000),~T309900000ifelse(. %in% c("yes"),1,0)) %>% sum) %>% 
  mutate(per=irri/ sumVar) 
colour <- c("dimgrey", "dimgrey","dimgrey", "darkolivegreen4","darkolivegreen4",
            "dodgerblue4","dodgerblue4","dodgerblue4")
ggplot()+
  labs(title=" ", 
       x=" ",y=" ", caption = "*53 HH in sample")+
  geom_line(data=A1_days_HH, aes(x = date, y = per, color = season)) + 
  stat_smooth(data=A1_days_HH, aes(x = date, y = per, color = season),method = "loess")+
  theme_minimal() +
  scale_colour_manual(values=colour)+
  theme(axis.text.x = element_text( vjust=0.08), 
        panel.grid.minor = element_blank(),legend.title = element_blank(),legend.position = "none")+ 
  theme(panel.grid.major.x = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 14, margin = margin(b = 10)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))


# days_of_usage          ----
days_of_usage <- A1_days_HH %>%
  select(date,sumVar,irri,per) %>% 
  rename(nu_HH_use_pmp=irri,total_HH_own_pmp=sumVar,HH_percentage=per)


tab_model(A1_lm)


#A_days_HH_per_saptari   ----
A_days_HH_per_saptari <- A1_seasons[,c(1:21,52)]
A_days_HH_per_saptari <- A_days_HH_per_saptari %>% 
  rowwise() %>% 
  mutate(sumVar = across(c(A104507035:T210904001),~ifelse(. %in% c("yes", "no"),1,0)) %>% sum) %>% 
  mutate(irri = across(c(A104507035:T210904001),~ifelse(. %in% c("yes"),1,0)) %>% sum) %>% 
  mutate(per=irri/ sumVar) 

#A_days_HH_per_RBS       ----
A_days_HH_per_RBS <- A1_seasons[,c(1,22:52)]
A_days_HH_per_RBS <- A_days_HH_per_RBS %>% 
  rowwise() %>% 
  mutate(sumVar = across(c(T300307001:T309900000),~ifelse(. %in% c("yes", "no"),1,0)) %>% sum) %>% 
  mutate(irri = across(c(T300307001:T309900000),~ifelse(. %in% c("yes"),1,0)) %>% sum) %>% 
  mutate(per=irri/ sumVar) 

#plots    ----
ggplot(data = A_days_HH_per_saptari, aes(x = date, y = per))+
  labs(title="District of Saptari",
       x=" ",y=" ", caption = " ")+
  geom_line(color = "darkolivegreen4", size = 0.5)+
  stat_smooth(color = "#FC4E07", fill = "#FC4E07",metho = "loess")+
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 10, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))

ggplot(data = A_days_HH_per_RBS, aes(x = date, y = per))+
  labs(title="Districts of Rautahat Bara Sarlahi",
       x=" ",y=" ", caption = " ")+
  geom_line(color = "lightsalmon4", size = 0.5)+
  stat_smooth(color = "#FC4E07", fill = "#FC4E07",metho = "loess")+
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 10, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))


colour <- c("dimgrey", "dimgrey","dimgrey", "darkolivegreen4","darkolivegreen4",
            "dodgerblue4","dodgerblue4","dodgerblue4")
ggplot()+
  labs(title="Saptari", 
       x=" ",y=" ", caption = "*24 HH in sample")+
  geom_line(data=A_days_HH_per_saptari, aes(x = date, y = per, color = season)) + 
  stat_smooth(data=A_days_HH_per_saptari, aes(x = date, y = per, color = season),method = "loess")+
  theme_minimal() +
  scale_colour_manual(values=colour)+
  theme(axis.text.x = element_text( vjust=0.08), 
        panel.grid.minor = element_blank(),legend.title = element_blank(),legend.position = "none")+ 
  theme(panel.grid.major.x = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 14, margin = margin(b = 10)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))

ggplot()+
  labs(title="Rautahat Bara Sarlahi", 
       x=" ",y=" ", caption = "*30 HH in sample")+
  geom_line(data=A_days_HH_per_RBS, aes(x = date, y = per, color = season)) + 
  stat_smooth(data=A_days_HH_per_RBS, aes(x = date, y = per, color = season),method = "loess")+
  theme_minimal() +
  scale_colour_manual(values=colour)+
  theme(axis.text.x = element_text(vjust=0.08), 
        panel.grid.minor = element_blank(),legend.title = element_blank(),legend.position = "none")+ 
  theme(panel.grid.major.x = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 14, margin = margin(b = 10)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))

monsoon      winter        summer          monsoon     winter     summer         monsoon   winter 


# rm(A_days,A_days_list,A_days_start_end,HH_list,A1_seasons,A1) ----

rm(A_days,A_days_list,A_days_start_end,HH_list,A1_seasons,A1)


#  T210708002 T210408001  T203901002  T200103002  T109203001  T210709001  T109902001 T103204001----

library(vistime)

vistime(A_days_start_end, events = "total_days", 
        groups = "HH",
        start = "start", end = "end")

# -----------------
dim(A1_seasons)
df151  <- A1_seasons[,1:51]

df53 <- diary_spip_terai[,c(1,26)] %>% distinct()

# diary ----

diary_saptari <- get_power(
  community = "SSE",
  lonlat = c(86.63533, 26.60646),
  pars = c("PRECTOT","ALLSKY_SFC_SW_DWN"),
  dates =c( "2017-06-02","2019-12-16"),
  temporal_average = "DAILY") %>% 
  add_column(district = "Saptari") %>% 
  rename(date=YYYYMMDD)


diary_rbs <- get_power(
  community = "SSE",
  lonlat = c(85.16984, 27.00147),
  pars = c("PRECTOT","ALLSKY_SFC_SW_DWN"),
  dates =c( "2018-06-21","2019-12-16"),
  temporal_average = "DAILY") %>% 
  add_column(district = "Rautahat_Bara_Sarlahi") %>% 
  rename(date=YYYYMMDD)

diary_4 <- rbind(diary_saptari[,7:10],diary_rbs[,7:10]) 

# irrigation_day_yesno y as binary varible ----
irrigation_day_yesno <- 
  gather(df151, "HH", "irrigation_day", -date) %>%
  drop_na() %>% 
  left_join(df53) %>% 
  left_join(diary_4) %>% 
  rename(ghi=ALLSKY_SFC_SW_DWN,precip=PRECTOT) %>% 
  mutate(irrigation_day=ifelse(irrigation_day=="yes",1,0))

# regrassion

model53 <- lm(irrigation_day~ ghi,irrigation_day_yesno)
model53 <- lm(irrigation_day~ precip,irrigation_day_yesno)
model53 <- lm(irrigation_day~ ghi+precip,irrigation_day_yesno)

summary(model53)
tab_model(model53,digits=3,p.style= "numeric",
          show.se = TRUE,string.ci = "Conf. Int (95%)",
          pred.labels = c("(Intercept)", "Solar Radiation (GHI)","Rain mm"))


