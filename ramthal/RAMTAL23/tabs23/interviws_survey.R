library(dplyr)
library(kableExtra)
library(tidyr)
library(haven)

rmtl_srvy22 %>%
  filter(mm4 == 1, farmers_hh=="inside_ramthal" ) %>% 
  count(mm5) %>% mutate(N=sum(n),n/N)
  


#Are you aware of any advantages of drip irrigation over other irrigation methods?	
# 0	No knowledge # 1	Increased yields # 2	Water saving
# 3	Fertilizer saving # 4	Less weeds # 5	Less labor requirements # 6	Other

rmtl_srvy22 %>%
  filter(mm4 == 1, farmers_hh=="inside_ramthal" ) %>% 
  select(hh_id,m3_0:m3_5 ) %>%
  pivot_longer(-c(hh_id), names_to = "ans",values_to = "yn") %>% 
  group_by(ans) %>% summarise(n=sum(yn),N=n()) %>%  
  mutate(pct=n/N) %>% 
  kbl() %>% kable_styling()




# Have you used it in [ _____ ]?
# [mw1c] If No, Why? (in Rabi)
# [mw7] In Kharif, when you did NOT irrigate, what are the reasons?

rmtl_srvy22$mw1c
# Sr=
rmtl_srvy22 %>% 
  filter(mm4 == 1, farmers_hh=="inside_ramthal" ,a5 !="Hungund") %>% 
  select(hh_id,starts_with("mw1c") ) %>%filter( mw1c != "" ) %>% 
  select(-mw1c__888 , -mw1c_other ,-mw1c) %>% 
  pivot_longer(-c(hh_id), names_to = "ans",values_to = "yn") %>% 
  group_by(ans) %>% summarise(n=sum(yn,na.rm = T),N=n()) %>%  
  mutate(pct=n/N) %>%  
  mutate(TH=c("a", "j", "b","c","d","e",  "f","g","h","i")) %>% 
  select(-ans) %>% 
  rename(Rn=n,RN=N,Rpct=pct)
# 1	The main piping was disfunctional
# 2	The laterals was not installed in my field or was damaged
### 3	Rainfall was sufficient
# 4	I wanted to irrigate, but company did not supply water 
# 5	I wanted to irrigate, but other farmers took all the water
# 6	Water was supplied only after I already sowed a rainfed crop
# 7	Water was not supplied when needed
# 8	I did not know when water was supplied
# 9	I do not trust the company
# 10	Water supply is unpredictable I cant count on it

rmtl_srvy22 %>% 
  filter(mm4 == 1, farmers_hh=="inside_ramthal" ,a5 !="Hungund") %>% 
  select(hh_id,starts_with("mw1c") ) %>%filter( mw1c != "" ) %>% 
  select(-mw1c__888 , -mw1c_other ,-mw1c) %>% 
  mutate(
    damages = mw1c_1 + mw1c_2,
    no_water = mw1c_4,
    farmers_took_all_water = mw1c_5,
    uncertainty_supply=+mw1c_6+mw1c_7+mw1c_8+mw1c_10,
    company_service = mw1c_9 + mw1c_4
         ) %>% 
  pivot_longer(-c(hh_id), names_to = "ans",values_to = "yn") %>% 
  filter(yn==1) %>% 
  group_by(ans) %>% count(ans) %>% ungroup() %>% 
  mutate(pct=n/284)
  






#### rmtl_srvy22$mw7_kharif_2022
##### Sk=
####   rmtl_srvy22 %>%
####   filter(mm4 == 1, farmers_hh=="inside_ramthal" ,a5 !="Hungund") %>%
####   select(hh_id,mw7_kharif_2022,mw7_kharif_2022_1:mw7_kharif_2022_10 ) %>%
####   filter( mw7_kharif_2022 != "" ) %>%
####   select(-mw7_kharif_2022  ) %>%
####   pivot_longer(-c(hh_id), names_to = "ans",values_to = "yn") %>%
####   group_by(ans) %>% summarise(n=sum(yn,na.rm = T),N=n()) %>%
####   mutate(pct=n/N)%>%
####   mutate(TH=c("a", "j", "b","c","d","e",  "f","g","h","i")) %>%
####   select(-ans)
#### 
#### full_join(Sr,Sk) %>% mutate(Rpct-pct) %>% arrange(TH)










