

muskmelon <- TALYAfarmer %>%
  filter(farmer_name=="R Chenna kista") %>%
  filter(harvest_yesno_talya100=="Yes") %>%
  rename(harvest_KG_CONTROL=harvest_KG,harvest_damage_CONTROL=harvest_damage,week_number=week.) %>%  
  select(2,18,19,20,harvest_KG_talya100,harvest_KG_CONTROL,harvest_damage_talya100,
         harvest_damage_CONTROL,KG_sold_TALYA100,KG_sold_CONTROL,average_price_TALYA100,
         revenue_TALYA100,revenue_CONTROL,average_price_TALYA100,8,harvest_yesno_talya100,
         week_number,year)

muskmelon <- muskmelon %>% mutate(ty_harvest_kg_ac = harvest_KG_talya100 /288,
                          ctrl_harvest_kg_ac = harvest_KG_CONTROL /288,
                          ty_damage_kg_ac = harvest_damage_talya100 /288,
                          ctrl_damage_kg_ac = harvest_damage_CONTROL /288,
                          ty_kg_sold_ac = KG_sold_TALYA100 /288,
                          ctrl_kg_sold_ac = KG_sold_CONTROL /288,
                          ty_revenue_ac = revenue_TALYA100 /288,
                          ctrl_revenue_ac = revenue_CONTROL /288)

muskmelon %>%
  summarise_at(vars(ty_harvest_kg_ac :ctrl_revenue_ac), sum, na.rm = TRUE) 

ty_harvest_kg_ac ctrl_harvest_kg_ac ty_damage_kg_ac ctrl_damage_kg_ac ty_kg_sold_ac
    3.368056           2.541667               0                 0      3.368056
ctrl_kg_sold_ac ty_revenue_ac ctrl_revenue_ac
    2.541667      36.97917        28.70833

    harvest > 3.368056/2.541667	
    [1] 1.325137
    sold  > 3.368056/2.541667
    [1] 1.325137
    revenue> 36.97917/28.70833
    [1] 1.288099
    
    
muskmelon_plot <- TALYAplot %>%
      filter(farmer_name=="R Chenna kista")
    
muskmelon_plot_Weight <- muskmelon_plot %>% 
      select(Fruit_Weight_control_1,Fruit_Weight_control_2,Fruit_Weight_control_3,
             Fruit_Weight_talya100_plot_1,
             Fruit_Weight_talya100_plot_2,
             Fruit_Weight_talya100_plot_3,weeknum.year) %>% 
      na_if(0) %>% 
      transmute(weeknum.year,MeanC = rowMeans(select(., Fruit_Weight_control_1:Fruit_Weight_control_3),na.rm = T),
                MeanT = rowMeans(select(., Fruit_Weight_talya100_plot_1:Fruit_Weight_talya100_plot_3),na.rm = T)) %>% 
      summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
      mutate(across(is.numeric, round)) %>% 







