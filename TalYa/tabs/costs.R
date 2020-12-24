x <- TALYAfarmer %>%inner_join(NE_talya_farmers) %>%filter(harvest==1) %>%
  select(starttime,farmer_name,expense_cost,expenses_names,expenses_names2,expenses_names3,
         labor_work_days,paid_labor_cost,hh_work_days,
         general_work_kind,transportation_cost_TALYA100,transportation_cost_CONTROL,
         comments_general,comments_talya100,comments_control,acre)
x[10,3] <- 2000
x[17,3] <- 2500
x[18,3] <- 1000
x[156,8] <- NA

x <- x %>% mutate(expense_cost=expense_cost/acre,
                  paid_labor_cost=paid_labor_cost/acre,
                  transportation_cost_TALYA100=transportation_cost_TALYA100/acre,
                  transportation_cost_CONTROL=transportation_cost_CONTROL/acre)

x1 <- x %>% filter(farmer_name=="D. Anjappa")	%>%
  summarise_at(c("expense_cost","paid_labor_cost",
                 "transportation_cost_TALYA100","transportation_cost_CONTROL"), sum, na.rm = TRUE)
x1$farmer_name <- "D. Anjappa"

x2 <- x %>% filter(farmer_name== "M Chenna krista") %>% 
  summarise_at(c("expense_cost","paid_labor_cost",
                 "transportation_cost_TALYA100","transportation_cost_CONTROL"), sum, na.rm = TRUE)
x2$farmer_name <- "M Chenna krista"

x3 <- x %>% filter(farmer_name== "R. Jakir Hussen") %>%
  summarise_at(c("expense_cost","paid_labor_cost",
                 "transportation_cost_TALYA100","transportation_cost_CONTROL"), sum, na.rm = TRUE)
x3$farmer_name <- "R. Jakir Hussen"

x4 <- x %>% filter(farmer_name== "Ranganath") %>% 
  summarise_at(c("expense_cost","paid_labor_cost",
                 "transportation_cost_TALYA100","transportation_cost_CONTROL"), sum, na.rm = TRUE)
x4$farmer_name <- "Ranganath"

x5 <- x %>% filter(farmer_name== "Siva reddy") %>% 
  summarise_at(c("expense_cost","paid_labor_cost",
                 "transportation_cost_TALYA100","transportation_cost_CONTROL"), sum, na.rm = TRUE)
x5$farmer_name <- "Siva reddy"

x6 <- x %>% filter(farmer_name== "T. Narasimha reddy") %>%
  summarise_at(c("expense_cost","paid_labor_cost",
                 "transportation_cost_TALYA100","transportation_cost_CONTROL"), sum, na.rm = TRUE)
x6$farmer_name <- "T. Narasimha reddy"

x7 <- x %>% filter(farmer_name== "Thimmireddy") %>%
  summarise_at(c("expense_cost","paid_labor_cost",
                 "transportation_cost_TALYA100","transportation_cost_CONTROL"), sum, na.rm = TRUE)
x7$farmer_name <- "Thimmireddy"

x8 <- x %>% filter(farmer_name== "Venkateswara")%>% 
  summarise_at(c("expense_cost","paid_labor_cost",
                            "transportation_cost_TALYA100","transportation_cost_CONTROL"), sum, na.rm = TRUE)
x8$farmer_name <- "Venkateswara"

x9 <- x %>% filter(farmer_name== "Viswanatha Reddy") %>%
  summarise_at(c("expense_cost","paid_labor_cost",
                 "transportation_cost_TALYA100","transportation_cost_CONTROL"), sum, na.rm = TRUE)
x9$farmer_name <- "Viswanatha Reddy"

cost <- rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9) %>% 
  inner_join(NE_talya_farmers) %>% 
  mutate(expense_cost=expense_cost/acre,
         paid_labor_cost=paid_labor_cost/acre,
         transportation_cost_TALYA100=transportation_cost_TALYA100/acre ,
         transportation_cost_CONTROL=transportation_cost_CONTROL/acre) %>% 
  group_by(mulching_control) %>% 
  summarise_at(c("expense_cost","paid_labor_cost",
                 "transportation_cost_TALYA100","transportation_cost_CONTROL"),
               mean, na.rm = TRUE) %>% 
  mutate(across(is.numeric,round))




  
-------------------------------------------------------------
  
  
Yield_sum <- talya_Yield_w %>% 
  mutate(ty_harvest_kgND = harvest_KG_talya100-harvest_damage_talya100,
         ctrl_harvest_kgND = harvest_KG_CONTROL-harvest_damage_CONTROL) %>%
           mutate(ty_revenue_ND=ty_harvest_kgND*average_price_TALYA100,
                  ctrl_revenue_ND= ctrl_harvest_kgND*average_price_TALYA100) %>%
  group_by(farmer_name) %>% 
           summarise_at(c("harvest_KG_talya100","harvest_KG_CONTROL",
                          "ty_harvest_kgND","ctrl_harvest_kgND",
                          "revenue_TALYA100","revenue_CONTROL",
                          "ty_revenue_ND","ctrl_revenue_ND"
           ), sum, na.rm = TRUE)

Yield_sum <- talya_Yield_w %>% 
  mutate(ty_harvest_kgND = harvest_KG_talya100-harvest_damage_talya100,
         ctrl_harvest_kgND = harvest_KG_CONTROL-harvest_damage_CONTROL) %>%
  mutate(ty_revenue_ND=ty_harvest_kgND*average_price_TALYA100,
         ctrl_revenue_ND= ctrl_harvest_kgND*average_price_TALYA100) %>%
  group_by(farmer_name) %>% 
  summarise_at(c("harvest_KG_talya100","harvest_KG_CONTROL",
                 "ty_harvest_kgND","ctrl_harvest_kgND",
                 "revenue_TALYA100","revenue_CONTROL",
                 "ty_revenue_ND","ctrl_revenue_ND"
  ), sum, na.rm = TRUE)

