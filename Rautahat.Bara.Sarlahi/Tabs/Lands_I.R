#proprty-----
# land_for_aquaculture_ponds
# [4.1c] Land for aquaculture, ponds
x <- Lands_I_Baseline_2018_ %>%filter(land_for_aquaculture_ponds>0 & land_for_aquaculture_ponds<300) %>% 
  summarise(mean(land_for_aquaculture_ponds)*0.0338,n())

x <- Lands_I_Baseline_2018_ %>%filter(land_for_aquaculture_ponds>0 & land_for_aquaculture_ponds<300) %>% 
  inner_join(Control_and_treatment_4_districts) %>%group_by(TreatmentControl) %>%
  summarise(mean(land_for_aquaculture_ponds)*0.0338,n())

y <- R_Lands_I_Endline_EPC_2019_ %>%
  filter(land_for_aquaculture_ponds>0 & land_for_aquaculture_ponds<300) %>% 
  group_by(TreatmentControl) %>%
  summarise(mean(land_for_aquaculture_ponds)*0.0338,n())


# [11.3] What is the total area of the pond? (in kathas)				
xx <- Aquaculture_Baseline_2018_ %>%inner_join(Control_and_treatment_4_districts) %>%
  filter(total_area_of_pond>0 & total_area_of_pond<300) %>%
  summarise(n(),mean(total_area_of_pond)*0.0338)

xx<- Aquaculture_Baseline_2018_ %>%inner_join(Control_and_treatment_4_districts) %>%
  filter(total_area_of_pond>0 & total_area_of_pond<300) %>%group_by(TreatmentControl) %>%
  summarise(n(),mean(total_area_of_pond)*0.0338)

yy <- Aquaculture_Endline_EPC_2019_%>%inner_join(Control_and_treatment_4_districts) %>%
  filter(total_area_of_pond>0 & total_area_of_pond<300) %>%group_by(TreatmentControl) %>%
  summarise(n(),mean(total_area_of_pond)*0.0338)
  






#homestead_dwelling_area
R_Lands_I_Baseline_2018_ %>% summarise(n(),n()/133,mean(homestead_dwelling_area),n()/133)

# land_for_cultivation
R_Lands_I_Baseline_2018_ %>% filter(land_for_cultivation>0) %>% 
  summarise(n(),mean(land_for_cultivation),n()/133)

# perm_fallow_land
ses <- R_Lands_I_Baseline_2018_ %>% filter(perm_fallow_land>0) %>% 
  summarise(n(),mean(perm_fallow_land),n()/133)

#orchard_land            
ses <- R_Lands_I_Baseline_2018_ %>% filter(orchard_land>0) %>% 
  summarise(n(),mean(orchard_land),n()/133) 

# total_property
ses <- R_Lands_I_Baseline_2018_ %>% filter(total_property>0) %>% 
  summarise(n(),mean(total_property),n()/133)



#women land-----

ses <- R_Lands_I_Baseline_2018_ %>%
  filter(woman_homestead>0) %>%
  summarise(count = n(),
            sum=sum(woman_homestead),
            mean=mean(woman_homestead))


ses <- R.Lands_I_Baseline_2018_ %>%
  filter(woman_land_cultivation>0) %>%
  summarise(count = n(),
            sum=sum(woman_land_cultivation),
            mean=mean(woman_land_cultivation),
            min=min(woman_land_cultivation),
            max=max(woman_land_cultivation),
  )

ses <- R.Lands_I_Baseline_2018_ %>%
  filter(woman_aquaculture_ponds>0) %>%
  summarise(count = n(),
            sum=sum(woman_aquaculture_ponds),
            mean=mean(woman_aquaculture_ponds),
            min=min(woman_aquaculture_ponds),
            max=max(woman_aquaculture_ponds),
  )

ses <- R.Lands_I_Baseline_2018_ %>%
  filter(woman_perm_fallow_land>0) %>%
  summarise(count = n(),
            sum=sum(woman_perm_fallow_land),
            mean=mean(woman_perm_fallow_land),
            min=min(woman_perm_fallow_land),
            max=max(woman_perm_fallow_land),
  )

ses <- R.Lands_I_Baseline_2018_ %>%
  filter(woman_orchard>0) %>%
  summarise(count = n(),
            sum=sum(woman_orchard),
            mean=mean(woman_orchard),
            min=min(woman_orchard),
            max=max(woman_orchard),
  )



ses <- R.Lands_I_Baseline_2018_ %>% 
  filter(landarea_women>0) %>% 
  summarise(sum= sum(landarea_women),
            mean=mean(landarea_women),
            min=min(landarea_women),
            max=max(landarea_women))

summary(Lands_I_Baseline_2018_$woman_homestead)
summary(Lands_I_Baseline_2018_$woman_perm_fallow_land)
summary(Lands_I_Baseline_2018_$woman_aquaculture_ponds)
summary(Lands_I_Baseline_2018_$woman_orchard)
summary(Lands_I_Baseline_2018_$woman_land_cultivation)

#rest----
summary(Lands_I_Baseline_2018_$how_many_agricultural_plots)
table(Lands_I_Baseline_2018_$how_many_agricultural_plots)

summary(Lands_I_Baseline_2018_$plot_size_pl1_bore_a)


