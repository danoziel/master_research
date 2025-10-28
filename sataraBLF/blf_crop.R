

blf_crop_focus <- 
  BLF_Interact_2025_11_11 %>% 
  rename(id=`_id`, uuid =`_uuid`) %>% 
  select( id, uuid, end, crop_focus,crop_focus_other ) %>% filter(crop_focus != "") %>% 
  mutate(crop_focus=ifelse(crop_focus=="other_crop","other_crop_1",
                           crop_focus )) %>% 
  separate(col = crop_focus ,
           into = c("crop", "col2", "col3"), sep = " ",
           fill = "right",extra = "merge") %>% 
  select( id, uuid, end, crop,crop_focus_other ) 


# blf_crop_other
 
blf_crop_focus %>% filter(!is.na(crop_focus_other)) %>% count(crop_focus_other)

blf_crop_other <- 
  blf_crop_focus %>% 
  filter(!is.na(crop_focus_other)) %>% 
  mutate(
    crop_other = case_when(
      str_detect(crop_focus_other, "soyabean|soyaben|suyabean|Soyabean|Soyaben|सोयाबीन") ~ "Soybean",
      str_detect(crop_focus_other, "हळद") ~      "turmeric",
      str_detect(crop_focus_other, "ginger|gingrer|आले Jinger|Ginger|Gingrer") ~ "Ginger", 
      str_detect(crop_focus_other, "Grass|grass|elephantgrass") ~ "Grass",
      str_detect(crop_focus_other, "Groundnut|groundnut|ground|Ground") ~ "Groundnut",
      str_detect(crop_focus_other, "banana|Banana") ~ "Banana",
      str_detect(crop_focus_other, "mango|Mango") ~ "Mango",
      str_detect(crop_focus_other, "strawberry|Strawberry|strawberry") ~ "Strawberry",
      str_detect(crop_focus_other, "sugarcane|Sugarcane") ~ "Sugarcane",  
      str_detect(crop_focus_other, "jowar|Jowar") ~ "Jowar",
      str_detect(crop_focus_other, "gahu|Wheat") ~ "Wheat", 
      str_detect(crop_focus_other, "Rice|rice|basamati") ~ "Rice",
      str_detect(crop_focus_other, "empty land|home|near home|none|pump|spray pump|2|ala|alavni|band|dukan|ghar|k|nahi|ran|tan|vand|गुलाब") ~ "Other",
      TRUE ~ "Other" )) %>% 
  select(-crop_focus_other)

### blf_crop
blf_crop <- 
  blf_crop_focus [1000:1959,] %>% 
  left_join(blf_crop_other) %>% 
  mutate(
    crop=ifelse(is.na(crop_other), crop ,crop_other)) %>%
  mutate(crop = str_to_sentence(crop)) %>% 
  select(id,crop,uuid,end) %>% 
  mutate(crop =ifelse(crop =="Other_crop_1","Other",crop ))

blf_crop %>% filter(crop != "Other") %>% count(crop) %>% mutate(pct=n/sum(n)) %>% filter(pct>0.015) %>% 
  ggplot(aes(x = crop, y = pct, fill = crop)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  labs(title = "What crop have you come to shop for?", 
       x = "", y = "% of farmers") +
  theme_minimal() +
  theme(legend.position = "none", 
        text = element_text(family = "serif"),
        axis.text.x = element_text(size = 12)
  ) +
  scale_fill_brewer(palette = "BrBG") +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            vjust = -0.5, color = "black", size = 3)


# COMMON PROBLEMS
Pr_pest <- 
  BLF_Interact_2025_11_11 %>% 
  rename(id=`_id`, uuid =`_uuid`) %>% 
  select(id,uuid, end,
         starts_with("id_pest_problem/"),
         pest_problem_photo_URL) %>% 
  pivot_longer( -c(id, uuid,end,pest_problem_photo_URL ),
                names_to = "problem",
                values_to = "problem_specific" ) %>% 
  filter(problem_specific==1)%>% select(problem,everything()) %>% 
  mutate(problem_specific = str_replace(problem , "id_pest_problem/", "")) %>% 
  mutate(problem_category ="Pest") %>% 
  mutate(problem = gsub("/", ".", problem)) %>% 
  rename(product_URL=pest_problem_photo_URL)


Pr_disease <- 
  BLF_Interact_2025_11_11 %>% 
  rename(id=`_id`, uuid =`_uuid`) %>% 
  select(id,uuid, end,
         starts_with("id_disease_problem/"),
         disease_problem_photo_URL) %>% 
  pivot_longer( -c(id, uuid,end,disease_problem_photo_URL ),
                names_to = "problem",
                values_to = "problem_specific" ) %>% 
  filter(problem_specific==1)%>% select(problem,everything()) %>% 
  mutate(problem_specific = str_replace(problem , "id_disease_problem/", "")) %>% 
  mutate(problem_category ="Disease") %>% 
  mutate(problem = gsub("/", ".", problem)) %>% 
  rename(product_URL=disease_problem_photo_URL)

Pr_weed <- 
  BLF_Interact_2025_11_11 %>% 
  rename(id=`_id`, uuid =`_uuid`) %>%  
  select(id,uuid, end,`reason_come_in_new/weed_control`,weed_control_photo_URL) %>% 
  filter(`reason_come_in_new/weed_control`==1) %>% 
  mutate(
    problem = "id_weeds_problem.weed_control", 
    problem_specific = "weed_control",
    problem_category ="Weeds"
  ) %>% rename(product_URL=weed_control_photo_URL) %>% 
  select(problem,id,uuid, end,product_URL, problem_specific,problem_category)




rbind(Pr_pest,Pr_disease,Pr_weed) %>%
  left_join(blf_crop) %>% 
  rename(date=end) %>% 
  select(problem, problem_category, problem_specific, 
         crop, id, uuid, date ) %>% 
  count(problem_category, problem_specific, crop) %>% 
  arrange(desc(n)) %>% 
  filter(!is.na(crop),crop != "Other", n>16
         ) %>%
  kable() %>% kable_minimal()



