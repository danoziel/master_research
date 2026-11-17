








# Predictors of Drip Adoption -----
# [model_DI_6y] [model_F_6y]

## Results Table
sjPlot::tab_model(model_DI_6y, model_F_6y ,show.se = T,digits = 4,show.stat  = F)

# Coefficients plot # 700X450
models_list <- list("Drip   16-22" = model_DI_6y,"Flood 16-22" = model_F_6y)
jtools::plot_summs(models_list, colors = c("#026bb9","#619847"), point.size = 4,line.size = 1.2, point.shape = 19) + theme(panel.grid.major.x = element_blank()) + geom_text(aes(label = round(estimate,2) ),size=4,position = position_dodge(1.5))







######## Location on pipe ----

# [summary_data_ir] [summary_data_ir] [summary_data_years]

library(ggplot2)  #45818e #76a5af  #a2c4c9
library(dplyr)
library(forcats)


summary_data <- summary_data_ir
summary_data <- summary_data_source_ramthal


summary_data %>% 
  filter(as.integer(proximity_location_f) < 15,
         proximity_location_f != "-998") %>%
  arrange(as.numeric(as.character(proximity_location_f))) %>%
  mutate(
    x_axis_labels = paste0(proximity_location_f, "\n(", N, ")"),
    x_axis_labels = fct_inorder(factor(x_axis_labels))
  ) %>%
  
  ggplot(aes(x = x_axis_labels, y = Mean)) +
  geom_bar(stat = "identity", fill = "#76a5af") +
  geom_text(aes(label = scales::percent(Mean, accuracy = 1)),
            position = position_dodge(width = 0.9),vjust = 2,size = 5,color = "black",fontface = "bold",
  ) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "gray30") +
  theme_minimal() +
  labs(
    x = "Proximity Location\n(Sample Size)", y = "% of HH", caption = paste0("N = 397") 
  ) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), # for ir AND source_ramthal
                     labels = scales::percent) +
  theme_minimal(base_family = "serif")+
  theme(axis.text.x = element_text(size = 12), panel.grid = element_blank())


summary_data <- summary_data_years 

summary_data %>% 
  filter(as.integer(proximity_location_f) < 15,
         proximity_location_f != "-998") %>%
  arrange(as.numeric(as.character(proximity_location_f))) %>%
  mutate(
    x_axis_labels = paste0(proximity_location_f, "\n(", N, ")"),
    x_axis_labels = fct_inorder(factor(x_axis_labels))
  ) %>%
  
  ggplot(aes(x = x_axis_labels, y = Mean)) +
  geom_bar(stat = "identity", fill =  "#76a5af") +
  geom_text(aes(label = sprintf("%.2f", Mean)), vjust = 2, size = 5, color = "black", fontface = "bold",
            ) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "gray30") +
  coord_cartesian(ylim = c(0, 3.1)) + 
  theme_minimal() +
  labs(
    x = "Proximity Location\n(Sample Size)", y = "Mean years of use", caption = paste0("N = 397") 
  ) +
  theme_minimal(base_family = "serif")+
  theme(axis.text.x = element_text(size = 12), panel.grid = element_blank())


# Designed Water Release vs. Actual Usage by Project Year ----
# [jain_report] [ir_year_season]

jain_report %>% select(year,project_year, kharif, rabi) %>% 
  pivot_longer(cols = c(kharif, rabi),names_to = "season",values_to = "pct_jain"
  ) %>% mutate(year=as.numeric(year)) %>% 
  left_join(ir_year_season %>% select(year,season, pct_usage)
  ) %>% select(-year)  %>% 
  mutate(
    season= ifelse(season == "kharif","Kharif","Rabi" ),
    project_year = factor(project_year, levels = c("1st", "2nd", "3rd", "4th", "5th")),
    season = factor(season, levels = c("Kharif", "Rabi"))) %>%
  ggplot(aes(x = project_year)) +
  geom_col(aes(y = pct_jain, fill = season)) +
  geom_text(aes(y = pct_jain, label = round(pct_jain * 100, 0)), # label = scales::percent(pct_jain, accuracy = 1)),
            vjust = -.75,size = 4)+
  geom_text(aes(y = pct_usage, label = round(pct_usage * 100, 0)),
            vjust = -.75,size = 4,color = "black")+
  geom_line(aes(y = pct_usage, group = 1),linewidth = 2,color = "#434343ff") +
  geom_point(aes(y = pct_usage),size = 2.5,shape = 21,fill = "#efefefff",color = "#434343ff"
  ) +
  facet_wrap(~ season, ncol = 1, scales = "fixed") + # facets: kharif on top, rabi bottom
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_fill_manual(values = c("Kharif" = "#6fa8dcff","Rabi"= "#a89072ff")) +
  labs(
    x = "Project Year",  y = "% of HH",
    title = "Designed Water Release vs. Actual Usage by Project Year", fill = NULL
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )



# [Water_Delivery_Delay] ----

ggplot(Water_Delivery_Delay, aes(x = factor(month, levels = month.abb))) +
  geom_col(aes(y = pct, fill = season))+
  geom_text(aes(y = pct, label = scales::percent(pct, accuracy = 1)),
            vjust = 1.2,size = 4)+
  scale_fill_manual(values = c("Kharif" = "#6fa8dcff","Rabi"= "#a89072ff","Summer"="gray"))+
  labs(
    x =NULL,  y = "% of HH",
    title = "Month in which usually water supply begins", fill = NULL
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 12),
    legend.position = c(0.08, 0.85),
    legend.text = element_text(size=12)
  )

