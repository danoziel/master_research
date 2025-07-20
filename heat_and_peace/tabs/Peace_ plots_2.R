










df_gender <- data.frame(
  gender = c("Male",     "Female"),
  estimate = c(0.0045,   0.0039),
  conf.low = c(0.0021,   0.0022),
  conf.high = c(0.0069,  0.0056))


ggplot(df_gender, aes(x = estimate, y = gender, color = gender)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = 1) +
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed", size = 1) +
  labs(
    x = NULL,
    y = NULL,
    title = "The number of incidents in the month",
    color = "Gender"       # Set legend title here
  ) +
  scale_color_manual(
    values = c("Male" = "#1b355b", "Female" = "#952929"),
    breaks = c("Male", "Female"),    # This arranges Male first
    labels = c("Male", "Female")
  ) +
  theme_classic(base_size = 15, base_family = "serif") +
  xlim(-0.000625, 0.01)+
  theme(legend.position = "bottom")

























library(haven)
survey <- read_dta("C:/Users/Dan/Downloads/survey.dta")
View(survey)

survey %>% count(incom)


file.show("C:/Users/Dan/Downloads/full survey analysus mar 25.do")
readLines("C:/Users/Dan/Downloads/full survey analysus mar 25.do", n = 20)

file.show("C:/Users/Dan/Downloads/data prep.do")
file.show("C:/Users/Dan/Downloads/TS_analysis_mar_25.do")
file.show("C:/Users/Dan/Downloads/TS_analysis.do")


survey_terror_merged <- read_dta("C:/Users/Dan/Downloads/survey_terror_merged.dta")
survey_terror_ts <- read_dta("C:/Users/Dan/Downloads/survey_terror_ts.dta")


survey_terror_merged$incidents_TL1M
survey_terror_ts$incidents_TL1M
survey$incidents_TL1M

incidents_TL1M
negot_sp

#|=============================================================================

# Figure 1   ======================================================================
# Figure 1: time evolution of the support for peacemaking over the study period
#|=============================================================================


# local conflict_bars (bar conflict date, col(gs14) yaxis(2))
# local incident_bars (bar inc_day_yn date if kill_day==0, col(red*0.25) yaxis(2)) ///
#   (bar inc_day_yn date if kill_day>0, col(red*0.5) yaxis(2)) ///
#   (bar inc_day_yn date if kill_day>5, col(red*1) yaxis(2)) ///
#   (bar inc_day_yn date if kill_day>10, col(red*1.5) yaxis(2))
# 
# tw   `conflict_bars' (line negot_sp_long date, lc(black) lw(thin))  `incident_bars', ///
#     ytitle("More    <--    Support for Negotiation    -->    Less", margin(medium) axis(1)) ///
#     xlabel(12419(360)21546, labsize(small) notick angle(45)) ///
#     xtitle("") ///
#     legend(order( 1 6 - "Red Lines Indicate Incidents" - " " 2 3 4 5) ///
#            label (1 "Conflict Period") ///
#            label(6 "Negotiation Support") ///
#            label(2 "0 Killed") ///
#            label(3 "1-5 Killed") ///
#            label(4 "6-10 Killed") ///
#            label(5 ">10 Killed"))

#|=============================================================================

# === GEMINI ==========
#|=============================================================================



library(haven)
df <- read_dta("C:/Users/Dan/Downloads/survey_terror_ts.dta")
df <- df %>%
  select(date, conflict, inc_day_yn, kill_day, negot_sp_long)
df=df[1:72,]

library(ggplot2)
library(dplyr)
library(lubridate)

# --- 1. Load Your Data ---

# Convert the 'date' column to a Date object
df$date <- as.Date(df$date)

# --- 2. Data Preparation for Plotting ---

# Define the colors for incidents
incident_colors <- c(
  "0 Killed" = "#FFDDDD",   # Very light red
  "1-5 Killed" = "#FF9999",  # Lighter red
  "6-10 Killed" = "#FF4444", # Medium red
  ">10 Killed" = "#FF0000"   # Bright red
)

# Create a 'kill_category' for easier mapping in ggplot
df <- df %>%
  mutate(
    kill_category = case_when(
      inc_day_yn == 0 | is.na(inc_day_yn) ~ NA_character_, # No incident or NA incident_day_yn
      kill_day == 0 ~ "0 Killed",
      kill_day >= 1 & kill_day <= 5 ~ "1-5 Killed",
      kill_day >= 6 & kill_day <= 10 ~ "6-10 Killed",
      kill_day > 10 ~ ">10 Killed",
      TRUE ~ NA_character_ # Catch any other cases
    )
  ) %>%
  # Ensure the order of kill_category for legend
  mutate(kill_category = factor(kill_category, levels = c("0 Killed", "1-5 Killed", "6-10 Killed", ">10 Killed")))

# --- Determine the scale for the secondary axis ---
# Define the y-range for the bars on the primary axis.
# Let's ensure these values are positive and distinct from the negotiation line.
# Based on your data, negot_sp_long min is around 1.8.
# So, placing bars between 0.5 and 1.5 might still work, or even lower if you want them less prominent.
# Let's try placing them at a low, fixed height for "presence" indication.

# Min value of negot_sp_long is ~1.8
# Max value of negot_sp_long is ~2.55
# Let's try to place the bars from y=0.1 to y=0.5 on the primary axis.
Y_BAR_MIN_PRIMARY <- 0.1
Y_BAR_MAX_PRIMARY <- 0.5

# The range on the primary axis for bars is (Y_BAR_MAX_PRIMARY - Y_BAR_MIN_PRIMARY) = 0.4
# This range needs to map to 0-1 on the secondary axis.

p= ggplot(df, aes(x = date)) +
  
  # --- Conflict Bars (using geom_rect for shaded periods) ---
  geom_rect(data = filter(df, conflict == 1),
            aes(xmin = date - 0.5, xmax = date + 0.5, # Span the day (adjust width as needed)
                ymin = Y_BAR_MIN_PRIMARY, ymax = Y_BAR_MAX_PRIMARY, # Define the height band
                fill = "Conflict Period"),
            alpha = 0.3,
            inherit.aes = FALSE) +
  
  # --- Incident Bars (using geom_rect) ---
  geom_rect(data = filter(df, inc_day_yn == 1),
            aes(xmin = date - 0.5, xmax = date + 0.5, # Span the day
                ymin = Y_BAR_MIN_PRIMARY, ymax = Y_BAR_MAX_PRIMARY, # Define the height band
                fill = kill_category), # Fill color based on kill_category
            alpha = 0.8,
            inherit.aes = FALSE) +
  
  # --- Negotiation Support Line (Primary Y-axis) --- 
  geom_line(aes(y = negot_sp_long, color = "Negotiation Support"), linewidth = 0.8) +
  
  # --- Scales and Labels ---
  scale_y_continuous(
    name = expression(paste( bold("More ← "), "Support for Negotiation", bold(" → Less"))),
    # Define the secondary axis transformation and labels
    sec.axis = sec_axis(
      trans = ~ (. - Y_BAR_MIN_PRIMARY) / (Y_BAR_MAX_PRIMARY - Y_BAR_MIN_PRIMARY), # Custom transformation
      name = "Incident",
      breaks = c(0, 1) # , Only show labels for Absent and Present # labels = c("Absent", "Present")
    ),
    limits = c(0, max(df$negot_sp_long, na.rm = TRUE) * 1.1),
    # Ensure y-axis starts at 0 or a reasonable low value, and extends beyond max negot_sp_long
    expand = c(0, 0) # No padding on the y-axis
  ) +
  scale_x_date( date_breaks = "1 year",
                date_labels = "%Y",
                expand = c(0, 0) ) + labs(x = ""
                ) +
  scale_fill_manual(
    name = "Event Type", # A more general legend title for fill
    values = c("Conflict Period" = "grey70", incident_colors),
    breaks = c("Conflict Period", names(incident_colors)),
    labels = c("Conflict Period", "0 Killed", "1-5 Killed", "6-10 Killed", ">10 Killed")
  ) +
  scale_color_manual( name = "Negotiation Support",
                      values = c("Negotiation Support" = "black")
  ) +
  # --- Legend Customization ---
  guides( fill = guide_legend(order = 1,title = NULL,override.aes = list(alpha = 1, linewidth = 0 )),
          color = guide_legend( order = 2, title = NULL )
  ) +
  theme_minimal() + theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8)),
    axis.title.y.left = element_text(margin = margin(r = 10)),
    legend.position = "right",
    legend.box = "vertical",
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(), # Remove vertical grid lines if they clutter with daily bars
    panel.grid.minor.x = element_blank()
  )

print(p)
#    1000 X 310

#|=============================================================================

# === exaple ==========
#|=============================================================================

library(ggplot2)
library(dplyr)

# --- 1. Create Dummy DataFrame ---
# Generate years from 1980 to 2020
years <- 1980:2020
num_years <- length(years)

set.seed(456) # Set a new seed for reproducibility of this simulation
happiness <- runif(num_years, min = 1, max = 5)

# Simulate raw chocolate consumption in kg (e.g., from 0.01 to 60 kg)
chocolate_kg <- runif(num_years, min = 0.01, max = 60)

# Create the dummy DataFrame
dummy_df <- data.frame(
  year = years,
  happiness = happiness,
  chocolate_kg = chocolate_kg
)

# --- 2. Categorize Chocolate Consumption ---
dummy_df <- dummy_df %>%
  mutate(
    # Create the categorical variable based on kg thresholds
    chocolate_category = case_when(
      chocolate_kg <= 1 ~ "Up to 1kg",
      chocolate_kg > 1 & chocolate_kg <= 10 ~ "1-10kg",
      chocolate_kg > 10 & chocolate_kg <= 50 ~ "10-50kg",
      chocolate_kg > 50 ~ "More than 50kg",
      TRUE ~ NA_character_ # Catch any unhandled cases, though should not occur with Inf
    ),
    # Map these categories to numeric values (0-1) for plotting height
    # This ensures the bars' heights correspond to positions on the 0-1 secondary axis
    chocolate_category_value = case_when(
      chocolate_kg <= 1 ~ 0.25, # Maps "Up to 1kg" to 0.25 on the 0-1 scale
      chocolate_kg > 1 & chocolate_kg <= 10 ~ 0.50, # Maps "1-10kg" to 0.50
      chocolate_kg > 10 & chocolate_kg <= 50 ~ 0.75, # Maps "10-50kg" to 0.75
      chocolate_kg > 50 ~ 1.00, # Maps "More than 50kg" to 1.00
      TRUE ~ NA_real_
    )
  ) %>%
  # Ensure chocolate_category is a factor with desired order for legend and coloring
  mutate(chocolate_category = factor(chocolate_category,
                                     levels = c("Up to 1kg", "1-10kg", "10-50kg", "More than 50kg")))


# --- 3. Define Y-axis Properties and Transformations ---
PRIMARY_Y_MIN <- 1
PRIMARY_Y_MAX <- 5

SECONDARY_Y_MIN <- 0
SECONDARY_Y_MAX <- 1

# Transformation for plotting chocolate_category_value (0-1) onto the primary Y-axis scale (1-5)
# This scales the 0.25-1.00 category values to the 1-5 range for visual plotting.
plot_chocolate_trans <- function(x) {
  x * (PRIMARY_Y_MAX - PRIMARY_Y_MIN) + PRIMARY_Y_MIN
}

# Inverse transformation for the secondary axis labels (scaling primary 1-5 back to 0-1)
# This function defines how the secondary axis numbers are derived from the primary axis numbers.
secondary_axis_trans <- function(x) {
  (x - PRIMARY_Y_MIN) / (PRIMARY_Y_MAX - PRIMARY_Y_MIN)
}


# --- 4. Generate the ggplot2 Plot ---
pE <- ggplot(dummy_df, aes(x = year)) +
  
  # --- Secondary Y-axis: Chocolate Consumption Bar Plot (using geom_rect) ---
  # Bars will start at PRIMARY_Y_MIN (1) and extend upwards based on chocolate_category_value.
  # The fill color will indicate the chocolate category.
  # Placed BEFORE geom_line so the line is drawn on top.
  geom_rect(aes(xmin = year - 0.4, xmax = year + 0.4, # Defines the width of each bar for each year
                ymin = PRIMARY_Y_MIN, # Bars start at the explicit minimum of the primary Y-axis (1)
                ymax = PRIMARY_Y_MAX, 
                fill = chocolate_category), # Fill color based on the discrete category
            alpha = 0.7, # Transparency of the bars
            inherit.aes = FALSE) + # Important: ensures xmin/xmax/ymin/ymax are taken directly, not inherited from ggplot()
  
  # --- Primary Y-axis: Happiness Line Plot ---
  # Placed AFTER geom_rect, so the line is drawn on top of the bars.
  geom_line(aes(y = happiness, color = "Happiness Index"), linewidth = 1) +
  
  # --- Scales and Labels ---
  scale_y_continuous(
    name = "Happiness Index (1-5)",
    limits = c(PRIMARY_Y_MIN, PRIMARY_Y_MAX),
    expand = c(0, 0), # Remove padding from Y-axis
    sec.axis = sec_axis(
      trans = ~ secondary_axis_trans(.), # Apply the inverse transformation for secondary axis labels
      name = "Chocolate Consumption Level (0-1)", # New name for the secondary axis
      breaks = c(SECONDARY_Y_MIN, SECONDARY_Y_MAX), # Explicitly show 0 and 1
      labels = c("Lower Level", "Higher Level") # Custom labels for clarity at 0 and 1
    )
  ) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(min(years), max(years), by = 5), # Show breaks every 5 years
    expand = c(0, 0) # Remove padding from X-axis
  ) +
  labs(title = "Happiness Index vs. Categorical Chocolate Consumption Over Time") +
  
  scale_color_manual(
    name = NULL, # No title for the line legend
    values = c("Happiness Index" = "darkblue")
  ) +
  scale_fill_manual(
    name = "Chocolate Category (kg/year)", # Title for the categorical fill legend
    values = c(
      "Up to 1kg" = "#F0F8FF",      # AliceBlue (very light)
      "1-10kg" = "#ADD8E6",         # LightBlue
      "10-50kg" = "#6A5ACD",        # SlateBlue (medium)
      "More than 50kg" = "#4B0082"  # Indigo (dark)
    ),
    drop = FALSE # Ensure all category levels are shown in legend even if not present in data
  ) +
  # --- Legend Customization ---
  guides(
    color = guide_legend(order = 1), # Place line legend first
    fill = guide_legend(order = 2,
                        override.aes = list(alpha = 1)) # Make the fill legend swatches fully opaque
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y.right = element_text(margin = margin(l = 10)), # Space for right axis title
    axis.title.y.left = element_text(margin = margin(r = 10)),  # Space for left axis title
    legend.position = "right", # Place legend on the right side
    legend.box = "vertical",
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_text(size = rel(0.9), face = "bold")
  )

print(pE)






#|=============================================================================

# Figure  =====================================================================
# 
#|=============================================================================

# full survey analysus mar 25.DO  |||
# reghdfe `q' i.gender i.age_range i.incom i.political_spectrum i.relig_scale i.education, a(date1)
# coefplot, drop(_cons) xline(0)

library(dplyr)
library(broom)
library(ggplot2)

df = survey_terror_merged


library(dplyr)
library(broom)
library(ggplot2)

# Run the regression (as factors for categorical variables)
model <- lm(
  negot_sp_long ~ factor(gender) + factor(age_range) + factor(incom) +
    factor(political_spectrum) + factor(relig_scale) + factor(education) + factor(date1),
  data = df
)

# Tidy the model and filter out the intercept
coefs <- tidy(model) %>%
  filter(term != "(Intercept)")

# Optional: remove date1 fixed effects from the plot
coefs_main <- coefs %>%
  filter(!grepl("date1", term))

# Plot the coefficients (like coefplot in Stata)
ggplot(coefs_main, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Coefficient Estimate",
    y = "Predictor",
    title = "Regression Coefficients: Individual Attributes"
  ) +
  theme_minimal()




# foreach var in gender age_range incom political relig education {
#   
#   levelsof `var', local(values)
#     local models
# 
#     foreach i in `values' {
#       eststo `var'_`i': qui reghdfe `q' inc_dayL1M if `var'==`i' ,a(gender age incom political relig education security_n before ) vce(cluster date1) 
#         local models `models' `var'_`i'
#     }
# 
#     coefplot `models', drop(_cons) xline(0)
#       graph save hetero_`var'.gph
#     graph export hetero_`var'.png
#     }


library(dplyr)
library(broom)
library(ggplot2)
library(purrr)

# List the subgroup variables (attributes)
key_vars <- c("gender", "age_range", "incom", "political_spectrum", "relig_scale", "education")
results_list <- list()

for (var in key_vars) {
  unique_vals <- df %>% pull(var) %>% unique() %>% na.omit()
  controls <- setdiff(key_vars, var)
  models <- lapply(unique_vals, function(val) {
    sub_df <- df %>% filter(.data[[var]] == val)
    # Build list of all required columns for the model
    model_cols <- c("negot_sp_long", "inc_dayL1M", controls)
    valid_rows <- sub_df %>%
      select(all_of(model_cols)) %>%
      filter(if_all(everything(), ~!is.na(.)))
    if (nrow(valid_rows) >= 2) {
      formula <- as.formula(
        paste("negot_sp_long ~ inc_dayL1M +", paste(controls, collapse = " + "))
      )
      mod <- lm(formula, data = valid_rows)
      broom::tidy(mod) %>%
        mutate(subgroup = var, subgroup_value = val)
    } else {
      NULL
    }
  })
  results_list[[var]] <- bind_rows(models)
}


all_results <- bind_rows(results_list)

# Remove intercept for plotting
plot_data <- all_results %>% filter(term != "(Intercept)")

# Example: plot for 'gender'
plot_data_gender <- plot_data %>% filter(subgroup == "gender")

ggplot(plot_data_gender, aes(x = estimate, y = term, color = as.factor(subgroup_value))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error),
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Coefficient Estimate",
    y = "Predictor",
    color = "Group",
    title = "Subgroup Regression Coefficients (Gender)"
  ) +
  theme_minimal()




# graph combine hetero_gender.gph hetero_education.gph hetero_incom.gph hetero_age_range.gph hetero_relig.gph hetero_political.gph, xcommon
# graph save hetero_all.gph
# graph export hetero_all.png

library(ggplot2)

ggplot(plot_data, aes(x = estimate, y = term, color = as.factor(subgroup_value))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error,
                     xmax = estimate + 1.96*std.error),
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~subgroup, scales = "free_y") +
  labs(
    x = "Coefficient Estimate",
    y = "Variable",
    color = "Group",
    title = "Subgroup Regression Coefficients by Attribute"
  ) +
  theme_minimal()




# foreach var in political {
#   levelsof `var', local(values)
#     local models
# 
#     foreach i in `values' {
#       eststo `var'_`i': qui reghdfe `q' inc_district_*L1M if `var'==`i' ,a(gender age incom political relig education security_n before ) vce(cluster date1) 
#         local models `models' `var'_`i'
#     }
# 
#     coefplot `models', drop(_cons) xline(0)
#       graph save hetero_`var'_location.gph
#     graph export hetero_`var'_location.png
#     }
  

location_vars <- grep("^inc_district_.*L1M$", names(df), value = TRUE)


library(dplyr)
library(broom)
library(purrr)

# For each value of political, run a regression for incidents by location
unique_political <- df %>% pull(political_spectrum) %>% unique() %>% na.omit()





models_loc <- lapply(unique_political, function(val) {
  sub_df <- df %>% filter(political_spectrum == val)
  # Ensure complete cases for the model
  model_cols <- c("negot_sp_long", location_vars, "gender", "age_range", "incom", "political_spectrum","relig_scale" , "education", "security_n", "before_intifada2")
  valid_rows <- sub_df %>%
    select(all_of(model_cols)) %>%
    filter(if_all(everything(), ~!is.na(.)))
  if (nrow(valid_rows) >= 2) {
    formula <- as.formula(
      paste("negot_sp_long ~", paste(location_vars, collapse = " + "), 
            "+ gender + age_range + incom + political_spectrum + relig_scale + education + security_n + before_intifada2")
    )
    mod <- lm(formula, data = valid_rows)
    broom::tidy(mod) %>%
      mutate(subgroup = "political_spectrum", subgroup_value = val)
  } else {
    NULL
  }
})

results_loc <- bind_rows(models_loc) %>%
  filter(term != "(Intercept)") # Remove intercept for plotting


ggplot(results_loc, aes(x = estimate, y = term, color = as.factor(subgroup_value))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error,
                     xmax = estimate + 1.96*std.error),
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Coefficient Estimate",
    y = "Incident Location Variable",
    color = "Political Group",
    title = "Effect of Incident Location by Political Group"
  ) +
  theme_minimal()



  # --- PLOT 4

# foreach var in political {
#   levelsof `var', local(values)
#     local models
# 
#     foreach i in `values' {
#       eststo `var'_`i': qui reghdfe `q' inc_district_*L1M if `var'==`i' ,a(gender age incom political relig education security_n before ) vce(cluster date1) 
#         local models `models' `var'_`i'
#     }
# 
#     coefplot `models', drop(_cons) xline(0)
#       graph save hetero_`var'_location.gph
#     graph export hetero_`var'_location.png
#     }
  

library(dplyr)
library(broom)
library(ggplot2)
library(purrr)

# 1. Identify incident-by-location variables
location_vars <- grep("^inc_district_.*L1M$", names(df), value = TRUE)

# 2. Loop over each political group, fit the model
unique_political <- df %>% pull(political_spectrum) %>% unique() %>% na.omit()

results_list <- lapply(unique_political, function(val) {
  sub_df <- df %>% filter(political_spectrum == val)
  # Ensure no NAs in model variables
  model_cols <- c("negot_sp_long", location_vars, "gender", "age_range", "incom", "political_spectrum", "relig_scale", "education", "security_n", "before_intifada2")
  valid_rows <- sub_df %>%
    select(all_of(model_cols)) %>%
    filter(if_all(everything(), ~!is.na(.)))
  if (nrow(valid_rows) >= 2) {
    formula <- as.formula(
      paste("negot_sp_long ~", paste(location_vars, collapse = " + "),
            "+ gender + age_range + incom + political_spectrum + relig_scale + education + security_n + before_intifada2")
    )
    mod <- lm(formula, data = valid_rows)
    broom::tidy(mod) %>%
      mutate(political_group = val)
  } else {
    NULL
  }
})

results_loc <- bind_rows(results_list) %>%
  filter(term != "(Intercept)")

# 3. Plot (like Stata coefplot)
ggplot(results_loc, aes(x = estimate, y = term, color = as.factor(political_group))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error,
                     xmax = estimate + 1.96*std.error),
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Coefficient Estimate",
    y = "Incident Location Variable",
    color = "Political Group",
    title = "Effect of Incident Location by Political Group"
  ) +
  theme_minimal()




# PLOT 5

# foreach var in political {
#   
#   levelsof `var', local(values)
#     local models
# 
#     foreach i in `values' {
#       eststo `var'_`i': qui reghdfe `q' inc_trgt_*L1M if `var'==`i' ,a(gender age incom political relig education security_n before ) vce(cluster date1) 
#         local models `models' `var'_`i'
#     }
# 
#     coefplot `models', drop(_cons) xline(0)
#       graph save hetero_`var'_trgt.gph
#     graph export hetero_`var'_trgt.png
#     }


library(dplyr)
library(broom)
library(ggplot2)
library(purrr)

# 1. Identify incident-by-target variables
target_vars <- grep("^inc_trgt_.*L1M$", names(df), value = TRUE)

# 2. Loop over each political group, fit the model
unique_political <- df %>% pull(political_spectrum) %>% unique() %>% na.omit()

results_target <- lapply(unique_political, function(val) {
  sub_df <- df %>% filter(political_spectrum == val)
  # Ensure no NAs in model variables
  model_cols <- c("negot_sp_long", target_vars, "gender", "age_range", "incom", "political_spectrum", "relig_scale", "education", "security_n", "before_intifada2")
  valid_rows <- sub_df %>%
    select(all_of(model_cols)) %>%
    filter(if_all(everything(), ~!is.na(.)))
  if (nrow(valid_rows) >= 2) {
    formula <- as.formula(
      paste("negot_sp_long ~", paste(target_vars, collapse = " + "),
            "+ gender + age_range + incom + political_spectrum + relig_scale + education + security_n + before_intifada2")
    )
    mod <- lm(formula, data = valid_rows)
    broom::tidy(mod) %>%
      mutate(political_group = val)
  } else {
    NULL
  }
})

results_trgt <- bind_rows(results_target) %>%
  filter(term != "(Intercept)")

# 3. Plot (like Stata coefplot)
ggplot(results_trgt, aes(x = estimate, y = term, color = as.factor(political_group))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error,
                     xmax = estimate + 1.96*std.error),
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Coefficient Estimate",
    y = "Incident Target Variable",
    color = "Political Group",
    title = "Effect of Incident Target by Political Group"
  ) +
  theme_minimal()






# graph combine hetero_political_location.gph hetero_political_trgt.gph, xcommon
# graph save hetero_political_2.gph
# graph export hetero_political_2.png





results_loc <- results_loc %>% mutate(panel = "Location")
results_trgt <- results_trgt %>% mutate(panel = "Target")
results_combined <- bind_rows(results_loc, results_trgt)


library(ggplot2)

ggplot(results_combined, aes(x = estimate, y = term, color = as.factor(political_group))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error,
                     xmax = estimate + 1.96*std.error),
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~panel, scales = "free_y") +
  labs(
    x = "Coefficient Estimate",
    y = "Variable",
    color = "Political Group",
    title = "Effect of Incident Location and Target by Political Group"
  ) +
  theme_minimal()






















  















  








