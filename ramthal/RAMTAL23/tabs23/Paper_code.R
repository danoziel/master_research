# PAPER CODE ----

library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)

# # # 2.1. Regional Agricultural Practices (desc)
# # # TABLE A1: Crop Cultivation Area and Adoption Rates
# # # 2.5. Summary statistics and balance tests
# # # 3.1. Empirical Strategy
# # # 3.2.1. Agricultural Outcomes 
# # # 3.2.2. Income Effects 
# # # Assets
# # # 3.2.3. Education and Migration
# # # 3.3. Drip Irrigation Utilization 
# # # 3.4. Cost Benefit Analysis 
# # # # 4.1. Predictors of Drip Usage
# # # 4.2. Barriers to implementation
#===============================================================================
#  SUMMARY STATISTICS  ----
#===============================================================================
SUMMARY_STATISTICS |> kableExtra::kbl() |> kableExtra::kable_classic()

#===============================================================================
# # #   Section: Regional Agricultural Practices  ----

print(df_summaries)

# # Objects in "df_summaries":
# acres_per_hh_season  | Ave. acre land cultivated per hh in Rabi and Kharif
# GCA_share_by_crop    | % of gross cultivated area by crop category
# pct_hh_target_crops  | % of households growing "target crops"
# CROP_season_land_pct_AND_hh_grow_pct | % of season cultivated area and % of households growing per crop
# drip_OR_flood_irrigation_hh_share    | % Households using flood or drip irrigation
# water_source_hh_share | Water source for irrigation (% Households)
# asset_own_hh_share    | Households owning assets (% Households)
# farming_incom         | Average annual household agricultural income: per HH and per acre (Rs.)
# income_source_type    | Farms relying exclusively on agricultural revenue OR depending on off-farm income
# HH_total_incom        | Average total annual household income (Rs.)


#===============================================================================
# # #   Figure: Drip irrigation and water source [2016–2022]  ----

print(drip_water_source_DFs)

# # # Objects in "drip_water_source_DFs":
# # Percentage of households in which infrastructure has been installed
# # Percentage of farmers who stated that they are part of the Ramatel project
# # % of drip users 2021-22
# # % of floode users 2021-22
# # Percentage of area irrigated via drip irrigation and size of the area (in acres)
# # Percentage Of Drip Adopters
# # Cumulative percent of adopters by year
# # Percentage of NEW adopters by year
# # Average years of use for drip irrigation


#===============================================================================
# # #   Appendix Table: Crop Cultivation Area and Adoption Rates ----

# Table:  Crop acre land table by groups
crop_acre_summary <- crop_summary_Paper %>% select(Crop, starts_with("mean_acre_"))

# Table: Crop Percentage of growers by groups
crop_pct_summary <- crop_summary_Paper %>% select(Crop, starts_with("pct_growers_"))

print(crop_summary_Paper)
print(crop_acre_summary)
print(crop_pct_summary)

#===============================================================================
# # #   BALANCE TESTS                                     ----
#
##  Analysis: t-tests by treatment group
library(rstatix)

tbl_t1 <- df_for_ttest  %>%
  group_by(vars) %>%
  do(tidy(t.test(val_bl ~ in1_out0, data = .))) %>% ungroup() %>% 
  mutate(across(c(estimate2, estimate1, p.value), ~ round(., 3))) %>% 
  select(vars,estimate2,estimate1, p.value) %>% 
  rename(In = estimate2 ,Out = estimate1) 

tbl_t2 <- 
  df_for_ttest  %>%  filter(site_area == "South" ) %>% 
  group_by(vars) %>%
  do(tidy(t.test(val_bl ~ in1_out0, data = .))) %>% ungroup() %>% 
  mutate(across(c(estimate2, estimate1, p.value), ~ round(., 3))) %>% 
  select(vars, estimate2,estimate1, p.value) %>% 
  rename(In_south = estimate2 ,Out_south = estimate1, p_south=p.value ) 

t_test_table <-  tbl_t1 %>% full_join(tbl_t2)

print(t_test_table)


#===============================================================================
# IMPACT ESTIMATION                                                          ####
#===============================================================================
##  Environment Setup: Data Preparation   ----
# ..............................................................................



Impact_Agriculture
Impact_Economic
df_assets
Impact_Social


# ______________________________________________________________________________

###  D |  Drip Irrigation Utilization  ----

####  Drip adoption rate ----
#  Drip adoption by year and all 6 years (2017–2022)
df_drip_use_Y_1to6 <- irrigation_BL_to_22 %>% select(hh_id, starts_with("drip"),-drip_use_BL) %>%pivot_longer(-hh_id, names_to = "vars", values_to = "val_22")

#  In baseline
df_drip_use_bl <- irrigation_BL_to_22 %>% select(hh_id, drip_use_BL) %>% rename(val_bl=drip_use_BL)

#  Reported project participation
df_im_in_project <- rmtl_cntrl_vars %>% select(hh_id,im_in_project) %>% pivot_longer(-hh_id, names_to = "vars", values_to = "val_22")

## Merge databases
df_drip_17_22 <- rbind(df_drip_use_Y_1to6,df_im_in_project) %>% left_join(df_drip_use_bl)


####  Irrigated land  ----
#   Share of drip area and drip irrigated acre.
#   Merge Endline and Baseline databases
df_drip_acre_pct <- 
  crop_acre_drip_22 %>% left_join(crop_acre_drip_BL) %>% # in impact.1.R
  rename_with(~ c("hh_id", "vars", "val_22", "val_bl"))

####  Water sources  ----
# Any source and project source (2016–2022)

# Endline: 
df_source_irri_22 <- a_source_irri %>%
  mutate(source_ramthal= ifelse(source_ramthal=="ramthal",1,0)) %>%
  mutate(across(c(source_borwell, source_canal, source_pond), ~ if_else(.x %in% c("ramthal", "rain"), 0, 1))) |> 
  mutate(any_source = if_else(source_borwell + source_canal + source_pond > 0, 1, 0)) |> 
  select(hh_id,source_ramthal  , any_source) |> 
  pivot_longer(cols = c(source_ramthal, any_source),names_to = "vars", values_to = "val_22")

# Baseline: 
df_source_irri_BL <- 
  bl13_source_irrigate |> mutate(irri_source_5y= ifelse(is.na(irri_source_5y ),0,1)) |> 
  summarise(val_bl=sum(irri_source_5y),.by=hh_id )|> mutate(val_bl= ifelse(val_bl==0,0,1)) |> 
  mutate(vars=c("any_source"))
df_source_irri_BL = df_source_irri_BL |> bind_rows(df_source_irri_BL |> mutate(val_bl=0, vars="source_ramthal"))


####  Merge Irrigated land and Water sources databases ----
df_drip_land_and_water_source <- 
  df_drip_acre_pct %>%
  mutate(val_22=ifelse(vars=="drip_pct" & val_22==1,NA,val_22) ) %>%
  rbind( df_source_irri_22 %>% left_join(df_source_irri_BL) )
  

# ------------------------------------------------------------------------------
##  Regression Estimation and Table Export                                    ----
# ..............................................................................
### Outcome variables: building the data frame for analysis ----

# Combine all modules and attach household controls
df_reg_22 <- bind_rows(
  Impact_Agriculture,
  Impact_Economic,
  df_assets,
  Impact_Social,
  df_drip_17_22,
  df_drip_land_and_water_source
  ) %>% 
  left_join(rmtl_cntrl_vars)

### Control variables: building the data frame for analysis ----

## The treatment variable (in_project = 1) and project areas
df_bl_00 <- rmtl_16_18_22_sample |> select(hh_id, in_project, cardinal_direction)

## Plot distance from the project boundary – positive values for 'inside', negative values for 'outside'.
df_bl_01 <- dist_boundary_vars

## hh haed_age. gender, education level [1-6 index]
df_bl_02 <- hh_head_bl |> select(hh_id, hh_head_age, hh_head_gender, hh_head_edu_level )

## Total acre  +  housing type: 1 Kutcha House, 2 Semi Pucca House, 3 Pucca House 
quantile(rmtl_baseline2016$D2, 0.995, na.rm = TRUE)
df_bl_03 <-  rmtl_baseline2016 %>%
  transmute(hh_id,
            housing_str321 = ifelse(B1 == 1, 3, ifelse(B1 == 3, 1, 2)),
            total_acre16   = D2,
            total_acre16   = if_else(total_acre16 > quantile(total_acre16, 0.998, na.rm = TRUE),
                                     NA_real_, total_acre16))

## job_income_sourceS: Salaried job or Casual work or daily labor by current household members
#  govPnsin_scheme: Government pension or scheme
#  rent_property:Rent/lease of property or land (land, house, vehicle, electronic appliances, tractor, etc.)
df_bl_04 <- rmtl_baseline2016 %>%
  select(hh_id, F5_source, F6_source, F7_source, F9_source, F10_source, F11_source) %>%
  mutate(across(-hh_id, ~ replace(.x, .x == 2, 0))) %>%
  transmute(hh_id,
            job_income_sourceS = F5_source + F6_source + F7_source + F11_source,
            govPnsin_scheme    = F9_source,
            rent_property      = F10_source)

## Asset ownership – binary variable: livestock_dairy, Bullock, Tractor, Plough, Thresher, Seed_drill, Motorcycle, Fridge
df_bl_05 <- df_bl_assets %>% mutate(across(-hh_id, ~ replace_na(.x, 0))) %>% 
  select(hh_id, Livestock_dairy = livestock_dairy_dum, Bullock = bullocks_dum, Tractor = tractor_dum, Plough = plough_dum, Thresher = thresher_dum, Seed_drill = seed_drill_dum, Motorcycle = motorcycles_dum, Fridge = fridge_dum)

# Final data for analysis
# rmtl_cntrl_vars <- 
list(df_bl_00, df_bl_01, df_bl_02, df_bl_03, df_bl_04, df_bl_05) %>%
  reduce(left_join, by = "hh_id") |> right_join(hh_2022)



###   Econometric Specifications  ----
# fml_bl: Regress baseline outcome on treatment indicator.
# fml_a:  Regress endline outcome on treatment indicator without controls.
# fml_b:  ANCOVA model: regress endline outcome on treatment, controlling for baseline outcome and household covariates.
# fml_c:  Spatial RDD model: regress endline outcome on treatment, boundary distance, interaction, baseline outcome, and household covariates.

fml_bl <- val_bl ~ in_project
fml_a <- val_22 ~ in_project
fml_b <- val_22 ~ in_project + val_bl + hh_head_age + hh_head_gender + hh_head_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge
fml_c <- val_22 ~ in_project + dist_Km_boundary + in_project * dist_Km_boundary + val_bl + hh_head_age + hh_head_gender + hh_head_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge

# Model grouping lists for mapping functions
formula_BL <- list(model_BL = fml_bl)
formulas_all <- list(model_a1 = fml_a, model_b1 = fml_b)
formulas_south <- list(model_b2 = fml_b, model_c2 = fml_c)

###   Functions for regression formulas  ---- 

# 1. Baseline Model Estimation (Full Sample)
#    Compute control group baseline means by outcome variable in wide format.
Intercept_bl <- 
  df_reg_22 %>% filter(in_project==0) %>% group_by(vars) %>% summarise(Mean = mean(val_bl,na.rm=T)) %>% pivot_wider (names_from = "vars", values_from = "Mean") %>% mutate(metric="(Intercept)", model_name="Model_BL") %>% select(model_name, metric,everything() ) 

# Fit baseline models across outcomes, extract treatment coefficients and fit statistics.
final_table_bl <-
  map_dfr(formula_BL, function(fml) {
    df_reg_22 %>% 
      group_by(vars) %>% 
      summarise(
        m = list(lm(fml, data = pick(everything()))),
        c = list(tidy(m[[1]]) %>% filter(term == "in_project") %>% select(estimate, std.error, p.value)),
        s = list(glance(m[[1]]) %>% select(nobs, r.squared)),
        .groups = "drop"
      ) %>% 
      unnest(c(c, s))
  }, .id = "model_name") %>% 
  select(-m) %>% 
  pivot_longer(c(estimate, std.error, p.value, nobs, r.squared), names_to = "metric") %>% 
  pivot_wider(names_from = vars, values_from = value) 

# 2. Endline Model Estimation | Full Sample (Model A and B)
#    Compute control group endline means by outcome variable for full sample.
control_mean <- 
  df_reg_22 %>% filter(in_project==0) %>% group_by(vars) %>% summarise(Mean = mean(val_22,na.rm=T)) %>% pivot_wider (names_from = "vars", values_from = "Mean") %>% mutate(model_name="Model_A1",metric="control_mean") %>% select(model_name, metric,everything() ) 

# Fit unadjusted and ANCOVA models across outcomes and reshape extracted metrics to wide format.
final_table <- 
  map_dfr(formulas_all, function(fml) {
    df_reg_22 %>% 
      group_by(vars) %>% 
      summarise(
        m = list(lm(fml, data = pick(everything()))),
        c = list(tidy(m[[1]]) %>% filter(term == "in_project") %>% select(estimate, std.error, p.value)),
        s = list(glance(m[[1]]) %>% select(nobs, r.squared)),
        .groups = "drop"
      ) %>% 
      unnest(c(c, s))
  }, .id = "model_name") %>% 
  select(-m) %>% 
  pivot_longer(c(estimate, std.error, p.value, nobs, r.squared), names_to = "metric") %>% 
  pivot_wider(names_from = vars, values_from = value) 

# 3. Endline Model Estimation | Southern Boundary Sample
#    Restricted to within 1.5 Km of the boundar (Model B and C)
df_reg_22_south <- 
  df_reg_22 %>% 
  mutate(dist_Km_boundary=ifelse(dist_to_boundary_m > 1600, NA,dist_Km_boundary)
  ) %>% filter(cardinal_direction =="south")

# Compute control group endline means across outcomes for the restricted southern sample.
control_mean_south <- 
  df_reg_22_south %>% filter(in_project==0) %>% group_by(vars) %>% summarise(Mean = mean(val_22,na.rm=T)) %>% pivot_wider (names_from = "vars", values_from = "Mean") %>% mutate(model_name="Model_A2",metric="control_mean") %>% select(model_name, metric,everything() )

# Fit ANCOVA and spatial discontinuity models on the southern sample and reshape extracted metrics to wide format.
final_table_south <- 
  map_dfr(formulas_south, function(fml) {
    df_reg_22_south %>% 
      group_by(vars) %>% 
      summarise(
        m = list(lm(fml, data = pick(everything()))),
        c = list(tidy(m[[1]]) %>% filter(term == "in_project") %>% select(estimate, std.error, p.value)),
        s = list(glance(m[[1]]) %>% select(nobs, r.squared)),
        .groups = "drop"
      ) %>% 
      unnest(c(c, s))
  }, .id = "model_name") %>% 
  select(-m) %>% 
  pivot_longer(c(estimate, std.error, p.value, nobs, r.squared), names_to = "metric") %>% 
  pivot_wider(names_from = vars, values_from = value) 

# 4. Output Table Compilation
#    Combine all results into a single output table
table_results <- Intercept_bl |> rbind(final_table_bl) |> 
  rbind(control_mean) |> rbind(final_table) |> 
  rbind(control_mean_south) |> rbind(final_table_south)

###   Print Results Table       ----
# Split final results by module


# ---------  Agricultural Outcomes

table_agri   <- table_results %>% select(model_name, metric, any_of(unique(Impact_Agriculture$vars)))

# ---------  Income Effects

table_economic <- table_results %>% 
  select(model_name, metric, any_of(unique(Impact_Economic$vars))) %>% 
  rename(
    Farming_income_HH    = income,
    Input_irrigation     = input_irrigation,
    Input_labor          = input_labor,
    Input_mechanization  = input_mechanization,
    External_support     = k_income_assistance,
    Off_Farm_earnings    = k_income_independent,
    Total_days           = labor_days_total,
    Farming_income_acre  = revenue_per_acre
  )

table_assets <- table_results %>% select(model_name, metric, any_of(unique(df_assets$vars)))


# ---------  Education and Migration

table_social <- table_results %>% 
  select(model_name, metric, any_of(unique(Impact_Social$vars))) %>% 
  rename(
    `Household literacy`        = literacy_rates,
    `2nd generation education`  = edu_gen2,
    `Private school enrollment` = private_school,
    `Seasonal migration`        = migrants_work_seasonal,
    `Permanent migration`       = migrants_work_permanent,
    `Off-farm work`             = offFarm_work_pct,
    `Female off-farm work`      = woman_offFarmwork_pct
  )

## -- Socio-political point of view Questions
library(dplyr)
library(tidyr)
library(haven)
library(purrr)

# Data frame for socio-political point of view Questions
df_social_views_22 <- 
  rmtl_srvy22 %>% select(hh_id, farmers_hh, starts_with("h"))

# Question H1
H1 <- df_social_views_22 %>% select(hh_id, farmers_hh,h1 ) %>% 
  group_by(farmers_hh) %>% 
  summarise(h1=mean(h1, na.rm = T) ) 

# Question H2
H2 <- df_social_views_22 %>%
  mutate(h2_label = as_factor(h2)) %>%
  count(farmers_hh, h2_label) %>%
  group_by(farmers_hh) %>%
  mutate(pct = round(n / sum(n), 3)) %>% ungroup

# Question H3
H3 <- df_social_views_22 %>% 
  select(hh_id, farmers_hh, starts_with("h3_"),-h3__777 ) %>% 
  pivot_longer(-c(hh_id ,farmers_hh) ,names_to = "ans", values_to = "n") %>% 
  group_by(farmers_hh,ans) %>% 
  summarise(pct =mean(n),.groups = "drop" ) %>% 
  mutate(Ans = recode(ans,
                      "h3_1" = "Skill/knowledge",
                      "h3_2" = "Access to Gov. schemes",
                      "h3_3" = "Ability to get loans" , # Ability to get loans (credit)
                      "h3_4" = "Having money to invest", # Having money to invest in inputs and machinery"
                      "h3_5" = "Political connections",
                      "h3_6" = "Luck/Good fortune", # Good fortune/luck/divine grace
                      "h3_7" = "Hard work")  ) 


# Question H4
H4 <- df_social_views_22 %>% 
  select(hh_id, farmers_hh, starts_with("h4_"),-h4__777 ) %>% 
  pivot_longer(-c(hh_id ,farmers_hh) ,
               names_to = "ans", values_to = "n") %>% 
  group_by(farmers_hh,ans) %>% 
  summarise(pct =mean(n),.groups = "drop" ) %>% 
  mutate(Ans = recode(ans,
                      "h4_1" = "Skill/knowledge",
                      "h4_2" = "Access to Gov. schemes",
                      "h4_3" = "Ability to get loans" , # Ability to get loans (credit)
                      "h4_4" = "Having money to invest", # Having money to invest in inputs and machinery"
                      "h4_5" = "Political connections",
                      "h4_6" = "Luck/Good fortune", # Good fortune/luck/divine grace
                      "h4_7" = "Hard work") )

# Question H18-23
labels_df <- map_dfr(names(select(df_social_views_22, h18:h23)), function(col) {
  lbls <- attr(df_social_views_22[[col]], "labels")
  tibble(
    var = col,
    label_1 = names(lbls)[lbls == 1],
    label_10 = names(lbls)[lbls == 10]
  )
})

H18_H23 <- df_social_views_22 %>%
  select(farmers_hh, h18:h23) %>%
  mutate(across(h18:h23, ~ as.numeric(zap_missing(.x)))) %>%
  group_by(farmers_hh) %>%
  summarise(across(h18:h23, ~ round(mean(.x, na.rm = TRUE), 2)), .groups = "drop") %>%
  pivot_longer(-farmers_hh, names_to = "var", values_to = "mean") %>%
  pivot_wider(names_from = farmers_hh, values_from = mean) %>%
  left_join(labels_df, by = "var") %>%
  select(var, label_1, label_10, everything())


Chapter_3 <- list(
  setNames(H1,c(attr(df_social_views_22$h1, "label"), "Freq")),
  setNames(H2,c(attr(df_social_views_22$h2, "label"), "Freq")),
  setNames(H3,c(attr(df_social_views_22$h3, "label"), "Freq")),
  setNames(H4,c(attr(df_social_views_22$h4, "label"), "Freq")),
  H18_H23)

Chapter_3 %>% 
  iwalk(~ {
    cat("\n....................................................\n")
    cat(.y, "\n")
    print(.x)
  })




# ---------  Drip Adoption

table_drip_17_22 <- table_results %>% select(model_name, metric, any_of(unique(df_drip_17_22$vars)))



# ---------  Drip Irrigated Land and Water Source

table_drip_land_and_water_source <- table_results %>% 
  select(model_name, metric, any_of(unique(df_drip_land_and_water_source$vars))) %>% 
  rename(
    `Water source: Other than Ramthal`                   = any_source,
    `Land under drip-irrigation: Acre`                   = drip_acre,
    `Land under drip-irrigation: % of the cultivated land` = drip_pct,
    `Water source: Gov. System (Ramthal)`                = source_ramthal
  )


# ------------------------------------------------------------------------------









#===========================================================
# COST-BENEFIT ANALYSIS  ----
#===========================================================
 # ... ----
## ---  1: Core Parameters       ----

discount_rate <- 0.05
time_horizon  <- 20
years         <- 1:time_horizon

# Project capital expenditure per acre
capex_total       <- 7861100000
total_project_hh  <- 15000
mean_land_holding <- 9.2921942
capex_per_acre    <- capex_total / (total_project_hh * mean_land_holding)

# Discount factors across time horizon (t = 1 to 20)
discount_factors <- 1 / ((1 + discount_rate)^years)


## ---  2: Extract Empirical Inputs from Survey Data & Regression Models ----

# 2a. Regression estimation of overall project effect (INR/acre)
df_profit_acre <- Impact_Economic |> filter(vars == "revenue_per_acre") |> 
  left_join(rmtl_cntrl_vars |> select(hh_id, in_project))
m1 <- lm(val_22 ~ in_project, data = df_profit_acre )
m1_tidy <- tidy(m1, conf.int = TRUE, conf.level = 0.95)

# Reference farming profit outside project area (control group mean)
profit_out <- m1_tidy$estimate[m1_tidy$term == "(Intercept)"] * 1000

# 2b. Regression estimation of Project's drip effect
farm_project_activities <- Impact_Economic |>
  left_join(rmtl_cntrl_vars |> select(hh_id, in_project)) |>
  left_join(irrigation_BL_to_22 |> select(hh_id, drip_use_2022)) |> 
  filter(vars == "revenue_per_acre", in_project == 1)
m2 <- lm(val_22 ~ drip_use_2022, data = farm_project_activities )
m2_tidy <- tidy(m2, conf.int = TRUE, conf.level = 0.95)

# In-project mean profits (INR/acre)
profit_drip_coef <- m2_tidy$estimate[m2_tidy$term == "drip_use_2022"] * 1000
profit_in_no_drip <- m2_tidy$estimate[m2_tidy$term == "(Intercept)"] * 1000
profit_in_drip <- profit_in_no_drip + profit_drip_coef

## ---  3: CBA Helper Functions  ----

# Annual incremental profit calculated from adoption rate
calc_inc_profit <- function(adoption_rate) {
  (adoption_rate * profit_in_drip + (1 - adoption_rate) * profit_in_no_drip) - profit_out
}

# Net Present Value across a cash flow vector (t = 0 to t = n)
calc_npv <- function(cf, r) {
  t <- 0:(length(cf) - 1)
  sum(cf / ((1 + r)^t))
}

# Internal Rate of Return calculation
calc_irr <- function(cf) {
  if (all(cf <= 0)) return(NA_real_)
  irr_obj <- function(r) calc_npv(cf, r)
  res <- tryCatch(
    uniroot(irr_obj, interval = c(-0.9999, 10), extendInt = "yes")$root,
    error = function(e) NA_real_
  )
  return(res)
}


## ---  4: Metric Computations by Block ----

# Block A: Actual net farming profit.
# Empirical Regression Scenarios (Constant Annual Streams) ---
block_a_scenarios <- list(
  "Point Estimate"  = m1_tidy$estimate[m1_tidy$term == "in_project"] * 1000,
  "Lower Bound 95%" = m1_tidy$conf.low[m1_tidy$term == "in_project"] * 1000,
  "Upper Bound 95%" = m1_tidy$conf.high[m1_tidy$term == "in_project"] * 1000
)

# Computes 20-year CBA metrics (PVB, NPV, BCR, IRR) for each scenario:
results_block_a <- map_dfr(names(block_a_scenarios), function(scen_name) {
  annual_gain <- block_a_scenarios[[scen_name]]
  pvb         <- sum(annual_gain * discount_factors)
  npv         <- pvb - capex_per_acre
  bcr         <- pvb / capex_per_acre
  net_cf      <- c(-capex_per_acre, rep(annual_gain, time_horizon))
  irr         <- calc_irr(net_cf)
  
  tibble(
    Block    = "Block A: Regression Estimates",
    Scenario = scen_name,
    PVC      = round(capex_per_acre, 2),
    PVB      = round(pvb, 2),
    NPV      = round(npv, 2),
    BCR      = round(bcr, 3),
    IRR      = round(irr, 4))
})

Actual_net_farming_profit_metrics <- results_block_a
print(Actual_net_farming_profit_metrics)

# Block B: Dynamic Adoption Rate Simulations

# 1. Extract observed adoption rates from the impact results table, Model_A1: Mean Comparison
obs_years_cols <- c("drip_use_2017", "drip_use_2018", "drip_use_2019", "drip_use_2020", "drip_use_2021", "drip_use_2022")

# Pull control mean
ctrl_means <- table_drip_17_22 |>
  filter(tolower(model_name) == "model_a1", metric == "control_mean") |>
  select(all_of(obs_years_cols)) |> as.numeric()

# Total adoption in treatment group (control_mean + estimate) across 2017-2022
treat_effects <- table_drip_17_22 |>
  filter(tolower(model_name) == "model_a1", metric == "estimate") |>
  select(all_of(obs_years_cols)) |> as.numeric()

observed_6y <- ctrl_means + treat_effects

# Adoption rate trajectories across the 20-year horizon for Block B scenarios:
# 1. Constant 100% adoption for all 20 years
# 2. Years 1-6 pulled from regression table, years 7-20 fixed at 5%
# 3. Linear growth of 7% per year, capped at 85%
block_b_rates <- list(
  "Full Adoption"           = rep(1.0, time_horizon),
  "Observed Adoption Rates" = c(observed_6y, rep(0.05, time_horizon - length(observed_6y))),
  "Sustained Growth"        = pmin(0.07 * years, 0.85)
)

# Computes 20-year CBA metrics (PVB, NPV, BCR, IRR) for each Block B adoption scenario
results_block_b <- map_dfr(names(block_b_rates), function(scen_name) {
  rates_seq   <- block_b_rates[[scen_name]]
  annual_gain <- calc_inc_profit(rates_seq)
  pvb         <- sum(annual_gain * discount_factors)
  npv         <- pvb - capex_per_acre
  bcr         <- pvb / capex_per_acre
  net_cf      <- c(-capex_per_acre, annual_gain)
  irr         <- calc_irr(net_cf)
  
  tibble(
    Block    = "Block B: Adoption Simulations",
    Scenario = scen_name,
    PVC      = round(capex_per_acre, 2),
    PVB      = round(pvb, 2),
    NPV      = round(npv, 2),
    BCR      = round(bcr, 3),
    IRR      = round(irr, 4)
  )
})

print(results_block_b)

## Unified Summary Table                ----

cba_unified_summary <- bind_rows(results_block_a, results_block_b)

# Add `Profit` values
cba_unified_summary$Profit <- c(
  round(m1_tidy$estimate[m1_tidy$term == "in_project"] * 1000, 2),
  round(m1_tidy$conf.low[m1_tidy$term == "in_project"] * 1000, 2),
  round(m1_tidy$conf.high[m1_tidy$term == "in_project"] * 1000, 2),
  round(profit_in_drip - profit_out, 2), "","")

#  columns to rows
cba_transposed_summary <- as.data.frame(t(cba_unified_summary[, c("Profit", "PVC", "PVB", "NPV", "IRR", "BCR")]))
colnames(cba_transposed_summary) <- cba_unified_summary$Scenario
print(cba_transposed_summary)

#==============================================================================
# Predictors of Drip Adoption -----
#==============================================================================


### --- 1: Building the data frame of predictors variables for analysis ----

# Irrigation Indicators
df1 <- irrigation_BL_to_22 %>% 
  select(hh_id, drip_use_6y, flood_use_6y, ir_use_BL)

# Demographics, Land and Livestock Ownership
df3 <- rmtl_cntrl_vars %>%
  select(hh_id, in_project, 
         hh_head_age, hh_head_gender, literate_hh_pct, 
         total_acre16,total_livestock,
  ) %>% mutate(own_livestock_01=ifelse(total_livestock == 0,0,1))

# Sources of Income
df5 <- rmtl_baseline2016 %>%
  select(hh_id, F5_year, F6_year, F7_year, F10_year, F11_year) %>%
  pivot_longer(-hh_id ,names_to = "income_type_f",values_to = "income22")  %>%   
  group_by(hh_id) %>% 
  summarise(income_NonCrop = sum(income22,na.rm = T)/1000,.groups = "drop") %>% 
  mutate(income_NonCrop=ifelse(income_NonCrop == 0,0,1) 
  )

# Participation in Expansion
df6 <- rmtl_baseline2016 %>%
  select(hh_id,I34) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
  setNames(c("hh_id", "attended_to_trainings")
  )

##  Unified Data
df_project <-
  hh_2022 %>% rename(in_project =in1_out0 ) %>%
  left_join(df1) %>%
  left_join(df3) %>%
  left_join(df5) %>%
  left_join(df6) %>%
  filter(in_project == 1) %>%
  mutate(total_acre16=ifelse(total_acre16 > 39, NA, total_acre16 ),
         total_acre16=total_acre16/10, hh_head_age=hh_head_age/10)

### --- 2: Summary statistics ----
# Summary statistics for irrigated and non-irrigated plots: Drip and flood irrigation
df_project %>% 
  select(-flood_use_6y) %>% mutate(irrigation_method="Drip") %>% rename( use_6y=drip_use_6y) %>% 
  rbind(
    df_project %>% select(-drip_use_6y) %>% mutate(irrigation_method="Flood") %>% rename( use_6y=flood_use_6y)
    ) %>%   
  filter(!is.na(use_6y)) |> 
  mutate(use_6y= ifelse(use_6y == 1, "yes","no") 
         ) %>%
  summarise(
    hh_head_age = mean(hh_head_age,na.rm=T)*10,
    hh_head_gender= mean(hh_head_gender,na.rm=T),
    literate_hh_pct= mean(literate_hh_pct,na.rm=T),
    attended_to_trainings =mean(attended_to_trainings,na.rm=T),
    own_livestock_01 =mean(own_livestock_01,na.rm=T),
    income_NonCrop =mean(income_NonCrop,na.rm=T),
    ir_use_BL =mean(ir_use_BL,na.rm=T),
    total_acre16=mean(total_acre16,na.rm=T)*10,.by = c(irrigation_method, use_6y)
  ) %>%
  pivot_longer(cols = -c(irrigation_method, use_6y), names_to = "name", values_to = "val") %>%
  mutate(val= ifelse(!name %in% c("hh_head_age", "total_acre16"),val*100, val) ) %>%
  pivot_wider(names_from = c(irrigation_method, use_6y), values_from = val, names_sep = "_") %>% 
  select(name, Drip_yes, Drip_no, Flood_yes, Flood_no) %>%
  kableExtra::kable() %>%kableExtra::kable_minimal()


### Linear Probability Model  ---- 

# LPM: Drip Adoption (Ever used 2016-2022)
model_DI_6y <-
  lm(drip_use_6y ~ hh_head_age + hh_head_gender + literate_hh_pct + attended_to_trainings + own_livestock_01+income_NonCrop + ir_use_BL + total_acre16,
     data = df_project )

# LPM: Flood Adoption (Ever used 2016-2022)
model_F_6y <-
  lm(flood_use_6y ~ hh_head_age + hh_head_gender + literate_hh_pct + attended_to_trainings + own_livestock_01+income_NonCrop + ir_use_BL + total_acre16,
     data = df_project )

## Results Table
summary(model_DI_6y)
summary(model_F_6y)

















#==============================================================================
# Barriers to implementation ----
#============================================================================== ----

library(readxl)
library(tidyverse)

interviews_2024excel <- read_excel("C:/Users/Dan/Downloads/interviews_2024excel.xlsx", 
                                   sheet = "Copy of why_no_use")

int_2024 <- interviews_2024excel

rmtl_2024

rmtl_In_srvy2022 <- rmtl_srvy22 %>% filter(mm4 == 1, in1_out0 == 1)

# ..............................................................................

##  Drip adoption rates                ----
#  Farmer use the drip at least once since installation
#  (Only farmers who are connected to the Ramthal infrastructure)
why_01a <- rmtl_2024 %>% select(hh_id ,drip_1use_2useOwmDrip) %>% summarise(Mean = mean(drip_1use_2useOwmDrip,na.rm=T) ) |> mutate(var="Adopt Drip [2024 interviews]")
why_01b <-rmtl_In_srvy2022 |> left_join(irrigation_BL_to_22  %>% select(hh_id,drip_use_6y) ) %>% summarise(Mean = mean(drip_use_6y,na.rm=T) ) |> mutate(var="Adopt Drip [2022 survey]")

# ..............................................................................

##  Reasons low drip utilization       ----
#   Farmers cited personal reasons regarding the low utilization of the drip irrigation system.
why_q1_ans <- int_2024 %>%
  filter(Q1_why_little_or_no_drip_YN == 1) %>%
  summarise(across(starts_with("Q1_Ans"), ~ mean(.x %in% 1))) %>%
  pivot_longer(everything(), names_to = "reason", values_to = "pct") %>%
  arrange(desc(pct))

why_q1_category <- why_q1_ans %>%
  mutate(reason = sub("^Q1_Ans ", "", reason),
         category = case_when(
           reason %in% c("low pressure", "Insufficient water", "high pressure",
                         "Uncertainty of water availability", "No water supply")
           ~ "Insufficiency of water supply",
           reason %in% c("Didn't get any laterals", "Low company maintenance", "Low quality pipes",
                         "Shepherds cut the pipes", "Pipes were burned or stolen",
                         "Farmers break the plot pipe", "Farmers break the main pipe",
                         "Pipes damaged in accidents")
           ~ "Infrastructure damage",
           reason %in% c("Nearby village uses Ramtal to fill ponds",
                         "Farmers reconfiguring the pipes to flood")
           ~ "On-field pipe reconfiguration")) %>%
  filter(!is.na(category)) %>%
  summarise(pct = sum(pct), .by = category) 


names(why_q1_ans)[names(why_q1_ans) == "reason"] <- "Why did you use drip irrigation so little, or not at all?"
names(why_q1_category)[names(why_q1_category) == "category"] <- "Why did you use drip irrigation so little, or not at all?"

# ..............................................................................

##  Insufficiency of water supply      ----

# % of farmers reported not knowing their scheduled irrigation turn timing.

# Water supply timing - Source of information
why_02_timing <- as.data.frame(round(prop.table(table(rmtl_2024$water_supply_info)), 2)) |> 
  mutate(Know_water_supply_schedule = if_else(Var1=="waterman_call_to_farmer","Yes","No")) |> summarise(pct=sum(Freq),.by = Know_water_supply_schedule)

### Volume Water released               ----
### % Water released of the allocated: Data from an internal report by Jain
why_02_jain_report <-
  data.frame(
    year = c("2017", "2018", "2019", "2020", "2021"),
    project_year = c("1st", "2nd", "3rd", "4th", "5th"),
    kharif = c(0.79, 0.43, 0.13, 0.20, 0.24),
    rabi   = c(0.85, 0.88, 0.61, 0.78, 0.23)
  )

### Drip irrigation adoption by season ----

library(dplyr)
library(stringr)
library(tidyr)

# The proportion of farmers using drip irrigation by season/year, out of the total number of farmers who have use at least once (2022 Survey)
why_02_ir_year_season <- 
  rmtl_In_srvy2022 %>% filter(mm5 == 1) %>%
  select(hh_id, starts_with(c("m20_kharif", "m20_rabi")), -ends_with("_2022") 
  ) %>% 
  pivot_longer( -hh_id,
                names_to = c("season", "year"),
                names_pattern = "m20_(.*)_(.*)",
                names_transform = list(year = as.integer),
                values_to = "use_ir") %>%
  summarise(n = sum(use_ir, na.rm = T), N = n(),.by = c(year, season)
  ) %>%
  mutate(pct_users_the_water_for_irrigation = n / N)

# Dataframe merging
why_02_jain_and_usage <- why_02_jain_report %>% 
  pivot_longer(cols = c(kharif, rabi),names_to = "season",values_to = "pct_Water_released_jain"
  ) %>% mutate(year=as.numeric(year)) %>% 
  left_join(why_02_ir_year_season %>% select(year,season, pct_users_the_water_for_irrigation)
  ) %>% select(-year)

why_02_ir_season <- 
  why_02_ir_year_season %>%
  summarise(pct_users_the_water_for_irrigation = sum(n) / sum(N),.by = season) %>%
  mutate(season = str_to_title(season))


### Water delivery delay rates         ----
# 
# * 80% reported delayed water delivery during the Kharif season.
# * 38% reported delayed water delivery during the Rabi season.

# When water is provided, in which month does it start?  (2022 Survey)
why_02_Water_Delivery_Delay <- 
  rmtl_In_srvy2022 %>% select(mw12) |> 
  filter(mw12 %in% c(1:3, 6:12)) %>%
  mutate(
    season = if_else(mw12 %in% 6:10, "Kharif", "Rabi"),
    Water_delivery_timing = if_else(mw12 %in% c(6, 11), "On Time", "Delayed")
  ) %>%
  count(season, Water_delivery_timing) %>%
  mutate(pct = n / sum(n),.by = season, .keep = "unused"
  )




##  Infrastructure Damage              ----
# 
# * 58% of main distribution lines were reported damaged in the 2022 survey
why_03a_damage <- as.data.frame(round(prop.table(table(pipe_status_survey2022 = factor(rmtl_srvy22_24$m35, 1:2, c("Works", "Damaged")))), 2))
why_03a_pipe_damage <- why_03a_damage |> filter(pipe_status_survey2022 == "Damaged")
  
# * 82% of farmers interviewed in the 2024 survey reported damaged pipes on their plots.
why_03b_damage <- as.data.frame(round(prop.table(table(pipe_status_intrvw2024 = factor(rmtl_2024$tap_status_1damged_0not, 0:1, c("Works", "Damaged")))), 2))
why_03b_pipe_damage <- why_03b_damage |> filter(pipe_status_intrvw2024 == "Damaged")

# * 42% of farmers cited infrastructure damage as a direct reason for not using drip irrigation.

##  On-field pipe reconfiguration      ----

# Deliberate damage or accident / What caused the damage
why_04_damage_cause <- as.data.frame(round(prop.table(table(Damage_cause_Deliberate_or_Accident =rmtl_2024$tap_cut_by_)), 2))


# pct flooding farmers using ramthal water
why_04a_flooding_with_Ramthal <- as.data.frame(round(prop.table(table(Flooding_with_Ramthal_Intrvw24 = rmtl_2024$irri_jain_flood)), 2)) |> filter(Flooding_with_Ramthal_Intrvw24==1)
why_04b_flooding_with_Ramthal <- rmtl_In_srvy2022  %>% select(hh_id) %>% left_join(a_source_irri) %>%  
  mutate(source_ramthal=ifelse(source_ramthal=="ramthal",1,0)) |> summarise(Using_Ramthal_aswater_source_survey22 = mean(source_ramthal))

# Has it ever happened to you that farmers before you used a large amount, leaving you without water?
library(haven)

why_04_mm10 <- rmtl_In_srvy2022 |> filter(!is.na(mm10)) %>% 
  transmute(hh_id,location_on_pipe = mm9+1, mm10) |> 
  drop_na() |> 
  mutate(category = case_when(
    mm10 == 1  ~ "Never happened",
    mm10 %in% c(2,3,4) ~ "Happens", 
    TRUE ~ "Happens rarely"))

why_04_mm10_A <- why_04_mm10 %>% 
  count(mm10 = as_factor(mm10, levels = "both")) %>% 
  mutate(pct = n / sum(n), .keep = "unused")|> 
  rename(`Has it ever happened that a farmer before you used up all the water?` = mm10)

why_04_mm10_B <- why_04_mm10 %>% 
  count(category) %>% mutate(pct_of_respondents = n / sum(n), .keep = "unused")

why_04_mm10_C <- 
  why_04_mm10 %>% 
  filter(location_on_pipe >= 1) %>% 
  summarise(`Mean "location_on_pipe" of the respondents` = mean(location_on_pipe),.by = category)

#...............................................................................

# Location on the pipeline and related variables  ----
  
# Dataframe for location_on_pipe and related estimated water variables
why_04_proximity_to_filter <- # proximity_to_filter
    rmtl_In_srvy2022  %>% transmute(hh_id, proximity_location=mm9+1) %>% 
  left_join(
    irrigation_BL_to_22 %>% select(-ir_use_2021_22) %>% 
      mutate(ir_total_years = rowSums(across(starts_with("ir_use_2")), na.rm = TRUE)) %>% 
      select(hh_id, ir_use_6y, ir_total_years)
  ) %>%
  left_join(a_source_irri %>% select(hh_id,source_ramthal)) %>%  
  mutate(source_ramthal=ifelse(source_ramthal=="ramthal",1,0))

# Farmers irrigating AND Location on the pipeline
summary_data_ir <- why_04_proximity_to_filter %>% 
  filter(proximity_location < 20 ) %>% 
  mutate(location_on_pipe = factor(proximity_location)) %>%
  group_by(location_on_pipe) %>%
  summarise(N = n(),
            Mean = mean(ir_use_6y ), 
            se = sqrt((Mean * (1 - Mean)) / N),
            t_val = qt(0.975, df = max(0, N - 1)),
            ci_lower = Mean - (t_val * se),
            ci_upper = Mean + (t_val * se), .groups = 'drop'
            ) %>%
  mutate(ci_lower = if_else(ci_lower < 0, 0, ci_lower),
         ci_upper = if_else(ci_upper > 1, 1, ci_upper)
         ) %>% rename(Mean_Farmers_irrigating_pct = Mean)

# Farmers use Ramthal as source AND Location on the pipeline
summary_data_source_ramthal <- why_04_proximity_to_filter %>% 
  filter(proximity_location < 20 ) %>% 
  mutate(location_on_pipe = factor(proximity_location)) %>%
  group_by(location_on_pipe) %>%
  summarise(N = n(),
            Mean = mean(source_ramthal ), 
            se = sqrt((Mean * (1 - Mean)) / N),
            t_val = qt(0.975, df = max(0, N - 1)),
            ci_lower = Mean - (t_val * se),
            ci_upper = Mean + (t_val * se), .groups = 'drop'
            ) %>% mutate(
              ci_lower = if_else(ci_lower < 0, 0, ci_lower),
              ci_upper = if_else(ci_upper > 1, 1, ci_upper)
              )

# Total years of irrigation (2017-2022) AND Location on the pipeline
summary_data_years <- why_04_proximity_to_filter %>% 
  filter(proximity_location < 20) %>% 
  mutate(location_on_pipe = factor(proximity_location)) %>%
  group_by(location_on_pipe) %>%
  summarise(N = n(),
            Mean = mean(ir_total_years, na.rm = TRUE),
            se = sd(ir_total_years, na.rm = TRUE) / sqrt(N),
            t_val = qt(0.975, df = max(0, N - 1)),
            ci_lower = Mean - (t_val * se),
            ci_upper = Mean + (t_val * se), .groups = 'drop'
            ) %>% mutate(
              ci_lower = if_else(ci_lower < 0, 0, ci_lower)
              )

#...............................................................................

# What triggered early pipe reconfigurations and When it started
why_05a <- as.data.frame(round(prop.table(table(What_triggered = int_2024$trend_cut_why)), 2))

# What triggered early pipe reconfigurations and When it started
why_05b <- as.data.frame(round(prop.table(table(When_started = int_2024$trend_cut)), 2))

#...............................................................................

## Ruling out alternative explanations ----

why_06_listA <- list(
  ### Trust in technology:
  as.data.frame(round(prop.table( table(Trust_in_technology=rmtl_2024$drip_trust)), 2)),
  
  # water_pressure_drip_compare_flood
   as.data.frame(round(prop.table(table(Water_pressure_drip_compare_flood=rmtl_2024$water_pressure_drip_compare_flood)), 2)),
  
  # water_volume_drip_compare_flood
  as.data.frame(round(prop.table(table(Water_volume_drip_compare_flood=rmtl_2024$water_volume_drip_compare_flood)), 2))
  
  )
  
  

# Drip advantages
#Are you aware of any advantages of drip irrigation over other irrigation methods?	
why_06_drip_advantages <- rmtl_In_srvy2022 %>% 
  select(hh_id, m3_0:m3_5) %>%
  pivot_longer(-hh_id, names_to = "drip_advantages", values_to = "yn") %>% 
  group_by(drip_advantages) %>% 
  summarise(pct = mean(yn)) %>% 
  mutate(drip_advantages = recode_values( drip_advantages,
                                          "m3_0" ~ "No knowledge",
                                          "m3_1" ~ "Increased yields",
                                          "m3_2" ~ "Water saving",
                                          "m3_3" ~ "Fertilizer saving",
                                          "m3_4" ~ "Less weeds",
                                          "m3_5" ~ "Less labor requirements"
  )) |> rename(`Advantages of drip irrigation over other irrigation methods`=drip_advantages)


#   training  / recommendations/ demo plot                   ----
library(haven)

why_06_m_list <- list(
  setNames(
    as.data.frame(round(prop.table(table(as_factor(rmtl_In_srvy2022$m42))), 2)),
    c("M42 : Have you ever gone to visit the demo plot?", "Freq")
  ),
  setNames(
    as.data.frame(round(prop.table(table(as_factor(rmtl_In_srvy2022$m34))), 2)),
    c(attr(rmtl_In_srvy2022$m34, "label"), "Freq")
  ),
  setNames(
    as.data.frame(round(prop.table(table(as_factor(rmtl_In_srvy2022$m51))), 2)),
    c(attr(rmtl_In_srvy2022$m51, "label"), "Freq")
  ),
  setNames(
    as.data.frame(round(prop.table(table(as_factor(rmtl_In_srvy2022$m52))), 2)),
    c(attr(rmtl_In_srvy2022$m52, "label"), "Freq")
  ),
  setNames(
    as.data.frame(round(prop.table(table(as_factor(rmtl_In_srvy2022$m56))), 2)),
    c(attr(rmtl_In_srvy2022$m56, "label"), "Freq")
  )
)







#...............................................................................

# Chapter_5 ----

#...............................................................................

Chapter_5 <- 
  list(rbind(why_01a,why_01b),
     why_q1_category,why_q1_ans,
     why_02_timing,
     why_02_jain_and_usage,why_02_ir_season, why_02_Water_Delivery_Delay,
     why_03a_pipe_damage,why_03b_pipe_damage,
     why_04_damage_cause,
     why_04a_flooding_with_Ramthal,why_04b_flooding_with_Ramthal,
     why_04_mm10_A, why_04_mm10_B, why_04_mm10_C,
     
     "Table: Irrigation/location_on_pipe Summary" = summary_data_ir,
     "Table: Ramthal Source/location_on_pipe" = summary_data_source_ramthal,
     "Table: Years of Irrigation/location_on_pipe Summary" = summary_data_years,
     
     "What triggered farmers to cut in the first place?" = why_05a, 
     "When it started?" = why_05b,
       why_06_listA, why_06_drip_advantages, why_06_m_list
     
)

# library(purrr)

Chapter_5 %>% 
  iwalk(~ {
    cat("\n=======================================================\n")
    cat(.y, "\n")
    cat("=======================================================\n")
    print(.x)
  })

# EXTRAS ----

#    [27] "disputs_0non_1BcozWater_2els"                  
freq(rmtl_2024$disputs_0non_1BcozWater_2els, cumul = FALSE)


# [m35b] How long has the main pipes been damaged?[months/years]
rmtl_In_srvy2022 %>% filter(m35b_month>=0, m35b_year>=0)  %>%  summarise(Mean = mean(m35b_month/12+ m35b_year ,na.rm=T)) |> mutate(var="Time_to_fix_pipe_InYears")


# END ----




