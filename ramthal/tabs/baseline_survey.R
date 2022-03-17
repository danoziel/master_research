library(tidyverse)
library(haven)
CMF_RAMTHAL_IRRIGATION_18_Aug_2016_cleaned <- read_dta("~/master_research/DATAs/ramthal_data/baseline_survey_2016/CMF_RAMTHAL_IRRIGATION_18 Aug 2016 - cleaned.dta")

CMF <- CMF_RAMTHAL_IRRIGATION_18_Aug_2016_cleaned %>%
  dplyr::select(1,6,7,"A17",matches("C2_"),matches("D4_"))







