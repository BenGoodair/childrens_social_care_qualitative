
if (!require("pacman")) install.packages("pacman")

pacman::p_load(MCMCglmm,ordinal,devtools,MASS,  sandwich,  lmtest,  clubSandwich,  
               modelsummary, cowplot,ggmap,googleway,hrbrthemes,viridis,jsonlite,survival, 
               httr, purrr, dplyr,gt, gtsummary, tidyverse,rattle,ggeffects, glmnet,caret, 
               rpart.plot,rpart, tidyr,MCMCplots, mice, stringr,randomForest,  curl, plm, readxl, zoo, 
               stringr, patchwork,  sf, clubSandwich, modelsummary, sjPlot,
               lmerTest, lme4, brms, glmmTMB, ggeffects, MCMCglmm, tidybayes, bayesplot,
               gridExtra, nnet, clubSandwich, fixest, patchwork)


####Outsourcing####

outsourcing <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                   dplyr::filter(variable=="Private provision", subcategory=="Place providers", category=="child characteristic at 31st March",
                                 year>2021) %>%
                   dplyr::rename(Local.authority = LA_Name,
                                 outsourcing = percent)%>%
                   dplyr::select(Local.authority, outsourcing)%>%
  dplyr::group_by(Local.authority)%>%
  dplyr::summarise(mean_outsourcing = mean(as.numeric(outsourcing), na.rm=T))%>%
  dplyr::ungroup()%>%
  arrange(desc(mean_outsourcing)) %>%  # Sort in descending order
  mutate(outsourcing_category = case_when(
    row_number() <= 20 ~ "high-outsourcing",
    row_number() > (n() - 20) ~ "low-outsourcing",
    TRUE ~ NA_character_
  ))
  
