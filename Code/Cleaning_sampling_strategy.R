
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
  dplyr::filter(!is.nan(mean_outsourcing))%>%
  arrange(desc(mean_outsourcing)) %>%  # Sort in descending order
  mutate(outsourcing_category = case_when(
    row_number() <= 20 ~ "high-outsourcing",
    row_number() > (n() - 20) ~ "low-outsourcing",
    TRUE ~ NA_character_
  ))



overspend <- full_join(
    read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_qualitative/main/Data/Raw/RA_2021-22_data_by_LA.csv"), skip=7)%>%
    dplyr::select(Local.authority, TOTAL.CHILDREN.S.SOCIAL.CARE)%>%
    dplyr::rename(budget_21 = TOTAL.CHILDREN.S.SOCIAL.CARE)%>%
    dplyr::mutate(Local.authority = Local.authority %>%
                    gsub('&', 'and', .) %>%
                    gsub('[[:punct:] ]+', ' ', .) %>%
                    gsub('[0-9]', '', .)%>%
                    toupper() %>%
                    gsub("CITY OF", "",.)%>%
                    gsub("UA", "",.)%>%
                    gsub("CITY", "",.)%>%
                    gsub("COUNTY OF", "",.)%>%
                    gsub("ROYAL BOROUGH OF", "",.)%>%
                    gsub("LEICESTER CITY", "LEICESTER",.)%>%
                    gsub("TELFORD AND THE WREKIN", "TELFORD AND WREKIN",.)%>%
                    gsub("NEWCASTLE UPON TYNE", "NEWCASTLE",.)%>%
                    gsub("UA", "",.)%>%
                    gsub("DARWIN", "DARWEN", .)%>%
                    gsub("THE MEDWAY TOWNS", "MEDWAY",.)%>%
                   # gsub("COUNTY DURHAM", "DURHAM", .)%>%
                    gsub("AND DARWEN", "WITH DARWEN", .)%>%
                    gsub("MEDWAY TOWNS", "MEDWAY",.)%>%
                    gsub("NE SOM", "NORTH EAST SOM", .)%>%
                    gsub("DURHAM", "COUNTY DURHAM", .)%>%
                    gsub("NEWCASTLE", "NEWCASTLE UPON TYNE", .)%>%
                    gsub("N E SOM", "NORTH EAST SOM", .)%>%
                    str_trim())%>%
      dplyr::filter(Local.authority!="",
                    budget_21!="0",
                    budget_21!="..."),
    read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_qualitative/main/Data/Raw/RA_2022-23_data.csv"), skip=6)%>%
    dplyr::select(Local.authority, TOTAL.CHILDREN.S.SOCIAL.CARE)%>%
    dplyr::rename(budget_22 = TOTAL.CHILDREN.S.SOCIAL.CARE)%>%
    dplyr::mutate(Local.authority = Local.authority %>%
                    gsub('&', 'and', .) %>%
                    gsub('[[:punct:] ]+', ' ', .) %>%
                    gsub('[0-9]', '', .)%>%
                    toupper() %>%
                    gsub("CITY OF", "",.)%>%
                    gsub("UA", "",.)%>%
                    gsub("COUNTY OF", "",.)%>%
                    gsub("CITY", "",.)%>%
                    gsub("ROYAL BOROUGH OF", "",.)%>%
                    gsub("LEICESTER CITY", "LEICESTER",.)%>%
                    gsub("UA", "",.)%>%
                    gsub("DARWIN", "DARWEN", .)%>%
                    gsub("THE MEDWAY TOWNS", "MEDWAY",.)%>%
                    gsub("TELFORD AND THE WREKIN", "TELFORD AND WREKIN",.)%>%
                    gsub("NEWCASTLE UPON TYNE", "NEWCASTLE",.)%>%
                   # gsub("COUNTY DURHAM", "DURHAM", .)%>%
                    gsub("AND DARWEN", "WITH DARWEN", .)%>%
                    gsub("MEDWAY TOWNS", "MEDWAY",.)%>%
                    gsub("DURHAM", "COUNTY DURHAM", .)%>%
                    gsub("NE SOM", "NORTH EAST SOM", .)%>%
                    gsub("NEWCASTLE", "NEWCASTLE UPON TYNE", .)%>%
                    gsub("N E SOM", "NORTH EAST SOM", .)%>%
                    str_trim())%>%
      dplyr::filter(Local.authority!="",
                    budget_22!="0",
                    budget_22!="…"),
  by = "Local.authority"
)%>%
  dplyr::full_join(.,
                   read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_qualitative/main/Data/Raw/RA_2023-24_data_Part_1_manual.csv"), 
                            skip=0)%>%
                     dplyr::select(Local.authority, TOTAL.CHILDREN.S.SOCIAL.CARE)%>%
                     dplyr::rename(budget_23 = TOTAL.CHILDREN.S.SOCIAL.CARE)%>%
                     dplyr::mutate(Local.authority = Local.authority %>%
                                     gsub('&', 'and', .) %>%
                                     gsub('[[:punct:] ]+', ' ', .) %>%
                                     gsub('[0-9]', '', .)%>%
                                     toupper() %>%
                                     gsub("CITY OF", "",.)%>%
                                     gsub("CITY", "",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("NEWCASTLE UPON TYNE", "NEWCASTLE",.)%>%
                                     gsub("COUNTY OF", "",.)%>%
                                     gsub("ROYAL BOROUGH OF", "",.)%>%
                                     gsub("TELFORD AND THE WREKIN", "TELFORD AND WREKIN",.)%>%
                                     gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("DARWIN", "DARWEN", .)%>%
                                   #  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                     gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                     gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                     gsub("DURHAM", "COUNTY DURHAM", .)%>%
                                     gsub("THE MEDWAY TOWNS", "MEDWAY",.)%>%
                                     gsub("NEWCASTLE", "NEWCASTLE UPON TYNE", .)%>%
                                     gsub("MEDWAY TOWNS", "MEDWAY",.)%>%
                                     gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                     str_trim())%>%
                     dplyr::filter(Local.authority!="",
                                   budget_23!="0",
                                   budget_23!="..."),
                   by="Local.authority")%>%
  dplyr::full_join(.,
                  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_qualitative/main/Data/Raw/RSX_2021-22_data_by_LA_Live.csv"), skip=10)%>%
                     dplyr::select(Local.authority, Children.Social.Care....Net.Current.Expenditure...C7...C3...C6.)%>%
                     dplyr::rename(spend_21 = Children.Social.Care....Net.Current.Expenditure...C7...C3...C6.)%>%
                    dplyr::mutate(Local.authority = Local.authority %>%
                                    gsub('&', 'and', .) %>%
                                    gsub('[[:punct:] ]+', ' ', .) %>%
                                    gsub('[0-9]', '', .)%>%
                                    toupper() %>%
                                    gsub("CITY OF", "",.)%>%
                                    gsub("UA", "",.)%>%
                                    gsub("COUNTY OF", "",.)%>%
                                    gsub("ROYAL BOROUGH OF", "",.)%>%
                                    gsub("TELFORD AND THE WREKIN", "TELFORD AND WREKIN",.)%>%
                                    gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                    gsub("CITY", "",.)%>%
                                    gsub("UA", "",.)%>%
                                    gsub("NEWCASTLE UPON TYNE", "NEWCASTLE",.)%>%
                                    gsub("COUNCIL", "",.)%>%
                                    gsub("CC", "",.)%>%
                                    gsub("THE MEDWAY TOWNS", "MEDWAY",.)%>%
                                    gsub("BC", "",.)%>%
                                    gsub("DARWIN", "DARWEN", .)%>%
                                   # gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                    gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                    gsub("DURHAM", "COUNTY DURHAM", .)%>%
                                    gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                    gsub("NEWCASTLE", "NEWCASTLE UPON TYNE", .)%>%
                                    gsub("MEDWAY TOWNS", "MEDWAY",.)%>%
                                    gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                    str_trim())%>%
                    dplyr::filter(Local.authority!="",
                                  spend_21!="0",
                                  spend_21!="[x]",
                                  spend_21!="..."),
                  by="Local.authority")%>%
  dplyr::full_join(.,
                   read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_qualitative/main/Data/Raw/RSX_2022-23_data_by_LA.csv"), skip=10)%>%
                     dplyr::select(Local.authority, Children.Social.Care....Net.Current.Expenditure...C7...C3...C6.)%>%
                     dplyr::rename(spend_22 = Children.Social.Care....Net.Current.Expenditure...C7...C3...C6.)%>%
                     dplyr::mutate(Local.authority = Local.authority %>%
                                     gsub('&', 'and', .) %>%
                                     gsub('[[:punct:] ]+', ' ', .) %>%
                                     gsub('[0-9]', '', .)%>%
                                     toupper() %>%
                                     gsub("CITY OF", "",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("COUNCIL", "",.)%>%
                                     gsub("CC", "",.)%>%
                                     gsub("BC", "",.)%>%
                                     gsub("TELFORD AND THE WREKIN", "TELFORD AND WREKIN",.)%>%
                                     gsub("COUNTY OF", "",.)%>%
                                     gsub("CITY", "",.)%>%
                                     gsub("THE MEDWAY TOWNS", "MEDWAY",.)%>%
                                     gsub("ROYAL BOROUGH OF", "",.)%>%
                                     gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("NEWCASTLE UPON TYNE", "NEWCASTLE",.)%>%
                                     gsub("MEDWAY TOWNS", "MEDWAY",.)%>%
                                     gsub("DARWIN", "DARWEN", .)%>%
                                   #  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                     gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                     gsub("DURHAM", "COUNTY DURHAM", .)%>%
                                     gsub("NEWCASTLE", "NEWCASTLE UPON TYNE", .)%>%
                                     gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                     gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                     str_trim())%>%
                     dplyr::filter(Local.authority!="",
                                   spend_22!="0",
                                   spend_22!="[x]",
                                   spend_22!="..."),
                   by="Local.authority")%>%
  dplyr::full_join(.,
                   read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_qualitative/main/Data/Raw/RSX_2023-24_data_by_LA.csv"), skip=10)%>%
                     dplyr::select(Local.authority, Children.Social.Care....Net.Current.Expenditure...C7...C3...C6.)%>%
                     dplyr::rename(spend_23 = Children.Social.Care....Net.Current.Expenditure...C7...C3...C6.)%>%
                     dplyr::mutate(Local.authority = Local.authority %>%
                                     gsub('&', 'and', .) %>%
                                     gsub('[[:punct:] ]+', ' ', .) %>%
                                     gsub('[0-9]', '', .)%>%
                                     toupper() %>%
                                     gsub("CITY OF", "",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("COUNCIL", "",.)%>%
                                     gsub("CC", "",.)%>%
                                     gsub("BC", "",.)%>%
                                     gsub("COUNTY OF", "",.)%>%
                                     gsub("ST HELENS M", "ST HELENS",.)%>%
                                     gsub("WIGAN M", "WIGAN",.)%>%
                                     gsub("STOCKPORT M", "STOCKPORT",.)%>%
                                     gsub("BURY M", "BURY",.)%>%
                                     gsub("ROYAL BOROUGH OF", "",.)%>%
                                     gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                     gsub("TELFORD AND THE WREKIN", "TELFORD AND WREKIN",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("CITY", "",.)%>%
                                     gsub("NEWCASTLE UPON TYNE", "NEWCASTLE",.)%>%
                                     gsub("THE MEDWAY TOWNS", "MEDWAY",.)%>%
                                     gsub("MEDWAY TOWNS", "MEDWAY",.)%>%
                                     gsub("DARWIN", "DARWEN", .)%>%
                                     gsub("DURHAM", "COUNTY DURHAM", .)%>%
                                     gsub("NEWCASTLE", "NEWCASTLE UPON TYNE", .)%>%
                                     gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                     gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                     gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                     str_trim())%>%
                     dplyr::filter(Local.authority!="",
                                   spend_23!="0",
                                   spend_23!="[x]",
                                   spend_23!="..."),
                   by="Local.authority")%>%
  dplyr::mutate(budget_21 = as.numeric(gsub(",", "", budget_21)),
                budget_22 = as.numeric(gsub(",", "", budget_22)),
                budget_23 = as.numeric(gsub(",", "", budget_23)),
                spend_21 = as.numeric(gsub(",", "", spend_21)),
                spend_22 = as.numeric(gsub(",", "", spend_22)),
                spend_23 = as.numeric(gsub(",", "", spend_23)),
                overspend_21 = (spend_21 - budget_21)/budget_21*100,
                overspend_22 = (spend_22 - budget_22)/budget_22*100,
                overspend_23 = (spend_23 - budget_23)/budget_23*100)%>%
  mutate(mean_overspend = rowMeans(dplyr::select(., overspend_21, overspend_22, overspend_23), na.rm = TRUE))%>%
  dplyr::filter(!is.nan(mean_overspend),
                mean_overspend>-50,
                mean_overspend<50)

categories <- left_join(outsourcing, overspend, by="Local.authority")%>%
  arrange(desc(mean_overspend)) %>%  # Sort in descending order
  mutate(overspend_category = case_when(
    row_number() <= 20 ~ "high-overspend",
    row_number() > (n() - 20) ~ "low-overspend",
    TRUE ~ NA_character_
  ))%>%
  dplyr::select(Local.authority, overspend_category, outsourcing_category)

write.csv(categories, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/childrens_social_care_qualitative/Data/Outputs/categories_final.csv")
write.csv(overspend, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/childrens_social_care_qualitative/Data/Outputs/overspend_data_final.csv")
write.csv(outsourcing, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/childrens_social_care_qualitative/Data/Outputs/outsourcing_data_final.csv")





####providers####



joiners17 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_17.csv"))  
joiners18 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_18.csv"))  
joiners19 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_19.csv"))  
joiners20 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_20.csv"))  
joiners21 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_21.csv"))  
joiners22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_22.csv"), skip=3)  
joiners23 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_23.csv"), skip=3)  

joiners <- rbind( #joiners17 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
  #joiners18 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places,First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
  joiners19 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
  joiners20 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
  joiners21 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
  joiners22 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
  joiners23 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.)
)%>%
  dplyr::rename(Date = Registration.date)%>%
  dplyr::mutate(leave_join = "Join",
                provider_status = NA)%>%
  dplyr::filter(Provision.type!="Adoption Support Agency",
                Provision.type!="Further Education College with Residential Accommodation",
                Provision.type!="Boarding School",
                Provision.type!="Residential Family Centre",
                Provision.type!="Residential Special School",
                Provision.type!="Voluntary Adoption Agency",
                Provision.type!="Residential Holiday Scheme for Disabled Children",
                Provision.type!="Independent Fostering Agency",
                Provision.type!="Voluntary Adoption Agency")


Leavers <- rbind( #joiners17 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
  #joiners18 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places,First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
  joiners19 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Date.closed)%>%
    dplyr::rename(Cancelled.or.resigned.date = Date.closed),
  joiners20 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Cancelled.or.resigned.date),
  joiners21 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Resigned.closed.date)%>%
    dplyr::rename(Cancelled.or.resigned.date=Resigned.closed.date),
  joiners22 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Cancelled.or.resigned.date),
  joiners23 %>% dplyr::filter(Leaver.status=="Leaver") %>% 
    dplyr::mutate(Cancelled.or.resigned.date = ifelse(First.effective.date..that.the.provider.became.resigned.!="",First.effective.date..that.the.provider.became.resigned.,
                                                      ifelse(First.effective.date..that.the.provider.became.resigned.==""&First.effective.date..that.the.provider.closed.=="", First.effective.date..that.the.provider.became.cancelled.,
                                                             First.effective.date..that.the.provider.closed.)))%>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places,Cancelled.or.resigned.date) 
)%>%
  dplyr::rename(Date = Cancelled.or.resigned.date)%>%
  dplyr::mutate(leave_join = "Leave",
                provider_status = NA)%>%
  dplyr::filter(Provision.type!="Adoption Support Agency",
                Provision.type!="Further Education College with Residential Accommodation",
                Provision.type!="Boarding School",
                Provision.type!="Residential Family Centre",
                Provision.type!="Residential Special School",
                Provision.type!="Voluntary Adoption Agency",
                Provision.type!="Residential Holiday Scheme for Disabled Children",
                Provision.type!="Independent Fostering Agency",
                Provision.type!="Voluntary Adoption Agency")


exits <- rbind(joiners, Leavers)

pre <- rbind(
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 3, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 4, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 5, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 6, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 7, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 8, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 9, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 10, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`)
)

all <- rbind(pre%>%
               dplyr::rename(Provision.type = `Provider type`,
                             Local.authority=`Local authority`)%>%
               dplyr::mutate(Local.authority = Local.authority %>%
                               gsub('&', 'and', .) %>%
                               gsub('[[:punct:] ]+', ' ', .) %>%
                               gsub('[0-9]', '', .)%>%
                               toupper() %>%
                               gsub("CITY OF", "",.)%>%
                               gsub("UA", "",.)%>%
                               gsub("COUNTY OF", "",.)%>%
                               gsub("ROYAL BOROUGH OF", "",.)%>%
                               gsub("LEICESTER CITY", "LEICESTER",.)%>%
                               gsub("UA", "",.)%>%
                               gsub("DARWIN", "DARWEN", .)%>%
                               gsub("COUNTY DURHAM", "DURHAM", .)%>%
                               gsub("AND DARWEN", "WITH DARWEN", .)%>%
                               gsub("NE SOM", "NORTH EAST SOM", .)%>%
                               gsub("N E SOM", "NORTH EAST SOM", .)%>%
                               str_trim())%>% dplyr::select(URN,Local.authority,Sector,Places,Date,leave_join), 
             exits%>% dplyr::select(URN,Local.authority,Sector,Places,Date, leave_join)%>%
               dplyr::mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%  # Convert Date to Date format
               dplyr::filter(Date >= as.Date("2018-04-01"))  # Filter for dates after 1st April 2018
)%>%  
  dplyr::distinct(URN,leave_join, .keep_all = T)%>%
  tidyr::pivot_wider(id_cols = c("Local.authority", "Sector", "URN", "Places"), names_from = "leave_join", values_from = as.character("Date"))%>%
  dplyr::mutate(Join = substr(Join, 1, 10),
                Leave = substr(Leave, 1, 10),
                Sector= ifelse(Sector=="Health Authority", "Local Authority", Sector))


leaves <- all %>%
  dplyr::select(URN,Leave)

all <- all %>%
  dplyr::select(-Leave)%>%
  full_join(., leaves, by= "URN")  %>%
  group_by(URN) %>%                      # Group by URN
  fill(Leave, .direction = "downup") %>% 
  fill(Join, .direction = "downup")%>%
  ungroup()%>%
  dplyr::distinct(URN, .keep_all = T)%>%
  dplyr::mutate(Local.authority = Local.authority %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())







df <-rbind( read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2015.csv"))%>%
              dplyr::mutate(year=2015,
                            Places= NA,
                            Organisation = NA, 
                            Registration.date = NA)%>%
              dplyr::rename(Registration.status=Reg.Status,
                            Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.Date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home")|
                              str_detect(Provision.type, "(?i)day"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2016.csv"), skip=1)%>%
              dplyr::mutate(year=2016,
                            Organisation = NA,
                            Registration.date = NA)%>%
              dplyr::rename(Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2017.csv"), skip=1)%>%
              dplyr::mutate(year=2017,
                            Organisation = NA,
                            Registration.date = NA)%>%
              dplyr::rename(Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2018.csv"), skip=1)%>%
              dplyr::mutate(year=2018,
                            Registration.date = NA)%>%
              dplyr::rename(Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2018_part2.csv"))%>%
              dplyr::mutate(year=2018,
                            Registration.date = NA,
                            Overall.experiences.and.progress.of.children.and.young.people = NA,
                            Latest.full.inspection.date = NA)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.name)%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2019.csv"), skip=1)%>%
              dplyr::mutate(year=2019)%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2020.csv"), skip=1)%>%
              dplyr::mutate(year=2020)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2021.csv"), skip=1)%>%
              dplyr::mutate(year=2021)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2022_yep.csv"), skip=4)%>%
              dplyr::mutate(year=2022)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Overall.experiences.and.progress.of.children.and.young.people =    Latest.full.inspection.overall.experiences.and.progress.of.children.and.young.people)%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2023.csv"), skip=3)%>%
              dplyr::mutate(year=2023)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider)%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people,Latest.full.inspection.date)
)%>%
  dplyr::mutate(Sector= ifelse(Sector=="Health Authority"|Sector=="Local authority"|Sector=="Health authority", "Local Authority", Sector),
                Homes=1)%>%
  dplyr::distinct(.keep_all = T)%>%
  group_by(URN) %>%                      # Group by URN
  fill(Places, .direction = "downup") %>% 
  fill(Organisation, .direction = "downup") %>% 
  ungroup()%>%
  bind_rows(., all %>% 
              dplyr::select(URN, Places, Local.authority)%>%
              dplyr::mutate(Homes = NA, 
                            Sector=NA,
                            Places = as.numeric(Places)))%>%
  group_by(URN) %>%                      # Group by URN
  fill(Places, .direction = "downup") %>% 
  ungroup()%>%
  dplyr::filter(!is.na(Sector))%>%
  dplyr::mutate(Local.authority = Local.authority %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::full_join(., all%>%
                     dplyr::select(-Places, -Local.authority, -Sector,)%>%
                     dplyr::filter(as.Date(Leave)>="2015-04-01"|
                                     is.na(Leave))%>%
                     dplyr::filter(as.Date(Join)<"2023-04-01"|
                                     is.na(Join)), by=c("URN"))%>%
  group_by(URN) %>%
  mutate(
    Registration.date = if_else(
      is.na(Registration.date), 
      NA_character_,  # Use NA_character_ for character type
      Registration.date
    )
  ) %>%
  fill(Registration.date, .direction = "down") %>%  # Fill downwards within each group
  fill(Registration.date, .direction = "up") %>%    # Fill upwards within each group
  ungroup()%>%
  dplyr::mutate(Registration.date = ifelse(URN == "SC022444" & Registration.date != "26/09/2001",
                                           "26/09/2001", Registration.date))


yes <- df %>% dplyr::filter(is.na(Local.authority))%>%
  dplyr::mutate(keep = 1)%>%
  dplyr::select(URN, keep)%>%
  full_join(., all, by="URN")%>%
  dplyr::filter(keep==1)


df <- df%>% dplyr::filter(!is.na(Local.authority))%>%
  bind_rows(., yes%>%
              dplyr::mutate(Places = as.numeric(Places)))%>%
  dplyr::mutate(Latest.full.inspection.date = as.Date(Latest.full.inspection.date, format = "%d/%m/%Y")) %>%
  group_by(URN) %>%
  filter( all(is.na(Latest.full.inspection.date)) |  # Keep if all dates in the group are NA
            Latest.full.inspection.date == max(Latest.full.inspection.date, na.rm = TRUE)) %>%
  ungroup()%>%
  dplyr::mutate(Overall.experiences.and.progress.of.children.and.young.people = ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Requires improvement", "Requires improvement to be good",
                                                                                       ifelse(Overall.experiences.and.progress.of.children.and.young.people == "", NA,
                                                                                              ifelse(Overall.experiences.and.progress.of.children.and.young.people =="Satisfactory", NA,
                                                                                                     ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Adequate", NA,
                                                                                                            Overall.experiences.and.progress.of.children.and.young.people)))))

####needs to be in 3 docs for 500 max in fame####








df <- df %>%
  dplyr::mutate(Organisation_fame_search = Organisation %>%
                  gsub("children's", "childrens", ., ignore.case = TRUE)%>%
                  gsub("Cedarways Residential Child Care  Larkhill House Limited", "Cedarways Residential Child Care Larkhill House Limited", ., ignore.case = TRUE)%>%
                  gsub("Achieving Aspirations  Community Interest Company", "Achieving Aspirations Community Interest Company", ., ignore.case = TRUE)%>%
                  gsub("Oak  house Childrens Home Ltd", "Oak house Childrens Home Ltd", ., ignore.case = TRUE)%>%
                  gsub("Overley Hall  Limited", "Overley Hall Limited", ., ignore.case = TRUE)%>%
                  gsub("Kattz ltd", "Kattz (ah) Ltd", ., ignore.case = TRUE)%>%
                  gsub("the partnership of care today", "Care Today Childrens Services", ., ignore.case = TRUE)%>%
                  gsub("Devon \\& Cornwall Autistic Community Trust \\(t/a Spectrum\\)", "Devon & Cornwall Autistic Community Trust", ., ignore.case = TRUE)%>%
                  gsub("Beacon Child Care Ltd", "BEACON CHILDCARE LTD", ., ignore.case = TRUE)%>%
                  gsub("Tulip Care One Limited t/as TulipCare", "Tulip Care One Limited", ., ignore.case = TRUE)%>%
                  gsub("Inspirations Leicestershire Limited comp number", "Inspirations Leicestershire Limited", ., ignore.case = TRUE)%>%
                  gsub("4 Pure Heart Limited", "4PureHeart Limited", ., ignore.case = TRUE)%>%
                  gsub("Lakeside @ Our Place Limited", "Lakeside@OurPlace Limited", ., ignore.case = TRUE)%>%
                  gsub("Next Stage 4Life LTD", "Next Stage 4 Life LTD", ., ignore.case = TRUE)%>%
                  gsub("The Exeter Royal Academy For Deaf Education", "Exeter Royal Academy For Deaf Education", ., ignore.case = TRUE)%>%
                  gsub("Pathways Residential Child Care Larkhill House Limited", "CEDARWAYS RESIDENTIAL CHILD CARE LARKHILL HOUSE LIMITED", ., ignore.case = TRUE)%>%
                  gsub("Leicester Young Mens Christian Association \\(incorporated\\) \\(the\\)", "YMCA LEICESTERSHIRE", ., ignore.case = TRUE)%>%
                  gsub("Slough childrens Services Trust Limited", "SLOUGH CHILDREN FIRST LIMITED", ., ignore.case = TRUE)%>%
                  gsub("Tjunction childrens Services Limited", "T-junction childrens Services Ltd", ., ignore.case = TRUE)%>%
                  gsub("Crystal Care Solutions Limited Company Number", "Crystal Care Solutions Limited", ., ignore.case = TRUE)%>%
                  gsub("The Partnership of Care Today childrens Services", "CARE TODAY (CHILDRENS SERVICES) LTD", ., ignore.case = TRUE)%>%
                  gsub("Broadwood Education Services", "KEYS EDUCATIONAL SERVICES LIMITED", ., ignore.case = TRUE)%>%
                  gsub("247Bluebell Care LTD", "247 Bluebell Care LTD", ., ignore.case = TRUE)%>%
                  gsub("Willows21 Limited", "Willows 21 Limited", ., ignore.case = TRUE)%>%
                  gsub("iMapcentre Ltd", "iMap centre Ltd", ., ignore.case = TRUE)%>%
                  gsub("solent Childcare LTD", "solent Child care LTD", ., ignore.case = TRUE)%>%
                  gsub("North West Youth Services Limited", "Northwest Youth Services Limited", ., ignore.case = TRUE)%>%
                  gsub("Achieving Aspirations cic", "Achieving Aspirations COMMUNITY INTEREST COMPANY", ., ignore.case = TRUE)%>%
                  gsub("Monach Intervention Services Limited", "Monarch Intervention Services Limited", ., ignore.case = TRUE)%>%
                  gsub("Sbl Care Services", "SBL CARESERVICES", ., ignore.case = TRUE)%>%
                  gsub("Lioncare Ltd Operating As The Lioncare Group", "Lioncare Ltd", ., ignore.case = TRUE)%>%
                  gsub("Smoothstone Care And Education Ltd", "Smooth stone Care And Education Ltd", ., ignore.case = TRUE)%>%
                  gsub("Foundations Children & Family Services Ltd", "STEP UP CHILDREN AND FAMILY SERVICES LIMITED", ., ignore.case = TRUE)%>%
                  gsub("A\\+t Home", "A & T HOME LIMITED", ., ignore.case = TRUE)%>%
                  gsub("Juvenis Care Services Ltd", "ACCALIA CARE SERVICES LTD", ., ignore.case = TRUE)%>%
                  gsub("Juvenis Care Services Ltd.", "ACCALIA CARE SERVICES LTD", ., ignore.case = TRUE)%>%
                  gsub("Stonelake London Limited", "Stone lake London Limited", ., ignore.case = TRUE)%>%
                  gsub("Homes2inspire Limited", "Homes 2 inspire Limited", ., ignore.case = TRUE)%>%
                  gsub("Blossom SC Ltd", "PARKER SC LTD", ., ignore.case = TRUE)%>%
                  gsub("Residential Childcare Community\\(Town Hall\\) LTD", "Residential Child care Community (Town Hall) LTD", ., ignore.case = TRUE)%>%
                  gsub("G&S caring for children and young people ltd", "G&SCARING FORCHILDREN AND YOUNG PEOPLE LTD", ., ignore.case = TRUE)%>%
                  gsub("Smoothstone Care & Education", "SMOOTH STONE CARE AND EDUCATION LIMITED", ., ignore.case = TRUE)%>%
                  gsub("ROC Family Support Ltd", "ROC FAMILY TIME LIMITED", ., ignore.case = TRUE)%>%
                  gsub("Lighthouse childrens Care LTD", "SUSSEX CHILDRENS CARE LTD", ., ignore.case = TRUE)%>%
                  gsub("Catch22 Charity Limited", "Catch 22 Charity Limited", ., ignore.case = TRUE)%>%
                  gsub("Evolve Childcare Ltd", "Evolve Child care Ltd", ., ignore.case = TRUE)%>%
                  gsub("TJY Care lytd", "TJY Care ltd", ., ignore.case = TRUE)%>%
                  gsub("Oasis Young People's Care Services \\(uk\\) Ltd", "OASIS CARE SERVICES LIMITED", ., ignore.case = TRUE)%>%
                  gsub("Birtenshaw - company number 02978546", "Birtenshaw", ., ignore.case = TRUE)%>%
                  gsub("Rubicon childrens Homes Limited", "RUBICON CHILDREN & FAMILY SERVICES LIMITED", ., ignore.case = TRUE)%>%
                  gsub("Horizon Residential childrens Home", "HORIZON RESIDENTIAL HOMES LIMITED", ., ignore.case = TRUE)%>%
                  gsub("Bright Futures Care Limited T/A Cornerstones", "Bright Futures Care Limited", ., ignore.case = TRUE)%>%
                  gsub("Tree House Care Fostering Solutions Ltd", "TreeHouse Care Fostering Solutions Ltd", ., ignore.case = TRUE)%>%
                  gsub("children assisted in a real environment \\(care\\) ltd", "children assisted in a real environment ltd", ., ignore.case = TRUE)%>%
                  gsub("[0-9]{7,}", "", .)%>%
                  gsub("'", "", .)%>%
                  gsub("T/A.*", "", .)%>%
                  gsub("T/a.*", "", .)%>%
                  str_trim(.)%>%
                  gsub("Southend on Sea Young Mens Christian Association", "SOUTHEND-ON-SEA YOUNG MEN'S CHRISTIAN ASSOCIATION", ., ignore.case = TRUE)  )



francois_clean <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/francois_clean.csv")


lookup <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/lookup_fame_search_clean_manual.csv")%>%
  dplyr::mutate(Organisation_fame_search = Organisation_fame_search %>%
                  gsub("Selected", "", .),
                Company.name = Company.name %>%
                  gsub(" In liquidation", "", .)%>%
                  gsub(" Dissolved", "",.)%>%
                  str_trim())%>%
  dplyr::distinct(Organisation_fame_search, .keep_all = T)%>%
  dplyr::bind_rows(., data.frame(Organisation_fame_search = "BEACON CHILDCARE LTD", Company.name = "BEACON CHILDCARE LTD", stringsAsFactors = FALSE))


df <- df %>%
  full_join(., lookup, by="Organisation_fame_search")%>%
  dplyr::left_join(., francois_clean, by="Company.name") %>%
  dplyr::mutate(Sector_merge = ifelse(Sector.x=="Local Authority", "Local Authority",
                                      ifelse(Sector.x=="Voluntary", "Third sector", Sector.y)),
                Sector_merge = ifelse(is.na(Sector_merge), "Unidentified for-profit", Sector_merge))



df <- df %>%
  dplyr::filter(is.na(Leave), 
                !is.na(URN),
                !is.na(Registration.date))%>%
  dplyr::select(URN, Local.authority, Sector_merge, Places, Organisation, Registration.date)
  

df_fin <- dplyr::left_join(df, categories%>%
                             dplyr::mutate(matched = 1,
                                           Local.authority = Local.authority %>%
                                             gsub('COUNTY DURHAM', 'DURHAM', .)), by="Local.authority")




  
