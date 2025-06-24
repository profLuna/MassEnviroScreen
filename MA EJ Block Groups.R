# Identify census block groups that meet definition of Massachusetts Environmental Justice Populations

# load necessary libraries
pacman::p_load(tidyverse, tidycensus, sf, tigris)
# library(tidyverse)
# library(tidycensus)
# library(sf)
# library(tigris)
options(tigris_use_cache = TRUE)

# identify census variables to download
v23 <- load_variables(year = 2023, "acs5", cache = TRUE)

# download current block group geometry
# use EPSG 326986 Massachusetts State State Plane
ma_blkgrp23_sf <- block_groups(state = "MA", year = 2023) %>% 
  st_transform(., crs = 26986) %>% 
  filter(!st_is_empty(.)) %>% 
  st_make_valid()

# get statewide median household income
ma_state23medHHincMA <- get_acs(geography = "state", year = 2023, state = "MA",
                                variables = c(medHHincMA = "B19013_001"), output = "wide") %>% 
  select(medHHincMAE) %>% 
  pull(.)

# get block group median household income
ma_blkgrp23medHHinc <- get_acs(geography = "block group", year = 2023, state = "MA",
                               variables = c(medHHinc = "B19013_001"), output = "wide") %>% 
  mutate(medHHincMA = ma_state23medHHincMA,
         medHHincMAPCT = medHHincE/medHHincMA*100) %>% 
  select(-NAME)

# get municipal median household income and assign to overlapping block groups
ma_blkgrp23medHHinc <- get_acs(geography = "county subdivision", year = 2023, state = "MA",
                               variables = c(medHHincMUNI = "B19013_001"), output = "wide",
                               geometry = TRUE) %>% 
  st_transform(., crs = 26986) %>% 
  filter(!st_is_empty(.)) %>% 
  st_make_valid() %>% 
  transmute(MUNI = NAME, medHHincMUNIE = medHHincMUNIE, medHHincMUNIM = medHHincMUNIM) %>% 
  st_join(ma_blkgrp23_sf, ., st_intersects, largest = TRUE) %>% 
  select(GEOID, MUNI, medHHincMUNIE, medHHincMUNIM) %>% 
  st_drop_geometry(.) %>% 
  left_join(ma_blkgrp23medHHinc, ., by = "GEOID") %>% 
  mutate(medHHincMUNIPCT = medHHincMUNIE/medHHincMA*100)

# Percent minority
ma_blkgrp23race <- get_acs(geography = "block group", year = 2023, state = "MA", 
                           variables = c(pop = "B03002_001", nhWhite = "B03002_003"),
                           output = "wide") %>% 
  mutate(minorityE = popE - nhWhiteE, minorityPctE = minorityE/popE*100) %>% 
  select(-popE, -popM, -starts_with("nhWhit"))

# Household Language by Household Limited English Speaking Status
ma_blkgrp23langpop <- get_acs(geography = "block group", year = 2023, state = "MA", 
                              variables = c(pop = "C16002_001"), output = "wide") %>% 
  select(-NAME)
var <- str_pad(c(4,7,10,13), width = 3, side = c("left"), pad = "0") %>% 
  paste0("C16002_", .)
ma_blkgrp23language <- get_acs(geography = "block group", year = 2023, state = "MA", 
                               variables = var) %>% 
  group_by(GEOID) %>% 
  summarize(limitEngE = sum(estimate),
            limitEngM = moe_sum(moe, limitEngE)) %>% 
  ungroup() %>% 
  left_join(., ma_blkgrp23langpop, by = "GEOID") %>%
  mutate(limitEngpctE = limitEngE/popE*100, 
         limitEngpctM = moe_prop(num = limitEngE, denom = popE, moe_num = limitEngM, 
                                 moe_denom = popM)*100) %>% 
  select(-popE, -popM)
rm(ma_blkgrp23langpop)

# process variables into a consistent df with appropriate estimates, counts, and percentages
list_df <- list(ma_blkgrp23race, ma_blkgrp23language, ma_blkgrp23medHHinc)
ma_blkgrp23 <- list_df %>% reduce(left_join, by = "GEOID") %>% 
  mutate(NAME = str_replace_all(str_remove(NAME, "; Massachusetts"), ";", ","),
         MUNI = str_remove_all(str_remove(MUNI, "((?=\\,).*)"), " town| Town| city| City"))
# remove all objects except ma_blkgrp23
rm(list = ls(pattern = "[^ma_blkgrp23 | ^ma_blkgrp23_sf]"))


# Classify EJ BGs according to MA environmental policy
ma_blkgrp23 <- ma_blkgrp23 %>% 
  mutate(MEDIANHHI = if_else(medHHincMAPCT <= 65, "I", ""),
         LIMITEDENGLISH = if_else(limitEngpctE >= 25, "E", ""),
         MINORITY_POPULATION = case_when(
           minorityPctE >= 40 ~ "M",
           minorityPctE >= 25 & medHHincMUNIPCT < 150 ~ "M",
           .default = ""),
         EJ = case_when(
           MEDIANHHI == "I" | 
             LIMITEDENGLISH == "E" | 
             MINORITY_POPULATION == "M" ~ "Yes",
           .default = "No")) %>% 
  unite(., col = EJ_CRITERIA, c(MEDIANHHI, LIMITEDENGLISH, MINORITY_POPULATION), 
        sep = "", remove = FALSE, na.rm = TRUE) %>% 
  mutate(EJ_CRITERIA = str_squish(EJ_CRITERIA),
         EJ_CRITERIA_CNT = nchar(EJ_CRITERIA),
         EJ_CRIT_DESC = case_when(
           EJ_CRITERIA == "E" ~ "Limited English",
           EJ_CRITERIA == "EM" ~ "Limited English and Minority",
           EJ_CRITERIA == "I" ~ "Low Income",
           EJ_CRITERIA == "IE" ~ "Low Income and Limited English",
           EJ_CRITERIA == "IEM" ~ "Low Income and Limited English and Minority",
           EJ_CRITERIA == "IM" ~ "Low Income and Minority",
           EJ_CRITERIA == "M" ~ "Minority",
           .default = ""
         ))

saveRDS(ma_blkgrp23, "ma_blkgrpEJ23.rds")
