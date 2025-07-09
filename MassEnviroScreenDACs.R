# Create a final MassEnviroScreen layer with DACs identified
# read in libraries
pacman::p_load(tidyverse, sf, tidycensus, tigris)
options(tigris_use_cache = TRUE)

# read in MassEnviroScreen data from MassEnviroScreen.R
MassEnviroScreen <- readRDS("MassEnviroScreen.rds")

# read in MA EJ population data from MA EJ Block Groups.R
MA_EJ23 <- readRDS("ma_blkgrpEJ23.rds")

# read in federally recognized tribal lands Land Area Representations from https://data.nativeland.info/ne/dataset/bureau-of-indian-affairs-land-area-representations
BIA <- st_read("data/BIA/bia_national_lar", "BIA_National_LAR", quiet = TRUE) %>% 
  select(LARName) %>% 
  st_make_valid(.)

# Use clean geometry for block groups and a join MA EJ pop data
MassEnviroScreen <- block_groups(state = "MA", year = 2023, cb = TRUE) %>% 
  filter(!st_is_empty(.)) %>% 
  select(GEOID) %>% 
  left_join(., st_drop_geometry(MassEnviroScreen), by = "GEOID") %>% 
  select(GEOID, COSUB, PollutionBurden10, 
         PopCharacteristics10, MassEnviroScore) %>% 
  mutate(PollutionBurden100 = PollutionBurden10*10,
         PopCharacteristics100 = PopCharacteristics10*10) %>% 
  select(-PollutionBurden10, -PopCharacteristics10) %>% 
  st_transform("+proj=longlat +datum=WGS84") %>% 
  left_join(., MA_EJ23, by = "GEOID") %>% 
  st_join(., st_transform(BIA, crs = st_crs(.))) %>% 
  replace_na(list(LARName = "None")) %>% 
  mutate(popMES = if_else(MassEnviroScore >= 75, 
                          "<b style=\"color:white;background-color:#FF0000;\">MassEnviroScore:</b> ",
                          "<b style=\"color:white;background-color:#053061;\">MassEnviroScore:</b> "),
         popMHI = if_else(medHHincMAPCT <= 65,
                          "<b style=\"color:white;background-color:#FF0000;\">Median Household Income:</b> ",
                          "<b style=\"color:white;background-color:#053061;\">Median Household Income:</b> "),
         popLEP = if_else(limitEngpctE >= 25,
                          "<b style=\"color:white;background-color:#FF0000;\">Limited English Households:</b> ",
                          "<b style=\"color:white;background-color:#053061;\">Limited English Households:</b> "),
         popLAR = if_else(LARName != "None",
                          "<b style=\"color:white;background-color:#FF0000;\">Tribal Territory:</b> ",
                          "<b style=\"color:white;background-color:#053061;\">Tribal Territory:</b> ")) %>% 
  mutate(DAC = if_else(MassEnviroScore >= 75 | 
                         medHHincMAPCT <= 65 | 
                         limitEngpctE >= 25 | 
                         LARName != "None", "Yes", "No"))

# save to rds and csv
saveRDS(MassEnviroScreen, "MassEnviroScreenDACs.rds")
write_csv(MassEnviroScreen, "MassEnviroScreenDACs.csv")
