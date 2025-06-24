# Generate MassEnviroScreen modeled on CalEnviroScreen
# load necessary libraries
pacman::p_load(tidyverse, tidycensus, sf, tigris, readxl, foreign, nngeo, terra)
# library(tidyverse)
# library(sf)
# library(tidycensus)
# library(tigris)
# library(readxl)
# library(foreign)
# library(nngeo) # to calculate nearest neighbor
# library(terra)
options(tigris_use_cache = TRUE)

## Generate socioeconomic factor indicators
# identify census variables to download
v23 <- load_variables(year = 2023, "acs5", cache = TRUE)

# POVERTY STATUS IN THE PAST 12 MONTHS OF PEOPLE IN HOUSING UNITS
ma_blkgrp23pov <- get_acs(geography = "block group", year = 2023, state = "MA", output = "wide",
                          variables = c(povHHStatus = "B17101_001",
                                        povHHBelow = "B17101_002")) %>% 
  mutate(povHHpctE = povHHBelowE/povHHStatusE*100,
         povHHpctM = moe_prop(num = povHHBelowE, denom = povHHStatusE, moe_num = povHHBelowM,
                              moe_denom = povHHStatusM)*100) %>% 
  select(-NAME, -starts_with("povHHStatus")) %>% 
  mutate(SEpctilePOV = percent_rank(povHHpctE)*100)

# POPULATION 25 YEARS AND OVER WITH LESS THAN HS EDUCATION
ma_blkgrp23edu <- get_acs(geography = "block group", year = 2023, state = "MA", 
                          variables = c(pop = "B15003_001"), output = "wide") %>% 
  select(-NAME)
var <- str_pad(c(2:16), width = 3, side = "left", pad = "0") %>% 
  paste0("B15003_",.)
ma_blkgrp23HS <- get_acs(geography = "block group", year = 2023, state = "MA", 
                         variables = var) %>% 
  group_by(GEOID) %>% 
  summarize(HSlessE = sum(estimate),
            HSlessM = moe_sum(moe, HSlessE)) %>% 
  ungroup() %>% 
  left_join(., ma_blkgrp23edu, by = "GEOID") %>% 
  mutate(HSlesspctE = HSlessE/popE*100, 
         HSlesspctM = moe_prop(num = HSlessE, denom = popE, moe_num = HSlessM, 
                               moe_denom = popM)*100) %>% 
  select(-popE, -popM) %>% 
  mutate(SEpctileHS = percent_rank(HSlesspctE)*100)
rm(ma_blkgrp23edu)

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
  select(-popE, -popM) %>% 
  mutate(SEpctileLEP = percent_rank(limitEngpctE)*100)
rm(ma_blkgrp23langpop)

# Employment Status for the Population 16 Years and Over
ma_blkgrp23employ <- get_acs(geography = "block group", year = 2023, state = "MA", 
                             variables = c(civemp = "B23025_003",
                                           unemp = "B23025_005"), output = "wide") %>% 
  mutate(unemploypctE = unempE/civempE*100, 
         unemploypctM = moe_prop(num = unempE, denom = civempE, moe_num = unempM, 
                                 moe_denom = civempM)) %>% 
  select(-NAME, -starts_with("civ")) %>% 
  mutate(SEpctileEMP = percent_rank(unemploypctE)*100)

# Percent of households in a census tract that are both low income (making less than 80% of the HUD Area Median Family Income) and severely burdened by housing costs (paying greater than 50% of their income to housing costs)
# Download HUD CHAS (Comprehensive Housing Affordability Strategy) data at Census tract level. See https://www.huduser.gov/portal/datasets/cp.html
# unzip("data/CHAS/2017thru2021-140-csv.zip")
# read in relevant table
hhburden <- read_csv("data/CHAS/140/Table12.csv") %>% 
  filter(st == "25") %>% 
  transmute(geoid = geoid, tract = tract, 
            hhburden = (T12_est7 + T12_est11 + T12_est24 + T12_est28 + T12_est41 + T12_est45 + 
                          T12_est58 + T12_est62 + T12_est75 + T12_est79 + T12_est93 + T12_est97 
                        + T12_est110 + T12_est114 + T12_est127 + T12_est131 + T12_est144 + 
                          T12_est148 + T12_est161 + T12_est166)/T12_est1 * 100) %>% 
  mutate(SEpctileHHB = percent_rank(hhburden)*100,
         geoid2 = str_trunc(geoid, 11 ,"left", ellipsis = ""))

# Downscale county subdivision to block groups by assigning same score to all block gorups within county subdivision
ma_cosub23 <- county_subdivisions(state = "MA", year = 2023) %>% 
  filter(!st_is_empty(.)) %>% 
  st_transform(., crs = 26986) %>%  # transform to MA State Plane
  rename(COSUB = NAME)

# Downscale to block groups by assigning same score to all block groups within tract
ma_blkgrp23 <- block_groups(state = "MA", year = 2023) %>% 
  filter(!st_is_empty(.)) %>% 
  st_transform(., crs = 26986) %>%  # transform to MA State Plane
  mutate(GEOID_TRACT = str_trunc(GEOID, 11 ,"right", ellipsis = "")) %>%  # tract-level GEOID for downscaling
  st_join(., select(ma_cosub23, COSUB), largest = TRUE)

ma_tract23 <- tracts(state = "MA", year = 2023) %>% 
  filter(!st_is_empty(.)) %>% 
  st_transform(., crs = 26986) # transform to MA State Plane

## Sensitive Population indicators
# load CDC places data with prevalence values by census tract. see https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/cwsq-ngmh/about_data
health_tract <- read_csv("data/PLACES/PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2024_release_20241027.csv") %>% 
  filter(StateDesc == "Massachusetts" & 
           Measure %in% c("High blood pressure among adults", # EPA recommended
                          # "Coronary heart disease among adults",
                          "Current asthma among adults",
                          "Cancer (non-skin) or melanoma among adults")) %>% 
  pivot_wider(id_cols = LocationID, names_from = Measure, values_from = Data_Value) %>% 
  mutate(SPpctileHPRSSR = percent_rank(`High blood pressure among adults`)*100,
         SPpctileASTHMA = percent_rank(`Current asthma among adults`)*100,
         SPpctileCANCER = percent_rank(`Cancer (non-skin) or melanoma among adults`)*100)

# load low birthweight data from MA Vital Stats. See https://www.mass.gov/info-details/birth-outcomes-data-of-massachusetts-residents 
lbw_cosub <- read_excel("data/MADPH/Birth_Community_Detailed_Topic_of_Massachusetts_Residents.xlsx", 
                        sheet = "Delivery Information") %>% 
  filter(`Delivery Information Topic` == "Birthweight" & 
           `Comparison Sub-Topic` %in% c("Low (LBW): <2500 grams", 
                                         "Very Low (VLBW): <1500 grams")) %>% 
  mutate(`Percent of live Birth` = as.numeric(`Percent of live Birth`)*100) %>% 
  group_by(City) %>% 
  summarize(LBWpct = sum(`Percent of live Birth`, na.rm = TRUE)) %>% 
  mutate(SPpctileLBW = percent_rank(LBWpct)*100)

# RECOMMENDED ENVTL HEALTH DISPARITY INDICATOR BY EPA. see https://www.epa.gov/environmentaljustice/indicators-environmental-health-disparities
# load pediatric asthma from MA Environmental Public Health Tracking. See https://matracking.ehs.state.ma.us/Health-Data/Asthma/index.html
asthma_cosub <- read_csv("data/MADPH/pediatricAsthma2017_23.csv") %>% 
  filter(`School Year` %in% c("2017-2018","2022-2023")) %>% 
  mutate(Prevalence = as.numeric(Prevalence)) %>% 
  group_by(Geography) %>% 
  summarize(Prevalence = mean(Prevalence, na.rm = TRUE)) %>% 
  mutate(SPpctileASTHMAped = percent_rank(Prevalence)*100)

# load myocardial infarction from MA Environmental Public Health Tracking. See https://matracking.ehs.state.ma.us/Health-Data/Asthma/index.html
myocardio_cosub <- read_xlsx("data/MADPH/MyoCardioInfarchospitalization2017_21per10k.xlsx") %>% 
  filter(str_detect(`Geo Description`, " - Average")) %>% 
  mutate(`Age Adjusted Rate` = as.numeric(`Age Adjusted Rate`),
         SPpctileMYOC = percent_rank(`Age Adjusted Rate`)*100)

# load ejscreen low life expectancy variable, although note that original data for that metric comes at tract level from Life Expectancy at Birth from CDC, National Center for Health Statistics https://www.cdc.gov/nchs/data-visualization/life-expectancy/index.html
life_blkgrp <- read_csv("data/EJSCREEN24/EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv") %>% 
  filter(ST_ABBREV == "MA") %>% 
  select(ID, P_LIFEEXPPCT) %>% 
  rename_with(~str_remove(., "P_"), .cols = P_LIFEEXPPCT) %>% 
  rename_with(~str_c("SPpctile", .), .cols = LIFEEXPPCT)


## Environmental Exposure Indicators
# download.file(url = "https://gaftp.epa.gov/EJScreen/2024/2.31_August_useMe/EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip", 
#               destfile = "EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip")
# unzip("EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip", exdir = ".")
# load ejscreen variables with percentile values; create for PRE1960PCT; rename
ejscreen <- read_csv("data/EJSCREEN24/EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv") %>% 
  filter(ST_ABBREV == "MA") %>% 
  select(ID, P_PM25, P_OZONE, P_DSLPM, P_NO2, P_PTRAF, P_RSEI_AIR, P_DWATER) %>% 
  rename_with(~str_remove(., "P_"), .cols = P_PM25:P_DWATER) %>% 
  rename_with(~str_c("EXPpctile", .), .cols = PM25:DWATER)

# Children's Lead Risk from Housing. Percentage of households within a census tract with likelihood of lead-based paint (LBP) hazards from the age of housing combined with the percentage of households that are both low-income (household income less than 80% of the county median family income) and have children under 6 years old. HERE WE USE HUD CHAS (Comprehensive Housing Affordability Strategy) data at Census tract level. See https://www.huduser.gov/portal/datasets/cp.html DIFFERENT FROM CALENVIROSCREEN METHOD. METRIC HERE IS HOUSING UNIT STRUCTURE BUILT BEFORE 1979 AND LESS THAN 80% HUD area median family income AND CHILDREN 6 OR YOUNGER. 
blrisk_tract <- read_csv("data/CHAS/140/Table13.csv") %>% 
  filter(st == "25") %>% 
  transmute(tract = paste0(st,cnty,tract), 
            blrisk = (T13_est21 + T13_est24 + T13_est27 + T13_est37 + T13_est40 + T13_est43 + 
                         T13_est70 + T13_est73 + T13_est76 + T13_est86 + T13_est89 + 
                        T13_est92)/T13_est1 * 100,
            EXPpctileBLRISK = percent_rank(blrisk)*100)


## Environmental Effects Indicators
# Weighted sum of sites undergoing cleanup actions by governmental authorities or by property owners. 
# read in MassDEP BWSC Downloadable Sites List from https://www.mass.gov/info-details/downloadable-contaminated-site-lists
# release.dbf - Primary release info
# actions.dbf - Actions that occurred against releases
# chemical.dbf - Chemicals that were released
# location.dbf - Location type for a release
# source.dbf - Sources of the release
# unzip("Release.zip")
# LOCATION <- read.dbf("LOCATION.DBF")
# RELEASE <- read.dbf("RELEASE.DBF")
# SOURCE <- read.dbf("SOURCE.DBF")
# ACTION <- read.dbf("ACTION.DBF")
# CHEMICAL <- read.dbf("CHEMICAL.DBF")

# Load census blocks with population
census2020 <- load_variables(year = 2020, dataset = "pl")
ma_blocks <- get_decennial(geography = "block", year = 2020, state = "MA", 
                           variables = "P1_001N", geometry = TRUE, output = "wide") %>% 
  filter(P1_001N > 0 & !st_is_empty(.)) %>% 
  st_transform(., crs = 26986) # transform to MA State Plane

# Load Superfund sites from EPA OLEM at https://edg.epa.gov/data/PUBLIC/OLEM/OLEM-OSRTI/NPL_Boundaries.zip
# unzip("data/EPA/NPL_Boundaries.zip")
# st_layers("data/EPA/NPL_Boundaries.gdb")
# superfund_poly <- st_read("data/EPA/NPL_Boundaries.gdb", "SITE_BOUNDARIES_SF") %>% 
#   filter(STATE_CODE == "MA") %>% 
#   st_transform(., crs = 26986)  # transform to MA State Plane
# # calculate distance from superfund poly to nearest neighboring block within 1000m
# superfund_nn <- st_nn(superfund_poly, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE)
#   # extract distances from second list as vector
# superfund_dist <- sapply(superfund_nn[[2]], "[", 1)
#   # bind distances
# superfund_poly$dists <- superfund_dist
#   # save object with dists to avoid having to repeat
# saveRDS(superfund_poly, file = "data/EPA/superfund_poly.rds")
  # read in processed data with distance to nearest populated block
superfund_poly <- readRDS("data/EPA/superfund_poly.rds")
# adjust weights by distance
superfund_poly <- superfund_poly %>% 
  mutate(superfundScore = case_when(
    dists > 1000 ~ 0,
    dists >= 750 & dists <= 1000 ~ 0.1*12,
    dists >= 500 & dists < 750 ~ 0.25*12,
    dists >= 250 & dists < 500 ~ 0.5*12,
    dists < 250 ~ 1*12
  ))
# sum up values by block group
superfund_poly <- superfund_poly %>% 
  select(superfundScore) %>% 
  st_join(., ma_blkgrp23) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(superfundScore = sum(superfundScore, na.rm = TRUE)) %>% 
  mutate(EFFCTpctileSUPERFUND = percent_rank(superfundScore)*100)

# Load Brownfields from EPA ACRES
# brownfields <- read_csv("data/EPA/Brownfield Properties (ACRES).csv") %>% 
#   filter(STATE_CODE == "MA") %>% 
#   st_as_sf(., coords = c("LONGITUDE83", "LATITUDE83"), crs = 4269) %>% 
#   st_transform(., crs = 26986)  # transform to MA State Plane
# # calculate distance from brownfield poly to nearest neighboring block within 1000m
# brownfields_nn <- st_nn(brownfields, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE)
# # extract distances from second list as vector
# brownfields_dist <- sapply(brownfields_nn[[2]], "[", 1)
# # bind distances
# brownfields$dists <- brownfields_dist
# # save object with dists to avoid having to repeat
# saveRDS(brownfields, file = "data/EPA/brownfields.rds")
# read in processed data with distance to nearest populated block
brownfields <- readRDS("data/EPA/brownfields.rds")
# adjust weights by distance
brownfields <- brownfields %>% 
  mutate(brownfieldsScore = case_when(
    dists > 1000 ~ 0,
    dists >= 750 & dists <= 1000 ~ 0.1*4,
    dists >= 500 & dists < 750 ~ 0.25*4,
    dists >= 250 & dists < 500 ~ 0.5*4,
    dists < 250 ~ 1*4
  ))
# sum up values by block group
brownfields <- brownfields %>% 
  select(brownfieldsScore) %>% 
  st_join(., ma_blkgrp23) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(brownfieldsScore = sum(brownfieldsScore, na.rm = TRUE)) %>% 
  mutate(EFFCTpctileBROWNFIELDS = percent_rank(brownfieldsScore)*100)


# load MA DEP 21E sites
# download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/c21e_pt.zip", destfile = "data/MASSGIS/c21e_pt.zip")
# # unzip("data/MASSGIS/c21e_pt.zip")
# C21E_pt <- st_read("data/MASSGIS/C21E_PT.shp")
# # calculate distance from 21E to nearest neighboring block within 1000m
# C21E_pt_nn <- st_nn(C21E_pt, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE)
# # extract distances from second list as vector
# C21E_pt_dist <- sapply(C21E_pt_nn[[2]], "[", 1)
# # bind distances
# C21E_pt$dists <- C21E_pt_dist
# # save object with dists to avoid having to repeat
# saveRDS(C21E_pt, file = "data/MASSGIS/C21E_pt.rds")
# read in processed data with distance to nearest populated block
C21E_pt <- readRDS("data/MASSGIS/C21E_pt.rds")
# adjust weights by distance
C21E_pt <- C21E_pt %>% 
  mutate(C21E_ptScore = case_when(
    dists > 1000 ~ 0,
    dists >= 750 & dists <= 1000 ~ 0.1*12,
    dists >= 500 & dists < 750 ~ 0.25*12,
    dists >= 250 & dists < 500 ~ 0.5*12,
    dists < 250 ~ 1*12
  ))
# sum up values by block group
C21E_pt <- C21E_pt %>% 
  select(C21E_ptScore) %>% 
  st_join(., ma_blkgrp23) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(C21E_ptScore = sum(C21E_ptScore, na.rm = TRUE)) %>% 
  mutate(EFFCTpctileC21E = percent_rank(C21E_ptScore)*100)


# load MA DEP AUL sites
# download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/aul_pt.zip", destfile = "data/MASSGIS/aul_pt.zip")
# unzip("data/MASSGIS/aul_pt.zip")
# aul_pt <- st_read("data/MASSGIS/AUL_PT.shp")
# Determine which sites are within 1000m or less of populated census blocks and assign weights based on distances
# calculate distance from aul to nearest neighboring block within 1000m WARNING - TAKES 15 MIN!
# aul_nn <- st_transform(ma_blocks, crs = st_crs(aul_pt)) %>% 
#   st_nn(aul_pt, ., k = 1, maxdist = 1000, returnDist = TRUE)
# # extract distances from second list as vector
# aul_dist <- sapply(aul_nn[[2]], "[", 1)
# # bind distances
# aul_pt$dists <- aul_dist
# # save object with dists to avoid having to repeat
# saveRDS(aul_pt, file = "data/MASSGIS/aul_pt.rds")
# read in processed data with distance to nearest populated block
aul_pt <- readRDS("data/MASSGIS/aul_pt.rds")
# adjust weights by distance
aul_pt <- aul_pt %>% 
  mutate(aul_ptScore = case_when(
    RAO_CLASS %in% c("A1","B1") | dists > 1000 ~ 0,
    (RAO_CLASS %in% c("C1","C2") | STATUS %in% c("TIERI", "TIER1D", "TIER 2")) & 
      dists >= 750 & dists <= 1000 ~ 0.1*12,
    (RAO_CLASS %in% c("C1","C2") | STATUS %in% c("TIERI", "TIER1D", "TIER 2")) &
      dists >= 500 & dists < 750 ~ 0.25*12,
    (RAO_CLASS %in% c("C1","C2") | STATUS %in% c("TIERI", "TIER1D", "TIER 2")) &
      dists >= 250 & dists < 500 ~ 0.5*12,
    (RAO_CLASS %in% c("C1","C2") | STATUS %in% c("TIERI", "TIER1D", "TIER 2")) &
      dists < 250 ~ 1*12,
    RAO_CLASS %in% c("A2","A3","A4","B2","B3") & dists >= 750 & dists <= 1000 ~ 0.1*10,
    RAO_CLASS %in% c("A2","A3","A4","B2","B3") & dists >= 500 & dists < 750 ~ 0.25*10,
    RAO_CLASS %in% c("A2","A3","A4","B2","B3") & dists >= 250 & dists < 500 ~ 0.5*10,
    RAO_CLASS %in% c("A2","A3","A4","B2","B3") & dists < 250 ~ 1*10,
    .default = 0
  ))
# sum up values by block group
aul_pt <- aul_pt %>% 
  select(aul_ptScore) %>% 
  st_join(., ma_blkgrp23) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(aul_ptScore = sum(aul_ptScore, na.rm = TRUE)) %>% 
  mutate(EFFCTpctileAUL_PT = percent_rank(aul_ptScore)*100)




## Groundwater Threats: Land disposal sites, LUSTs, cleanup sites, dairy CAFOs
# US EPA's UST Finder data is a national composite of leaking underground storage tanks, underground storage tank facilities, and underground storage tanks as of 2018-2021. Data downloaded via ArcGIS Pro at https://epa.maps.arcgis.com/home/item.html?id=5a3ae0ed53564b6fa519f08e30e79e93 
# # load USTs
# st_layers("data/EPA/USTfinder.gdb")
# USTfeatures <- st_read("USTfinder.gdb", "USTfacilities") %>% 
#   filter(State == "Massachusetts") %>% 
#   st_transform(., crs = 26986)  # transform to MA State Plane
# 
# USTreleases <- st_read("data/EPA/USTfinder.gdb", "USTreleases") %>% 
#   filter(State == "Massachusetts" & !st_is_empty(.)) %>% 
#   # filter(!st_is_empty(.)) %>% 
#   st_zm(., drop = TRUE) %>% # GEOS doesn't support 3D geometry
#   st_transform(., crs = 26986)  # transform to MA State Plane
# 
# USTs <- st_read("data/EPA/USTfinder.gdb", "USTs") %>% 
#   filter(State == "Massachusetts")
# # calculate distance from UST to nearest neighboring block within 1000m
# UST_nn <- st_nn(USTreleases, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE)
# # extract distances from second list as vector
# UST_dist <- sapply(UST_nn[[2]], "[", 1)
# # bind distances
# USTreleases$dists <- UST_dist
# # save object with dists to avoid having to repeat
# saveRDS(USTreleases, file = "data/EPA/USTreleases.rds")
# read in processed data with distance to nearest populated block
USTreleases <- readRDS("data/EPA/USTreleases.rds")
# adjust weights by distance
USTreleases <- USTreleases %>% 
  mutate(USTScore = case_when(
    dists > 1000 ~ 0,
    dists >= 750 & dists <= 1000 ~ 0.1*3,
    dists >= 500 & dists < 750 ~ 0.25*3,
    dists >= 250 & dists < 500 ~ 0.5*3,
    dists < 250 ~ 1*3
  ))
# sum up values by block group
USTreleases <- USTreleases %>% 
  select(USTScore) %>% 
  st_join(., ma_blkgrp23) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(USTScore = sum(USTScore, na.rm = TRUE)) %>% 
  mutate(EFFCTpctileUST = percent_rank(USTScore)*100)

# MA DEP Groundwater Discharge Permits
# download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/gwp.zip", "data/MASSGIS/gwp.zip")
# unzip("data/MASSGIS/gwp.zip")
# GWP <- st_read("data/MASSGIS/GWP_PT.shp")
# # calculate distance from GWP to nearest neighboring block within 1000m
# GWP_nn <- st_nn(GWP, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE)
# # extract distances from second list as vector
# GWP_dist <- sapply(GWP_nn[[2]], "[", 1)
# # bind distances
# GWP$dists <- GWP_dist
# # save object with dists to avoid having to repeat
# saveRDS(GWP, file = "data/MASSGIS/GWP.rds")
# read in processed data with distance to nearest populated block
GWP <- readRDS("data/MASSGIS/GWP.rds")
# adjust weights by distance
GWP <- GWP %>% 
  mutate(GWPScore = case_when(
    dists > 1000 ~ 0,
    TYPE %in% c("I","S") & dists >= 750 & dists <= 1000 ~ 0.1*5,
    TYPE %in% c("I","S") & dists >= 500 & dists < 750 ~ 0.25*5,
    TYPE %in% c("I","S") & dists >= 250 & dists < 500 ~ 0.5*5,
    TYPE %in% c("I","S") & dists < 250 ~ 1*5,
    .default = 2
  ))
# sum up values by block group
GWP <- GWP %>% 
  select(GWPScore) %>% 
  st_join(., ma_blkgrp23) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(GWPScore = sum(GWPScore, na.rm = TRUE)) %>% 
  mutate(EFFCTpctileGWP = percent_rank(GWPScore)*100)


# Hazardous Waste - MA DEP Major Facilities
# download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/bwpmajor_pt.zip", "data/MASSGIS/bwpmajor_pt.zip")
# dir.create("bwp")
# unzip("bwpmajor_pt.zip", exdir = "data/MASSGIS")
# # read in all major facilities
# BWPMAJOR_PT <- st_read("data/MASSGIS", "BWPMAJOR_PT")
# # calculate distance from BWP to nearest neighboring block within 1000m
# BWP_nn <- st_nn(BWPMAJOR_PT, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE)
# # extract distances from second list as vector
# BWP_dist <- sapply(BWP_nn[[2]], "[", 1)
# # bind distances
# BWPMAJOR_PT$dists <- BWP_dist
# # save object with dists to avoid having to repeat
# saveRDS(BWPMAJOR_PT, file = "data/MASSGIS/BWPMAJOR_PT.rds")
# read in processed data with distance to nearest populated block
BWPMAJOR_PT <- readRDS("data/MASSGIS/BWPMAJOR_PT.rds")
# adjust weights by distance
BWPMAJOR_PT <- BWPMAJOR_PT %>% 
  mutate(BWPScore = case_when(
    dists > 1000 ~ 0,
    (LQG_MA == "Y" | LQG_RCRA == "Y" | LQTU == "Y") & dists >= 750 & dists <= 1000 ~ 0.1*2,
    (LQG_MA == "Y" | LQG_RCRA == "Y" | LQTU == "Y") & dists >= 500 & dists < 750 ~ 0.25*2,
    (LQG_MA == "Y" | LQG_RCRA == "Y" | LQTU == "Y") & dists >= 250 & dists < 500 ~ 0.5*2,
    (LQG_MA == "Y" | LQG_RCRA == "Y" | LQTU == "Y") & dists < 250 ~ 1*2,
    (TSDF == "Y" | HWR == "Y") & dists >= 750 & dists <= 1000 ~ 0.1*7,
    (TSDF == "Y" | HWR == "Y") & dists >= 500 & dists < 750 ~ 0.25*7,
    (TSDF == "Y" | HWR == "Y") & dists >= 250 & dists < 500 ~ 0.5*7,
    (TSDF == "Y" | HWR == "Y") & dists < 250 ~ 1*7,
    .default = 1 # NEED TO ALSO WEIGHT BY DISTANCE
  ))
# sum up values by block group
BWPMAJOR_PT <- BWPMAJOR_PT %>% 
  select(BWPScore) %>% 
  st_join(., ma_blkgrp23) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(BWPScore = sum(BWPScore, na.rm = TRUE)) %>% 
  mutate(EFFCTpctileBWPMAJOR_PT = percent_rank(BWPScore)*100)


# Solid Waste
# Acquire MassDEP Solid Waste Diversion and Disposal layer (only valid until 2016)
# download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/solidwaste.zip", "data/MASSGIS/solidwaste.zip")
# dir.create("solidwaste")
# unzip("data/MASSGIS/solidwaste.zip", exdir = "data/MASSGIS")
# # read in solid waste polygons
# sw_poly <- st_read("data/MASSGIS", "SW_LD_POLY")
# # read in solid waste dumping grounds
# sw_dump <- st_read("data/MASSGIS", "SW_LD_POLY_DUMPINGGROUND")
# # read in solid waste points
# sw_pt <- st_read("data/MASSGIS", "SW_LD_PT")
# # calculate distance from SW to nearest neighboring block within 1000m
# sw_poly_nn <- st_nn(sw_poly, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE)
# # extract distances from second list as vector
# sw_poly_dist <- sapply(sw_poly_nn[[2]], "[", 1)
# # bind distances
# sw_poly$dists <- sw_poly_dist
# # save object with dists to avoid having to repeat
# saveRDS(sw_poly, file = "data/MASSGIS/sw_poly.rds")
# read in processed data with distance to nearest populated block
sw_poly <- readRDS("data/MASSGIS/sw_poly.rds")
# adjust weights by distance
sw_poly <- sw_poly %>% 
  mutate(SWScore = case_when(
    dists > 1000 ~ 0, 
    # CATGRPGIS == "DG" & dists >= 750 & dists <= 1000 ~ 0.1*6,
    # CATGRPGIS == "DG" & dists >= 500 & dists < 750 ~ 0.25*6,
    # CATGRPGIS == "DG" & dists >= 250 & dists < 500 ~ 0.5*6,
    # CATGRPGIS == "DG" & dists < 250 ~ 1*6,
    STATUS == "Active" & 
      WASTE_TYPE %in% c("ASH", "C&D WASTE", "MSW", "SLUDGE") & 
      dists >= 750 & dists <= 1000 ~ 0.1*8,
    STATUS == "Active" & 
      WASTE_TYPE %in% c("ASH", "C&D WASTE", "MSW", "SLUDGE") & 
      dists >= 500 & dists < 750 ~ 0.25*8,
    STATUS == "Active" & 
      WASTE_TYPE %in% c("ASH", "C&D WASTE", "MSW", "SLUDGE") & 
      dists >= 250 & dists < 500 ~ 0.5*8,
    STATUS == "Active" & 
      WASTE_TYPE %in% c("ASH", "C&D WASTE", "MSW", "SLUDGE") & 
      dists < 250 ~ 1*8,
    STATUS == "Active" & 
      WASTE_TYPE %in% c("TIRES", "WOODWASTE") & 
      dists >= 750 & dists <= 1000 ~ 0.1*4,
    STATUS == "Active" & 
      WASTE_TYPE %in% c("TIRES", "WOODWASTE") & 
      dists >= 500 & dists < 750 ~ 0.25*4,
    STATUS == "Active" & 
      WASTE_TYPE %in% c("TIRES", "WOODWASTE") & 
      dists >= 250 & dists < 500 ~ 0.5*4,
    STATUS == "Active" & 
      WASTE_TYPE %in% c("TIRES", "WOODWASTE") & 
      dists < 250 ~ 1*4,
    .default = 1
  ))
# sum up values by block group
sw_poly <- sw_poly %>% 
  select(SWScore) %>% 
  st_join(., ma_blkgrp23) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(SWScore = sum(SWScore, na.rm = TRUE)) %>% 
  mutate(EFFCTpctileSW_POLY = percent_rank(SWScore)*100)


# Impaired Waters - MassDEP 2022 Integrated List of Waters (305(b)/303(d))
# Identify streams and rivers < 100km in length that fall within 1 km of populated block; 2km for rivers > 100km. Identify lakes, bays, estuaries or shoreline < 25km2 within 1km; 2km for those > 25km2. Count number of unique pollutants from arcs and separately from polys falling within each block group. Sum these counts. 
# pseudo code: 1) get distances to nearest populated block; 2) filter by distance and water body size criteria; 3) join to overlapping block groups; 4) count number of unique pollutants from arcs and separately from polys falling within each block group. Sum these counts for each block group.
# download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/il2022_shp.zip", "data/MASSGIS/il2022_shp.zip")
# dir.create("waters")
# unzip("data/MASSGIS/il2022_shp.zip", exdir = "data/MASSGIS")
# read in streams, rivers lines
# IL_2022_ARC <- st_read("data/MASSGIS", "IL_2022_ARC") %>% 
#   filter(CATEGORY == "5")
# # read in Lakes, Estuaries
# IL_2022_POLY <- st_read("data/MASSGIS", "IL_2022_POLY") %>% 
#   filter(CATEGORY == "5")
# read in table of attributes
IL_ATTAINS_2022 <- read.dbf("data/MASSGIS/IL_ATTAINS_2022.dbf") %>% 
  filter(CATEGORY == "5" & POLTNT_FLG == "Y")

# recode causes to identify unique pollutants
IL_ATTAINS_2022 <- IL_ATTAINS_2022 %>% 
  mutate(CAUSE_UNIQUE = case_when(
    str_detect(CAUSE, "COLI|FECAL|ENTEROCOCCUS")  ~ "E. COLI",
    str_detect(CAUSE, "FLOCCULANT|ODOR|SCUM|TRASH|SEWAGE") ~ "SEWAGE",
    str_detect(CAUSE, "PCB") ~ "PCBs",
    str_detect(CAUSE, "ALGA|PLANTS|NUTRIENT|OXYGEN|CHLOROPHYL") ~ "EUTROPHICATION",
    str_detect(CAUSE, "OIL|PETROL") ~ "PETROLEUM",
    str_detect(CAUSE, "LEAD") ~ "LEAD",
    str_detect(CAUSE, "PHOS") ~ "PHOSPHORUS",
    str_detect(CAUSE, "DDT") ~ "DDT",
    str_detect(CAUSE, "CADMIUM") ~ "CADMIUM",
    str_detect(CAUSE, "COPPER") ~ "COPPER",
    str_detect(CAUSE, "MERCURY") ~ "MERCURY",
    str_detect(CAUSE, "CHLORIDE|SALINITY") ~ "CHLORIDE",
    str_detect(CAUSE, "METAL") ~ "METALS UNSPECIFIED",
    str_detect(CAUSE, "HYDROGEN SULFIDE") ~ "HYDROGEN SULFIDE",
    str_detect(CAUSE, "CHLORDANE") ~ "CHLORDANE",
    str_detect(CAUSE, "AMMONIA") ~ "AMMONIA",
    str_detect(CAUSE, "ARSENIC") ~ "ARSENIC",
    str_detect(CAUSE, "NITROGEN") ~ "NITROGEN",
    str_detect(CAUSE, "ALUMINUM") ~ "ALUMINUM",
    str_detect(CAUSE, "DIOXIN") ~ "DIOXIN",
    str_detect(CAUSE, "PAH") ~ "PAHs",
    str_detect(CAUSE, "PHTHALATE") ~ "PHTHALATE",
    str_detect(CAUSE, "CHROM") ~ "CHROMIUM",
    str_detect(CAUSE, "PCP") ~ "PCP",
    str_detect(CAUSE, "Toxicity|Contaminants|ABNORMAL|BIOASS") ~ "TOXINS UNSPECIFIED",
    .default = NULL
  ))

# # calculate distance from water body to nearest populated block - WARNING TAKES 1 HOUR 40 MIN TO RUN!
# IL_arcs_nn <- st_nn(IL_2022_ARC, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE)
# IL_polys_nn <- st_nn(IL_2022_POLY, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE)
# # extract distances from second list as vector
# IL_arcs_dist <- sapply(IL_arcs_nn[[2]], "[", 1)
# IL_polys_dist <- sapply(IL_polys_nn[[2]], "[", 1)
# # bind distances
# IL_2022_ARC$dists <- IL_arcs_dist
# IL_2022_POLY$dists <- IL_polys_dist
# # save object with dists to avoid having to repeat
# saveRDS(IL_2022_ARC, file = "data/MASSGIS/IL_2022_ARC.rds")
# saveRDS(IL_2022_POLY, file = "data/MASSGIS/IL_2022_POLY.rds")
# read in processed data with distance to nearest populated block
IL_2022_ARC <- readRDS("data/MASSGIS/IL_2022_ARC.rds")
IL_2022_POLY <- readRDS("data/MASSGIS/IL_2022_POLY.rds")

# filter based on distances, join overlapping block groups to assign GEOID, join wtih attributes, group by GEOID, and sum unique pollutants
arc_pollutants <- IL_2022_ARC %>% 
  filter((AU_SIZE <= 60 & dists <= 1000) | (AU_SIZE > 60 & dists < 2000)) %>% 
  st_join(., select(ma_blkgrp23, GEOID)) %>% 
  st_drop_geometry(.) %>% 
  inner_join(IL_ATTAINS_2022, ., by = "AU_ID") %>% 
  group_by(GEOID) %>% 
  summarize(cause_cntArc = n_distinct(CAUSE_UNIQUE))

poly_pollutants <- IL_2022_POLY %>% 
  mutate(AREAkm2 = as.numeric(st_area(.))/10^6) %>% 
  filter((AREAkm2 <= 25 & dists <= 1000) | (AREAkm2 > 25 & dists < 2000)) %>% 
  st_join(., select(ma_blkgrp23, GEOID)) %>% 
  st_drop_geometry(.) %>% 
  inner_join(IL_ATTAINS_2022, ., by = "AU_ID") %>% 
  group_by(GEOID) %>% 
  summarize(cause_cntPoly = n_distinct(CAUSE_UNIQUE))

# bring together and sum and percentile
IL_sum <- ma_blkgrp23 %>% 
  st_drop_geometry(.) %>% 
  select(GEOID) %>% 
  left_join(., arc_pollutants, by = "GEOID") %>% 
  left_join(., poly_pollutants, by = "GEOID") %>% 
  replace_na(., list(cause_cntArc = 0, cause_cntPoly = 0)) %>% 
  mutate(IL_count = cause_cntArc + cause_cntPoly,
         EFFCTpctileIL = percent_rank(IL_count)*100)



## Climate Risks/Vulnerabilities (following Colorado EnviroScreen model)
### Drought. Sum of weekly total percent of an area experiencing a severe, extreme, or exceptional drought (categories D2, D3, or D4).The U.S. Drought Monitor reports the percentage of each county experiencing each of the six drought levels (None, D0, D1, D2, D3, and D4) each week. The sum of areas experiencing D2, D3, D4 level droughts was calculated weekly across all weeks from January 2019 to December 2024. The sum of the weekly drought values across that time period was used to define the Drought measure. All census tracts and census block groups received the Drought value for the county in which they are located.
# read in drought monitor data from U.S. Drought Monitor 2019-2023 https://droughtmonitor.unl.edu/Data.aspx
drought <- read_csv("data/USDA/dm_export_20190101_20250504.csv") %>% 
  filter(MapDate < 20250000 & MapDate > 20190000) %>% # limit to 2019 to 2024
  rowwise() %>% 
  mutate(droughtSum = sum(D2, D3, D4), # sum severe/extreme/exceptional drought pcts per week
         CountyFIPS = str_sub(FIPS, start = -3)) %>% 
  group_by(CountyFIPS) %>% 
  summarize(droughtSum = sum(droughtSum)) %>% # aggregate all weeks by county
  mutate(CLIMpctilDrought = percent_rank(droughtSum)*100)

# Wildfire risk. The mean wildfire hazard potential within each geographic area is used as the Wildfire risk score. U.S. Department of Agriculture (USDA), U.S. Forest Service (USFS) https://www.fs.usda.gov/rds/archive/catalog/RDS-2015-0047-4
fire <- rast("data/USDA/Data/whp2023_GeoTIF/whp2023_cls_conus.tif") %>% 
  project(., "epsg:26986") %>% 
  crop(., vect(ma_blkgrp23)) %>% 
  extract(., vect(ma_blkgrp23), fun = mean, na.rm = TRUE, bind = TRUE) %>% 
  st_as_sf(.) %>% 
  transmute(GEOID = GEOID, 
            WHPmean = class_desc,
            CLIMpctilWHP = percent_rank(WHPmean)*100) %>% 
  st_drop_geometry(.)

# Flood risk. The area of all features with 1% Annual Chance Flood Hazard within a geographic area divided by the total area of the geographic area. If no flood areas were found within the geographic area, a value of zero was used. MassGIS https://www.mass.gov/info-details/massgis-data-fema-national-flood-hazard-layer
flood <- st_read("data/MASSGIS/FEMA_NFHL_POLY.shp") %>% 
  filter(FLD_ZONE %in% c("A", "AE", "AH", "AO", "VE")) %>% 
  st_intersection(., ma_blkgrp23) %>% 
  mutate(fldArea = as.numeric(st_area(.))) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(fldArea = sum(fldArea))

flood <- ma_blkgrp23 %>% 
  transmute(GEOID = GEOID,
            Area = as.numeric(st_area(.))) %>% 
  left_join(., flood, by = "GEOID") %>% 
  replace_na(., list(fldArea = 0)) %>% 
  mutate(pctFldArea = fldArea/Area*100,
         CLIMpctilFLD = percent_rank(pctFldArea)*100) %>% 
  st_drop_geometry(.)

# Heat. Average number of days between May and September from 2019 through 2023 in which daily high temperature exceeded the 90th percentile of historical daily high temperatures. Data Source: National Environmental Public Health Tracking Network via the U.S. Centers for Disease Control (CDC), Heat & Heat Related Illness (HRI), Historical Temperature & Heat Index, 2019-2023 https://ephtracking.cdc.gov/ . Query: Heat & Health-Related Illness (HRI > Historical Temperature & Heat Index > Annual Number of Extreme Heat Days > MA Census Tracts > 2019 - 2023 > Heat Metric Max Daily Temp > Relative Threshold 90th Percentile)
heat <- read_csv("data/CDC/data_134739.csv") %>% 
  mutate(CensusTract = as.character(CensusTract)) %>% 
  group_by(CensusTract) %>% 
  summarize(heatMean = mean(Value, na.rm = TRUE)) %>% 
  mutate(CLIMpctilHEAT = percent_rank(heatMean)*100)



# BRING IT ALL TOGETHER
MassEnviroScreen <- ma_blkgrp23 %>% 
  select(GEOID, GEOID_TRACT, COSUB, COUNTYFP) %>% 
  left_join(., ejscreen, by = c("GEOID" = "ID")) %>% 
  left_join(., select(blrisk_tract, tract, EXPpctileBLRISK), 
            by = c("GEOID_TRACT" = "tract")) %>% 
  # left_join(., select(bll_cosub, COSUB, EXPpctileBLL), by = "COSUB") %>% 
  # left_join(., select(bll_cosub2, Geography, EXPpctileBLL2), by = c("COSUB" = "Geography")) %>% 
  left_join(., select(superfund_poly, GEOID, EFFCTpctileSUPERFUND), by = "GEOID") %>% 
  left_join(., select(brownfields, GEOID, EFFCTpctileBROWNFIELDS), by = "GEOID") %>% 
  left_join(., select(C21E_pt, GEOID, EFFCTpctileC21E), by = "GEOID") %>% 
  left_join(., select(aul_pt, GEOID, EFFCTpctileAUL_PT), by = "GEOID") %>% 
  left_join(., select(USTreleases, GEOID, EFFCTpctileUST), by = "GEOID") %>% 
  left_join(., select(GWP, GEOID, EFFCTpctileGWP ), by = "GEOID") %>% 
  left_join(., select(BWPMAJOR_PT, GEOID, EFFCTpctileBWPMAJOR_PT), by = "GEOID") %>% 
  left_join(., select(sw_poly, GEOID, EFFCTpctileSW_POLY), by = "GEOID") %>% 
  left_join(., select(IL_sum, GEOID, EFFCTpctileIL), by = "GEOID") %>% 
  left_join(., select(health_tract, LocationID, SPpctileHPRSSR, SPpctileCANCER),
            by = c("GEOID_TRACT" = "LocationID")) %>% 
  left_join(., select(lbw_cosub, City, SPpctileLBW), by = c("COSUB" = "City")) %>% 
  left_join(., select(asthma_cosub, Geography, SPpctileASTHMAped), 
            by = c("COSUB" = "Geography")) %>% 
  left_join(., life_blkgrp, by = c("GEOID" = "ID")) %>% 
  left_join(., select(ma_blkgrp23HS, GEOID, SEpctileHS), by = "GEOID") %>% 
  left_join(., select(hhburden, geoid2, SEpctileHHB), by = c("GEOID_TRACT" = "geoid2")) %>% 
  left_join(., select(ma_blkgrp23language, GEOID, SEpctileLEP), by = "GEOID") %>% 
  left_join(., select(ma_blkgrp23pov, GEOID, SEpctilePOV), by = "GEOID") %>% 
  left_join(., select(ma_blkgrp23employ, GEOID, SEpctileEMP), by = "GEOID") %>% 
  left_join(., select(drought, CountyFIPS, CLIMpctilDrought), 
            by = c("COUNTYFP" = "CountyFIPS")) %>% 
  left_join(., select(fire, GEOID, CLIMpctilWHP), by = "GEOID") %>% 
  left_join(., select(flood, GEOID, CLIMpctilFLD), by = "GEOID") %>% 
  left_join(., select(heat, CensusTract, CLIMpctilHEAT), 
            by = c("GEOID_TRACT" = "CensusTract")) %>% 
  mutate(across(c(EXPpctilePM25:EXPpctileRSEI_AIR, EFFCTpctileSUPERFUND:EFFCTpctileIL), 
                ~replace_na(.x, 0))) %>% 
  rowwise() %>% # compute average component scores
  mutate(AvgExposure = mean(c_across(EXPpctilePM25:EXPpctileBLRISK), na.rm = TRUE),
         AvgEffect = mean(c_across(EFFCTpctileSUPERFUND:EFFCTpctileIL), na.rm = TRUE),
         AvgClimate = mean(c_across(CLIMpctilDrought:CLIMpctilHEAT), na.rm = TRUE),
         AvgSensitivePops = mean(c_across(SPpctileHPRSSR:SPpctileLIFEEXPPCT), na.rm = TRUE),
         AvgSocioEconFacts = mean(c_across(SEpctileHS:SEpctileEMP), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(AvgEffect0_5 = AvgEffect*0.5, # half weight Effect scores
         AvgClimate0_5 = AvgClimate*0.5) %>% # half weight Effect scores
  rowwise() %>% 
  # Avg component group values for Pollution Burden and Population Characteristics 
  mutate(PollutionBurden = sum(c(AvgExposure,AvgEffect0_5,AvgClimate0_5), na.rm = TRUE)/2,
         PopCharacteristics = sum(c(AvgSensitivePops,AvgSocioEconFacts), na.rm = TRUE)/2) %>% 
  ungroup() %>% 
  mutate(PollutionBurden10 = percent_rank(PollutionBurden)*10, # scaled scores, 1 - 10
         PopCharacteristics10 = percent_rank(PopCharacteristics)*10,
         MassEnviroScoreRaw = PollutionBurden10 * PopCharacteristics10,
         MassEnviroScore = percent_rank(MassEnviroScoreRaw)*100) # scaled final score 1 - 100

# save for later analysis
saveRDS(MassEnviroScreen, "MassEnviroScreen.rds")