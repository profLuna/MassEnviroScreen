# Generate MassEnviroScreen modeled on CalEnviroScreen
# load necessary libraries
pacman::p_load(tidyverse, tidycensus, sf, tigris, readxl, foreign, nngeo, terra)
options(tigris_use_cache = TRUE)

## Generate socioeconomic factor indicators
# identify census variables to download
# v23 <- load_variables(year = 2023, "acs5", cache = TRUE)

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
           Measure %in% c("High blood pressure among adults",
                          "Coronary heart disease among adults",
                          "Chronic obstructive pulmonary disease among adults",
                          # "Current asthma among adults",
                          "Cancer (non-skin) or melanoma among adults")) %>% 
  pivot_wider(id_cols = LocationID, names_from = Measure, values_from = Data_Value) %>% 
  mutate(SPpctileHPRSSR = percent_rank(`High blood pressure among adults`)*100,
         SPpctileHRTDIS = percent_rank(`Coronary heart disease among adults`)*100,
         # SPpctileASTHMA = percent_rank(`Current asthma among adults`)*100,
         SPpctileCANCER = percent_rank(`Cancer (non-skin) or melanoma among adults`)*100,
         SPpctileCOPD = percent_rank(`Chronic obstructive pulmonary disease among adults`)*100)

# # load low birthweight data from MA Vital Stats. See https://www.mass.gov/info-details/birth-outcomes-data-of-massachusetts-residents 
# lbw_cosub <- read_excel("data/MADPH/Birth_Community_Detailed_Topic_of_Massachusetts_Residents.xlsx", 
#                         sheet = "Delivery Information") %>% 
#   filter(`Delivery Information Topic` == "Birthweight" & 
#            `Comparison Sub-Topic` %in% c("Low (LBW): <2500 grams", 
#                                          "Very Low (VLBW): <1500 grams")) %>% 
#   mutate(`Percent of live Birth` = as.numeric(`Percent of live Birth`)*100) %>% 
#   group_by(City) %>% 
#   summarize(LBWpct = sum(`Percent of live Birth`, na.rm = TRUE)) %>% 
#   mutate(SPpctileLBW = percent_rank(LBWpct)*100)

# RECOMMENDED ENVTL HEALTH DISPARITY INDICATOR BY EPA. see https://www.epa.gov/environmentaljustice/indicators-environmental-health-disparities
# # load pediatric asthma from MA Environmental Public Health Tracking. See https://matracking.ehs.state.ma.us/Health-Data/Asthma/index.html
# asthma_cosub <- read_csv("data/MADPH/pediatricAsthma2017_23.csv") %>% 
#   filter(`School Year` %in% c("2017-2018","2022-2023")) %>% 
#   mutate(Prevalence = as.numeric(Prevalence)) %>% 
#   group_by(Geography) %>% 
#   summarize(PedAsthmaPrevalence = mean(Prevalence, na.rm = TRUE)) %>% 
#   mutate(SPpctileASTHMAped = percent_rank(PedAsthmaPrevalence)*100)

# Calculate weighted average percent pediatric asthma per block group. load pediatric asthma by school by BG from MassDEP Cumulative Impact Analysis in Air Quality Permitting at https://www.mass.gov/info-details/cumulative-impact-analysis-in-air-quality-permitting#cia-guidance-and-tools-
# associate K-12 schools with block groups within 1/2 mile
download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/schools.zip", destfile = "data/MASSGIS/schools.zip")
unzip("data/MASSGIS/schools.zip", exdir = "data/MASSGIS")
schools <- st_read("data/MASSGIS/Schools", "SCHOOLS_PT")
schools_halfmile <- st_join(schools["SCHID"], ma_blkgrp23["GEOID"], 
                            join = st_is_within_distance, dist = 805)
# join to schools with enrollment and ashtma data and calc weighted average by block group
asthma_blkgrp <- read_xlsx("data/DEP/Indicator data for cumulative impact analysis UPDATED Jan 2025.xlsx", skip = 1, sheet = "Pediatric Asthma by School") %>% 
  transmute(SCHID = `School Code`,
            sch_enroll = as.numeric(`Average Enrollment Count`),
            pedAsthmaPrevalence = as.numeric(`Pediatric Asthma Prevalence\r\n(% of students)`)) %>% 
  filter(!is.na(pedAsthmaPrevalence) | !is.na(sch_enroll)) %>% 
  inner_join(schools_halfmile, ., by = "SCHID") %>% # assign BG ID to each school
  group_by(GEOID) %>% 
  summarize(pedAsthmaPrevalence = weighted.mean(pedAsthmaPrevalence, sch_enroll)) %>% 
  ungroup() %>% 
  mutate(SPpctileAsthmaPed = percent_rank(pedAsthmaPrevalence)*100) %>% 
  st_drop_geometry()


# # load school list with associated block groups within 1/2 mile
# DEP_schools_blkgrp <- read_xlsx("data/DEP/Indicator data for cumulative impact analysis UPDATED Jan 2025.xlsx", sheet = "Block Group-Schools") %>% 
#   transmute(GEOID = `Block Group`, 
#             SCHID = `SCHOOL CODE\r\n(SCHID)`)
# # join BGs to schools and compute weighted average percentage of pediatric asthma per BG
# asthma_blkgrp <- read_xlsx("data/DEP/Indicator data for cumulative impact analysis UPDATED Jan 2025.xlsx", skip = 1, sheet = "Pediatric Asthma by School") %>% 
#   transmute(SCHID = `School Code`,
#             sch_enroll = as.numeric(`Average Enrollment Count`),
#             pedAsthmaPrevalence = as.numeric(`Pediatric Asthma Prevalence\r\n(% of students)`)) %>% 
#   filter(!is.na(pedAsthmaPrevalence) | !is.na(sch_enroll)) %>% 
#   left_join(., DEP_schools_blkgrp, by = "SCHID") %>% # assign BG ID to each school
#   group_by(GEOID) %>% 
#   summarize(pedAsthmaPrevalence = weighted.mean(pedAsthmaPrevalence, sch_enroll)) %>% 
#   ungroup() %>% 
#   mutate(SPpctileAsthmaPed = percent_rank(pedAsthmaPrevalence)*100)

# # load myocardial infarction from MA Environmental Public Health Tracking. See https://matracking.ehs.state.ma.us/Health-Data/Asthma/index.html
# myocardio_cosub <- read_xlsx("data/MADPH/MyoCardioInfarchospitalization2017_21per10k.xlsx") %>% 
#   filter(str_detect(`Geo Description`, " - Average")) %>% 
#   mutate(`Myocardio Age Adjusted Rate` = as.numeric(`Age Adjusted Rate`),
#          SPpctileMYOC = percent_rank(`Myocardio Age Adjusted Rate`)*100)

# # load ejscreen low life expectancy variable, although note that original data for that metric comes at tract level from Life Expectancy at Birth from CDC, National Center for Health Statistics https://www.cdc.gov/nchs/data-visualization/life-expectancy/index.html
# life_blkgrp <- read_csv("data/EJSCREEN24/EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv") %>% 
#   filter(ST_ABBREV == "MA") %>% 
#   select(ID, P_LIFEEXPPCT) %>% 
#   rename_with(~str_remove(., "P_"), .cols = P_LIFEEXPPCT) %>% 
#   rename_with(~str_c("SPpctile", .), .cols = LIFEEXPPCT)

# load MADPH premature mortality rate, low birth weight, and confirmed elevated blood levels by census tract. MDPH provided an age-adjusted premature mortality rate (PMR per 100,000) by tract. Average Annual Prevalence of Males and Females with estimated confirmed blood lead levels >= 5 micrograms/decilieter in 2017 - 2021 that were between 9 and less than 48 months of age. Acquired from MassDEP Cumulative Impact Analysis in Air Quality Permitting at https://www.mass.gov/info-details/cumulative-impact-analysis-in-air-quality-permitting#cia-guidance-and-tools-
DEP_LBW_PMR_tract <- read_xlsx("data/DEP/Indicator data for cumulative impact analysis UPDATED Jan 2025.xlsx", skip = 1, sheet = "Indicators by Tract") %>% 
  transmute(GEOID_TRACT = as.character(Tract), 
            BLL = `Elevated  Blood Lead\r\r\n\r\r\n(per 1000 screened)`,
            SPpctileBLL = percent_rank(`Elevated  Blood Lead\r\r\n\r\r\n(%tile)`)*100,
            PMR = `PreMature Mortality Rate (PMR) \r\n(pre 100,000 residents)`,
            SPpctilePMR = percent_rank(`PMR\r\r\n(%tile)`)*100, # Sensitive pop indicator
            LBW = `Low Birth Weight\r\r\n\r\r\n(per 100 live singlton births)`,
            SPpctileLBW = percent_rank(`Low Birth Weight\r\r\n\r\r\n(%tile)`)*100) 


## Environmental Exposure Indicators
# download.file(url = "https://gaftp.epa.gov/EJScreen/2024/2.31_August_useMe/EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip", 
#               destfile = "EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip")
# unzip("EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip", exdir = ".")
# load ejscreen variables with percentile values; create for PRE1960PCT; rename
ejscreen <- read_csv("data/EJSCREEN24/EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv") %>% 
  filter(ST_ABBREV == "MA") %>% 
  select(ID, PM25, P_PM25, OZONE, P_OZONE, DSLPM, P_DSLPM, NO2, P_NO2, PTRAF, P_PTRAF, 
         DWATER, P_DWATER) %>% 
  rename_with(function(x) {gsub("P_", "EXPpctile", x)}) %>% 
  mutate(across(PM25:EXPpctileDWATER, ~replace_na(.x, 0)))
  # select(ID, P_PM25, P_OZONE, P_DSLPM, P_NO2, P_PTRAF, P_RSEI_AIR, P_DWATER) %>% 
  # rename_with(~str_remove(., "P_"), .cols = P_PM25:P_DWATER) %>% 
  # rename_with(~str_c("EXPpctile", .), .cols = PM25:DWATER)

# Use EPA's 2020 AirToxScreen total cancer risk at block level and aggregate to block groups. See https://www.epa.gov/AirToxScreen/2020-airtoxscreen-assessment-results
airtox2020_blkgrp <- read_xlsx("data/EPA/Region1_CancerRisk_by_block_srcgrp.xlsx") %>% 
  filter(State == "MA") %>% 
  select(Block, Population, `Total Cancer Risk (per million)`) %>% 
  mutate(GEOID = str_sub(Block, 1,12)) %>% 
  group_by(GEOID) %>% 
  summarize(CancerRisk = weighted.mean(`Total Cancer Risk (per million)`, Population)) %>% 
  ungroup() %>% 
  mutate(CancerRisk = replace_na(CancerRisk, 0), 
         EXPpctileCancerRisk = percent_rank(CancerRisk)*100)
# Use EPA 2019 AirToxScreen Respiratory Hazard Index at tract level. Note that this uses 2010 census tract boundaries. Need to use spatial join to assign to block groups. See https://www.epa.gov/AirToxScreen/2019-airtoxscreen-assessment-results 
ma_tracts2010 <- tracts(state = "MA", year = 2019) %>% 
  select(GEOID) %>% 
  st_transform(., crs = st_crs(ma_blkgrp23))
# join airtox to 2010 tracts and areal weighted interpolation to assign to 2023 block groups
airtox2019_blkgrp <- read_xlsx("data/EPA/2019_National_allHI_byTract.xlsx") %>% 
  filter(str_starts(Tract, "25")) %>% 
  select(Tract, `Respiratory HI`) %>% 
  left_join(ma_tracts2010, ., by = c("GEOID" = "Tract")) %>% 
  st_interpolate_aw(x = .["Respiratory HI"], to = ma_blkgrp23, 
                    extensive = FALSE, keep_NA = TRUE) %>% 
  transmute(GEOID = ma_blkgrp23$GEOID,
            `Respiratory HI` = replace_na(Respiratory.HI, 0),
            EXPpctileRespHI = percent_rank(`Respiratory HI`)*100) %>% 
  st_drop_geometry(.)


# # Children's Lead Risk from Housing. Percentage of households within a census tract with likelihood of lead-based paint (LBP) hazards from the age of housing combined with the percentage of households that are both low-income (household income less than 80% of the county median family income) and have children under 6 years old. HERE WE USE HUD CHAS (Comprehensive Housing Affordability Strategy) data at Census tract level. See https://www.huduser.gov/portal/datasets/cp.html DIFFERENT FROM CALENVIROSCREEN METHOD. METRIC HERE IS HOUSING UNIT STRUCTURE BUILT BEFORE 1979 AND LESS THAN 80% HUD area median family income AND CHILDREN 6 OR YOUNGER. 
# blrisk_tract <- read_csv("data/CHAS/140/Table13.csv") %>% 
#   filter(st == "25") %>% 
#   transmute(tract = paste0(st,cnty,tract), 
#             blrisk = (T13_est21 + T13_est24 + T13_est27 + T13_est37 + T13_est40 + T13_est43 + 
#                          T13_est70 + T13_est73 + T13_est76 + T13_est86 + T13_est89 + 
#                         T13_est92)/T13_est1 * 100,
#             EXPpctileBLRISK = percent_rank(blrisk)*100)


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
# census2020 <- load_variables(year = 2020, dataset = "pl")
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
    dists < 250 ~ 1*12,
    .default = 0
  ))
# sum up values by block group
superfund_poly <- superfund_poly %>% 
  select(superfundScore) %>% 
  st_join(., ma_blkgrp23) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(superfundScore = sum(superfundScore, na.rm = TRUE))

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
    dists >= 750 & dists <= 1000 ~ 0.1*7,
    dists >= 500 & dists < 750 ~ 0.25*7,
    dists >= 250 & dists < 500 ~ 0.5*7,
    dists < 250 ~ 1*7,
    .default = 1
  ))
# sum up values by block group
brownfields <- brownfields %>% 
  select(brownfieldsScore) %>% 
  st_join(., ma_blkgrp23) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(brownfieldsScore = sum(brownfieldsScore, na.rm = TRUE))


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
    STATUS %in% c("TIERI", "TIER1D") & dists > 1000 ~ 0,
    STATUS %in% c("TIERI", "TIER1D") & dists >= 750 & dists <= 1000 ~ 0.1*12,
    STATUS %in% c("TIERI", "TIER1D") & dists >= 500 & dists < 750 ~ 0.25*12,
    STATUS %in% c("TIERI", "TIER1D") & dists >= 250 & dists < 500 ~ 0.5*12,
    STATUS %in% c("TIERI", "TIER1D") & dists < 250 ~ 1*12,
    STATUS == "TIERII" & dists > 1000 ~ 0,
    STATUS == "TIERII" & dists >= 750 & dists <= 1000 ~ 0.1*9,
    STATUS == "TIERII" & dists >= 500 & dists < 750 ~ 0.25*9,
    STATUS == "TIERII" & dists >= 250 & dists < 500 ~ 0.5*9,
    STATUS == "TIERII" & dists < 250 ~ 1*9,
    .default = 1
  ))
# sum up values by block group
C21E_pt <- C21E_pt %>% 
  select(C21E_ptScore) %>% 
  st_join(., ma_blkgrp23) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(C21E_ptScore = sum(C21E_ptScore, na.rm = TRUE))


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
    STATUS %in% c("TIERI", "TIER1D") & dists > 1000 ~ 0,
    STATUS %in% c("TIERI", "TIER1D") & dists >= 750 & dists <= 1000 ~ 0.1*12,
    STATUS %in% c("TIERI", "TIER1D") & dists >= 500 & dists < 750 ~ 0.25*12,
    STATUS %in% c("TIERI", "TIER1D") & dists >= 250 & dists < 500 ~ 0.5*12,
    STATUS %in% c("TIERI", "TIER1D") & dists < 250 ~ 1*12,
    STATUS == "TIER 2" & dists > 1000 ~ 0,
    STATUS == "TIER 2" & dists >= 750 & dists <= 1000 ~ 0.1*9,
    STATUS == "TIER 2" & dists >= 500 & dists < 750 ~ 0.25*9,
    STATUS == "TIER 2" & dists >= 250 & dists < 500 ~ 0.5*9,
    STATUS == "TIER 2" & dists < 250 ~ 1*9,
    RAO_CLASS %in% c("A1","B1") | dists > 1000 ~ 0,
    RAO_CLASS %in% c("A3","A4","C1","C2") & dists >= 750 & dists <= 1000 ~ 0.1*7,
    RAO_CLASS %in% c("A3","A4","C1","C2") & dists >= 500 & dists < 750 ~ 0.25*7,
    RAO_CLASS %in% c("A3","A4","C1","C2") & dists >= 250 & dists < 500 ~ 0.5*7,
    RAO_CLASS %in% c("A3","A4","C1","C2") & dists < 250 ~ 1*7,
    RAO_CLASS %in% c("B2", "B3") & dists >= 750 & dists <= 1000 ~ 0.1*4,
    RAO_CLASS %in% c("B2", "B3") & dists >= 500 & dists < 750 ~ 0.25*4,
    RAO_CLASS %in% c("B2", "B3") & dists >= 250 & dists < 500 ~ 0.5*4,
    RAO_CLASS %in% c("B2", "B3") & dists < 250 ~ 1*4,
    RAO_CLASS == "A2" & dists >= 750 & dists <= 1000 ~ 0.1*1,
    RAO_CLASS == "A2" & dists >= 500 & dists < 750 ~ 0.25*1,
    RAO_CLASS == "A2" & dists >= 250 & dists < 500 ~ 0.5*1,
    RAO_CLASS == "A2" & dists < 250 ~ 1*1,
    .default = 1
  ))
# sum up values by block group
aul_pt <- aul_pt %>% 
  select(aul_ptScore) %>% 
  st_join(., ma_blkgrp23) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(aul_ptScore = sum(aul_ptScore, na.rm = TRUE))

# bring pollution cleanup sites together
cleanup_all <- full_join(superfund_poly, brownfields, by = "GEOID") %>% 
  full_join(C21E_pt, by = "GEOID") %>% 
  full_join(aul_pt, by = "GEOID") %>% 
  rowwise() %>% 
  mutate(cleanup_score = sum(c_across(ends_with("Score")), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(EFFCTpctileCleanup = percent_rank(cleanup_score)*100)


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
  summarize(USTScore = sum(USTScore, na.rm = TRUE))

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
  summarize(GWPScore = sum(GWPScore, na.rm = TRUE))

# bring groundwater threats together
gwater_all <- left_join(USTreleases, GWP, by = "GEOID") %>% 
  rowwise() %>% 
  mutate(gwater_score = sum(c_across(ends_with("Score")), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(EFFCTpctileGrndWater = percent_rank(gwater_score)*100)


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
  mutate(TSDFpt = if_else(TSDF == "Y", 10, NA), 
         HWRpt = if_else(HWR == "Y", 7, NA),
         LQGpt = if_else(LQG_MA == "Y" | LQG_RCRA == "Y" | LQTU == "Y", 1, NA),
         AIRpt = if_else(AIR == "Y", 1, NA),
         RCRApt = if_else(LQG_RCRA == "Y", 2, NA)) %>% 
  rowwise() %>% 
  mutate(TSDFpt2 = if_else(!is.na(TSDF), sum(c_across(c(TSDFpt, LQGpt, AIRpt, RCRApt)), 
                                             na.rm = TRUE), NA),
         HWRpt2 = if_else(!is.na(HWR), sum(c_across(c(HWRpt, LQGpt, AIRpt, RCRApt)), 
                                           na.rm = TRUE), NA)) %>% 
  ungroup() %>% 
  mutate(BWPScore = case_when(
    dists > 1000 ~ 0,
    TSDF == "Y" & dists >= 750 & dists <= 1000 ~ 0.1*TSDFpt2,
    TSDF == "Y" & dists >= 500 & dists < 750 ~ 0.25*TSDFpt2,
    TSDF == "Y" & dists >= 250 & dists < 500 ~ 0.5*TSDFpt2,
    TSDF == "Y" & dists < 250 ~ 1*TSDFpt2,
    HWR == "Y" & dists >= 750 & dists <= 1000 ~ 0.1*HWRpt2,
    HWR == "Y" & dists >= 500 & dists < 750 ~ 0.25*HWRpt2,
    HWR == "Y" & dists >= 250 & dists < 500 ~ 0.5*HWRpt2,
    HWR == "Y" & dists < 250 ~ 1*HWRpt2,
    ((LQG_MA == "Y" | LQG_RCRA == "Y" | LQTU == "Y") & (is.na(TSDF) & is.na(HWR))) & 
      dists >= 750 & dists <= 1000 ~ 0.1*2,
    ((LQG_MA == "Y" | LQG_RCRA == "Y" | LQTU == "Y") & (is.na(TSDF) & is.na(HWR))) & 
      dists >= 500 & dists < 750 ~ 0.25*2,
    ((LQG_MA == "Y" | LQG_RCRA == "Y" | LQTU == "Y") & (is.na(TSDF) & is.na(HWR))) & 
      dists >= 250 & dists < 500 ~ 0.5*2,
    ((LQG_MA == "Y" | LQG_RCRA == "Y" | LQTU == "Y") & (is.na(TSDF) & is.na(HWR))) & 
      dists < 250 ~ 1*2,
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


# Acquire MassDEP Solid Waste Diversion and Disposal layer
# Solid Waste
download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/solidwaste.zip", "data/MASSGIS/solidwaste.zip")
unzip("data/MASSGIS/solidwaste.zip", exdir = "data/MASSGIS")
# read in land disposal solid waste polygons
sw_poly <- st_read("data/MASSGIS", "SW_LD_POLY")
# read in handling facilities: woodwaste, compost, other site points
sw_hf_wwcooth <- st_read("data/MASSGIS", "BWP_PT_HF_WWCOOTH")
# read in handling facilities: large transfer stations, 50 tons or more/day site points
sw_hf_transfer <- st_read("data/MASSGIS", "BWP_PT_HF_TRANSFER")
# read in handling facilities: small transfer stations < 50 tons/day site points
sw_hf_transfer_sm <- st_read("data/MASSGIS", "BWP_PT_HF_TRANS_SM")
# read in handling facilities: construction & demolition processors site points
sw_hf_CD_PROC <- st_read("data/MASSGIS", "BWP_PT_HF_CD_PROC")
# read in Recycling, Composting and other waste Conversion Operations site points
sw_recyc_conv <- st_read("data/MASSGIS", "BWP_PT_OPX_RECY_CONV")
# read in active combustion facilities site points
sw_combust <- st_read("data/MASSGIS", "BWP_PT_COMBUSTION")
# read in inactive or historic combustion facilities site points
sw_combust_inact <- st_read("data/MASSGIS", "BWP_PT_COMBUSTION_HISTORIC")
# combine point files
sw_pt <- sw_combust %>% 
  mutate(CLASS_TYPE = "COMBUST", .after = REGION)
sw_pt <- sw_combust_inact %>% 
  mutate(CLASS_TYPE = "COMBUST INACTIVE", .after = REGION) %>% 
  bind_rows(sw_pt, .)
sw_pt <- sw_hf_CD_PROC %>% 
  mutate(CLASS_TYPE = "HF CD PROC", .after = REGION) %>% 
  bind_rows(sw_pt, .)
sw_pt <- sw_hf_transfer %>% 
  mutate(CLASS_TYPE = "HF TRANSFER", .after = REGION) %>% 
  bind_rows(sw_pt, .)
sw_pt <- sw_hf_transfer_sm %>% 
  mutate(CLASS_TYPE = "HF TRANSFER SM", .after = REGION) %>% 
  bind_rows(sw_pt, .)
sw_pt <- sw_hf_wwcooth %>% 
  bind_rows(sw_pt, .)
sw_pt <- sw_recyc_conv %>% 
  bind_rows(sw_pt, .)
# calculate distance from SW to nearest neighboring block within 1000m
sw_poly_nn <- st_nn(sw_poly, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE)
sw_pt_nn <- st_nn(sw_pt, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE)
# extract distances from second list as vector
sw_poly_dist <- sapply(sw_poly_nn[[2]], "[", 1)
sw_pt_dist <- sapply(sw_pt_nn[[2]], "[", 1)
# bind distances
sw_poly$dists <- sw_poly_dist
sw_pt$dists <- sw_pt_dist
# save object with dists to avoid having to repeat
saveRDS(sw_poly, file = "data/MASSGIS/sw_poly.rds")
saveRDS(sw_pt, file = "data/MASSGIS/sw_pt.rds")
# read in objects
sw_poly <- readRDS("data/MASSGIS/sw_poly.rds")
sw_pt <- readRDS("data/MASSGIS/sw_pt.rds")
# adjust weights by distance for sw polys
sw_poly <- sw_poly %>% 
  mutate(SWScore = case_when(
    dists > 1000 | is.na(dists) ~ 0, 
    str_detect(CATGRPGIS, "DG") & dists >= 750 & dists <= 1000 ~ 0.1*6,
    str_detect(CATGRPGIS, "DG") & dists >= 500 & dists < 750 ~ 0.25*6,
    str_detect(CATGRPGIS, "DG") & dists >= 250 & dists < 500 ~ 0.5*6,
    str_detect(CATGRPGIS, "DG") & dists < 250 ~ 1*6,
    STATUS == "Active" & 
      str_detect(CATGRPGIS, "LF") &
      WASTE_TYPE %in% c("ASH", "C&D WASTE", "MSW", "SLUDGE") & 
      dists >= 750 & dists <= 1000 ~ 0.1*8,
    STATUS == "Active" & 
      str_detect(CATGRPGIS, "LF") &
      WASTE_TYPE %in% c("ASH", "C&D WASTE", "MSW", "SLUDGE") & 
      dists >= 500 & dists < 750 ~ 0.25*8,
    STATUS == "Active" & 
      str_detect(CATGRPGIS, "LF") &
      WASTE_TYPE %in% c("ASH", "C&D WASTE", "MSW", "SLUDGE") & 
      dists >= 250 & dists < 500 ~ 0.5*8,
    STATUS == "Active" & 
      str_detect(CATGRPGIS, "LF") &
      WASTE_TYPE %in% c("ASH", "C&D WASTE", "MSW", "SLUDGE") & 
      dists < 250 ~ 1*8,
    STATUS == "Active" & 
      str_detect(CATGRPGIS, "LF") &
      WASTE_TYPE %in% c("TIRES", "WOODWASTE") & 
      dists >= 750 & dists <= 1000 ~ 0.1*4,
    STATUS == "Active" & 
      str_detect(CATGRPGIS, "LF") &
      WASTE_TYPE %in% c("TIRES", "WOODWASTE") & 
      dists >= 500 & dists < 750 ~ 0.25*4,
    STATUS == "Active" & 
      str_detect(CATGRPGIS, "LF") &
      WASTE_TYPE %in% c("TIRES", "WOODWASTE") & 
      dists >= 250 & dists < 500 ~ 0.5*4,
    STATUS == "Active" & 
      str_detect(CATGRPGIS, "LF") &
      WASTE_TYPE %in% c("TIRES", "WOODWASTE") & 
      dists < 250 ~ 1*4,
    .default = 1
  ))
# adjust weights by distance for sw points
sw_pt <- sw_pt %>% 
  mutate(SWScore = case_when(
    CLASS_TYPE == "COMBUST" ~ 10,
    CLASS_TYPE == "COMBUST INACTIVE" ~ 1,
    CLASS_TYPE %in% c("COMPOST", "CMPOST","GPRECY", "IPRECY", "IPCNVR") ~ 4,
    CLASS_TYPE %in% c("GPCMPT", "IPCMPT", "SMHNDL", "HF TRANSFER SM") ~ 2,
    CLASS_TYPE == "GPDGST" ~ 3,
    CLASS_TYPE %in% c("HF CD PROC", "HF TRANSFER", "LGHNDL") ~ 5,
    .default = 1
  ))
# sum up values by block group
sw_poly <- sw_poly %>% 
  transmute(SWScorePoly = SWScore) %>% 
  st_join(., ma_blkgrp23) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(SWScorePoly = sum(SWScorePoly, na.rm = TRUE))

sw_pt <- sw_pt %>% 
  transmute(SWScorePt = SWScore) %>% 
  st_join(., ma_blkgrp23) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(SWScorePt = sum(SWScorePt, na.rm = TRUE))

sw_all <- ma_blkgrp23 %>% 
  st_drop_geometry(.) %>% 
  select(GEOID) %>% 
  full_join(., sw_pt, by = "GEOID") %>% 
  full_join(., sw_poly, by = "GEOID") %>% 
  rowwise(.) %>% 
  mutate(SWScore = sum(c_across(SWScorePt:SWScorePoly), na.rm = TRUE)) %>% 
  ungroup(.) %>% 
  mutate(EFFCTpctileSW = percent_rank(SWScore)*100)


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
# Note that NFHL DFIRMs are not available for entire northwest quadrant of the state. Need to use Q3 data to supplement. Franklin County is not available in either dataset as of July 2025, so supplement for Franklin County with "riverine flooding - exposure - impacted area" from FEMA NRI at census tract level, downscaled to block groups. 
NFHL <- st_read("data/MASSGIS/FEMA_NFHL_POLY.shp") %>% 
  select(FLD_ZONE) %>% 
  filter(FLD_ZONE %in% c("A", "AE", "AH", "AO", "VE"))
Q3 <- st_read("data/MASSGIS/", "Q3FLOOD_POLY_NO_NFHL") %>% 
  filter(ZONE %in% c("AE", "A", "D", "AO")) %>% 
  transmute(FLD_ZONE = ZONE)
NRI <- st_read("data/FEMA/NRI_GDB_CensusTracts.gdb", "NRI_CensusTracts") %>% 
  filter(STATE == "Massachusetts" & COUNTY == "Franklin") %>% 
  transmute(GEOID_TRACT = TRACTFIPS, Area = AREA, fldArea = RFLD_EXP_AREA) %>% 
  st_drop_geometry(.) %>% 
  inner_join(ma_blkgrp23, ., by = "GEOID_TRACT") %>% 
  select(GEOID, Area, fldArea)
# bind NFHL and Q3 together and intersect with block groups
flood <- bind_rows(NFHL, Q3) %>% 
  st_intersection(., ma_blkgrp23) %>% 
  mutate(fldArea = as.numeric(st_area(.))) %>% 
  st_drop_geometry(.) %>%
  group_by(GEOID) %>% 
  summarize(fldArea = sum(fldArea))
# aggregate to block group polygons for NFHL and Q3
flood <- ma_blkgrp23 %>% 
  transmute(GEOID = GEOID,
            Area = as.numeric(st_area(.))) %>% 
  inner_join(., flood, by = "GEOID") %>% 
  replace_na(., list(fldArea = 0))
# isolate NRI block groups that are not present in flood, bind to flood, and compute pct flood
flood <- anti_join(NRI, st_drop_geometry(flood), by = "GEOID") %>% 
  bind_rows(flood, .) %>% 
  st_drop_geometry(.) %>%
  mutate(pctFldArea = fldArea/Area*100,
         CLIMpctilFLD = percent_rank(pctFldArea)*100)

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
  # left_join(., select(blrisk_tract, tract, blrisk, EXPpctileBLRISK), 
  #           by = c("GEOID_TRACT" = "tract")) %>% 
  left_join(., select(airtox2020_blkgrp, GEOID, CancerRisk, EXPpctileCancerRisk), 
            by = "GEOID") %>% 
  left_join(., select(airtox2019_blkgrp, GEOID, `Respiratory HI`, EXPpctileRespHI), 
            by = "GEOID") %>% 
  left_join(., select(cleanup_all, GEOID, cleanup_score, EFFCTpctileCleanup), by = "GEOID") %>% 
  left_join(., select(gwater_all, GEOID, gwater_score, EFFCTpctileGrndWater), by = "GEOID") %>% 
  left_join(., select(BWPMAJOR_PT, GEOID, BWPScore, EFFCTpctileBWPMAJOR_PT), by = "GEOID") %>% 
  left_join(., select(sw_all, GEOID, SWScore, EFFCTpctileSW), by = "GEOID") %>% 
  left_join(., select(IL_sum, GEOID, IL_count, EFFCTpctileIL), by = "GEOID") %>% 
  left_join(., select(health_tract, LocationID, `High blood pressure among adults`, 
                      SPpctileHPRSSR, `Coronary heart disease among adults`, SPpctileHRTDIS,
                      `Chronic obstructive pulmonary disease among adults`, SPpctileCOPD,
                      # `Current asthma among adults`, SPpctileASTHMA,
                      `Cancer (non-skin) or melanoma among adults`, 
                      SPpctileCANCER),
            by = c("GEOID_TRACT" = "LocationID")) %>% 
  left_join(., select(asthma_blkgrp, GEOID, pedAsthmaPrevalence, SPpctileAsthmaPed), 
            by = "GEOID") %>% 
  # left_join(., select(lbw_cosub, City, LBWpct, SPpctileLBW), by = c("COSUB" = "City")) %>% 
  # left_join(., select(asthma_cosub, Geography, PedAsthmaPrevalence, SPpctileASTHMAped), 
  #           by = c("COSUB" = "Geography")) %>% 
  # left_join(., life_blkgrp, by = c("GEOID" = "ID")) %>% 
  left_join(., select(DEP_LBW_PMR_tract, GEOID_TRACT, BLL, SPpctileBLL, LBW, SPpctileLBW, PMR, 
                      SPpctilePMR),
            by = "GEOID_TRACT") %>% 
  left_join(., select(ma_blkgrp23HS, GEOID, HSlesspctE, SEpctileHS), by = "GEOID") %>% 
  left_join(., select(hhburden, geoid2, hhburden, SEpctileHHB), by = c("GEOID_TRACT" = "geoid2")) %>% 
  left_join(., select(ma_blkgrp23language, GEOID, limitEngpctE, SEpctileLEP), by = "GEOID") %>% 
  left_join(., select(ma_blkgrp23pov, GEOID, povHHpctE, SEpctilePOV), by = "GEOID") %>% 
  left_join(., select(ma_blkgrp23employ, GEOID, unemploypctE, SEpctileEMP), by = "GEOID") %>% 
  left_join(., select(drought, CountyFIPS, droughtSum, CLIMpctilDrought), 
            by = c("COUNTYFP" = "CountyFIPS")) %>% 
  left_join(., select(fire, GEOID, WHPmean, CLIMpctilWHP), by = "GEOID") %>% 
  left_join(., select(flood, GEOID, pctFldArea, CLIMpctilFLD), by = "GEOID") %>% 
  left_join(., select(heat, CensusTract, heatMean, CLIMpctilHEAT), 
            by = c("GEOID_TRACT" = "CensusTract")) %>% 
  mutate(across(c(starts_with("EXPpctile"), starts_with("EFFCTpctile"), 
                  starts_with("CLIMpctil")), 
                ~replace_na(.x, 0))) %>% 
  rowwise() %>% # compute average component scores
  mutate(AvgExposure = mean(c_across(starts_with("EXPpctile")), na.rm = TRUE),
         AvgEffect = mean(c_across(starts_with("EFFCTpctile")), na.rm = TRUE),
         AvgClimate = mean(c_across(starts_with("CLIMpctil")), na.rm = TRUE),
         AvgSensitivePops = mean(c_across(starts_with("SPpctile")), na.rm = TRUE),
         AvgSocioEconFacts = mean(c_across(starts_with("SEpctile")), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(AvgEffect0_5 = AvgEffect*0.5, # half weight Effect scores
         AvgClimate0_5 = AvgClimate*0.5) %>% # half weight Effect scores
  rowwise() %>% 
  # Avg component group values for Pollution Burden and Population Characteristics 
  mutate(PollutionBurden = sum(c(AvgExposure,AvgEffect0_5,AvgClimate0_5), na.rm = TRUE)/2,
         PopCharacteristics = sum(c(AvgSensitivePops,AvgSocioEconFacts), na.rm = TRUE)/2) %>% 
  ungroup() %>% 
  mutate(PollutionBurden10 = (PollutionBurden/max(PollutionBurden))*10, # scaled scores, 1 - 10
         PopCharacteristics10 = (PopCharacteristics/max(PopCharacteristics))*10,
         MassEnviroScoreRaw = PollutionBurden10 * PopCharacteristics10,
         MassEnviroScore = percent_rank(MassEnviroScoreRaw)*100) # scaled final score 1 - 100

# Attach EJ pop identifiers and identify Unfairly Burdened Areas (UBAs)
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
  mutate(PollutionBurden100 = PollutionBurden10*10,
         PopCharacteristics100 = PopCharacteristics10*10) %>% 
  # select(-PollutionBurden10, -PopCharacteristics10) %>% 
  st_transform("+proj=longlat +datum=WGS84") %>% 
  left_join(., select(MA_EJ23, GEOID, NAME, minorityPctE, medHHincE, medHHincMA, medHHincMUNIPCT, medHHincMUNIE, medHHincMUNIPCT, medHHincMAPCT, EJ_CRITERIA, EJ:EJ_CRIT_DESC), by = "GEOID") %>% 
  st_join(., st_transform(BIA, crs = st_crs(.))) %>% 
  replace_na(list(LARName = "None")) %>% 
  mutate(pedAsthmaPctSt = pedAsthmaPrevalence/12.23*100, # state average from DPH
         LBWPctSt = LBW/2.17*100,
         BLLPctSt = BLL/18.4*100,
         PMRPctSt = PMR/292.5*100,
         CHDPctSt = `Coronary heart disease among adults`/4.6*100,
         PM25PctSt = PM25/6.52*100,
         OZONEPctSt = OZONE/56.7*100,
         UBA = if_else(MassEnviroScore >= 75 | 
                         medHHincMAPCT <= 65 | 
                         limitEngpctE >= 25 | 
                         LARName != "None" |
                         pedAsthmaPctSt > 200 |
                         LBWPctSt > 200 |
                         BLLPctSt > 200 |
                         PMRPctSt > 200 |
                         CHDPctSt > 200 |
                         PM25PctSt > 200 |
                         OZONEPctSt > 200,
                       "Yes", "No")) %>% 
  mutate(popMES = if_else(MassEnviroScore >= 75, 
                          "<b style=\"color:white;background-color:#FF0000;\">MassEnviroScore:</b> ",
                          "<b style=\"color:white;background-color:#053061;\">MassEnviroScore:</b> ", missing = "<b style=\"color:white;background-color:#053061;\">MassEnviroScore:</b> "),
         popMHI = if_else(medHHincMAPCT <= 65,
                          "<b style=\"color:white;background-color:#FF0000;\">Median Household Income:</b> ",
                          "<b style=\"color:white;background-color:#053061;\">Median Household Income:</b> ", missing = "<b style=\"color:white;background-color:#053061;\">Median Household Income:</b> "),
         popLEP = if_else(limitEngpctE >= 25,
                          "<b style=\"color:white;background-color:#FF0000;\">Limited English Households:</b> ",
                          "<b style=\"color:white;background-color:#053061;\">Limited English Households:</b> ", missing = "<b style=\"color:white;background-color:#053061;\">Limited English Households:</b> "),
         popLAR = if_else(LARName != "None",
                          "<b style=\"color:white;background-color:#FF0000;\">Tribal Territory:</b> ",
                          "<b style=\"color:white;background-color:#053061;\">Tribal Territory:</b> ", missing = "<b style=\"color:white;background-color:#053061;\">Tribal Territory:</b> "),
         popASTHMA = if_else(pedAsthmaPctSt > 200,
                          "<b style=\"color:white;background-color:#FF0000;\">Pediatric Asthma:</b> ",
                          "<b style=\"color:white;background-color:#053061;\">Pediatric Asthma:</b> ", missing = "<b style=\"color:white;background-color:#053061;\">Pediatric Asthma:</b> "),
         popLBW = if_else(LBWPctSt > 200,
                             "<b style=\"color:white;background-color:#FF0000;\">Low Birth Weight:</b> ",
                             "<b style=\"color:white;background-color:#053061;\">Low Birth Weight:</b> ", missing = "<b style=\"color:white;background-color:#053061;\">Low Birth Weight:</b> "),
         popBLL = if_else(BLLPctSt > 200,
                          "<b style=\"color:white;background-color:#FF0000;\">Elevated Blood Lead:</b> ",
                          "<b style=\"color:white;background-color:#053061;\">Elevated Blood Lead:</b> ", missing = "<b style=\"color:white;background-color:#053061;\">Elevated Blood Lead:</b> "),
         popPMR = if_else(PMRPctSt > 200,
                          "<b style=\"color:white;background-color:#FF0000;\">Premature Mortality:</b> ",
                          "<b style=\"color:white;background-color:#053061;\">Premature Mortality:</b> ", missing = "<b style=\"color:white;background-color:#053061;\">Premature Mortality:</b> "),
         popCHD = if_else(CHDPctSt > 200,
                          "<b style=\"color:white;background-color:#FF0000;\">Heart Disease:</b> ",
                          "<b style=\"color:white;background-color:#053061;\">Heart Disease:</b> ", missing = "<b style=\"color:white;background-color:#053061;\">Heart Disease:</b> "),
         popPM25 = if_else(PM25PctSt > 200,
                          "<b style=\"color:white;background-color:#FF0000;\">PM25:</b> ",
                          "<b style=\"color:white;background-color:#053061;\">PM25:</b> ", missing = "<b style=\"color:white;background-color:#053061;\">PM25:</b> "),
         popOZONE = if_else(OZONEPctSt > 200,
                           "<b style=\"color:white;background-color:#FF0000;\">Ozone:</b> ",
                           "<b style=\"color:white;background-color:#053061;\">Ozone:</b> ",
                           missing = "<b style=\"color:white;background-color:#053061;\">Ozone:</b> "))


# save for later analysis and mapping
saveRDS(MassEnviroScreen, "MassEnviroScreen.rds")
# write to CSV
MassEnviroScreen %>% 
  st_drop_geometry() %>% 
  write_csv(., file = "MassEnviroScreenUBAs.csv")
