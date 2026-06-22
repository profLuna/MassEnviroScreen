# Generate MassEnviroScreen cumulative burden geospatial data

# load necessary libraries
pacman::p_load(tidyverse, tidycensus, sf, tigris, readxl, 
               foreign, nngeo, terra, rmapshaper, prism)
options(tigris_use_cache = TRUE)

### Population Characteristics - Socioeconomic Factors
# identify census variables to download
# v24 <- load_variables(year = 2024, "acs5", cache = TRUE)

# POVERTY STATUS IN THE PAST 12 MONTHS OF PEOPLE IN HOUSING UNITS
ma_blkgrp24pov <- get_acs(geography = "block group", year = 2024, state = "MA", output = "wide",
                          variables = c(TotalPop = "B01003_001",
                                        TotalHH = "B11001_001",
                                        povHHStatus = "B17101_001",
                                        povHHBelow = "B17101_002")) %>% 
  mutate(povHHpctE = if_else(povHHStatusE > 0, povHHBelowE/povHHStatusE*100, 0),
         povHHpctM = moe_prop(num = povHHBelowE, denom = povHHStatusE, moe_num = povHHBelowM,
                              moe_denom = povHHStatusM)*100) %>% 
  select(-NAME, -starts_with("povHHStatus")) %>% 
  mutate(SEpctilePOV = percent_rank(povHHpctE)*100)


# POPULATION 25 YEARS AND OVER WITH LESS THAN HS EDUCATION
# start with universe of Population 25 years and over
ma_blkgrp24edu <- get_acs(geography = "block group", year = 2024, state = "MA", 
                          variables = c(pop = "B15003_001"), output = "wide") %>% 
  select(-NAME)

# create vector variable strings for ed attainment below HS
var <- str_pad(c(2:16), width = 3, side = "left", pad = "0") %>% 
  paste0("B15003_",.)

# acquire variables for ed attainment below HS and compute percentage
ma_blkgrp24HS <- get_acs(geography = "block group", year = 2024, state = "MA", 
                         variables = var) %>% 
  group_by(GEOID) %>% 
  summarize(HSlessE = sum(estimate),
            HSlessM = moe_sum(moe, HSlessE)) %>% 
  ungroup() %>% 
  left_join(., ma_blkgrp24edu, by = "GEOID") %>% 
  mutate(HSlesspctE = if_else(popE > 0, HSlessE/popE*100, 0), 
         HSlesspctM = moe_prop(num = HSlessE, denom = popE, moe_num = HSlessM, 
                               moe_denom = popM)*100) %>% 
  select(-popE, -popM) %>% 
  mutate(SEpctileHS = percent_rank(HSlesspctE)*100)

# cleanup
rm(ma_blkgrp24edu)


# Household Language by Household Limited English Speaking Status
# start with universe of households
ma_blkgrp24langpop <- get_acs(geography = "block group", year = 2024, state = "MA", 
                              variables = c(pop = "C16002_001"), output = "wide") %>% 
  select(-NAME)

# create vector of variables for limited English speaking households
var <- str_pad(c(4,7,10,13), width = 3, side = c("left"), pad = "0") %>% 
  paste0("C16002_", .)

# acquire variables for limited English speaking households and compute percentage
ma_blkgrp24language <- get_acs(geography = "block group", year = 2024, state = "MA", 
                               variables = var) %>% 
  group_by(GEOID) %>% 
  summarize(limitEngE = sum(estimate),
            limitEngM = moe_sum(moe, limitEngE)) %>% 
  ungroup() %>% 
  left_join(., ma_blkgrp24langpop, by = "GEOID") %>%
  mutate(limitEngpctE = if_else(popE > 0, limitEngE/popE*100, 0), 
         limitEngpctM = moe_prop(num = limitEngE, denom = popE, moe_num = limitEngM, 
                                 moe_denom = popM)*100) %>% 
  select(-popE, -popM) %>% 
  mutate(SEpctileLEP = percent_rank(limitEngpctE)*100)

# cleanup
rm(ma_blkgrp24langpop)


# Employment Status for the Population 16 Years and Over
ma_blkgrp24employ <- get_acs(geography = "block group", year = 2024, state = "MA", 
                             variables = c(civemp = "B23025_003",
                                           unemp = "B23025_005"), output = "wide") %>% 
  mutate(unemploypctE = if_else(civempE > 0, unempE/civempE*100, 0), 
         unemploypctM = moe_prop(num = unempE, denom = civempE, moe_num = unempM, 
                                 moe_denom = civempM)) %>% 
  select(-NAME, -starts_with("civ")) %>% 
  mutate(SEpctileEMP = percent_rank(unemploypctE)*100)


# Percent of households in a census tract that are both low income (making less than 80% of the HUD Area Median Family Income) and severely burdened by housing costs (paying greater than 50% of their income to housing costs)
# Download HUD CHAS (Comprehensive Housing Affordability Strategy) data at Census tract level. See https://www.huduser.gov/portal/datasets/cp.html
# unzip("data/CHAS/2018thru2022-140-csv.zip")
# read in relevant table
hhburden <- read_csv("data/CHAS/140/Table12.csv") %>% 
  filter(st == "25") %>% 
  transmute(geoid = geoid, tract = tract, 
            hhburden = if_else(T12_est1 > 0, (T12_est7 + T12_est11 + T12_est24 + T12_est28 + T12_est41 + T12_est45 + 
                          T12_est58 + T12_est62 + T12_est75 + T12_est79 + T12_est93 + T12_est97 
                        + T12_est110 + T12_est114 + T12_est127 + T12_est131 + T12_est144 + 
                          T12_est148 + T12_est161 + T12_est165)/T12_est1 * 100, 0)) %>% 
  mutate(SEpctileHHB = percent_rank(hhburden)*100,
         geoid2 = str_trunc(geoid, 11 ,"left", ellipsis = ""))

# Downscale county subdivision to block groups by assigning same score to all block groups within county subdivision
ma_cosub24 <- county_subdivisions(state = "MA", year = 2024) %>% 
  filter(!st_is_empty(.)) %>% 
  st_transform(., crs = 26986) %>%  # transform to MA State Plane
  rename(COSUB = NAME)

# Downscale CHAS to block groups by assigning same score to all block groups within tract
ma_blkgrp24 <- block_groups(state = "MA", year = 2024) %>% 
  filter(!st_is_empty(.)) %>% 
  st_transform(., crs = 26986) %>%  # transform to MA State Plane
  mutate(GEOID_TRACT = str_trunc(GEOID, 11 ,"right", ellipsis = "")) %>%  # tract-level GEOID for downscaling
  st_join(., select(ma_cosub24, COSUB), largest = TRUE)

# download census tract geography
ma_tract24 <- tracts(state = "MA", year = 2024) %>% 
  filter(!st_is_empty(.)) %>% 
  st_transform(., crs = 26986) # transform to MA State Plane



## Sensitive Population indicators
# load CDC places data with prevalence values by census tract. see https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/cwsq-ngmh/about_data
health_tract <- read_csv("data/CDC/PLACES__Local_Data_for_Better_Health,_Census_Tract_Data,_2025_release_20260119.csv") %>% 
  filter(StateDesc == "Massachusetts" & 
           Measure %in% c("High blood pressure among adults",
                          "Coronary heart disease among adults",
                          "Chronic obstructive pulmonary disease among adults",
                          "Cancer (non-skin) or melanoma among adults")) %>% 
  pivot_wider(id_cols = LocationID, names_from = Measure, values_from = Data_Value) %>% 
  mutate(GEOID_TRACT = as.character(LocationID),
         SPpctileHPRSSR = percent_rank(`High blood pressure among adults`)*100,
         SPpctileHRTDIS = percent_rank(`Coronary heart disease among adults`)*100,
         SPpctileCANCER = percent_rank(`Cancer (non-skin) or melanoma among adults`)*100,
         SPpctileCOPD = percent_rank(`Chronic obstructive pulmonary disease among adults`)*100)


# read in pediatric asthma prevalence from DEP. weighted value by block group. block group assignment performed by DPH for DEP. Indicator Data for Cumulative Impact Analysis (UPDATED Dec 2025) from https://www.mass.gov/info-details/cumulative-impact-analysis-in-air-quality-permitting#cia-guidance-and-tools 
# read in list of block groups with assigned schools
blkgrp_schools <- read_xlsx("data/DEP/Indicator-data-for-cumulative-impact-analysis-UPDATED-Dec-2025.xlsx", sheet = "Block Group-Schools")

# read in schools with asthma prevalence values
ped_asthma_shools <- read_xlsx("data/DEP/Indicator-data-for-cumulative-impact-analysis-UPDATED-Dec-2025.xlsx", sheet = "Pediatric Asthma by School", skip = 1) %>% 
  mutate(across(`Average Case Count`:`Pediatric Asthma Prevalence\r\n(%tile)`, 
                as.numeric))

# join by school code and aggregate by block group as enrollment weighted average
asthma_blkgrp <- left_join(ped_asthma_shools, blkgrp_schools, 
                             by = c("School Code" = "SCHOOL CODE\r\n(SCHID)")) %>% 
  rename(GEOID = `Block Group`) %>% 
  group_by(GEOID) %>% 
  summarize(pedAsthmaPrevalence = weighted.mean(`Pediatric Asthma Prevalence\r\n(% of students)`, `Average Enrollment Count`, na.rm = TRUE),
            n_schools = n()) %>% 
  mutate(SPpctileAsthmaPed = percent_rank(pedAsthmaPrevalence)*100)


# load MADPH premature mortality rate, low birth weight, and confirmed elevated blood levels by census tract. MDPH provided an age-adjusted premature mortality rate (PMR per 100,000) by tract. Average Annual Prevalence of Males and Females with estimated confirmed blood lead levels >= 5 micrograms/decilieter in 2017 - 2021 that were between 9 and less than 48 months of age. Indicator Data for Cumulative Impact Analysis (UPDATED Dec 2025) from https://www.mass.gov/info-details/cumulative-impact-analysis-in-air-quality-permitting#cia-guidance-and-tools
DEP_LBW_BLL_tract <- read_xlsx("data/DEP/Indicator-data-for-cumulative-impact-analysis-UPDATED-Dec-2025.xlsx", skip = 1, sheet = "Indicators by Tract") %>% 
  transmute(GEOID_TRACT = as.character(Tract), 
            BLL = `Elevated  Blood Lead\r\r\n\r\r\n(per 1000 screened)`,
            SPpctileBLL = `Elevated  Blood Lead\r\r\n\r\r\n(%tile)`,
            LBW = `Low Birth Weight\r\r\n\r\r\n(per 100 live singlton births)`,
            SPpctileLBW = `Low Birth Weight\r\r\n\r\r\n(%tile)`,
            PMR = `PreMature Mortality Rate (PMR) \r\n(pre 100,000 residents)`,
            SPpctilePMR = `PMR\r\r\n(%tile)`)



## Environmental Exposure Indicators

# download.file(url = "https://gaftp.epa.gov/EJScreen/2024/2.31_August_useMe/EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip", 
#               destfile = "EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip")
# unzip("EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip", exdir = ".")
# load ejscreen variables with percentile values; create for PRE1960PCT; rename
ejscreen <- read_csv("data/EJSCREEN24/EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv") %>% 
  filter(ST_ABBREV == "MA") %>% 
  select(ID, PTRAF, P_PTRAF) %>% 
  rename_with(function(x) {gsub("P_", "EXPpctile", x)})


# acquire daily PM25 data by census tract from EPA's Bayesian space-time downscaling fusion model (downscaler) - Derived Estimates of Air Quality output from https://www.epa.gov/hesc/rsig-related-downloadable-data-files#output . Calculate annual daily average. 
pm25 <- read_csv("data/EPA/2022_pm25_daily_average.txt") %>% 
  filter(str_starts(FIPS, "25")) %>% 
  mutate(across(Longitude:`pm25_daily_average_stderr(ug/m3)`, as.numeric)) %>% 
  group_by(FIPS) %>% 
  summarize(PM25 = mean(`pm25_daily_average(ug/m3)`, na.rm = TRUE)) %>% 
  rename(GEOID_TRACT = FIPS) %>% 
  mutate(EXPpctilePM25 = percent_rank(PM25)*100)


# acquire daily maximum 8-hour ozone data by census tract from EPA's Bayesian space-time downscaling fusion model (downscaler) - Derived Estimates of Air Quality output from https://www.epa.gov/hesc/rsig-related-downloadable-data-files#output . Calculate the annual mean of the 10 highest MDA8 O3 concentrations (the “peak concentration metric”), consistent with the form of the O3 for NAAQS. Follows EPA practice in EJScreen. While the form of the O3 NAAQS is based on the annual 4th highest MDA8 O3 value, here we look at an average across the top 10 days which will span days above and below the value of the 4th high. By looking at an average across multiple days rather than a single day, this metric provides more year-to-year stability while still representing concentrations that correspond to peak ozone exposure. See Environmental Justice Mapping and Screening Tool, EJScreen Technical Documentation for Version 2.3, July 31, 2024. 
ozone <- read_csv("data/EPA/2022_ozone_daily_8hour_maximum.txt") %>% 
  filter(str_starts(FIPS, "25")) %>% 
  mutate(across(Longitude:`ozone_daily_8hour_maximum_stderr(ppb)`, as.numeric)) %>% 
  group_by(FIPS) %>% 
  slice_max(order_by = `ozone_daily_8hour_maximum(ppb)`, n = 10) %>% 
  group_by(FIPS) %>% 
  summarize(OZONE = mean(`ozone_daily_8hour_maximum(ppb)`, na.rm = TRUE)) %>% 
  rename(GEOID_TRACT = FIPS) %>% 
  mutate(EXPpctileOZONE = percent_rank(OZONE)*100)


# Acquire average annual nitrogen dioxide (NO2) surface level concentrations from Nawaz, M. Omar. “Monthly and Annual US TROPOMI Surface NO2 Estimates (~1km × 1km)”. Environmental Science and Technology Air. Zenodo, January 14, 2025. https://doi.org/10.5281/zenodo.14646034. 
# data are provided in netCDF format from George Washington University at https://gwu.app.box.com/s/8id0gcje44o9qye1pbe42qw8ce8ioluj?page=4&sortColumn=name&sortDirection=DESC
# read in the data as spatRaster
no2_2024 <- rast("data/NO2/annual_mean_tropomi_lur_conus_surface_no2_2024.v1.02.nc")

# download block groups in NAD83 to match raster CRS; don't try to reproject!
ma_blkgrpNAD83 <- block_groups(state = "MA", year = 2024, cb = TRUE) %>%
  filter(!st_is_empty(.))

# reproject block groups to match NO2 raster; don't try to reproject raster!
ma_blkgrpCRS84 <- ma_blkgrpNAD83 %>% 
  select(GEOID) %>% 
  st_transform(., crs = st_crs(no2_2024))

# crop and extract values
no2 <- crop(no2_2024, vect(ma_blkgrpCRS84)) %>% 
  extract(., vect(ma_blkgrpCRS84), fun = mean, na.rm = TRUE, bind = TRUE) %>% 
  st_as_sf(.) %>% 
  transmute(GEOID = GEOID, 
            NO2 = surface_no2,
            EXPpctileNO2 = percent_rank(NO2)*100) %>% 
  st_drop_geometry(.)


# Use EPA's 2020 AirToxScreen total cancer risk at block level and aggregate to block groups using population-weighted mean. See https://www.epa.gov/AirToxScreen/2020-airtoxscreen-assessment-results
airtox2020_blkgrp <- read_xlsx("data/EPA/Region1_CancerRisk_by_block_srcgrp.xlsx") %>% 
  filter(State == "MA") %>% 
  rowwise() %>% 
  mutate(`Total Cancer Risk (per million)` = sum(c_across(`PT-StationaryPoint Cancer Risk (per million)`:`BACKGROUND Cancer Risk (per million)`), na.rm = T)) %>% 
  ungroup() %>% 
  select(Block, Population, `Total Cancer Risk (per million)`) %>% 
  mutate(GEOID = str_sub(Block, 1,12)) %>% 
  group_by(GEOID) %>% 
  summarize(CancerRisk = weighted.mean(`Total Cancer Risk (per million)`, Population)) %>%
  ungroup() %>% 
  mutate(EXPpctileCancerRisk = percent_rank(CancerRisk)*100)


# For Diesel PM, use EPA's 2020 AirToxScreen 2020 National Concentration Summaries by Region - Ambient Concentrations at block level and aggregate to block groups using population-weighted mean. See https://www.epa.gov/AirToxScreen/2020-airtoxscreen-assessment-results
airtoxDSLPM2020_blkgrp <- read_xlsx("data/EPA/Region1_2020ATS_Ambient_Concentrations.xlsx") %>% 
  filter(State == "MA") %>% 
  select(Block, Population, `DIESEL PM`) %>% 
  mutate(GEOID = str_sub(Block, 1,12)) %>% 
  group_by(GEOID) %>% 
  summarize(DSLPM = weighted.mean(`DIESEL PM`, Population)) %>% 
  ungroup() %>% 
  mutate(EXPpctileDSLPM = percent_rank(DSLPM)*100)


# Use EPA 2019 AirToxScreen Respiratory Hazard Index at tract level. Note that this uses 2010 census tract boundaries. Spatially interpolate to block groups using areal weighting method. See https://www.epa.gov/AirToxScreen/2019-airtoxscreen-assessment-results 
# acquire 2010 census tract geography
ma_tracts2010 <- tracts(state = "MA", year = 2019) %>% 
  select(GEOID) %>% 
  st_transform(., crs = st_crs(ma_blkgrp24))

# join airtox to 2010 tracts and perform areal weighted interpolation to assign to 2023 block groups
airtox2019_blkgrp <- read_xlsx("data/EPA/2019_National_RespHI_by_tract_srcgrp.xlsx") %>% 
  filter(str_starts(Tract, "25")) %>% 
  rowwise() %>% 
  mutate(`Respiratory HI` = sum(c_across(`PT-StationaryPoint Respiratory (hazard quotient)`:`BACKGROUND Respiratory (hazard quotient)`), na.rm = T)) %>% 
  ungroup() %>% 
  select(Tract, `Respiratory HI`) %>% 
  left_join(ma_tracts2010, ., by = c("GEOID" = "Tract")) %>% 
  st_interpolate_aw(x = .["Respiratory HI"], to = ma_blkgrp24, 
                    extensive = FALSE, keep_NA = TRUE) %>% 
  transmute(GEOID = ma_blkgrp24$GEOID,
            `Respiratory HI` = Respiratory.HI,
            EXPpctileRespHI = percent_rank(`Respiratory HI`)*100) %>% 
  st_drop_geometry(.)


# Drinking Water Non-Compliance Score
# EPA SDWIS Federal Reporting Services > (dropdown) SDWIS Data Reports > Violations
# o	PWS Type = ‘Community water system’
# o	Compliance Period Begin Date > 01-OCT-2021
# o	Primacy Agency Code = ‘MA’
# Data downloaded from USEPA SDWIS Federal Reporting Services 1/12/2026 SDWIS Data Reports > Violations https://sdwis.epa.gov/ords/sfdw_pub/r/sfdw/sdwis_fed_reports_public/9?p9_report=VIO
# from Q4 2021 01-OCT-2021 through Q4 2025 31-DEC-2025
violations <- read_xlsx("data/EPA/Violation Report_20260112_Q42021Q42025.xlsx", 
                        skip = 4)

# assign points to violations. following EPA in EJScreen. 
violations <- violations %>% 
  mutate(Points = case_when(
    `Violation Type` == "Treatment Technique (SWTR and GWR)" ~ 10,
    `Violation Type` == "Maximum Contaminant Level Violation, Average" ~ 5,
    `Violation Type` == "Maximum Contaminant Level Violation, E. coli (RTCR)" ~ 5,
    `Violation Type` == "Treatment Technique No Certif. Operator" ~ 5,
    `Violation Type` == "Treatment Technique, Level 1 Assessment (RTCR)" ~ 5,
    `Violation Type` == "Treatment Technique, Level 2 Assessment (RTCR)" ~ 5,
    `Violation Type` == "WQP Entry Point/Tap Treatment Technique Non-Compliance" ~ 5,
    `Violation Type` == "Failure To Address Deficiency" ~ 5,
    `Violation Type` == "LSL Inventory" ~ 5,
    `Violation Type` == "Lead Service Line Replacement (LSLR)" ~ 5,
    `Violation Type` == "OCCT/SOWT Study/Recommendation" ~ 5,
    `Violation Type` == "OCCT/SOWT Treatment Installation/Demonstration" ~ 5,
    `Violation Type` == "Public Education" ~ 5,
    .default = 1))

# only count violations as unique Rule Name per compliance period
violationsPWS <- violations %>% 
  group_by(`PWS ID`, `Compliance Period Begin Date`, `Rule Name`) %>% 
  summarize(`PWS Name` = first(`PWS Name`), `Rule Name` = first(`Rule Name`), 
            Points = max(Points), Violations = n()) %>% 
  group_by(`PWS ID`) %>% # sum up severity points
  summarize(`PWS Name` = first(`PWS Name`), Score = sum(Points), 
            Violations = sum(Violations))

# MassGIS Data: MassDEP Estimated Public Drinking Water System Service Area Boundaries https://www.mass.gov/info-details/massgis-data-massdep-estimated-public-drinking-water-system-service-area-boundaries March 2025
# Community PWS refers to a public water system that serves at least 15 service connections used by year-round residents or regularly serves at least 25 year-round residents. 
PWSpolys <- st_read("data/MASSGIS/DEP_PWS_Water_Service_Areas/PWS_WATER_SERVICE_AREA_COMM_POLY.shp") %>% 
  st_make_valid() %>% 
  filter(PWS_STATUS == "A" & # active PWS
           PWSPOP_WIN >= 25 & PWSPOP_SUM >= 25 & # at least 25 year-round residents
           PWSNUM_SRV >= 15 & # at least 15 service connections
           !st_is_empty(.)) %>% 
  mutate(PWS_ID_C = paste0("MA", PWS_ID_C)) %>% 
  left_join(., violationsPWS, by = c("PWS_ID_C" = "PWS ID")) %>% 
  replace_na(list(Score = 0, Violations = 0)) # PWS with no listed violations are 0

# Load census blocks with population
# census2020 <- load_variables(year = 2020, dataset = "pl")
ma_blocks <- get_decennial(geography = "block", year = 2020, state = "MA", 
                           variables = "P1_001N", geometry = TRUE, output = "wide") %>% 
  filter(P1_001N > 0 & !st_is_empty(.)) %>% 
  mutate(GEOID_BG = str_trunc(GEOID, width = 12, side = "right", 
                              ellipsis = "")) %>%  # block group-level
  st_transform(., crs = 26986) # transform to MA State Plane

# census 2020 for block group level
ma_blkgrp20 <- get_decennial(geography = "block group", year = 2020, state = "MA", 
                             variables = c(BGpop = "P1_001N"), geometry = TRUE, 
                             output = "wide", 
                             cb = FALSE) %>%
  filter(BGpop > 0 & !st_is_empty(.)) %>% 
  st_transform(., crs = 26986) # transform to MA State Plane

# intersect public water supply areas, census blocks, and census block groups. Score of block is weighted by pct of respective block group population. Blocks that intersect more than one PWS take average of scores. Sum scores to block group level. 
ma_blkgrpDWScore <- st_join(ma_blocks, PWSpolys, left = FALSE) %>% 
  st_drop_geometry(.) %>% 
  inner_join(., select(st_drop_geometry(ma_blkgrp20), GEOID, BGpop), 
             by = c("GEOID_BG" = "GEOID")) %>% 
  mutate(PctBGpop = P1_001N/BGpop,
         ScoreW = Score * PctBGpop) %>% 
  group_by(GEOID) %>% # aggregate back to blocks
  summarize(GEOID_BG = first(GEOID_BG),
            ScoreW = mean(ScoreW, na.rm = TRUE),
            PWS = paste(PWS_NAME, collapse = ", ")) %>% 
  mutate(PWS = str_remove_all(PWS, "NA, |, NA")) %>% 
  group_by(GEOID_BG) %>% # aggregate to block groups
  summarize(DWATER = sum(ScoreW, na.rm = TRUE)) %>% 
  mutate(EXPpctileDWATER = percent_rank(DWATER)*100)

# identify all PWS that intersect with each census block group
ma_blkgrp20pws <- st_join(ma_blkgrp20, PWSpolys, left = FALSE) %>% 
  st_drop_geometry(.) %>%
  arrange(PWS_NAME) %>% 
  group_by(GEOID) %>% 
  summarize(PWS = paste(PWS_NAME, collapse = ", ")) %>% 
  mutate(PWS = str_remove_all(PWS, "NA, |, NA"))

# FINAL JOIN for Drinking Water Non-Compliance Score with names of PWS that intersect each block group
# join scored block groups to block groups with names of intersecting PWS
DWATER <- inner_join(ma_blkgrp20pws, ma_blkgrpDWScore,  
                                by = c("GEOID" = "GEOID_BG"))



## Environmental Effects Indicators

### Pollution Cleanup Sites
# Weighted sum of sites undergoing cleanup actions by governmental authorities or by property owners. 
# load MA DEP 21E sites
# download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/c21e_pt.zip", destfile = "data/MASSGIS/c21e_pt.zip")
# # unzip("data/MASSGIS/c21e_pt.zip")
C21E_pt <- st_read("data/MASSGIS/C21E_PT.shp")

# calculate distance from 21E to nearest neighboring block within 1000m
C21E_pt_nn <- st_nn(C21E_pt, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE,
                    parallel = 4)

# extract distances from second list as vector
C21E_pt_dist <- sapply(C21E_pt_nn[[2]], "[", 1)

# bind distances
C21E_pt$dists <- C21E_pt_dist

# save object with dists to avoid having to repeat
saveRDS(C21E_pt, file = "data/MASSGIS/C21E_pt.rds")

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
  st_join(., ma_blkgrp24) %>% 
  st_drop_geometry(.) %>%
  group_by(GEOID) %>% 
  summarize(C21E_ptScore = sum(C21E_ptScore, na.rm = TRUE))


# load MA DEP AUL sites
# download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/aul_pt.zip", destfile = "data/MASSGIS/aul_pt.zip")
# unzip("data/MASSGIS/aul_pt.zip")
aul_pt <- st_read("data/MASSGIS/AUL_PT.shp")

# isolate AUL sites with non-duplicated RTN for 21E sites
aul_pt <- readRDS("data/MASSGIS/C21E_pt.rds") %>% 
  st_drop_geometry(.) %>% 
  anti_join(aul_pt, ., by = "RTN")

# Determine which sites are within 1000m or less of populated census blocks and assign weights based on distances
# calculate distance from aul to nearest neighboring block within 1000m WARNING - TAKES 15 MIN!
aul_nn <- st_transform(ma_blocks, crs = st_crs(aul_pt)) %>%
  st_nn(aul_pt, ., k = 1, maxdist = 1000, returnDist = TRUE, parallel = 4)

# extract distances from second list as vector
aul_dist <- sapply(aul_nn[[2]], "[", 1)

# bind distances
aul_pt$dists <- aul_dist

# save object with dists to avoid having to repeat
saveRDS(aul_pt, file = "data/MASSGIS/aul_pt.rds")

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
  st_join(., ma_blkgrp24) %>% 
  st_drop_geometry(.) %>%
  group_by(GEOID) %>% 
  summarize(aul_ptScore = sum(aul_ptScore, na.rm = TRUE))

# Load Superfund sites from EPA OLEM at https://edg.epa.gov/data/PUBLIC/OLEM/OLEM-OSRTI/NPL_Boundaries.zip
# unzip("data/EPA/NPL_Boundaries.zip")
# st_layers("data/EPA/NPL_Boundaries.gdb")
superfund_poly <- st_read("data/EPA/NPL_Boundaries.gdb", "SITE_BOUNDARIES_SF") %>%
  filter(STATE_CODE == "MA") %>%
  st_transform(., crs = 26986)  # transform to MA State Plane

# calculate distance from superfund poly to nearest neighboring block within 1000m
superfund_nn <- st_nn(superfund_poly, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE, parallel = 4)

  # extract distances from second list as vector
superfund_dist <- sapply(superfund_nn[[2]], "[", 1)

  # bind distances
superfund_poly$dists <- superfund_dist

  # save object with dists to avoid having to repeat
saveRDS(superfund_poly, file = "data/EPA/superfund_poly.rds")

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
  st_join(., ma_blkgrp24) %>% 
  st_drop_geometry(.) %>%
  group_by(GEOID) %>% 
  summarize(superfundScore = sum(superfundScore, na.rm = TRUE))

# Load Brownfields from EPA ACRES from Facility Registry Service as FRS gdb https://catalog.data.gov/dataset/epa-facility-registry-service-frs-acres8 for whole country. downloaded Jan 21, 2026. Last update 2025-12-16. 
# st_layers("data/EPA/FRS_INTERESTS.gdb")
brownfields <- st_read("data/EPA/FRS_INTERESTS.gdb", "ACRES") %>% 
  filter(STATE_CODE == "MA") %>% 
  st_transform(., crs = 26986)  # transform to MA State Plane

# create 30m buffer around brownfields to check for overlaps
bbuffer <- brownfields %>% 
  select(REGISTRY_ID) %>% 
  st_buffer(., dist = 30)

# exclude brownfields that overlap with C21E, AUL, or NPL sites
brownfields <- st_read("data/MASSGIS/C21E_PT.shp") %>% 
  st_join(bbuffer, ., left = FALSE) %>% 
  st_drop_geometry() %>% 
  group_by(REGISTRY_ID) %>% 
  summarize(overlaps = n()) %>% 
  filter(overlaps > 0) %>% 
  anti_join(brownfields, ., by = "REGISTRY_ID")

# repeat for AUL sites
brownfields <- st_read("data/MASSGIS/AUL_PT.shp") %>% 
  st_join(bbuffer, ., left = FALSE) %>% 
  st_drop_geometry() %>% 
  group_by(REGISTRY_ID) %>% 
  summarize(overlaps = n()) %>% 
  filter(overlaps > 0) %>% 
  anti_join(brownfields, ., by = "REGISTRY_ID")

# repeat for superfund sites
brownfields <- readRDS("data/EPA/superfund_poly.rds") %>% 
  st_join(bbuffer, ., left = FALSE) %>% 
  st_drop_geometry() %>% 
  group_by(REGISTRY_ID) %>% 
  summarize(overlaps = n()) %>% 
  filter(overlaps > 0) %>% 
  anti_join(brownfields, ., by = "REGISTRY_ID")

# calculate distance from brownfield poly to nearest neighboring block within 1000m
brownfields_nn <- st_nn(brownfields, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE,
                        parallel = 4)

# extract distances from second list as vector
brownfields_dist <- sapply(brownfields_nn[[2]], "[", 1)

# bind distances
brownfields$dists <- brownfields_dist

# save object with dists to avoid having to repeat
saveRDS(brownfields, file = "data/EPA/brownfields.rds")

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
  st_join(., ma_blkgrp24) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(brownfieldsScore = sum(brownfieldsScore, na.rm = TRUE))

# bring pollution cleanup sites together
cleanup_all <- full_join(superfund_poly, brownfields, by = "GEOID") %>% 
  full_join(C21E_pt, by = "GEOID") %>% 
  full_join(aul_pt, by = "GEOID") %>% 
  rowwise() %>% 
  mutate(cleanup_score = sum(c_across(ends_with("Score")), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(EFFCTpctileCleanup = percent_rank(cleanup_score)*100)


## Groundwater Threats: Land disposal sites, LUSTs, cleanup sites
# US EPA's UST Finder data is a national composite of leaking underground storage tanks, underground storage tank facilities, and underground storage tanks as of 2018-2021. See https://www.epa.gov/ust/ust-finder. Data downloaded via ArcGIS Pro at https://epa.maps.arcgis.com/home/item.html?id=5a3ae0ed53564b6fa519f08e30e79e93 
# load USTs
# st_layers("data/EPA/USTfinder.gdb")
USTfeatures <- st_read("USTfinder.gdb", "USTfacilities") %>%
  filter(State == "Massachusetts") %>%
  st_transform(., crs = 26986)  # transform to MA State Plane

USTreleases <- st_read("data/EPA/USTfinder.gdb", "USTreleases") %>%
  filter(State == "Massachusetts" & !st_is_empty(.)) %>%
  # filter(!st_is_empty(.)) %>%
  st_zm(., drop = TRUE) %>% # GEOS doesn't support 3D geometry
  st_transform(., crs = 26986)  # transform to MA State Plane

USTs <- st_read("data/EPA/USTfinder.gdb", "USTs") %>%
  filter(State == "Massachusetts")

# calculate distance from UST to nearest neighboring block within 1000m
UST_nn <- st_nn(USTreleases, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE)

# extract distances from second list as vector
UST_dist <- sapply(UST_nn[[2]], "[", 1)

# bind distances
USTreleases$dists <- UST_dist

# save object with dists to avoid having to repeat
saveRDS(USTreleases, file = "data/EPA/USTreleases.rds")

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
  st_join(., ma_blkgrp24) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(USTScore = sum(USTScore, na.rm = TRUE))

# MA DEP Groundwater Discharge Permits
# download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/gwp.zip", "data/MASSGIS/gwp.zip")
# unzip("data/MASSGIS/gwp.zip")
GWP <- st_read("data/MASSGIS/GWP_PT.shp")

# calculate distance from GWP to nearest neighboring block within 1000m
GWP_nn <- st_nn(GWP, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE)

# extract distances from second list as vector
GWP_dist <- sapply(GWP_nn[[2]], "[", 1)

# bind distances
GWP$dists <- GWP_dist

# save object with dists to avoid having to repeat
saveRDS(GWP, file = "data/MASSGIS/GWP.rds")

# read in processed data with distance to nearest populated block
GWP <- readRDS("data/MASSGIS/GWP.rds")

# adjust weights by distance
GWP <- GWP %>% 
  mutate(GWPScore = case_when(
    dists > 1000 ~ 0,
    TYPE %in% c("I","S") & dists >= 750 & dists <= 1000 ~ 0.1*3,
    TYPE %in% c("I","S") & dists >= 500 & dists < 750 ~ 0.25*3,
    TYPE %in% c("I","S") & dists >= 250 & dists < 500 ~ 0.5*3,
    TYPE %in% c("I","S") & dists < 250 ~ 1*3,
    !TYPE %in% c("I","S") & dists >= 750 & dists <= 1000 ~ 0.1*2,
    !TYPE %in% c("I","S") & dists >= 500 & dists < 750 ~ 0.25*2,
    !TYPE %in% c("I","S") & dists >= 250 & dists < 500 ~ 0.5*2,
    !TYPE %in% c("I","S") & dists < 250 ~ 1*2,
    .default = 2
  ))

# sum up values by block group
GWP <- GWP %>% 
  select(GWPScore) %>% 
  st_join(., ma_blkgrp24) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(GWPScore = sum(GWPScore, na.rm = TRUE))

# bring groundwater threats together
gwater_all <- left_join(USTreleases, GWP, by = "GEOID") %>% 
  rowwise() %>% 
  mutate(gwater_score = sum(c_across(ends_with("Score")), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(EFFCTpctileGrndWater = percent_rank(gwater_score)*100)



# Hazardous Waste Generators and Facilities
# MA DEP Major Facilities
# download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/bwpmajor_pt.zip", "data/MASSGIS/bwpmajor_pt.zip")
# dir.create("bwp")
# unzip("bwpmajor_pt.zip", exdir = "data/MASSGIS")
# read in all major facilities
BWPMAJOR_PT <- st_read("data/MASSGIS", "BWPMAJOR_PT")

# calculate distance from BWP to nearest neighboring block within 1000m
BWP_nn <- st_nn(BWPMAJOR_PT, ma_blocks, k = 1, maxdist = 1000, returnDist = TRUE)

# extract distances from second list as vector
BWP_dist <- sapply(BWP_nn[[2]], "[", 1)

# bind distances
BWPMAJOR_PT$dists <- BWP_dist

# save object with dists to avoid having to repeat
saveRDS(BWPMAJOR_PT, file = "data/MASSGIS/BWPMAJOR_PT.rds")

# read in processed data with distance to nearest populated block
BWPMAJOR_PT <- readRDS("data/MASSGIS/BWPMAJOR_PT.rds")

# assign weights by facility type
BWPMAJOR_PT <- BWPMAJOR_PT %>% 
  mutate(TSDFpt = if_else(TSDF == "Y", 10, NA), 
         HWRpt = if_else(HWR == "Y", 7, NA),
         LQGpt = if_else(LQG_MA == "Y" | LQTU == "Y", 1, NA),
         AIRpt = if_else(AIR == "Y", 1, NA),
         RCRApt = if_else(LQG_RCRA == "Y", 3, NA)) %>% 
  rowwise() %>% 
  mutate(TSDFpt2 = if_else(TSDF == "Y", sum(c_across(c(TSDFpt, LQGpt, AIRpt, RCRApt)), 
                                             na.rm = TRUE), NA),
         HWRpt2 = if_else(HWR == "Y", sum(c_across(c(HWRpt, LQGpt, AIRpt, RCRApt)), 
                                           na.rm = TRUE), NA)) %>% 
  ungroup() %>% 
  replace_na(list(dists = 1001)) %>% # NA indicates dist > 1000m
  mutate(BWPScore = case_when(# adjust weights by distance
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
    .default = 0 # 0 value for facilities that are not TSDF, HWR, or LQG
  ))

# sum up values by block group
BWPMAJOR_PT <- BWPMAJOR_PT %>% 
  select(BWPScore) %>% 
  st_join(., ma_blkgrp24) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(BWPScore = sum(BWPScore, na.rm = TRUE)) %>% 
  mutate(EFFCTpctileBWPMAJOR_PT = percent_rank(BWPScore)*100)



### Solid Waste Sites and Facilities
# Acquire MassDEP Solid Waste Diversion and Disposal layer
# download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/solidwaste.zip", "data/MASSGIS/solidwaste.zip")
# unzip("data/MASSGIS/solidwaste.zip", exdir = "data/MASSGIS")
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
  st_join(., ma_blkgrp24) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(SWScorePoly = sum(SWScorePoly, na.rm = TRUE))

sw_pt <- sw_pt %>% 
  transmute(SWScorePt = SWScore) %>% 
  st_join(., ma_blkgrp24) %>% 
  st_drop_geometry(.) %>% 
  group_by(GEOID) %>% 
  summarize(SWScorePt = sum(SWScorePt, na.rm = TRUE))

sw_all <- ma_blkgrp24 %>% 
  st_drop_geometry(.) %>% 
  select(GEOID) %>% 
  full_join(., sw_pt, by = "GEOID") %>% 
  full_join(., sw_poly, by = "GEOID") %>% 
  rowwise(.) %>% 
  mutate(SWScore = sum(c_across(SWScorePt:SWScorePoly), na.rm = TRUE)) %>% 
  ungroup(.) %>% 
  mutate(EFFCTpctileSW = percent_rank(SWScore)*100)



### Impaired Water Bodies 
# MassDEP 2022 Integrated List of Waters (305(b)/303(d))
# Identify streams and rivers < 100km in length that fall within 1 km of populated block; 2km for rivers > 100km. Identify lakes, bays, estuaries or shoreline < 25km2 within 1km; 2km for those > 25km2. Count number of unique pollutants from arcs and separately from polys falling within each block group. Sum these counts. 
# download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/il2022_shp.zip", "data/MASSGIS/il2022_shp.zip")
# dir.create("waters")
# unzip("data/MASSGIS/il2022_shp.zip", exdir = "data/MASSGIS")
# read in table of attributes and recode causes to identify unique pollutants
IL_ATTAINS_2022 <- read.dbf("data/MASSGIS/IL_ATTAINS_2022.dbf") %>%  
  filter(CATEGORY == "5" & POLTNT_FLG == "Y") %>% 
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

# calculate distance from water body to nearest populated block - WARNING TAKES 1 HOUR 40 MIN TO RUN!
# read in streams, rivers lines
IL_2022_ARC <- st_read("data/MASSGIS", "IL_2022_ARC") %>%
  filter(CATEGORY == "5")

# read in Lakes, Estuaries
IL_2022_POLY <- st_read("data/MASSGIS", "IL_2022_POLY") %>%
  filter(CATEGORY == "5")

# calculate nearest neighbor distances from water bodies to populated blocks within 2km
IL_arcs_nn <- st_nn(IL_2022_ARC, ma_blocks, k = 1, maxdist = 2000, returnDist = TRUE)
IL_polys_nn <- st_nn(IL_2022_POLY, ma_blocks, k = 1, maxdist = 2000, returnDist = TRUE)

# extract distances from second list as vector
IL_arcs_dist <- sapply(IL_arcs_nn[[2]], "[", 1)
IL_polys_dist <- sapply(IL_polys_nn[[2]], "[", 1)

# bind distances
IL_2022_ARC$dists <- IL_arcs_dist
IL_2022_POLY$dists <- IL_polys_dist

# save object with dists to avoid having to repeat
saveRDS(IL_2022_ARC, file = "data/MASSGIS/IL_2022_ARC.rds")
saveRDS(IL_2022_POLY, file = "data/MASSGIS/IL_2022_POLY.rds")

# read in processed data with distance to nearest populated block
IL_2022_ARC <- readRDS("data/MASSGIS/IL_2022_ARC.rds")
IL_2022_POLY <- readRDS("data/MASSGIS/IL_2022_POLY.rds")

# filter based on distances, join overlapping block groups to assign GEOID, join with attributes, group by GEOID, and sum unique pollutants
arc_pollutants <- IL_2022_ARC %>% 
  mutate(AU_SIZEkm = AU_SIZE*1.609344) %>% # convert miles to km
  filter((AU_SIZEkm <= 100 & dists <= 1000) | (AU_SIZEkm > 100 & dists < 2000)) %>% 
  st_join(., select(ma_blkgrp24, GEOID)) %>% 
  st_drop_geometry(.) %>% 
  inner_join(IL_ATTAINS_2022, ., by = "AU_ID", relationship = "many-to-many") %>% 
  group_by(GEOID) %>% 
  summarize(cause_cntArc = n_distinct(CAUSE_UNIQUE))

poly_pollutants <- IL_2022_POLY %>% 
  mutate(AREAkm2 = as.numeric(st_area(.))/10^6) %>% 
  filter((AREAkm2 <= 25 & dists <= 1000) | (AREAkm2 > 25 & dists < 2000)) %>% 
  st_join(., select(ma_blkgrp24, GEOID)) %>% 
  st_drop_geometry(.) %>% 
  inner_join(IL_ATTAINS_2022, ., by = "AU_ID", relationship = "many-to-many") %>% 
  group_by(GEOID) %>% 
  summarize(cause_cntPoly = n_distinct(CAUSE_UNIQUE))

# bring together and sum and percentile
IL_sum <- ma_blkgrp24 %>% 
  st_drop_geometry(.) %>% 
  select(GEOID) %>% 
  left_join(., arc_pollutants, by = "GEOID") %>% 
  left_join(., poly_pollutants, by = "GEOID") %>% 
  rowwise() %>% 
  mutate(IL_count = sum(c_across(cause_cntArc:cause_cntPoly), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(EFFCTpctileIL = percent_rank(IL_count)*100)



## Climate Risks/Vulnerabilities

### Drought Score. Annualized drought frequency and severity score.
# First, create drought status using MA drought regions and ranks. Data from Massachusetts Drought Management Task Force History of Drought Declarations in Massachusetts webpage (https://www.mass.gov/info-details/history-of-drought-declarations-in-massachusetts). Drought status in Massachusetts is reported by month from September 2001 through December 2025.  
# Extract region drought status, exclude subregions, standardize names, and reshape into tidy format
dstatus <- read_xlsx("data/MADCR/DroughtStatusHistory_2026_02_21.xlsx", 
                     sheet = "D2001_25") %>% 
  drop_na(Month) %>% # drop most subregions where split cells create NA for month and year, but not all!
  mutate(Islands = if_else(Year < 2019, `Cape Cod`, Islands)) %>% # before 2019 Cape & Islands were one region
  pivot_longer(cols = Western:Islands, names_to = "Region", values_to = "Status") %>% 
  filter(!str_detect(Status, "Basin|County")) %>% # filter out remaining subregions
  mutate(Region = if_else(str_detect(Region, "CT"), "Connecticut River Valley", Region), # clean up names
         Status = if_else(str_detect(Status, "Significant"), "Significant", Status),
         Status = str_squish(Status)) # remove white space

# Extract subregional records and reshape into tidy format
dstatusSub <- read_xlsx("data/MADCR/DroughtStatusHistory_2026_02_21.xlsx", 
                        sheet = "D2001_25") %>% 
  fill(c(Month, Year), .direction = "down") %>% # split cells in xlsx have NA values for month and year
  mutate(Islands = if_else(Year < 2019, `Cape Cod`, Islands)) %>% # before 2019 Cape & Islands were one region
  pivot_longer(cols = Western:Islands, names_to = "Region", values_to = "Status") %>% 
  filter(str_detect(Status, "Basin|County")) %>% # isolate subregions
  separate(col = Status, into = c("Region", "Status"), sep = "-") %>% 
  mutate(Status = str_squish(Status))

# create tidy data frame and fill in "Normal" status for missing periods, per conversation with DCR
Region <- unique(dstatus$Region) %>% 
  rep(., 25*12)

Year <- seq.int(from = 2001, to = 2025) %>% 
  rep(., 7*12) 

Month <- c("January", "February", "March", "April", "May", "June", 
           "July", "August", "September", "October", "November","December")

Month <- paste0("Mid-",Month) %>% # add mid-month time steps
  c(Month, .) %>% 
  rep(., 25*7)

# assemble into data frame with unique combinations of Region, Year, and Month
dstatusFill <- crossing(Month, Year, Region)

# join by region, month, and year to observed drought record
dstatusFill <- left_join(dstatusFill, dstatus, by = c("Region" = "Region", "Month" = "Month", "Year" = "Year")) %>% 
  filter(!(str_detect(Month, "Mid-") & is.na(Status))) %>% # drop mid-month NAs
  replace_na(list(Status = "Normal")) # fill NA with "Normal" status per conversation with DCR

# assign names of drought region, county, and basin to block groups for joining
# create drought region geometry
droughtRegions <- st_read("data/MASSGIS/townssurvey_gdb/townssurvey.gdb",
                          "TOWNSSURVEY_POLYM") %>% 
  st_cast("MULTIPOLYGON") %>%  
  mutate(Region = case_when(
    COUNTY == "BERKSHIRE" ~ "Western",
    COUNTY %in% c("FRANKLIN", "HAMPSHIRE", "HAMPDEN") ~ "Connecticut River Valley",
    COUNTY == "WORCESTER" ~ "Central",
    COUNTY %in% c("ESSEX", "MIDDLESEX", "SUFFOLK") | TOWN == "BROOKLINE" ~ "Northeast",
    COUNTY %in% c("BRISTOL", "NORFOLK", "PLYMOUTH") & TOWN != "BROOKLINE" ~ "Southeast",
    COUNTY == "BARNSTABLE" ~ "Cape Cod",
    COUNTY %in% c("DUKES", "NANTUCKET") ~ "Islands"
  )) %>% 
  group_by(Region) %>% 
  summarize(Counties = paste(unique(COUNTY), collapse = ", ")) %>% 
  mutate(Counties = case_when(
    Counties == "ESSEX, MIDDLESEX, NORFOLK, SUFFOLK" ~ "ESSEX, MIDDLESEX, SUFFOLK (plus Brookline)",
    Counties == "NORFOLK, PLYMOUTH, BRISTOL" ~ "NORFOLK, PLYMOUTH, BRISTOL (minus Brookline)",
    .default = Counties
  ))

# load counties
county <- counties(state = "MA", cb = TRUE) %>% 
  select(COUNTYFP, NAMELSAD) %>% 
  rename(COUNTY = NAMELSAD) %>% 
  st_transform(., crs = 26986)

# load watershed basins
majbasins <- st_read("data/MASSGIS/MAJBAS_POLY.shp") %>% 
  transmute(MAJBASIN = paste0(str_to_title(NAME), " Basin"))

# each block group joined with respective drought region, county, and major basin
ma_blkgrp24d <- block_groups(state = "MA", cb = TRUE, year = 2024) %>% 
  select(GEOID, COUNTYFP) %>% 
  st_transform(., crs = st_crs(droughtRegions)) %>% 
  st_join(., select(droughtRegions, Region), largest = TRUE) %>% 
  left_join(., st_drop_geometry(county), by = "COUNTYFP") %>% 
  st_join(., majbasins, largest = TRUE)

# assign drought levels and frequencies to block groups
ma_blkgrp24d <- left_join(ma_blkgrp24d, dstatusFill, by = "Region", 
                          relationship = "many-to-many")

# assign subregion (basin) drought levels and frequencies to block groups for months where available
ma_blkgrp24d <- left_join(ma_blkgrp24d, dstatusSub, 
                          by = c("MAJBASIN" = "Region", "Month", "Year"), 
                          relationship = "many-to-many") %>% 
  mutate(Status = if_else(!is.na(Status.y), Status.y, Status.x)) %>% # replace region with subregion status where present
  select(-Status.x, -Status.y)

# assign subregion (county) drought levels and frequencies to block groups for months where available
ma_blkgrp24d <- left_join(ma_blkgrp24d, dstatusSub, 
                          by = c("COUNTY" = "Region", "Month", "Year"), 
                          relationship = "many-to-many") %>% 
  mutate(Status = if_else(!is.na(Status.y), Status.y, Status.x)) %>% # replace region with county status where present
  select(-Status.x, -Status.y)

# Calculate drought score for each drought region as annualized drought frequency weighted by drought severity
# The weighted, annualized frequency value represents the number of recorded Drought occurrences, in event-months, each year over the period of record (months/12). The number of event-months for each drought category is multiplied by the drought severity. These products are summed and the sum is divided by the number years in the record. 
# read in table with drought by month from Drought Status History pdf converted to table. 
droughtScore <- ma_blkgrp24d %>% 
  st_drop_geometry() %>% 
  group_by(GEOID, Status) %>% 
  summarize(MonthCnt = n()) %>% 
  ungroup() %>% 
  mutate(weight = case_when(
    Status == "Normal" ~ 0,
    Status == "Mild" | Status == "Advisory" ~ 1,
    Status == "Significant" | Status == "Watch" ~ 2,
    Status == "Critical" | Status == "Warning" ~ 3,
    Status == "Emergency" ~ 4
  ),
  MonthCntXweight = MonthCnt*weight) %>% 
  group_by(GEOID) %>% 
  summarize(TotalMonths = sum(MonthCnt),
            DroughtScore = sum(MonthCntXweight)) %>% 
  mutate(AnnFreqWeight = DroughtScore/(TotalMonths/12),
         CLIMpctilDrought = percent_rank(AnnFreqWeight)*100)

# Add drought region, county, and major basin names to each BG
droughtScore <- ma_blkgrp24d %>% 
  st_drop_geometry() %>% 
  group_by(GEOID) %>% 
  summarize(DroughtRegion = first(Region), 
            County = first(COUNTY), 
            Basin = first(MAJBASIN),
            FirstYear = min(Year),
            LastYear = max(Year)) %>% 
  left_join(droughtScore, ., by = "GEOID")

# Add count of months by status
droughtScore <- ma_blkgrp24d %>% 
  st_drop_geometry() %>% 
  group_by(GEOID, Status) %>% 
  summarize(MonthCnt = n()) %>% 
  pivot_wider(id_cols = GEOID, names_from = Status, values_from = MonthCnt) %>% 
  replace(is.na(.), 0) %>% 
  transmute(GEOID = GEOID, Normal = Normal, 
            Mild = Mild + Advisory, 
            Significant = Significant + Watch, 
            Critical = Critical + Warning) %>% 
  left_join(droughtScore, ., by = "GEOID") %>% 
  relocate(TotalMonths, .before = Normal)

# Export drought data as csv for sharing
write_csv(droughtScore, "data/MADCR/droughtScore.csv")
# as shapefile
block_groups(state = "MA", cb = TRUE, year = 2024) %>% 
  st_transform(., crs = 26986) %>%  # transform to MA State Plane
  left_join(., droughtScore, by = "GEOID") %>% 
  st_write(., "data/MADCR/droughtScore.shp", delete_layer = TRUE)


# Wildfire risk
# read in Annualized Wildfire Impacts to All HVRA (High Value Resources or Assets)
# This dataset is a 30-m cell size representation of wildfire risk to a Highly Valued Resource or Assets (HVRA). This dataset has been multiplied by burn probability and considers the likelihood of wildfire, or “expected” wildfire risk.
# Data downloaded from the Northeast-Midwest State Foresters Alliance Risk Explorer application at https://northeastwrap.uat.timmonsdev.com/Map/Public#whats-your-risk. Data acquired by county and provided as ESRI ArcGIS geopackages. Gridded AIA data extracted from ArcGIS gepoackages as GeoTIFFs.
# assemble file names and full paths to tif files
filenames <- dir("data/MADCR/", recursive = TRUE, full.names = TRUE, pattern = "\\.tif$")

# read and convert to list of spatRasters
AIAlist <- lapply(filenames, rast)
# check resolution of all tiffs in case of differences
# lapply(AIAlist, res)

# make a SpatRasterCollection from raster list
rsrc <- sprc(AIAlist)

# mosaic SpatRasterCollection to a single, statewide raster. rasters are resampled to a common resolution based on the first raster in the list. 
AIArast <- merge(rsrc) %>% 
  subst(., from = NA, to = 0) # replace NA with 0 to fill in missing grid cells so that block groups averages make sense

# extract values to block groups. note that raw values range from 0 to -0.01930478, with lower values indicating greater risk. percentiles are calculated from absolute values to reflect magnitude of risk.
fire <- block_groups(state = "MA", cb = TRUE, year = 2024) %>% 
  select(GEOID) %>% 
  st_transform(., crs = st_crs(AIArast)) %>% 
  vect(.) %>% 
  extract(AIArast, ., fun = mean, na.rm = TRUE, bind = TRUE) %>% 
  st_as_sf(.) %>% 
  st_drop_geometry(.) %>% 
  transmute(GEOID = GEOID, 
            AIAmean = BarnstableAIA,
            CLIMpctilFIRE = percent_rank(abs(AIAmean))*100)


# Flood Risk. Percentage of developed, populated areas where there is a one-percent chance or greater risk of flooding annually. FEMA Flood Map Service Center (https://msc.fema.gov/portal/advanceSearch). As of November 2025, finalized NFHL DFIRMs are not available for the northwest quadrant of Massachusetts – Berkshire County, Hampshire County, Franklin County, and portions of northern Worcester County. These areas were supplemented with preliminary NFHL DFIRMS from FEMA (https://hazards.fema.gov/femaportal/prelimdownload/). 
# read in FEMA National Flood Hazard Layer (NFHL)
NFHL <- st_read(dsn = "data/FEMA/NFHL_25_20251128.gdb", layer = "S_FLD_HAZ_AR") %>% 
  select(FLD_ZONE) %>% 
  filter(FLD_ZONE %in% c("A", "AE", "AH", "AO", "VE") & !st_is_empty(.)) %>% 
  st_transform(., crs = 26986) %>%  # transform to MA State Plane 
  st_make_valid() %>% 
  rename(geometry = SHAPE)

# read in preliminary FEMA data 
NFHLpWorcester <- st_read("data/FEMA/FIRMDB_01312025_Worcester-County_Massachusetts/S_Fld_Haz_Ar.shp") %>% 
  select(FLD_ZONE) %>% 
  filter(FLD_ZONE %in% c("A", "AE", "AH", "AO", "VE") & !st_is_empty(.)) %>% 
  st_transform(., crs = 26986) %>%  # transform to MA State Plane 
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") # for mixed geometry cases

# some of preliminary Worcester data overlaps with final data
# identify intersecting polygons and retain only non-intersecting polygons
t <- lengths(st_intersects(NFHLpWorcester, NFHL)) > 0 # logical vector of intersections

NFHLpWorcester <- NFHLpWorcester[!t,] # keep non-intersecting polygons

# preliminary data for Franklin County
NFHLpFranklin <- st_read("data/FEMA/FIRMDB_05222024_Franklin-County_Massachusetts/S_Fld_Haz_Ar.shp") %>% 
  select(FLD_ZONE) %>% 
  filter(FLD_ZONE %in% c("A", "AE", "AH", "AO", "VE") & !st_is_empty(.)) %>% 
  st_transform(., crs = 26986) %>%  # transform to MA State Plane 
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON")  %>% st_cast("POLYGON")

# preliminary data for Hampshire County
NFHLpHampshire <- st_read("data/FEMA/FIRMDB_08272025_Hampshire-County_Massachusetts/S_Fld_Haz_Ar.shp") %>% 
  select(FLD_ZONE) %>% 
  filter(FLD_ZONE %in% c("A", "AE", "AH", "AO", "VE") & !st_is_empty(.)) %>% 
  st_transform(., crs = 26986) %>%  # transform to MA State Plane 
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON")  %>% st_cast("POLYGON")

# read in Q3 flood poly for Berkshire county from MassGIS. Flood coverage for Berkshire County supplemented with data from the FEMA Q3 Flood Zones from Paper FIRMs layer from MassGIS (https://www.mass.gov/info-details/massgis-data-fema-q3-flood-zones-from-paper-firms)
Q3Berkshire <- st_read("data/MASSGIS/", "Q3FLOOD_POLY_NO_NFHL") %>% 
  filter(ZONE %in% c("AE", "A", "D", "AO") & COUNTY == "BERKSHIRE" & !st_is_empty(.)) %>% 
  transmute(FLD_ZONE = ZONE) %>% 
  st_make_valid()

# create flood polygon risk around rivers in Town of Mount Washington which does not participate in NFHIP has no FEMA flood risk maps. Follow MA Wetlands Regulations approach at 310 CMR 10.57. In areas without FEMA data, “assume the area of flood risk is synonymous with the 200-foot Riverfront Area around rivers and the 100-foot buffer around Rivers in Densely Developed Areas.”
# download MassDEP Hydrography (1:25,000) from https://www.mass.gov/info-details/massgis-data-massdep-hydrography-125000
# read in municipal boundary for Mount Washington from MassGIS
mtWash <- st_read(dsn = "data/MASSGIS/townssurvey_gdb/townssurvey.gdb", 
                  layer = "TOWNSSURVEY_POLYM") %>% 
  filter(TOWN == "MOUNT WASHINGTON")

# # crop developed polygons to Mt Washington to look for densely developed areas
# lu2024dev_mt <- mtWash %>% 
#   st_filter(lu2024devPoly, .)

# read in streams from MassGIS, clip to Mount Washington, create 200' buffer since there are no "denseley developed areas"
stream_flood <- st_read("data/MASSGIS/hydro25k/HYDRO25K_ARC.shp") %>% 
  filter(ARC_CODE %in% c(4:7)) %>%  # stream, intermittent stream, ditch/canal, aqueduct 
  st_filter(., mtWash) %>% 
  st_buffer(., dist = 60.96) %>% # 200' foot buffer
  st_union() %>% # merge overlapping buffer polygons
  st_cast(., "POLYGON") %>% # separate polygons instead of one multipolygon
  st_as_sf() %>% 
  mutate(FLD_ZONE = "SFHA") %>% # for consistency in binding rows
  rename(geometry = x)

# bind flood risk polygons into one layer for the whole state
flood <- bind_rows(NFHL, NFHLpWorcester, NFHLpHampshire, NFHLpFranklin, Q3Berkshire, 
                   stream_flood, .id = "Source") %>% 
  st_union() %>% # merge overlapping polygons to avoid double counting
  st_cast(., "POLYGON") %>% # separate polygons instead of one multipolygon
  st_as_sf() %>% 
  st_make_valid() %>% 
  mutate(FLD_ZONE = "SFHA",
         SFHAsqm = as.numeric(st_area(.))) %>% 
  rename(geometry = x)

# clean up
rm(list = ls(pattern = "NFHL|Q3|stream|mtWash"))

# export flood layer for sharing
st_write(flood, "data/FEMA/flood.shp", delete_layer = TRUE)

# load NLCD 2024 Annual Land Cover raster from https://www.mrlc.gov/
# NOTE: NLCD annual data 2022-2024 has known bug with most categories showing up as null. In ArcGIS, use "Build Raster Attribute Table" tool and then select "overwrite" and "convert colormap" checkboxes.
# load corrected tiff file
lu2024 <- rast("data/NLCD/Annual_NLCD_LndCov_2024_CU_C1V1_mj1wm7imvsght1.tiff")
# # check categories pixel values and land cover classes
# as.data.frame(levels(lu2024), check.names = FALSE)
# # get count of pixels per category
# freq(lu2024)

# isolate developed pixels
lu2024dev <- mask(lu2024, lu2024, c(21,22,23,24), inverse = TRUE)

# convert raster to vector polygons
lu2024devPoly <- as.polygons(lu2024dev) %>% 
    st_as_sf(.) %>% 
    st_union() %>% 
    st_make_valid() %>% 
    st_transform(., crs = 26986) # transform to MA State Plane

# dasymetric mapping of block-level population based on developed areas of census blocks
ma_blocksdev <- ms_clip(target = ma_blocks, clip = lu2024devPoly) %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON") %>%
  mutate(blockDevArea_sqm = as.numeric(st_area(.))) %>% 
  st_make_valid()

# clean up
rm(lu2024, lu2024dev)

# calculate new area of intersected blocks and group back to blocks (creates multipolygons). allows you to calculate total developed area within each census block. 
ma_block20devAREA_df <- ma_blocksdev %>% 
    st_drop_geometry() %>% 
    group_by(GEOID) %>% 
    summarize(blockDevArea_sqm = sum(blockDevArea_sqm, na.rm = TRUE))

# assign population of block to developed area intersection by area proportion
# if multiple developed areas within a single block, assign population based on proportion of block-level developed area.
# intersect developed block areas with flood polygons. determine proportion of developed area that intersects with flood. population affected by flood is proportion of developed block area intersected by flood x block pop.
# use `mapshaper` library, node.js, and `system` for large spatial objects. see https://rdrr.io/cran/rmapshaper/f/vignettes/rmapshaper.Rmd 
ma_block20devflood <- ms_clip(target = ma_blocksdev, clip = flood, 
                              sys = TRUE, sys_mem = 16) %>% 
    st_cast("MULTIPOLYGON") %>% 
    st_cast("POLYGON") %>% 
    mutate(polyID = paste0(GEOID, "_", row_number())) #create a unique ID for each flooded developed dryland polygon

# read in building footprints from MassGIS https://www.mass.gov/info-details/massgis-data-building-structures-2-d
buildings <- st_read("data/MASSGIS/structures.gdb", "STRUCTURES_POLY") %>% 
  filter(!st_is_empty(.),
         AREA_SQ_FT >= 60) %>% # Minimum structure size 60sqft - lower limit for tiny house or mobile home 
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON")

# Exclude flooded developed dryland polygons that do not intersect buildings
# Count the number of buildings intersecting each flooded developed dryland polygon
# spatial join buildings to flooded developed dryland polygons
buildings_devflood <- st_join(buildings, ma_block20devflood)

# group by unique ID and count
buildings_devflood_cnt <- buildings_devflood %>% 
  st_drop_geometry() %>% 
  drop_na(polyID) %>% 
  group_by(polyID) %>% 
  summarize(bldg_cnt = n())

# filter out flooded developed dryland polygons with no buildings 
ma_block20devflood <- buildings_devflood_cnt %>% 
  inner_join(ma_block20devflood, ., by = "polyID")

# count number of buildings per census block group as denominator to calculate percent of buildings flooded per block group
buildings_blkgrp20 <- st_join(buildings, ma_blkgrp20)

# group by unique ID and count
buildings_blkgrp20_cnt <- buildings_blkgrp20 %>% 
  st_drop_geometry() %>% 
  drop_na(GEOID) %>% 
  group_by(GEOID) %>% 
  summarize(blkgrp20bldg_cnt = n())

# calculate total area of developed area intersection with flood area. group by GEOID to get total flooded developed area by block and then compute proportion
ma_block20devfloodAgg <- ma_block20devflood %>% 
    mutate(blockDevFldArea_sqm = as.numeric(st_area(.))) %>% 
    group_by(GEOID) %>% 
    summarize(blockDevFldArea_sqm = sum(blockDevFldArea_sqm, na.rm = TRUE),
              bldg_cnt = sum(bldg_cnt, na.rm = TRUE)) %>% 
    left_join(., ma_block20devAREA_df, by = "GEOID") %>% 
    mutate(floodProp = blockDevFldArea_sqm/blockDevArea_sqm)

# export block-level dasymetric flood exposed pop and buildings data
# as shapefile
ma_blocks %>% 
  st_drop_geometry() %>% 
  inner_join(ma_block20devfloodAgg, ., by = "GEOID") %>% 
  mutate(PopFlood = floodProp*P1_001N) %>% 
  replace(is.na(.), 0) %>% 
  rename(DevFldArea = blockDevFldArea_sqm,
         DevArea = blockDevArea_sqm) %>% 
  mutate(PopFlood = round(PopFlood)) %>% 
  st_write(., "data/FEMA/ma_block20devfloodAgg.shp", 
           delete_layer = TRUE)


# group by block to sum total affected population by block; group by block group and sum population affected. 
# full join to block group
# calculate proportion of block group population affected by flood; NA is 0. 
ma_blkgrp20devfloodAgg <- ma_block20devfloodAgg %>% 
  st_drop_geometry() %>% 
  left_join(ma_blocks, ., by = "GEOID") %>% 
  st_drop_geometry() %>% 
  mutate(PopFlood = floodProp*P1_001N) %>% 
  group_by(GEOID_BG) %>% 
  summarize(DevArea_sqm = sum(blockDevArea_sqm, na.rm = TRUE),
            DevFldArea_sqm = sum(blockDevFldArea_sqm, na.rm = TRUE),
            PopFlood = sum(PopFlood, na.rm = TRUE),
            bldg_cnt = sum(bldg_cnt, na.rm = TRUE)) %>% 
  left_join(ma_blkgrp20, ., by = c("GEOID" = "GEOID_BG")) %>% 
  left_join(., buildings_blkgrp20_cnt, by = "GEOID") %>% 
  mutate(PopFloodPct = PopFlood/BGpop*100,
         BldFloodPct = bldg_cnt/blkgrp20bldg_cnt*100,
         CLIMpctilFLD = percent_rank(PopFloodPct)*100) %>% 
  st_drop_geometry()

# export ma_blkgrp20devfloodAgg for sharing
# as shapefile
left_join(select(ma_blkgrp20, GEOID), ma_blkgrp20devfloodAgg, by = "GEOID") %>% 
  rename(DevFldArea = DevFldArea_sqm, 
         BG_bldgCnt = blkgrp20bldg_cnt,
         BldgFlood = bldg_cnt) %>% 
  st_write(., "data/FEMA/ma_blkgrp20devfloodAgg.shp", delete_layer = TRUE)
# as CSV
ma_blkgrp20devfloodAgg %>% 
  rename(DevFldArea = DevFldArea_sqm, 
         BG_bldgCnt = blkgrp20bldg_cnt,
         BldgFlood = bldg_cnt) %>% 
  write_csv(., "data/FEMA/ma_blkgrp20devfloodAgg.csv")


# Heat. Number of unhealthy heat events over the last 30 years (1996 - 2025) in which air temperatures rose to 85F or higher for 3 or more days in a row.
# Originating geographic scale of data: 800m pixels or grid cells 
# Source: PRISM Group, Oregon State University, https://prism.oregonstate.edu, data created April 1996 through October 2025, accessed 29 Dec 2025.
# set download folder
prism_set_dl_dir("data/PRISM")

# Acquire PRISM daily maximum temp data for 1996 - 2025, in summer increments (May 1 - Oct 1). 
# loop through years and months of interest; CAUTION takes ~6 hours to download and ~210GB of storage space
for(i in 1996:2025) {
  get_prism_dailys(type = "tmax",
                   minDate = paste0(i, "-05-01"),
                   maxDate = paste0(i, "-10-01"),
                   keepZip = FALSE,
                   resolution = "800m")
}

# assemble file names and full paths to raster files
filenames <- dir("data/PRISM/", recursive = TRUE, full.names = TRUE, pattern = "\\.bil$")

# read and convert to multi-layer Spatraster and stack them
tmaxStack <- rast(filenames)

# stack rasters and crop to MA
tmaxStack <- tmaxStack %>%
  crop(., vect(ma_blkgrpNAD83))

# write out full stack for MA to save time later
writeRaster(tmaxStack19962025, "data/PRISM/tmaxStack19962025.tif", overwrite = TRUE, 
            gdal=c("COMPRESS=NONE", "TFW=YES"), datatype = "FLT4S")

# read in combined 1996 - 2025 tmax tif for MA
tmaxStack19962025 <- rast("data/PRISM/tmaxStack19962025.tif")

# function to detect runs of a certain value
ff = function(x,...){
  runs = rle(x >= 29.44) # convert 85 Fahrenheit threshold to Celsius 
  sum(runs$lengths[runs$values] >= 3)
}

# apply run length encoding function to raster stack to count up 3+ day runs of 85F+ days
tmaxCount <- app(tmaxStack19962025, fun = ff, na.rm = TRUE)

# calculate mean count of 3+ day runs of 85+F per BG
heat <- tmaxCount %>% 
  extract(., vect(ma_blkgrpNAD83), fun = mean, na.rm = TRUE, bind = TRUE) %>% 
  st_as_sf(.) %>% 
  transmute(GEOID = GEOID, 
            HeatEvents = lyr.1,
            CLIMpctilHEAT = percent_rank(HeatEvents)*100) %>%
  st_transform(., crs = 26986) %>% 
  st_drop_geometry(.)




# BRING IT ALL TOGETHER
MassEnviroScreen <- ma_blkgrp24 %>% 
  select(GEOID, GEOID_TRACT, COSUB, COUNTYFP) %>% 
  left_join(., ejscreen, by = c("GEOID" = "ID")) %>% 
  left_join(., select(pm25, GEOID_TRACT, PM25, EXPpctilePM25), by = "GEOID_TRACT") %>% 
  left_join(., select(ozone, GEOID_TRACT, OZONE, EXPpctileOZONE), by = "GEOID_TRACT") %>% 
  left_join(., select(no2, GEOID, NO2, EXPpctileNO2), by = "GEOID") %>%
  left_join(., select(DWATER, GEOID, DWATER, EXPpctileDWATER, PWS), by = "GEOID") %>% 
  left_join(., select(airtox2020_blkgrp, GEOID, CancerRisk, EXPpctileCancerRisk), 
            by = "GEOID") %>% 
  left_join(., select(airtoxDSLPM2020_blkgrp, GEOID, DSLPM, EXPpctileDSLPM), 
            by = "GEOID") %>% 
  left_join(., select(airtox2019_blkgrp, GEOID, `Respiratory HI`, EXPpctileRespHI), 
            by = "GEOID") %>% 
  left_join(., select(cleanup_all, GEOID, cleanup_score, EFFCTpctileCleanup), by = "GEOID") %>% 
  left_join(., select(gwater_all, GEOID, gwater_score, EFFCTpctileGrndWater), by = "GEOID") %>% 
  left_join(., select(BWPMAJOR_PT, GEOID, BWPScore, EFFCTpctileBWPMAJOR_PT), by = "GEOID") %>% 
  left_join(., select(sw_all, GEOID, SWScore, EFFCTpctileSW), by = "GEOID") %>% 
  left_join(., select(IL_sum, GEOID, IL_count, EFFCTpctileIL), by = "GEOID") %>% 
  left_join(., select(health_tract, GEOID_TRACT, `High blood pressure among adults`, 
                      SPpctileHPRSSR, `Coronary heart disease among adults`, 
                      SPpctileHRTDIS,
                      `Chronic obstructive pulmonary disease among adults`, SPpctileCOPD,
                      `Cancer (non-skin) or melanoma among adults`, 
                      SPpctileCANCER),
            by = "GEOID_TRACT") %>% 
  left_join(., select(asthma_blkgrp, GEOID, pedAsthmaPrevalence, SPpctileAsthmaPed), 
            by = "GEOID") %>% 
  left_join(., select(DEP_LBW_BLL_tract, GEOID_TRACT, BLL, SPpctileBLL, LBW, SPpctileLBW,
                      PMR, SPpctilePMR),
            by = "GEOID_TRACT") %>% 
  left_join(., select(ma_blkgrp24HS, GEOID, HSlesspctE, SEpctileHS), by = "GEOID") %>% 
  left_join(., select(hhburden, geoid2, hhburden, SEpctileHHB), by = c("GEOID_TRACT" = "geoid2")) %>% 
  left_join(., select(ma_blkgrp24language, GEOID, limitEngpctE, SEpctileLEP), by = "GEOID") %>% 
  left_join(., select(ma_blkgrp24pov, GEOID, TotalPopE, TotalHHE, povHHpctE, 
                      SEpctilePOV), by = "GEOID") %>% 
  left_join(., select(ma_blkgrp24employ, GEOID, unemploypctE, SEpctileEMP), by = "GEOID") %>% 
  left_join(., select(droughtScore, GEOID, AnnFreqWeight, CLIMpctilDrought),
            by = "GEOID") %>%
  left_join(., select(fire, GEOID, AIAmean, CLIMpctilFIRE), by = "GEOID") %>% 
  left_join(., select(ma_blkgrp20devfloodAgg, GEOID, BldFloodPct, PopFloodPct, 
                      CLIMpctilFLD), 
            by = "GEOID") %>% 
  left_join(., select(heat, GEOID, HeatEvents, CLIMpctilHEAT), by = "GEOID") %>% 
  mutate(across(c(starts_with("EXPpctile"), starts_with("EFFCTpctile"), 
                  starts_with("CLIMpctil"))
                )
         ) %>% 
  replace_na(list(cleanup_score = 0, EFFCTpctileCleanup = 0, gwater_score = 0, 
                  EFFCTpctileGrndWater = 0, BWPScore = 0, EFFCTpctileBWPMAJOR_PT = 0, 
                  pctFldArea = 0, CLIMpctilFLD = 0)) %>% # NA means hazard not present
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


# Attach income criterion and identify Burdened Areas (BAs)
# download current block group geometry
# use EPSG 326986 Massachusetts State State Plane
ma_blkgrp24_sf <- block_groups(state = "MA", year = 2024) %>% 
  st_transform(., crs = 26986) %>% 
  filter(!st_is_empty(.)) %>% 
  st_make_valid()

# get statewide median household income
ma_state24medHHincMA <- get_acs(geography = "state", year = 2024, state = "MA",
                                variables = c(medHHincMA = "B19013_001"), output = "wide") %>% 
  select(medHHincMAE) %>% 
  pull(.)

# get block group median household income
ma_blkgrp24medHHinc <- get_acs(geography = "block group", year = 2024, state = "MA",
                               variables = c(medHHinc = "B19013_001"), output = "wide")

# get tract MHHI to fill in missing values at BG level
ma_tract24medHHinc <- get_acs(geography = "tract", year = 2024, state = "MA",
                              variables = c(medHHincT = "B19013_001"), output = "wide") %>% 
  select(GEOID, medHHincTE)

# replace missing BG medHHincE values with tract values
ma_blkgrp24medHHinc <- ma_blkgrp24medHHinc %>% 
  mutate(GEOID_TRACT = str_trunc(GEOID, 11 ,"right", ellipsis = "")) %>%
  left_join(., ma_tract24medHHinc, by = c("GEOID_TRACT" = "GEOID")) %>% 
  mutate(medHHincE = if_else(is.na(medHHincE), medHHincTE, medHHincE),
         medHHincMA = ma_state24medHHincMA,
         medHHincMAPCT = medHHincE/medHHincMA*100) %>% 
  select(-NAME, -GEOID_TRACT, -medHHincTE)

# get municipal median household income and assign to overlapping block groups
ma_blkgrp24medHHinc <- get_acs(geography = "county subdivision", year = 2024, state = "MA",
                               variables = c(medHHincMUNI = "B19013_001"), output = "wide",
                               geometry = TRUE) %>% 
  st_transform(., crs = 26986) %>% 
  filter(!st_is_empty(.)) %>% 
  st_make_valid() %>% 
  transmute(MUNI = NAME, medHHincMUNIE = medHHincMUNIE, medHHincMUNIM = medHHincMUNIM) %>% 
  st_join(ma_blkgrp24_sf, ., st_intersects, largest = TRUE) %>% 
  select(GEOID, MUNI, medHHincMUNIE, medHHincMUNIM) %>% 
  st_drop_geometry(.) %>% 
  left_join(ma_blkgrp24medHHinc, ., by = "GEOID") %>% 
  mutate(medHHincMUNIPCT = medHHincMUNIE/medHHincMA*100)

# Percent minority
ma_blkgrp24race <- get_acs(geography = "block group", year = 2024, state = "MA", 
                           variables = c(pop = "B03002_001", nhWhite = "B03002_003"),
                           output = "wide") %>% 
  mutate(minorityE = popE - nhWhiteE, minorityPctE = minorityE/popE*100) %>% 
  select(-popE, -popM, -starts_with("nhWhit"))

# process variables into a consistent df with appropriate estimates, counts, and percentages
ma_blkgrp24_inc <- left_join(ma_blkgrp24race, ma_blkgrp24medHHinc, by = "GEOID") %>% 
  mutate(NAME = str_replace_all(str_remove(NAME, "; Massachusetts"), ";", ","),
         MUNI = str_remove_all(str_remove(MUNI, "((?=\\,).*)"), " town| Town| city| City"))



# Use clean geometry for block groups and a join income and minority pop data
MassEnviroScreen <- block_groups(state = "MA", year = 2024, cb = TRUE) %>% 
  filter(!st_is_empty(.)) %>% 
  select(GEOID) %>% 
  left_join(., st_drop_geometry(MassEnviroScreen), by = "GEOID") %>% 
  st_transform("+proj=longlat +datum=WGS84") %>% 
  left_join(., select(ma_blkgrp24_inc, GEOID, NAME, minorityPctE, medHHincE, medHHincMA, medHHincMUNIPCT, medHHincMUNIE, medHHincMUNIPCT, medHHincMAPCT), by = "GEOID") %>% 
  mutate(UBA = if_else(round(MassEnviroScore,0) >= 75 | 
                         round(medHHincMAPCT,0) <= 65, 
                       "Yes", "No", missing = "No"))


# save for later analysis and mapping
saveRDS(MassEnviroScreen, paste0("MassEnviroScreen",Sys.Date(),".rds"))
# write to CSV
MassEnviroScreen %>% 
  st_drop_geometry() %>% 
  write_csv(., file = paste0("MassEnviroScreen",Sys.Date(),".csv"))
# write fields to CSV
MassEnviroScreen %>% 
  st_drop_geometry() %>% 
  names() %>% 
  data.frame() %>% 
  rename("FIELD" = ".") %>% 
  write_csv(., file = paste0("MES_Fields",Sys.Date(),".csv"))
