setwd("C:/Users/RalphArvin/Desktop/work-s2018/mammalsb/rscripts")
setwd("C:/Users/imralpharvin/Desktop/work-s2018/mammalsb/rscripts")
# Copyright (C) 2018 Ralph Arvin De Castro.
# Program Description: 

#############################################################################################################################

##### SECTION 2: TRAIT ASSIGNMENT #####
# This section is designed to assign trait data from BOLD and Pantheria and match it to BOLD sequence data.

#For data manipulation:
#install.packages("data.table")
library(data.table)
# For importing xslx data to data.frame 
#install.packages("readxl")
library("readxl")
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
source("GetTraitSpecificDataBIN.R")
source("GetTraitSpecificData.R")
source("GetTraitInfo.R")

##########################################GETTING ALL SOURCES############################################
##### Source 1: Pantheria ####
# Read excel sheet data
pantheriaMammalData <- read_excel("Pantheria.xlsx")
# Select all columns
selectedTraits<- c("MSW05_Order","MSW05_Family","MSW05_Genus","MSW05_Binomial", "5-1_AdultBodyMass_g" ,"8-1_AdultForearmLen_mm", "13-1_AdultHeadBodyLen_mm", "2-1_AgeatEyeOpening_d" ,"3-1_AgeatFirstBirth_d", "18-1_BasalMetRate_mLO2hr" ,"5-2_BasalMetRateMass_g" ,"7-1_DispersalAge_d" ,"9-1_GestationLen_d", "22-1_HomeRange_km2"          
                   ,"22-2_HomeRange_Indiv_km2","14-1_InterbirthInterval_d", "15-1_LitterSize","16-1_LittersPerYear","17-1_MaxLongevity_m", "5-3_NeonateBodyMass_g"       , "13-2_NeonateHeadBodyLen_mm", "21-1_PopulationDensity_n/km2", "10-1_PopulationGrpSize", "23-1_SexualMaturityAge_d", "10-2_SocialGrpSize","24-1_TeatNumber"             
                   , "25-1_WeaningAge_d" , "5-4_WeaningBodyMass_g" , "13-3_WeaningHeadBodyLen_mm","26-1_GR_Area_km2" , "26-2_GR_MaxLat_dd" ,"26-3_GR_MinLat_dd", "26-4_GR_MidRangeLat_dd","26-5_GR_MaxLong_dd"          
                   , "26-6_GR_MinLong_dd", "26-7_GR_MidRangeLong_dd","27-1_HuPopDen_Min_n/km2" , "27-2_HuPopDen_Mean_n/km2", "27-3_HuPopDen_5p_n/km2", "27-4_HuPopDen_Change"        
                   , "28-1_Precip_Mean_mm" , "28-2_Temp_Mean_01degC" ,"30-1_AET_Mean_mm","30-2_PET_Mean_mm","1-1_ActivityCycle",  "6-1_DietBreadth", "12-1_HabitatBreadth", "12-2_Terrestriality", "6-2_TrophicLevel")
# Filter the original data using the selectedTraits vector as the subset
pantheriaMammalData <- pantheriaMammalData[selectedTraits]
# Rename columns
colnames(pantheriaMammalData) <- c("order", "family", "genus", "species_name", "body_mass", "forearm_length", "headbody_length", "eyeopening_age", "firstbirth_age", "bmr_rate", "bmr_mass", "dispersal_age", "gestation_length", "home_range", "home_range_indiv", "interbirth_interval", "litter_size", "litters_pyear", "max_longevity", "neonate_bodymass", "neonate_headbodylength", "pop_density", "pop_grpsize", "sexualmaturity_age", "social_grpsize", "teatnumber", "weaning_age", "weaning_bodymass", "weaning_bodylength", "GR_area", "GR_maxlat", "GR_minlat", "GR_midrangelat", "GR_maxlong", "GR_minlong", "GR_midrangelong", "hupopden_min", "hupopden_mean", "hupopden_5p", "hupopden_change", "precip_mean", "temp_mean", "AET_mean", "PET_mean", "activity_cycle", "diet_breadth", "habitat_breadth", "terrestriality", "trophic_level")
# Changing -999 values to NA
pantheriaMammalData[pantheriaMammalData == -999] <- NA
# Converting to data table
pantheriaMammalData <- as.data.table(pantheriaMammalData)
pantheriaMammalData <- pantheriaMammalData[,!c(1,2,3)]

##### Source 2: Placental mammal data #####
# Read mammal data
placentalMammalData <- read_excel("placental.xlsx")
# Remove columns that are not needed for analysis
placentalMammalData$refs <- NULL
# Rename columns
colnames(placentalMammalData) <- c("order", "family", "genus", "species", "body_mass", "gestation_length", "neonate_bodymass", "weaning_age", "weaning_bodymass", "firstbirth_age", "max_longevity", "litter_size", "litters_pyear");
# Changing -999 values to NA
placentalMammalData[placentalMammalData == -999] <- NA
# Converting to data table
placentalMammalData <- as.data.table(placentalMammalData)
# Setting species name
placentalMammalData <- unite(placentalMammalData, "species_name", c("genus","species"), sep = " ", remove = FALSE)
# Converting months to days
placentalMammalData <- placentalMammalData[, gestation_length := gestation_length*30]
placentalMammalData <- placentalMammalData[, weaning_age := weaning_age *30]
placentalMammalData <- placentalMammalData[, firstbirth_age := firstbirth_age *30]
# Data table reorganization
placentalMammalData <- placentalMammalData[, !c(1,2,4,5)]

##### Source 3: Amniote #####
# Read mammal data
amnioteMammalData <- read.csv("Amniote.csv")
# Filter mammals
amnioteMammalData <- filter(amnioteMammalData, class == "Mammalia")
# Subset columns needed for analysis
amnioteMammalData<- amnioteMammalData[, c(1:5, 9:16, 22, 29, 32, 36)]
# Setting null valuescol
amnioteMammalData[amnioteMammalData == -999] <- NA
# Converting to data table
amnioteMammalData <- as.data.table(amnioteMammalData)
# Setting species name
amnioteMammalData <- unite(amnioteMammalData, "species_name", c("genus","species"), sep = " ", remove = FALSE)
# Rename columns
amnioteMammalData <- rename(amnioteMammalData,  litter_size = litter_or_clutch_size_n, litters_pyear = litters_or_clutches_per_y, body_mass = adult_body_mass_g, max_longevity = maximum_longevity_y, gestation_length = gestation_d, weaning_age = weaning_d, neonate_bodymass = birth_or_hatching_weight_g, weaning_bodymass = weaning_weight_g, interbirth_interval = inter_litter_or_interbirth_interval_y, adult_svl_length = adult_svl_cm, neonate_svl_length = birth_or_hatching_svl_cm, maturity_length = no_sex_maturity_d)
# Convert years to days
amnioteMammalData <- amnioteMammalData[, interbirth_interval := interbirth_interval *365]
# Convert years to months
amnioteMammalData <- amnioteMammalData[, max_longevity := max_longevity *12]
# Data table reorganization
dfSpeciesInfo <- amnioteMammalData[, c(2:5)]
amnioteMammalData <- amnioteMammalData[, !c(1,2,3,5,6)]

##### Source 4: Anage  #####
# Filter the original data using the selectedTraits vector as the subset
# Read mammal data
anageMammalData <- read_excel("anage.xlsx")
# Subset columns needed for analysis
anageMammalData<- anageMammalData[, c(4:8, 12, 14:17, 19:21, 26:30)]
# Filter mammals
anageMammalData <- filter(anageMammalData, Class == "Mammalia")
# Rename columns
anageMammalData <- rename(anageMammalData, class = Class, order = Order, family = Family, genus = Genus, species = Species, gestation_length = "Gestation/Incubation (days)", litter_size = "Litter/Clutch size", litters_pyear = "Litters/Clutches per year", interbirth_interval =  "Inter-litter/Interbirth interval", neonate_bodymass = "Birth weight (g)" ,  body_mass = "Adult weight (g)", growth_rate = "Growth rate (1/days)", max_longevity = "Maximum longevity (yrs)", imr_pyear = "IMR (per yr)", mrdt = "MRDT (yrs)", metabolic_rate = "Metabolic rate (W)", body_mass = "Body mass (g)", temp_mean = "Temperature (K)")
# Setting species name
anageMammalData <- unite(anageMammalData, "species_name", c("genus","species"), sep = " ", remove = FALSE)
# Converting to data table
anageMammalData <- as.data.table(anageMammalData)
# Convert years to months
anageMammalData <- anageMammalData[, max_longevity := max_longevity *12]
# Data table reorganization
anageMammalData <- anageMammalData[, !c(1,2,3,5,6)]

##### Source 5: BOLD #####
# Filtering for presence of a latitude value.
dfLatitudeSpecies <- dfFiltered[grep("[0-9]", lat)]
# Convert the latitude (lat) column to number instead of character type
dfLatitudeSpecies[, lat_num := as.numeric(lat)]


######################################################################################################################
##### Traits ####
# Single row per species

dfFilteredSingle <- dfcytB[!duplicated(species_name)][, .(accession_number, species_name)]

#### TRAIT: BODY MASS ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "body_mass")]
placentalTrait <- placentalMammalData[, c("species_name", "body_mass")]
amnioteTrait <- amnioteMammalData[, c("species_name", "body_mass")]
anageTrait <- anageMammalData[, c("species_name", "body_mass")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait , placentalTrait, amnioteTrait, anageTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 3:6]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$body_mass =rowMeans(dfTrait[,c(3:6)], na.rm=TRUE)
dfBodyMass <- dfTrait[,!c(2:6)]

#### TRAIT: FOREARM LENGTH ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "forearm_length")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(forearm_length)]
dfForearmLength <- dfTrait[,!c(2)]

#### TRAIT: HEADBODY LENGTH ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "headbody_length")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(headbody_length)]
dfHeadBodyLength <- dfTrait[,!c(2)]

#### TRAIT: EYE OPENING AGE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "eyeopening_age")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(eyeopening_age)]
dfEyeOpeningAge <- dfTrait[,!c(2)]

#### TRAIT: BMR Rate ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "bmr_rate")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(bmr_rate)]
dfBmrRate <- dfTrait[,!c(2)]

#### TRAIT: BMR Mass ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "bmr_mass")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(bmr_mass)]
dfBmrMass <- dfTrait[,!c(2)]

#### TRAIT: MAX LONGEVITY ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "max_longevity")]
placentalTrait <- placentalMammalData[, c("species_name", "max_longevity")]
amnioteTrait <- amnioteMammalData[, c("species_name", "max_longevity")]
anageTrait <- anageMammalData[, c("species_name", "max_longevity")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait , placentalTrait, amnioteTrait, anageTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 3:6]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$max_longevity = rowMeans(dfTrait[,c(3:6)], na.rm=TRUE)
dfMaxLongevity <- dfTrait[,!c(2:6)]

#### TRAIT: Sexual Maturity Age ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "sexualmaturity_age")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(sexualmaturity_age)]
dfSexualMaturityAge <- dfTrait[,!c(2)]

#### TRAIT: Snout to vent Length ####
amnioteTrait <- amnioteMammalData[, c("species_name", "adult_svl_length")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(adult_svl_length)]
dfAdultSvlLength <- dfTrait[,!c(2)]

#### TRAIT: Maturity ####
amnioteTrait <- amnioteMammalData[, c("species_name", "maturity_length")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(maturity_length)]
dfMaturityLength <- dfTrait[,!c(2)]

#### TRAIT: Growth rate ####
anageTrait <- anageMammalData[, c("species_name", "growth_rate")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, anageTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(growth_rate)]
dfGrowthRate<- dfTrait[,!c(2)]


#### TRAIT: Imr Per year ####
anageTrait <- anageMammalData[, c("species_name", "imr_pyear")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, anageTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(imr_pyear)]
dfImrPerYear<- dfTrait[,!c(2)]

#### TRAIT: Mrdt ####
anageTrait <- anageMammalData[, c("species_name", "mrdt")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, anageTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(mrdt)]
dfMrdt<- dfTrait[,!c(2)]

#### TRAIT: Metabolic rate ####
anageTrait <- anageMammalData[, c("species_name", "metabolic_rate")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, anageTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(metabolic_rate)]
dfMetabolicRate<- dfTrait[,!c(2)]

#### TRAIT: First birth age ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "firstbirth_age")]
placentalTrait <- placentalMammalData[, c("species_name", "firstbirth_age")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait , placentalTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 3:4]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$firstbirth_age=rowMeans(dfTrait[,c(3:4)], na.rm=TRUE)
dfFirstBirthAge <- dfTrait[,!c(2:4)]

#### TRAIT: Gestation Length ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "gestation_length")]
placentalTrait <- placentalMammalData[, c("species_name", "gestation_length")]
amnioteTrait <- amnioteMammalData[, c("species_name", "gestation_length")]
anageTrait <- anageMammalData[, c("species_name", "gestation_length")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait , placentalTrait, amnioteTrait, anageTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 3:6]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$gestation_length =rowMeans(dfTrait[,c(3:6)], na.rm=TRUE)
dfGestationLength <- dfTrait[,!c(2:6)]

#### TRAIT: Interbirth Interval ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "interbirth_interval")]
amnioteTrait <- amnioteMammalData[, c("species_name", "interbirth_interval")]
anageTrait <- anageMammalData[, c("species_name", "interbirth_interval")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait , amnioteTrait, anageTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 3:5]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$interbirth_interval =rowMeans(dfTrait[,c(3:5)], na.rm=TRUE)
dfInterbirthInterval <- dfTrait[,!c(2:4)]

#### TRAIT: LITTER SIZE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "litter_size")]
placentalTrait <- placentalMammalData[, c("species_name", "litter_size")]
amnioteTrait <- amnioteMammalData[, c("species_name", "litter_size")]
anageTrait <- anageMammalData[, c("species_name", "litter_size")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait , placentalTrait, amnioteTrait, anageTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 3:6]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$litter_size =rowMeans(dfTrait[,c(3:6)], na.rm=TRUE)
dfLitterSize <- dfTrait[,!c(2:6)]

#### TRAIT: LITTER PER YEAR ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "litters_pyear")]
placentalTrait <- placentalMammalData[, c("species_name", "litters_pyear")]
amnioteTrait <- amnioteMammalData[, c("species_name", "litters_pyear")]
anageTrait <- anageMammalData[, c("species_name", "litters_pyear")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait , placentalTrait, amnioteTrait, anageTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 3:6]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$litters_pyear =rowMeans(dfTrait[,c(3:6)], na.rm=TRUE)
dfLittersPerYear <- dfTrait[,!c(2:6)]

#### TRAIT: NEONATE BODY MASS ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "neonate_bodymass")]
placentalTrait <- placentalMammalData[, c("species_name", "neonate_bodymass")]
amnioteTrait <- amnioteMammalData[, c("species_name", "neonate_bodymass")]
anageTrait <- anageMammalData[, c("species_name", "neonate_bodymass")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait , placentalTrait, amnioteTrait, anageTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 3:6]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$neonate_bodymass =rowMeans(dfTrait[,c(3:6)], na.rm=TRUE)
dfNeonateBodyMass <- dfTrait[,!c(2:6)]

#### TRAIT: NEONATE HEAD BODY LENGTH ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "neonate_headbodylength")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(neonate_headbodylength)]
dfNeonateHeadBodyLength <- dfTrait[,!c(2)]

#### TRAIT: NEONATE SVL LENGTH ####
amnioteTrait <- amnioteMammalData[, c("species_name", "neonate_svl_length")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(neonate_svl_length)]
dfNeonateSvlLength <- dfTrait[,!c(2)]

#### TRAIT: TEAT NUMBER ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "teatnumber")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(teatnumber)]
dfTeatNumber <- dfTrait[,!c(2)]

#### TRAIT: WEANING AGE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "weaning_age")]
amnioteTrait <- amnioteMammalData[, c("species_name", "weaning_age")]
placentalTrait <- placentalMammalData[, c("species_name", "weaning_age")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait , amnioteTrait, placentalTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 3:5]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$weaning_age=rowMeans(dfTrait[,c(3:5)], na.rm=TRUE)
dfWeaningAge <- dfTrait[,!c(2:4)]

#### TRAIT: WEANING BODY MASS ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "weaning_bodymass")]
amnioteTrait <- amnioteMammalData[, c("species_name", "weaning_bodymass")]
placentalTrait <- placentalMammalData[, c("species_name", "weaning_bodymass")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait , amnioteTrait, placentalTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 3:5]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$weaning_bodymass=rowMeans(dfTrait[,c(3:5)], na.rm=TRUE)
dfWeaningBodyMass <- dfTrait[,!c(2:4)]

#### TRAIT: WEANING BODY LENGTH ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "weaning_bodylength")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(weaning_bodylength)]
dfWeaningBodyLength <- dfTrait[,!c(2)]

#### TRAIT: DISPERSAL AGE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "dispersal_age")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(dispersal_age)]
dfDispersalAge <- dfTrait[,!c(2)]

#### TRAIT: HOME_RANGE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "home_range")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(home_range)]
dfHomeRange <- dfTrait[,!c(2)]

#### TRAIT: HOME_RANGE INDIVIDUAL ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "home_range_indiv")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(home_range_indiv)]
dfHomeRangeIndividual <- dfTrait[,!c(2)]

#### TRAIT: POPULATION DENSITY ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "pop_density")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(pop_density)]
dfPopulationDensity <- dfTrait[,!c(2)]

#### TRAIT: POPULATION GROUP SIZE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "pop_grpsize")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(pop_grpsize)]
dfPopulationGroupSize <- dfTrait[,!c(2)]

#### TRAIT: SOCIAL GROUP SIZE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "social_grpsize")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(social_grpsize)]
dfSocialGroupSize <- dfTrait[,!c(2)]

#### TRAIT: MEDIAN LATITUDE ####
# Conversion to absolute values before median latitude values are calculated.
dfLatitudeSpecies[, abs_lat_num := abs(lat_num)]
# Determine a median latitude for each BIN using absolute values.
dfLatitudeSpecies[, median_lat := median(abs_lat_num)]

#### TRAIT: LATITUDE RANGE ####
# Get maximum latitude for each bin
dfLatitudeSpecies[, max_lat := max(lat_num)]
# Get minimum latitude for each bin
dfLatitudeSpecies[, min_lat := min(lat_num)]
# Subtract maximum latitude and minimum latitude
dfLatitudeSpecies[, range_lat := max_lat - min_lat]
# Datatable organization
dfLatitudeSpecies <- dfLatitudeSpecies[, !c(14,15,17,18)]
# Get the trait specific datatable.
dfLatitudeMedian <- setDT(GetTraitSpecificDataBIN(dfLatitudeSpecies, 14))
dfLatitudeRange <- setDT(GetTraitSpecificDataBIN(dfLatitudeSpecies, 15))
# Datatable reorganization
setnames(dfLatitudeMedian, "species_label", "species_name")
setnames(dfLatitudeRange, "species_label", "species_name")
dfFiltered <- dfFiltered[, .(bin_uri, filtered_bin_size, recordID, order_name = order_label, family_name = family_label, genus_name = genus_label,
                             species_name = species_label, nucleotides)]
dfLatitudeMedian <- dfLatitudeMedian[, !c(1,3)]
dfLatitudeRange <- dfLatitudeRange[, !c(1,3)]

#### TRAIT: GR AREA ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "GR_area")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(GR_area)]
dfGrArea <- dfTrait[,!c(2)]

#### TRAIT: GR MAXIMUM LATITUDE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "GR_maxlat")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(GR_maxlat)]
dfGrMaxLat <- dfTrait[,!c(2)]

#### TRAIT: GR MINIMUM LATITUDE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "GR_minlat")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(GR_minlat)]
dfGrMinLat <- dfTrait[,!c(2)]

#### TRAIT: GR MIDDLE RANGE LATITUDE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "GR_midrangelat")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(GR_midrangelat)]
dfGrMidRangeLat <- dfTrait[,!c(2)]

#### TRAIT: GR MAXIMUM LONGITUDE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "GR_maxlong")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(GR_maxlong)]
dfGrMaxLong <- dfTrait[,!c(2)]

#### TRAIT: GR MINIMUM LONGITUDE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "GR_minlong")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(GR_minlong)]
dfGrMinLong <- dfTrait[,!c(2)]

#### TRAIT: GR MID RANGE LONGITUDE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "GR_midrangelong")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(GR_midrangelong)]
dfGrMidRangeLong <- dfTrait[,!c(2)]

#### TRAIT: HUMAN POPULATION DENSITY MINIMUM ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "hupopden_min")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(hupopden_min)]
dfHumanPopulationMin <- dfTrait[,!c(2)]

#### TRAIT: HUMAN POPULATION DENSITY MEAN ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "hupopden_mean")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(hupopden_mean)]
dfHumanPopulationMean <- dfTrait[,!c(2)]

#### TRAIT: HUMAN POPULATION DENSITY 5p ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "hupopden_5p")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(hupopden_5p)]
dfHumanPopulation5p <- dfTrait[,!c(2)]

#### TRAIT: HUMAN POPULATION DENSITY CHANGE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "hupopden_change")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(hupopden_change)]
dfHumanPopulationChange <- dfTrait[,!c(2)]

#### TRAIT: PRECIPITATION MEAN ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "precip_mean")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(precip_mean)]
dfPrecipitationMean <- dfTrait[,!c(2)]

#### TRAIT: Temperature Mean ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "temp_mean")]
anageTrait <- anageMammalData[, c("species_name", "temp_mean")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait , anageTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 3:4]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$temp_mean=rowMeans(dfTrait[,c(3:4)], na.rm=TRUE)
dfTemperatureMean <- dfTrait[,!c(2:4)]

#### TRAIT:AET MEAN ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "AET_mean")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(AET_mean)]
dfAETMean <- dfTrait[,!c(2)]

#### TRAIT:PET MEAN ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "PET_mean")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(PET_mean)]
dfPETMean <- dfTrait[,!c(2)]

#### TRAIT:ACTIVITY CYCLE ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "activity_cycle")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(activity_cycle)]
dfTrait$activity_cycle <- as.factor(dfTrait$activity_cycle)
dfActivityCycle <- dfTrait[,!c(2)]

#### TRAIT:DIET BREADTH ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "diet_breadth")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(diet_breadth)]
dfTrait$diet_breadth <- as.factor(dfTrait$diet_breadth)
dfDietBreadth <- dfTrait[,!c(2)]

#### TRAIT:HABITAT BREADTH ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "habitat_breadth")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(habitat_breadth)]
dfTrait$habitat_breadth <- as.factor(dfTrait$habitat_breadth)
dfHabitatBreadth <- dfTrait[,!c(2)]

#### TRAIT:TERRESTRIALITY ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "terrestriality")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(terrestriality)]
dfTrait$terrestriality <- as.factor(dfTrait$terrestriality)
dfTerrestriality <- dfTrait[,!c(2)]

#### TRAIT:TROPHIC LEVEL ####
pantheriaTrait <- pantheriaMammalData[, c("species_name", "trophic_level")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, pantheriaTrait))
dfTrait<- dfTrait[!is.na(accession_number)]
dfTrait<- dfTrait[!is.na(trophic_level)]
dfTrait$trophic_level <- as.factor(dfTrait$trophic_level)
dfTrophicLevel <- dfTrait[,!c(2)]

######################################################################################################################
#### Merging traits ####
#dfSpeciesInfo <- dfFiltered[!duplicated(species_name)]
#dfSpeciesInfo <- dfSpeciesInfo[,c(4:7)]
dfTraits <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfSpeciesInfo, dfFilteredSingle, dfBodyMass, dfForearmLength, dfHeadBodyLength, dfEyeOpeningAge, dfBmrRate, dfBmrMass, dfMaxLongevity, dfSexualMaturityAge, dfAdultSvlLength, dfMaturityLength, dfGrowthRate, dfImrPerYear,dfMrdt, dfMetabolicRate, dfFirstBirthAge, dfGestationLength, dfInterbirthInterval, dfLitterSize, dfLittersPerYear, dfNeonateBodyMass, dfNeonateHeadBodyLength, dfNeonateSvlLength, dfTeatNumber, dfWeaningAge, dfWeaningBodyMass, dfWeaningBodyLength, dfDispersalAge, dfHomeRange, dfHomeRangeIndividual, dfPopulationDensity, dfPopulationGroupSize, dfSocialGroupSize, dfLatitudeMedian, dfLatitudeRange, dfGrArea, dfGrMaxLat, dfGrMinLat, dfGrMidRangeLat, dfGrMaxLong, dfGrMinLong, dfGrMidRangeLong, dfHumanPopulationMin, dfHumanPopulationMean, dfHumanPopulation5p, dfHumanPopulationChange, dfPrecipitationMean, dfTemperatureMean, dfAETMean, dfPETMean, dfActivityCycle, dfDietBreadth, dfHabitatBreadth, dfTerrestriality, dfTrophicLevel))
dfTraits<- dfTraits[!is.na(accession_number)]
missing <- dfTraits[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 6:59]
missing <- which(missing == TRUE)
dfTraits <-dfTraits[!missing]

GetTraitInfo(dfTraits$body_mass)
GetTraitInfo(dfTraits$forearm_length)
GetTraitInfo(dfTraits$headbody_length)
GetTraitInfo(dfTraits$eyeopening_age)
GetTraitInfo(dfTraits$bmr_rate)
GetTraitInfo(dfTraits$bmr_mass)
GetTraitInfo(dfTraits$max_longevity)
GetTraitInfo(dfTraits$sexualmaturity_age)
GetTraitInfo(dfTraits$adult_svl_length)
GetTraitInfo(dfTraits$maturity_length)
GetTraitInfo(dfTraits$growth_rate)
GetTraitInfo(dfTraits$imr_pyear)
GetTraitInfo(dfTraits$mrdt)
GetTraitInfo(dfTraits$metabolic_rate)
GetTraitInfo(dfTraits$firstbirth_age)
GetTraitInfo(dfTraits$gestation_length)
GetTraitInfo(dfTraits$litter_size)
GetTraitInfo(dfTraits$litters_pyear)
GetTraitInfo(dfTraits$neonate_bodymass)
GetTraitInfo(dfTraits$neonate_headbodylength)
GetTraitInfo(dfTraits$neonate_svl_length)
GetTraitInfo(dfTraits$teatnumber)
GetTraitInfo(dfTraits$weaning_bodylength)
GetTraitInfo(dfTraits$dispersal_age)
GetTraitInfo(dfTraits$home_range)
GetTraitInfo(dfTraits$home_range_indiv)
GetTraitInfo(dfTraits$pop_density)
GetTraitInfo(dfTraits$pop_grpsize)
GetTraitInfo(dfTraits$social_grpsize)
GetTraitInfo(dfTraits$median_lat)
GetTraitInfo(dfTraits$range_lat)
GetTraitInfo(dfTraits$GR_area)
GetTraitInfo(dfTraits$GR_maxlat)
GetTraitInfo(dfTraits$GR_minlat)
GetTraitInfo(dfTraits$GR_midrangelat)
GetTraitInfo(dfTraits$GR_maxlong)
GetTraitInfo(dfTraits$GR_minlong)
GetTraitInfo(dfTraits$GR_midrangelong)
GetTraitInfo(dfTraits$hupopden_min)
GetTraitInfo(dfTraits$hupopden_mean)
GetTraitInfo(dfTraits$hupopden_5p)
GetTraitInfo(dfTraits$hupopden_change)
GetTraitInfo(dfTraits$precip_mean)
GetTraitInfo(dfTraits$temp_mean)
GetTraitInfo(dfTraits$AET_mean)
GetTraitInfo(dfTraits$PET_mean)
#Factors
GetTraitInfo(dfTraits$activity_cycle)
GetTraitInfo(dfTraits$diet_breadth)
GetTraitInfo(dfTraits$habitat_breadth)
GetTraitInfo(dfTraits$terrestriality)
GetTraitInfo(dfTraits$trophic_level)

#Take out traits that does not meet the criteria
dfTraits$maturity_length <- NULL
dfTraits$imr_pyear <- NULL
dfTraits$mrdt <- NULL
dfTraits$weaning_bodylength <- NULL

dfPreCentroid <- merge(dfcytB, dfTraits, by = "species_name")[, 1:3]
# Dataframe reorganization and renaming.
setnames(dfPreCentroid, "accession_number.x", "accession_number")

rm(dfFilteredSingle, dfBodyMass, dfForearmLength, dfHeadBodyLength, dfEyeOpeningAge, dfBmrRate, dfBmrMass, dfMaxLongevity, dfSexualMaturityAge, dfAdultSvlLength, dfMaturityLength, dfGrowthRate, dfImrPerYear,dfMrdt, dfMetabolicRate, dfFirstBirthAge, dfGestationLength, dfInterbirthInterval, dfLitterSize, dfLittersPerYear, dfNeonateBodyMass, dfNeonateHeadBodyLength, dfNeonateSvlLength, dfTeatNumber, dfWeaningAge, dfWeaningBodyMass, dfWeaningBodyLength, dfDispersalAge, dfHomeRange, dfHomeRangeIndividual, dfPopulationDensity, dfPopulationGroupSize, dfSocialGroupSize, dfLatitudeMedian, dfLatitudeRange, dfGrArea, dfGrMaxLat, dfGrMinLat, dfGrMidRangeLat, dfGrMaxLong, dfGrMinLong, dfGrMidRangeLong, dfHumanPopulationMin, dfHumanPopulationMean, dfHumanPopulation5p, dfHumanPopulationChange, dfPrecipitationMean, dfTemperatureMean, dfAETMean, dfPETMean, dfActivityCycle, dfDietBreadth, dfHabitatBreadth, dfTerrestriality, dfTrophicLevel)
rm(pantheriaMammalData, placentalMammalData, amnioteMammalData, anageMammalData, pantheriaTrait, amnioteTrait, placentalTrait, anageTrait)
rm(selectedTraits, missing, missingbm); rm( dfLatitudeSpecies, dfResolve, dfTrait)
