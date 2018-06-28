setwd("C:/Users/RalphArvin/Desktop/work-s2018/mammalsb/rscripts")
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
source("GetTraitSpecificDataBIN.R")
source("GetTraitSpecificData.R")
source("GetTraitInfo.R")

#### TRAIT: MEDIAN LATITUDE ####
# Filtering for presence of a latitude value.
dfLatitudeSpecies <- dfFiltered[grep("[0-9]", lat)]
# Convert the latitude (lat) column to number instead of character type
dfLatitudeSpecies[, lat_num := as.numeric(lat)]
# Conversion to absolute values before median latitude values are calculated.
dfLatitudeSpecies[, abs_lat_num := abs(lat_num)]
# Determine a median latitude for each BIN using absolute values.
dfLatitudeSpecies[, median_lat := median(abs_lat_num), keyby = bin_uri]

#### TRAIT: LATITUDE RANGE ####
# Get maximum latitude for each bin
dfLatitudeSpecies[, max_lat := max(lat_num), keyby = bin_uri]
# Get minimum latitude for each bin
dfLatitudeSpecies[, min_lat := min(lat_num), keyby = bin_uri]
# Subtract maximum latitude and minimum latitude
dfLatitudeSpecies[, range_lat := max_lat - min_lat, keyby = bin_uri]
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

#### TRAIT: PANTHERIA XLSX ####
# Read excel sheet data
rawMammalData <- read_excel("Pantheria.xlsx")
# Select all columns
selectedTraits<- c("MSW05_Order","MSW05_Family","MSW05_Genus","MSW05_Binomial", "5-1_AdultBodyMass_g" ,"8-1_AdultForearmLen_mm", "13-1_AdultHeadBodyLen_mm", "2-1_AgeatEyeOpening_d" ,"3-1_AgeatFirstBirth_d", "18-1_BasalMetRate_mLO2hr" ,"5-2_BasalMetRateMass_g" ,"7-1_DispersalAge_d" ,"9-1_GestationLen_d", "22-1_HomeRange_km2"          
,"22-2_HomeRange_Indiv_km2","14-1_InterbirthInterval_d", "15-1_LitterSize","16-1_LittersPerYear","17-1_MaxLongevity_m", "5-3_NeonateBodyMass_g"       , "13-2_NeonateHeadBodyLen_mm", "21-1_PopulationDensity_n/km2", "10-1_PopulationGrpSize", "23-1_SexualMaturityAge_d", "10-2_SocialGrpSize","24-1_TeatNumber"             
, "25-1_WeaningAge_d" , "5-4_WeaningBodyMass_g" , "13-3_WeaningHeadBodyLen_mm","26-1_GR_Area_km2" , "26-2_GR_MaxLat_dd" ,"26-3_GR_MinLat_dd", "26-4_GR_MidRangeLat_dd","26-5_GR_MaxLong_dd"          
, "26-6_GR_MinLong_dd", "26-7_GR_MidRangeLong_dd","27-1_HuPopDen_Min_n/km2" , "27-2_HuPopDen_Mean_n/km2", "27-3_HuPopDen_5p_n/km2", "27-4_HuPopDen_Change"        
, "28-1_Precip_Mean_mm" , "28-2_Temp_Mean_01degC" ,"30-1_AET_Mean_mm","30-2_PET_Mean_mm","1-1_ActivityCycle",  "6-1_DietBreadth", "12-1_HabitatBreadth", "12-2_Terrestriality", "6-2_TrophicLevel")
# Filter the original data using the selectedTraits vector as the subset
traitData <- rawMammalData[selectedTraits]
# Rename columns
colnames(traitData) <- c("order", "family", "genus", "species_name", "body_mass", "forearm_length", "headbody_length", "eyeopening_age", "firstbirth_age", "bmr_rate", "bmr_mass", "dispersal_age", "gestation_length", "home_range", "home_range_indiv", "interbirth_interval", "litter_size", "litters_pyear", "max_longevity", "neonate_bodymass", "neonate_headbodylength", "pop_density", "pop_grpsize", "sexualmaturity_age", "social_grpsize", "teatnumber", "weaning_age", "weaning_bodymass", "weaning_bodylength", "GR_area", "GR_maxlat", "GR_minlat", "GR_midrangelat", "GR_maxlong", "GR_minlong", "GR_midrangelong", "hupopden_min", "hupopden_mean", "hupopden_5p", "hupopden_change", "precip_mean", "temp_mean", "AET_mean", "PET_mean", "activity_cycle", "diet_breadth", "habitat_breadth", "terrestriality", "trophic_level")
# Changing -999 values to NA
traitData[traitData == -999] <- NA
# Converting to data table
traitData <- as.data.table(traitData)
traitData <- traitData[,!c(1,2,3)]

# Merge Species name
mergedSpecies <- merge(dfFiltered, traitData, by = "species_name")
# Single row per species
dfFilteredSingle <- dfFiltered[!duplicated(species_name)][, .(bin_uri, species_name, filtered_bin_size)]
#  
dfTraits <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, dfLatitudeMedian,dfLatitudeRange, traitData ))
#
dfTraits<- dfTraits[!is.na(bin_uri)]
missing <- dfTraits[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 4:50]
missing <- which(missing == TRUE)
dfTraits <-dfTraits[!missing]

dfPreCentroid <- merge(dfFiltered, dfTraits, by = "bin_uri")[, 1:8]
dfPreCentroid<- dfPreCentroid[,c(7,1,2,3,4,5,6,8)]
# Dataframe reorganization and renaming.
setnames(dfPreCentroid, "species_name.x", "species_name")
setnames(dfPreCentroid, "filtered_bin_size.x", "filtered_bin_size")

rm(selectedTraits, missing); rm(traitData,dfLatitudeMedian,dfLatitudeRange, dfLatitudeSpecies, mergedSpecies, rawMammalData, dfFilteredSingle, dfResolve)
