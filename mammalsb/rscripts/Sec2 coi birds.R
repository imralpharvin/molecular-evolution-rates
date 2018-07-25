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


##### Source 1: Amniote #####
# Read mammal data
amnioteMammalData <- read.csv("Amniote.csv")
# Filter mammals
amnioteMammalData <- filter(amnioteMammalData, class == "Aves")
# Subset columns needed for analysis
amnioteMammalData<- amnioteMammalData[, c(1:5, 8:12, 15, 17:29, 36)]
# Setting null valuescol
amnioteMammalData[amnioteMammalData == -999] <- NA
# Converting to data table
amnioteMammalData <- as.data.table(amnioteMammalData)
# Setting species name
amnioteMammalData <- unite(amnioteMammalData, "species_name", c("genus","species"), sep = " ", remove = FALSE)
# Rename columns
amnioteMammalData <- rename(amnioteMammalData,  female_maturity = female_maturity_d , litter_size = litter_or_clutch_size_n,  litters_pyear = litters_or_clutches_per_y, body_mass = adult_body_mass_g, max_longevity = maximum_longevity_y, hatching_weight = birth_or_hatching_weight_g, egg_mass = egg_mass_g, incubation = incubation_d, fledging_age = fledging_age_d, male_maturity = male_maturity_d ,female_body_mass = female_body_mass_g, male_body_mass = male_body_mass_g, egg_width = egg_width_mm, egg_length = egg_length_mm , fledging_mass = fledging_mass_g , adult_svl = adult_svl_cm,  interbirth_interval = inter_litter_or_interbirth_interval_y)
# Convert years to days
amnioteMammalData <- amnioteMammalData[, interbirth_interval := interbirth_interval *365]
# Convert years to months
amnioteMammalData <- amnioteMammalData[, max_longevity := max_longevity *12]
# Data table reorganization
amnioteMammalData <- amnioteMammalData[, !c(1,2,3,5,6)]

##### Source 4: Anage  #####
# Filter the original data using the selectedTraits vector as the subset
# Read mammal data
anageMammalData <- read_excel("anage.xlsx")
# Filter mammals
anageMammalData <- filter(anageMammalData, Class == "Aves")
# Subset columns needed for analysis
anageMammalData<- anageMammalData[, c(4:8, 10:12, 14:15, 17, 19:21, 28:29)]
# Rename columns
anageMammalData <- rename(anageMammalData, class = Class, order = Order, family = Family, genus = Genus, species = Species, female_maturity = "Female maturity (days)" , male_maturity = "Male maturity (days)", gestation_length = "Gestation/Incubation (days)", litter_size = "Litter/Clutch size", litters_pyear = "Litters/Clutches per year", neonate_bodymass = "Birth weight (g)" ,  body_mass = "Adult weight (g)", growth_rate = "Growth rate (1/days)", max_longevity = "Maximum longevity (yrs)",  metabolic_rate = "Metabolic rate (W)", body_mass = "Body mass (g)")
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
dfFilteredSingle <- dfFiltered[!duplicated(species_name)][, .(bin_uri, species_name, filtered_bin_size)]

#### TRAIT: MEDIAN LATITUDE ####
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

#### TRAIT: Female Maturity ####
amnioteTrait <- amnioteMammalData[, c("species_name", "female_maturity")]
anageTrait <- anageMammalData[, c("species_name", "female_maturity")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait , anageTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 4:5]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$female_maturity=rowMeans(dfTrait[,c(4:5)], na.rm=TRUE)
dfFemaleMaturity <- dfTrait[,!c(2:5)]

#### TRAIT: Litter Size ####
amnioteTrait <- amnioteMammalData[, c("species_name", "litter_size")]
anageTrait <- anageMammalData[, c("species_name", "litter_size")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait , anageTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 4:5]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$litter_size =rowMeans(dfTrait[,c(4:5)], na.rm=TRUE)
dfLitterSize <- dfTrait[,!c(2:5)]

#### TRAIT: Litters Per Year ####
amnioteTrait <- amnioteMammalData[, c("species_name", "litters_pyear")]
anageTrait <- anageMammalData[, c("species_name", "litters_pyear")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait , anageTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 4:5]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$litter_pyear =rowMeans(dfTrait[,c(4:5)], na.rm=TRUE)
dfLittersPerYear <- dfTrait[,!c(2:5)]

#### TRAIT:Body Mass ####
amnioteTrait <- amnioteMammalData[, c("species_name", "body_mass")]
anageTrait <- anageMammalData[, c("species_name", "body_mass")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait , anageTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 4:5]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$body_mass =rowMeans(dfTrait[,c(4:5)], na.rm=TRUE)
dfBodyMass <- dfTrait[,!c(2:5)]

#### TRAIT: Female Body Mass ####
amnioteTrait <- amnioteMammalData[, c("species_name", "female_body_mass")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
dfTrait<- dfTrait[!is.na(female_body_mass)]
dfFemaleBodyMass <- dfTrait[,!c(2:3)]

#### TRAIT: Male Body Mass ####
amnioteTrait <- amnioteMammalData[, c("species_name", "male_body_mass")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
dfTrait<- dfTrait[!is.na(male_body_mass)]
dfMaleBodyMass <- dfTrait[,!c(2:3)]

#### TRAIT: Neonate Body Mass ####
amnioteTrait <- amnioteMammalData[, c("species_name", "hatching_weight")]
anageTrait <- anageMammalData[, c("species_name", "neonate_bodymass")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait , anageTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 4:5]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$neonate_body_mass =rowMeans(dfTrait[,c(4:5)], na.rm=TRUE)
dfNeonateBodyMass <- dfTrait[,!c(2:5)]

#### TRAIT: Egg Mass ####
amnioteTrait <- amnioteMammalData[, c("species_name", "egg_mass")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
dfTrait<- dfTrait[!is.na(egg_mass)]
dfEggMass <- dfTrait[,!c(2:3)]

#### TRAIT: Incubation ####
amnioteTrait <- amnioteMammalData[, c("species_name", "incubation")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
dfTrait<- dfTrait[!is.na(incubation)]
dfIncubation <- dfTrait[,!c(2:3)]

#### TRAIT: Fledging Age ####
amnioteTrait <- amnioteMammalData[, c("species_name", "fledging_age")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
dfTrait<- dfTrait[!is.na(fledging_age)]
dfFledgingAge <- dfTrait[,!c(2:3)]

#### TRAIT: Male Maturity ####
amnioteTrait <- amnioteMammalData[, c("species_name", "male_maturity")]
anageTrait <- anageMammalData[, c("species_name", "male_maturity")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait , anageTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 4:5]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$male_maturity =rowMeans(dfTrait[,c(4:5)], na.rm=TRUE)
dfMaleMaturity <- dfTrait[,!c(2:5)]



#### TRAIT: Interbirth Interval ####
amnioteTrait <- amnioteMammalData[, c("species_name", "interbirth_interval")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
dfTrait<- dfTrait[!is.na(interbirth_interval)]
dfInterbirthInterval <- dfTrait[,!c(2:3)]



#### TRAIT: Egg Width ####
amnioteTrait <- amnioteMammalData[, c("species_name", "egg_width")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
dfTrait<- dfTrait[!is.na(egg_width)]
dfEggWidth <- dfTrait[,!c(2:3)]



#### TRAIT: Egg Length ####
amnioteTrait <- amnioteMammalData[, c("species_name", "egg_length")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
dfTrait<- dfTrait[!is.na(egg_length)]
dfEggLength <- dfTrait[,!c(2:3)]


#### TRAIT: Fledging Mass ####
amnioteTrait <- amnioteMammalData[, c("species_name", "fledging_mass")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
dfTrait<- dfTrait[!is.na(fledging_mass)]
dfFledgingMass <- dfTrait[,!c(2:3)]




#### TRAIT: Adult SvL ####
amnioteTrait <- amnioteMammalData[, c("species_name", "adult_svl")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
dfTrait<- dfTrait[!is.na(adult_svl)]
dfAdultSVL <- dfTrait[,!c(2:3)]


#### TRAIT: Gestation Length ####
anageTrait <- anageMammalData[, c("species_name", "gestation_length")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, anageTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
dfTrait<- dfTrait[!is.na(gestation_length)]
dfGestationLength <- dfTrait[,!c(2:3)]




#### TRAIT: Growth Rate ####
anageTrait <- anageMammalData[, c("species_name", "growth_rate")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, anageTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
dfTrait<- dfTrait[!is.na(growth_rate)]
dfGrowthRate <- dfTrait[,!c(2:3)]


#### TRAIT: Metabolic Rate ####
anageTrait <- anageMammalData[, c("species_name", "metabolic_rate")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, anageTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
dfTrait<- dfTrait[!is.na(metabolic_rate)]
dfMetabolicRate <- dfTrait[,!c(2:3)]


 

#### TRAIT: Max Longevity ####
amnioteTrait <- amnioteMammalData[, c("species_name", "max_longevity")]
anageTrait <- anageMammalData[, c("species_name", "max_longevity")]
dfTrait <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, amnioteTrait , anageTrait))
dfTrait<- dfTrait[!is.na(bin_uri)]
missingbm <- dfTrait[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 4:5]
missingbm <- which(missingbm == TRUE)
dfTrait <-dfTrait[!missingbm]
dfTrait$max_longevity =rowMeans(dfTrait[,c(4:5)], na.rm=TRUE)
dfMaxLongevity <- dfTrait[,!c(2:5)]




######################################################################################################################
#### Merging traits ####
dfTraits <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, dfLatitudeMedian, dfLatitudeRange, dfFemaleMaturity, dfLitterSize, dfLittersPerYear, dfBodyMass, dfFemaleBodyMass, dfMaleBodyMass, dfNeonateBodyMass, dfEggMass, dfIncubation, dfFledgingAge, dfMaleMaturity, dfInterbirthInterval, dfEggWidth, dfEggLength , dfFledgingMass, dfAdultSVL, dfGestationLength, dfGrowthRate, dfMetabolicRate, dfMaxLongevity  ))
dfTraits<- dfTraits[!is.na(bin_uri)]
missing <- dfTraits[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 4:25]
missing <- which(missing == TRUE)
dfTraits <-dfTraits[!missing]

GetTraitInfo(dfTraits$median_lat)
GetTraitInfo(dfTraits$range_lat)
GetTraitInfo(dfTraits$female_maturity)
GetTraitInfo(dfTraits$litter_size)
GetTraitInfo(dfTraits$litter_pyear)
GetTraitInfo(dfTraits$body_mass)
GetTraitInfo(dfTraits$female_body_mass)
GetTraitInfo(dfTraits$male_body_mass)
GetTraitInfo(dfTraits$neonate_body_mass)
GetTraitInfo(dfTraits$egg_mass)
GetTraitInfo(dfTraits$incubation)
GetTraitInfo(dfTraits$fledging_age)
GetTraitInfo(dfTraits$male_maturity)
GetTraitInfo(dfTraits$interbirth_interval)
GetTraitInfo(dfTraits$egg_width)
GetTraitInfo(dfTraits$egg_length)
GetTraitInfo(dfTraits$fledging_mass)
GetTraitInfo(dfTraits$adult_svl)
GetTraitInfo(dfTraits$gestation_length)
GetTraitInfo(dfTraits$growth_rate)
GetTraitInfo(dfTraits$metabolic_rate)
GetTraitInfo(dfTraits$max_longevity)

#Take out traits that does not meet the criteria

dfTraits$interbirth_interval <- NULL
dfTraits$egg_length <- NULL
dfTraits$egg_width <- NULL


dfPreCentroid <- merge(dfFiltered, dfTraits, by = "species_name")[, 1:8]
# Dataframe reorganization and renaming.
setnames(dfPreCentroid, "bin_uri.x", "bin_uri")
setnames(dfPreCentroid, "filtered_bin_size.x", "filtered_bin_size")

rm(dfFilteredSingle, dfLatitudeMedian, dfLatitudeRange, dfFemaleMaturity, dfLitterSize, dfLittersPerYear, dfBodyMass, dfFemaleBodyMass, dfMaleBodyMass, dfNeonateBodyMass, dfEggMass, dfIncubation, dfFledgingAge, dfMaleMaturity, dfInterbirthInterval, dfEggWidth, dfEggLength , dfFledgingMass, dfAdultSVL, dfGestationLength, dfGrowthRate, dfMetabolicRate, dfMaxLongevity )
rm(amnioteMammalData, anageMammalData, pantheriaTrait, amnioteTrait, placentalTrait, anageTrait)
rm(selectedTraits, missing, missingbm); rm( dfLatitudeSpecies, dfResolve, dfTrait)
