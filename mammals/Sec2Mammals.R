#User: Ralph Arvin De Castro
#Program Description: This program extracts trait data from Pantheria, a mammal trait dataset
#Contribution: Jacqueline May (Latitude data)

#Set working directory 
setwd("C:/Users/RalphArvin/Desktop/ralph-workfiles/r-workspaces/Mammals")

#For data manipulation:
#install.packages("data.table")
library(data.table)
# For importing xslx data to data.frame 
#install.packages("readxl")
library("readxl")
#Open necessary functions and R scripts
source("GetTraitSpecificDataBIN.R")
source("GetTraitSpecificData.R")
source("GetTraitInfo.R")

### TRAIT: MEDIAN LATITUDE ### (Jacqueline May)
# Currently, median latitude is the only trait whose information is   t    aken from BOLD.
# Filtering for presence of a latitude value.
dfLatitudeSpecies <- dfFiltered[grep("[0-9]", lat)]
# Convert the latitude (lat) column to number instead of character type
dfLatitudeSpecies[, lat_num := as.numeric(lat)]
# Conversion to absolute values before median latitude values are calculated.
dfLatitudeSpecies[, abs_lat_num := abs(lat_num)]
# Determine a median latitude for each BIN using absolute values.
dfLatitudeSpecies[, median_lat := median(abs_lat_num), keyby = bin_uri]
# While considering traits for eventual multivariate analyses, it is necessary for them to have an adequate sample size 
# (i.e. over x # rows of data, depending on your purposes). In addition, they should exhibit some amount of variation across the observations.
# Use the GetTraitSpecificDataBIN function to obtain a subset of data for those species that have latitude data available.
# Get the trait specific datatable.
dfLatitudeMedian <- setDT(GetTraitSpecificDataBIN(dfLatitudeSpecies, 16))
# Get information for the trait.
GetTraitInfo(dfLatitudeMedian, "median_lat", type = "continuous")

### TRAIT: LATITUDE RANGE###
# Get max
dfLatitudeSpecies[, max_lat_num := max(lat_num) , keyby = bin_uri]
# Get min
dfLatitudeSpecies[, min_lat_num := min(lat_num),  keyby = bin_uri]
# Get range
dfLatitudeSpecies[, range_lat := max_lat_num - min_lat_num]
# Remove max and min
dfLatitudeSpecies$max_lat_num <- NULL
dfLatitudeSpecies$min_lat_num <- NULL
dfLatitudeRange <- setDT(GetTraitSpecificDataBIN(dfLatitudeSpecies, 17))
# Get information for the trait.
GetTraitInfo(dfLatitudeRange, "range_lat", type = "continuous")
#7. Datatable reorganization for dfFiltered.
dfFiltered <- dfFiltered[, .(bin_uri, filtered_bin_size, recordID, order_name = order_label, family_name = family_label, genus_name = genus_label,
                             species_label, nucleotides)]

### TRAIT: PANTHERIA XLSX ###
#1.Reading the data to variable allMammals
allMammals <- read_excel("Pantheria.xlsx")
#2.Select traits by column and store it in a vector
selectedTraits <- c("MSW05_Binomial", "5-1_AdultBodyMass_g","8-1_AdultForearmLen_mm","18-1_BasalMetRate_mLO2hr",  "15-1_LitterSize", "17-1_MaxLongevity_m", "23-1_SexualMaturityAge_d", "10-2_SocialGrpSize","12-1_HabitatBreadth","6-1_DietBreadth", "6-2_TrophicLevel", "1-1_ActivityCycle" )
#3.Filter the original data using the selectedTraits vector as the subset
traitData <- allMammals[selectedTraits]
#4.Renaming columns
colnames(traitData) <- c("species_label", "AdultBodyMass(g)", "AdultForearmLength(mm)", "BasalMetRate(mLO2hr)","LitterSize", "MaxLongevity(months)", "SexualMaturityAge(days)", "SocialGrpSize", "HabitatBreadth", "DietBreadth", "TrophicLevel", "ActivityCycle")
#5.Changing -999 values to NA
traitData[traitData == -999] <- NA
#6 Converting to data table
traitData <- as.data.table(traitData)
#7 Merge Species name
mergedSpecies <- merge(dfFiltered, traitData, by = "species_label")
#7 single row per species
dfFilteredSingle <- dfFiltered[!duplicated(species_label)][, .(bin_uri, species_label, filtered_bin_size)]
#8 Merge with latitude
dfTraits <- merge(dfFilteredSingle, dfLatitudeMedian, all = T, by = "bin_uri")
dfTraits <- merge(dfTraits, dfLatitudeRange, all = T, by = "bin_uri")
dfTraits <- dfTraits[, c(1:3,6,9)]
names(dfTraits)[2]<-"species_label"
names(dfTraits)[3]<-"filtered_bin_size"
#9 
dfTraits <- Reduce(function(...) merge(..., all = T, by = "species_label"), list(dfTraits, traitData))
#10
traitFiltered <- dfTraits[!is.na(bin_uri)]
#11
missing <- traitFiltered[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 4:16]
missing <- which(missing == TRUE)
traitFiltered <-traitFiltered[!missing]
dfPreCentroid <- merge(dfFiltered, traitFiltered, by = "bin_uri")[, 1:8]
# Dataframe reorganization and renaming.
setnames(dfPreCentroid, "species_label.x", "species_label")
setnames(dfPreCentroid, "filtered_bin_size.x", "filtered_bin_size")


