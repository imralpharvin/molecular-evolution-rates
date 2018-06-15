setwd("C:/Users/RalphArvin/Desktop/work-s2018/mammals/rscripts")


#For data manipulation:
#install.packages("data.table")
library(data.table)
# For importing xslx data to data.frame 
#install.packages("readxl")
library("readxl")
source("GetTraitSpecificDataBIN.R")
source("GetTraitSpecificData.R")
source("GetTraitInfo.R")

### TRAIT: MEDIAN LATITUDE ###
# Currently, median latitude is the only trait whose information is taken from BOLD. The rest of the data will be obtained from FishBase.
#1. Filtering for presence of a latitude value.
dfLatitudeSpecies <- dfFiltered[grep("[0-9]", lat)]
#2. Convert the latitude (lat) column to number instead of character type
dfLatitudeSpecies[, lat_num := as.numeric(lat)]
#3. Conversion to absolute values before median latitude values are calculated.
dfLatitudeSpecies[, abs_lat_num := abs(lat_num)]

#4. Determine a median latitude for each BIN using absolute values.
dfLatitudeSpecies[, median_lat := median(abs_lat_num), keyby = bin_uri]
#Range
dfLatitudeSpecies[, max_lat := max(lat_num), keyby = bin_uri]
dfLatitudeSpecies[, min_lat := min(lat_num), keyby = bin_uri]

dfLatitudeSpecies[, range_lat := max_lat - min_lat, keyby = bin_uri]
dfLatitudeSpecies <- dfLatitudeSpecies[, !c(14,15,17,18)]

# While considering traits for eventual multivariate analyses, it is necessary for them to have an adequate sample size 
# (i.e. over x # rows of data, depending on your purposes). In addition, they should exhibit some amount of variation across the observations.

# Use the GetTraitSpecificDataBIN function to obtain a subset of data for those species that have latitude data available.
#5. Get the trait specific datatable.
dfLatitudeMedian <- setDT(GetTraitSpecificDataBIN(dfLatitudeSpecies, 14))
dfLatitudeRange <- setDT(GetTraitSpecificDataBIN(dfLatitudeSpecies, 15))
setnames(dfLatitudeMedian, "species_label", "species_name")
setnames(dfLatitudeRange, "species_label", "species_name")

#6. Get information for the trait.
GetTraitInfo(dfLatitudeMedian, "median_lat", type = "continuous")
GetTraitInfo(dfLatitudeRange, "range_lat", type = "continuous")


#7. Datatable reorganization for dfFiltered.
dfFiltered <- dfFiltered[, .(bin_uri, filtered_bin_size, recordID, order_name = order_label, family_name = family_label, genus_name = genus_label,
                             species_name = species_label, nucleotides)]
### TRAIT: PANTHERIA XLSX ###
#1.Reading the data to variable allMammals
rawMammalData <- read_excel("Pantheria.xlsx")

#2.Select traits by column and store it in a vector
selectedTraits <- c("MSW05_Order","MSW05_Family","MSW05_Genus","MSW05_Binomial", "5-1_AdultBodyMass_g","8-1_AdultForearmLen_mm","18-1_BasalMetRate_mLO2hr",  "15-1_LitterSize", "17-1_MaxLongevity_m", "23-1_SexualMaturityAge_d", "10-2_SocialGrpSize","12-1_HabitatBreadth","6-1_DietBreadth", "6-2_TrophicLevel", "1-1_ActivityCycle" )

#3.Filter the original data using the selectedTraits vector as the subset
traitData <- rawMammalData[selectedTraits]

#4.Renaming columns
colnames(traitData) <- c("order", "family", "genus", "species_name", "AdultBodyMass(g)", "AdultForearmLength(mm)", "BasalMetRate(mLO2hr)","LitterSize", "MaxLongevity(months)", "SexualMaturityAge(days)", "SocialGrpSize", "HabitatBreadth", "DietBreadth", "TrophicLevel", "ActivityCycle")

#5.Changing -999 values to NA
traitData[traitData == -999] <- NA

#6 Converting to data table
traitData <- as.data.table(traitData)
traitData <- traitData[,!c(1,2,3)]
dfLatitudeMedian <- dfLatitudeMedian[, !c(1,3)]
dfLatitudeRange <- dfLatitudeRange[, !c(1,3)]

#7 Merge Species name
mergedSpecies <- merge(dfFiltered, traitData, by = "species_name")

#7 single row per species
dfFilteredSingle <- dfFiltered[!duplicated(species_name)][, .(bin_uri, species_name, filtered_bin_size)]

#8 Merge with latitude
#dfTraits <- merge(dfFilteredSingle, dfLatitude, all = T, by = "bin_uri")


#9 
dfTraits <- Reduce(function(...) merge(..., all = T, by = "species_name"), list(dfFilteredSingle, dfLatitudeMedian,dfLatitudeRange, traitData ))

#10
dfTraits<- dfTraits[!is.na(bin_uri)]

#12
missing <- dfTraits[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = 4:16]
missing <- which(missing == TRUE)
dfTraits <-dfTraits[!missing]


dfPreCentroid <- merge(dfFiltered, dfTraits, by = "bin_uri")[, 1:8]
dfPreCentroid<- dfPreCentroid[,c(7,1,2,3,4,5,6,8)]
# Dataframe reorganization and renaming.
setnames(dfPreCentroid, "species_name.x", "species_name")
setnames(dfPreCentroid, "filtered_bin_size.x", "filtered_bin_size")


rm(selectedTraits, missing); rm(traitData,dfLatitudeMedian,dfLatitudeRange, dfLatitudeSpecies, mergedSpecies, rawMammalData, dfFilteredSingle, dfResolve)
