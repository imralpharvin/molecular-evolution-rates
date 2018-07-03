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
#install.packages("readxl")
library("readxl")
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
source("GetTraitSpecificDataBIN.R")
source("GetTraitSpecificData.R")
source("GetTraitInfo.R")

##### Placental mammal data #####
# Read mammal data
placentalMammalData <- read_excel("placental.xlsx")
# Remove columns that are not needed for analysis
placentalMammalData$refs <- NULL
# Rename columns
colnames(placentalMammalData) <- c("order", "family", "genus", "species_name", "body_mass", "gestation_length", "neonate_bodymass", "weaning_age", "weaning_bodymass", "firstbirth_age", "max_longevity", "litter_size", "litters_pyear");
# Changing -999 values to NA
placentalMammalData[placentalMammalData == -999] <- NA
# Converting to data table
placentalMammalData <- as.data.table(placentalMammalData)
# Setting species name
placentalMammalData <- unite(placentalMammalData, "species", c("genus","species_name"), sep = " ", remove = FALSE)
# Converting months to days
placentalMammalData <- placentalMammalData[, gestation_length := gestation_length*30]
placentalMammalData <- placentalMammalData[, weaning_age := weaning_age *30]
placentalMammalData <- placentalMammalData[, firstbirth_age := firstbirth_age *30]

##### Amniote mammal data #####
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

##### Anage  mammal data #####
# Filter the original data using the selectedTraits vector as the subset
# Read mammal data
anageMammalData <- read_excel("anage.xlsx")
# Subset columns needed for analysis
anageMammalData<- anageMammalData[, c(4:8, 12, 14:17, 19:21, 26:30)]
# Filter mammals
anageMammalData <- filter(anageMammalData, Class == "Mammalia")
# Rename columns
anageMammalData <- rename(anageMammalData, class = Class, order = Order, family = Family, genus = Genus, species = Species, gestation_length = "Gestation/Incubation (days)", litter_size = "Litter/Clutch size", litters_pyear = "Litters/Clutches per year", interbirth_length =  "Inter-litter/Interbirth interval", neonate_bodymass = "Birth weight (g)" ,  body_mass = "Adult weight (g)", growth_rate = "Growth rate (1/days)", max_longevity = "Maximum longevity (yrs)", imr_pyear = "IMR (per yr)", mrdt = "MRDT (yrs)", metabolic_rate = "Metabolic rate (W)", body_mass = "Body mass (g)", temp_mean = "Temperature (K)")
# Setting species name
anageMammalData <- unite(anageMammalData, "species_name", c("genus","species"), sep = " ", remove = FALSE)
# Converting to data table
anageMammalData <- as.data.table(anageMammalData)
# Convert years to months
anageMammalData <- anageMammalData[, max_longevity := max_longevity *12]

eltonMammalData <- read_excel("eltontrait.xlsx")

   
