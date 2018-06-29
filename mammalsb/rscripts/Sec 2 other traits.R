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
library(dplyr) 
library(tidyr)
source("GetTraitSpecificDataBIN.R")
source("GetTraitSpecificData.R")
source("GetTraitInfo.R")

# Read mammal data
placentalMammalData <- read_excel("placental.xlsx")
placentalMammalData$refs <- NULL
# Rename columns
colnames(placentalMammalData) <- c("order", "family", "genus", "species_name", "body_mass", "gestation_length", "neonate_bodymass", "weaning_age", "weaning_bodymass", "firstbirth_age", "max_longevity", "litter_size", "litters_pyear");
# Changing -999 values to NA
placentalMammalData[placentalMammalData == -999] <- NA
# Converting to data table
placentalMammalData <- as.data.table(placentalMammalData)
placentalMammalData <- unite(placentalMammalData, "species", c("genus","species_name"), sep = " ", remove = FALSE)


amnioteMammalData <- read.csv("Amniote.csv")
amnioteMammalData <- filter(amnioteMammalData, class == "Mammalia")
amnioteMammalData<- amnioteMammalData[, c(1:5, 9:16, 22, 29, 32, 36)]
amnioteMammalData[amnioteMammalData == -999] <- NA
amnioteMammalData <- unite(amnioteMammalData, "species_name", c("genus","species"), sep = " ", remove = FALSE)
amnioteMammalData <- rename(amnioteMammalData,  litter_size = litter_or_clutch_size_n, litters_pyear = litters_or_clutches_per_y, body_mass = adult_body_mass_g, max_longevity = maximum_longevity_y, gestation_length = gestation_d, weaning_age = weaning_d, neonate_bodymass = birth_or_hatching_weight_g, weaning_bodymass = weaning_weight_g, interbirth_length = inter_litter_or_interbirth_interval_y, adult_svl_length = adult_svl_cm, neonate_svl_length = birth_or_hatching_svl_cm, maturity_length = no_sex_maturity_d)


#placentalMammalData <- unite(placentalMammalData, "species", c("genus","species_name"), sep = " ", remove = FALSE)

# Filter the original data using the selectedTraits vector as the subset

anageMammalData <- read_excel("anage.xlsx")
anageMammalData<- anageMammalData[, c(4:8, 12:21)]
anageMammalData <- filter(anageMammalData, Class == "Mammalia")
anageMammalData <- rename(anageMammalData, class = Class, order = Order, family = Family, genus = Genus, species = Species, gestation_length = "Gestation/Incubation (days)", weaning_age = "Weaning (days)", litter_size = "Litter/Clutch size", litters_pyear = "Litters/Clutches per year", interbirth_length =  "Inter-litter/Interbirth interval", neonate_bodymass = "Birth weight (g)" , weaning_bodymass= "Weaning weight (g)", body_mass = "Adult weight (g)", growth_rate = "Growth rate (1/days)", max_longevity = "Maximum longevity (yrs)")




eltonMammalData <- read_excel("eltontrait.xlsx")

   
