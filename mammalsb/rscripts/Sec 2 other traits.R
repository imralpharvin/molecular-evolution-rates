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
traitData <- traitData[,!c(1,2,3)]

amnioteMammalData <- read.csv("Amniote.csv")

anageMammalData <- read_excel("anage.xlsx")
eltonMammalData <- read_excel("eltontrait.xlsx")

