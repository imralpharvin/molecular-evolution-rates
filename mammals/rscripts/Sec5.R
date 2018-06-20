setwd("C:/Users/RalphArvin/Desktop/work-s2018/mammals/rscripts")


#install.packages("data.table")
library(data.table)
# For phylogenetic tree manipulation and analysis:
#install.packages("adephylo")
library(adephylo)
#install.packages("ape")
library(ape)
#install.packages("caper")
library(caper)
#install.packages("phytools")
library(phytools)
# For statistical analysis/graphs:
#install.packages("car")
library(car)
#install.packages("plotly")
library(plotly)
#install.packages("Rmisc")
library(Rmisc)
# Load the function(s) designed for this script:
#install.packages("devtools")
library(devtools)
#install_github("helixcn/phylotools")
library(phylotools)
source("GetTraitInfo.R")
source("TestPhyloSig.R")
source("PGLS.R")
source("MergeAndPGLS.R")

#############################################################################################################################

# A phylogenetic tree containing branch length data for your species is required for this section.
# Read in your phylogenetic tree.
mainTree <- read.tree(file = "bootstrap_example.tree")
temp <- 1
while( temp <= length(mainTree$tip.label)){
  mainTree$tip.label[temp] <- dfCentroidSeqsNO$species_name[as.numeric(mainTree$tip.label[temp])]
  temp = temp + 1;
}
# Root the tree using your chosen outgroup species.
mainTree <- root(mainTree, outgroup = "Tachyglossus aculeatus", resolve.root = T)

### TRAIT: NUMBER OF NODES.
# Match mainTree with data subset. This will ensure the tree has only the tips we need for data analysis.
dfTraits <- dfTraits[match(mainTree$tip.label, dfTraits$species_name), ]
dfTraits[, number_of_nodes := distRoot(mainTree, method = "nNodes")]

### TRAIT: BRANCH LENGTHS.
# Let's calculate the sum of branch lengths now (from root to tip). These values will serve as our measurement of molecular evolution rate.
dfTraits[, branch_length := distRoot(mainTree, method = "patristic")]
# Get info about the branch lengths.
GetTraitInfo(dfTraits$branch_length)
# Range within which 95% of the values fall.
quantile(dfTraits$branch_length, probs = c(.025, .975))

# Take a closer look at branch length outliers. Some contaminated sequences might have STILL gotten through, so it is best to check!
# Using the IQR to detect statistical outliers.
lowerQuantile <- quantile(dfTraits$branch_length)[2]
upperQuantile <- quantile(dfTraits$branch_length)[4]
iqr <- upperQuantile - lowerQuantile
upperThreshold <- (iqr * 3) + upperQuantile
lowerThreshold <-  lowerQuantile - (iqr * 3)
# Extreme short branches.
dfShort <- dfTraits[branch_length < lowerThreshold][, c(1, 14:15)]
# Get the sequence information in case you want to BLAST the sequence (also, we aren't interested in outgroup species here,
# that's why we are using dfCentroidSeqsNO).
dfShort <- merge(dfShort, dfCentroidSeqsNO, by = "species_name")
# Do the same for the extreme long branches.
dfLong <- dfTraits[branch_length > upperThreshold][, c(1, 14:15)]
dfLong <- merge(dfLong, dfCentroidSeqsNO, by = "species_name")
# Remove from dataset, if desired.
dfTraits <- RemoveSequences(dfTraits, c(dfShort$species_name, dfLong$species_name))


### SINGLE VARIABLE REGRESSION ANALYSIS ###
# Running a single variable PGLS regression analysis for each trait to determine whether significance can be detected. If so, they will be included 
# in the multivariable regression model selection process.

# First, make sure the trait data and phylo tree match (in case species were removed).
mainTree <- drop.tip(phy = mainTree, tip = mainTree$tip.label[!mainTree$tip.label %in% dfTraits$species_name])
dfTraits <- dfTraits[match(mainTree$tip.label, dfTraits$species_name), ]

### SINGLE-VARIABLE PGLS ANALYSES ###
# Use the PGLS function to perform single-variable (with number of nodes as a control variable) for all of the traits. 
# e.g. branch_length ~ trait_of_interest + number_of_nodes
# We will do this by looping through all of the columns containing the trait data using lapply.
traits <- as.list(colnames(dfTraits[, 4:18]))
# Set to dataframe.
dfTraits <- as.data.frame(dfTraits)

# Start the loop.
singleVarResults <- lapply(traits, function(x) {
  # We only want the columns containing species name and dependent and independent variables.
  data <- dfTraits[, c("species_name", x, "branch_length", "number_of_nodes")]
  # Remove NA values.
  data <- data[complete.cases(data), ]
  # Perform PGLS. The trait of interest in this case will always be the 2nd column.
  caper <- PGLS(data, mainTree, branch_length ~ data[, 2] + number_of_nodes)
  # Take the summary of the results.
  caperSum <- summary(caper)
})
# Assign names to the list of results based on the trait of interest.
names(singleVarResults) <- traits
