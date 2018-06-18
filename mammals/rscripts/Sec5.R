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
source("GetTraitInfo.R")
source("TestPhyloSig.R")
source("PGLS.R")
source("MergeAndPGLS.R")

#############################################################################################################################

# A phylogenetic tree containing branch length data for your species is required for this section.
# Read in your phylogenetic tree.
mainTree <- read.tree(file = "bootstrap_example.tre")

mainTree$tip.label <- gsub("_", " ", mainTree$tip.label)
# Root the tree using your chosen outgroup species.
# Match mainTree with data subset. This will ensure the tree has only the tips we need for data analysis.
mainTree <- drop.tip(phy = mainTree, tip = mainTree$tip.label[!mainTree$tip.label %in% dfCentroidSeqsNO$species_name])

dfTraits2 <- dfTraits[match(mainTree$tip.label, dfTraits$species_name), ]
dfTraits[, number_of_nodes := distRoot(mainTree, method = "nNodes")]
