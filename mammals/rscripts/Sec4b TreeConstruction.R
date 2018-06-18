setwd("C:/Users/imralpharvin/Desktop/work-s2018/mammals/rscripts")


#install.packages("ape")
#install.packages("phangorn")
#install.packages("seqinr")
#source("https://bioconductor.org/biocLite.R")
#biocLite("Biostrings")

library(ape)
library(phangorn)
library(seqinr)
library(Biostrings)

#Converting data table to DNAStringSet object
mammalDSA <- DNAStringSet(dfCentroidSeqsNO$nucleotides)

#Write dna file for dna string set
write.dna(mammalDSA, "mammals.DNA")

#Read  dna file
mammals <- read.dna("mammals.dna", format="interleaved")

#DNA to phyDAT format
mammals_phyDat <- phyDat(mammals, type = "DNA", levels = NULL)

#mt <- modelTest(mammals_phyDat)
#print(mt)

#Compute pairwise distance
dm <- dist.ml(mammals_phyDat, model="JC69")

#NJ tree estimation
mammals_NJ  <- NJ(dm)
#Plot NJ tree
plot(mammals_NJ, main = "Neighbor Joining")


write.tree(mammals_NJ, file="bootstrap_example.tre")

