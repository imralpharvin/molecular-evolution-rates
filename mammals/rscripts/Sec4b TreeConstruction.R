setwd("C:/Users/RalphArvin/Desktop/work-s2018/mammals/rscripts")


#install.packages("ape")
#install.packages("phangorn")
#install.packages("seqinr")
library(ape)
library(phangorn)
library(seqinr)
library(Biostrings)

mammalDSA <- DNAStringSet(dfCentroidSeqsNO$nucleotides)
write.dna(mammalDSA, "mammals.DNA")


mammals <- read.dna("mammals.dna", format="interleaved")
mammals_phyDat <- phyDat(mammals, type = "DNA", levels = NULL)
mammals10 <- subset(mammals_phyDat, 1:10)
mammals10_phyDat <- phyDat(mammals10, type = "DNA", levels = NULL)

mt <- modelTest(mammals10)
print(mt)
dm <- dist.ml(mammals10, model="JC69")

mammals_UPGMA <- upgma(dm)
mammals_NJ  <- NJ(dm)
plot(mammals_UPGMA, main="UPGMA")
plot(mammals_NJ, main = "Neighbor Joining")
