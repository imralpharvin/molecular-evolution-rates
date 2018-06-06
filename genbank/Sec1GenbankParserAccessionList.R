setwd("C:/Users/RalphArvin/Desktop/ralph-workfiles/r-workspaces/NCBI Genbank")

####WITH ACCESSION LIST####

library(ape)
library("Biostrings")
packageVersion("ape")

accessionList <- read.csv("accessionlist.csv")
accessionList <- as.vector(accessionList$accession)
genbankInfo<-read.GenBank(accessionList[1:100],species.names=T)
write.dna(genbankInfo,"cytochromeb.fasta", format="fasta")
cytB <- readDNAStringSet("cytochromeb.fasta")
seq_name = names(cytB)
sequence = paste(cytB)
dfcytB <- data.frame(seq_name, sequence)