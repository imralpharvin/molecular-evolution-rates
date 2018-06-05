setwd("C:/Users/RalphArvin/Desktop/Ralph - Work files/R workspaces/NCBI Genbank")

library(ape)
packageVersion("ape")

accessionList <- read.csv("accessionlist.csv")
accessionList <- as.vector(accessionList$accession)
genbankInfo<-read.GenBank(accessionList[1:100],species.names=T)
dfgenbankInfo <- data.frame(accesionlist=labels(genbankInfo), species=attr(genbankInfo, "species"),
                      sequences=sapply(genbankInfo, paste, collapse=""))
