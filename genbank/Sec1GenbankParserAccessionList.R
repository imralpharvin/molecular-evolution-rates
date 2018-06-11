setwd("C:/Users/RalphArvin/Desktop/ralph-workfiles/r-workspaces/NCBI Genbank")

####WITH ACCESSION LIST####

library(ape)
library("Biostrings")
packageVersion("ape")

#Read file with accession list
accessionList <- read.csv("accessionlist.csv")
accessionList <- as.vector(accessionList$accession)
numSearch <- length(accessionList)
numSearch <- 500

#Setting up boundaries
tempMin = 1
tempMax = 300

if(numSearch < 300){
  tempMax <- numSearch
  genbankInfo<-read.GenBank(accessionList[tempMin:tempMax],species.names=T)
  species <- attr(genbankInfo, "species")
  write.dna(genbankInfo,"cytochromeb.fasta", format="fasta")
  
} else{
  
  genbankInfo<-read.GenBank(accessionList[tempMin:tempMax],species.names=T)
  write.dna(genbankInfo,"cytochromeb.fasta", format="fasta")
  species <- attr(genbankInfo, "species")
  while(tempMax != numSearch){
    
    tempMin = tempMin + 300
    tempMax = tempMax + 300
    
    if(numSearch <= tempMax ){
      
      tempMax = numSearch
      
    }
  
    genbankInfo<-read.GenBank(accessionList[tempMin:tempMax],species.names=T)
    write.dna(genbankInfo,"cytochromeb.fasta", format="fasta", append = TRUE)
    new <- attr(genbankInfo, "species")
    species <- append(species, new)
    
  }
}

#Read fasta
cytB <- readDNAStringSet("cytochromeb.fasta")

#Put to file
seq_name = names(cytB)
sequence = paste(cytB)
dfcytB <- data.frame(species, seq_name,  sequence)