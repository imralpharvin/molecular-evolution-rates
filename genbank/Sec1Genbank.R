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


####SEARCH TERM####
library(ape)
library("Biostrings")
library (rentrez)

searchTerm <- "cytochrome b[All Fields] AND \"Mus Musculus\"[Organism]"
numSearch <- 600
searchResult <- entrez_search(db="nuccore", term=searchTerm, retmax=numSearch)
tempMin = 1
tempMax = 300

searchSequences <- entrez_fetch(db="nuccore", id=searchResult$ids[tempMin:tempMax] ,rettype="fasta")
write(searchSequences, "searchSequences.fasta", sep="\n")

while(tempMax != numSearch){
  tempMin = tempMin + 300
  tempMax = tempMax + 300
  if(numSearch <= tempMax ){
    tempMax = numSearch
  }
  
  if(FALSE){
  if(tempMin < 798 && tempMax > 798) {
  print("stucks")
  searchSequences <- entrez_fetch(db="nuccore", id=searchResult$ids[tempMin:797] ,rettype="fasta")
  write(searchSequences, "searchSequences.fasta", sep="\n", append = TRUE)
  
  searchSequences <- entrez_fetch(db="nuccore", id=searchResult$ids[799: tempMax] ,rettype="fasta")
  #write(searchSequences, "searchSequences.fasta", sep="\n", append = TRUE)
  }else if(tempMax == 798){
    searchSequences <- entrez_fetch(db="nuccore", id=searchResult$ids[tempMin:797] ,rettype="fasta")
    write(searchSequences, "searchSequences.fasta", sep="\n", append = TRUE)
  } 
  }
  else{
  searchSequences <- entrez_fetch(db="nuccore", id=searchResult$ids[tempMin:tempMax] ,rettype="fasta")
  write(searchSequences, "searchSequences.fasta", sep="\n", append = TRUE)
  }
}

cytB <- readDNAStringSet("searchSequences.fasta")
seq_name = names(cytB)
sequence = paste(cytB)
dfcytB <- data.frame(seq_name, sequence)

