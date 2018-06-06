setwd("C:/Users/RalphArvin/Desktop/ralph-workfiles/r-workspaces/NCBI Genbank")

####SEARCH TERM####
library(ape)
library("Biostrings")
library (rentrez)

searchTerm <- "cytochrome b[All Fields] AND \"Mammalia\"[Organism]"
numSearch <- 1000
searchResult <- entrez_search(db="nuccore", term=searchTerm, retmax=numSearch)
searchResult
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
  #commented out
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
  #commented out
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

