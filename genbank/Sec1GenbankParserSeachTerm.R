setwd("C:/Users/RalphArvin/Desktop/ralph-workfiles/r-workspaces/NCBI Genbank")

####SEARCH TERM####
library(ape)
library("Biostrings")
library (rentrez)

#Put gene and organism here
searchTerm <- "cytochrome b[All Fields] AND \"Sus\"[Organism]"
#Put how many searches
#search genbank
searchResult <- entrez_search(db="nuccore", term=searchTerm)
searchResult
numSearch <- searchResult$count
searchResult <- entrez_search(db="nuccore", term=searchTerm, retmax= numSearch)
searchResult


#Setting up boundaries
tempMin = 1
tempMax = 300

if(numSearch < 300){
  tempMax <- numSearch
  searchSequences <- entrez_fetch(db="nuccore", id=searchResult$ids[tempMin:tempMax] ,rettype="fasta")
  write(searchSequences, "searchSequences.fasta", sep="\n")
} else{
  searchSequences <- entrez_fetch(db="nuccore", id=searchResult$ids[tempMin:tempMax] ,rettype="fasta")
  write(searchSequences, "searchSequences.fasta", sep="\n")

  while(tempMax != numSearch){
    tempMin = tempMin + 300
    tempMax = tempMax + 300
  if(numSearch <= tempMax ){
    tempMax = numSearch
  }
  searchSequences <- entrez_fetch(db="nuccore", id=searchResult$ids[tempMin:tempMax] ,rettype="fasta")
  write(searchSequences, "searchSequences.fasta", sep="\n", append = TRUE)
}
}

cytB <- readDNAStringSet("searchSequences.fasta")
seq_name = names(cytB)
sequence = paste(cytB)
dfcytB <- data.frame(seq_name, sequence)

