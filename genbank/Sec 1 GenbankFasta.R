
setwd("C:/Users/RalphArvin/Desktop/work-s2018/genbank")

####NCBI Cytochrome B####

library(ape)
library("Biostrings")
packageVersion("ape")
#install.packages("taxize")
library("taxize")
library(stringr)
library(data.table)

#Read fasta
setwd("C:/Users/RalphArvin/Desktop")
cytB <- readDNAStringSet("sequence.fasta")
setwd("C:/Users/RalphArvin/Desktop/work-s2018/genbank")
seq_name = names(cytB)
sequence = paste(cytB)

#Put to data frama and convert to data table
dfcytB <- data.frame(seq_name,  sequence)
dfcytB <- as.data.table(dfcytB)

#Split accession number and species name
dfcytB[, c("accession_number", "genus_label", "species_label") := tstrsplit(seq_name, " ")][split1 == "", seq_name := split2]
dfcytB$species_name <- do.call(paste, c(dfcytB[, c("genus_label", "species_label")], sep = " ")) 
dfcytB$genus_label <- NULL
dfcytB$species_label <- NULL
dfcytB <- dfcytB[,c("accession_number", "species_name", "sequence")]

#Filter 1: Remove unverified sequences
dfcytB <- dfcytB[!species_name %like% "[:]"]
dfcytB <- dfcytB[!species_name %like% "[0-9]"]
dfcytB <- dfcytB[order(dfcytB$species_name),]

#Filter 2:
dfcytB[, c("split1", "split2") := tstrsplit(sequence, "^[-N]+")][split1 == "", sequence := split2]
dfcytB[, c("split1","split2") := NULL]
dfcytB[, sequence := tstrsplit(sequence, "[-N]+$")]

#Filter 3:
dfcytB[, internal_gapN := str_count(sequence, c("[N-]"))]
# Remove sequence if the number of Ns or gaps is greater than 1% (0.01) of the total length of the sequence.
dfcytB <- dfcytB[, percentage_gapN := internal_gapN/nchar(sequence)][!percentage_gapN > 0.01]
# Remove these columns as they are no longer needed.
dfcytB[, c("internal_gapN", "percentage_gapN") := NULL]

#Filter 4: Remove short and long sequences
dfcytB <- dfcytB[nchar(gsub("-", "", sequence)) %between% c(500, 1500)]
