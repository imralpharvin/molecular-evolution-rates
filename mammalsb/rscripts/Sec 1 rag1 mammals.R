
setwd("C:/Users/RalphArvin/Desktop/work-s2018/genbank")

####Rag Cytochrome B####

library(ape)
library("Biostrings")
packageVersion("ape")
#install.packages("taxize")
library("taxize")
library(stringr)
library(data.table)

#Read fasta
setwd("C:/Users/RalphArvin/Desktop/data")
Rag <- readDNAStringSet("Rag 1 mammals.fasta")
setwd("C:/Users/RalphArvin/Desktop/work-s2018/genbank")
seq_name = names(Rag)
sequence = paste(Rag)

#Put to data frama and convert to data table
dfRag <- data.frame(seq_name,  sequence)
dfRag <- as.data.table(dfRag)

#Split accession number and species name
dfRag[, c("accession_number", "genus_label", "species_label") := tstrsplit(seq_name, " ")][split1 == "", seq_name := split2]
dfRag$species_name <- do.call(paste, c(dfRag[, c("genus_label", "species_label")], sep = " ")) 
dfRag$genus_label <- NULL
dfRag$species_label <- NULL
dfRag <- dfRag[,c("accession_number", "species_name", "sequence")]

#Filter 1: Remove unverified sequences
dfRag <- dfRag[!species_name %like% "[:]"]
dfRag <- dfRag[!species_name %like% "[0-9]"]
dfRag <- dfRag[order(dfRag$species_name),]

#Filter 2:
dfRag[, c("split1", "split2") := tstrsplit(sequence, "^[-N]+")][split1 == "", sequence := split2]
dfRag[, c("split1","split2") := NULL]
dfRag[, sequence := tstrsplit(sequence, "[-N]+$")]

#Filter 3: Internal N percent
dfRag[, internal_gapN := str_count(sequence, c("[N-]"))]
# Remove sequence if the number of Ns or gaps is greater than 1% (0.01) of the total length of the sequence.
dfRag <- dfRag[, percentage_gapN := internal_gapN/nchar(sequence)][!percentage_gapN > 0.01]
# Remove these columns as they are no longer needed.
dfRag[, c("internal_gapN", "percentage_gapN") := NULL]

#Filter 4: Remove short and long sequences
dfRag <- dfRag[nchar(gsub("-", "", sequence)) %between% c(500, 1500)]
rm(Rag)

