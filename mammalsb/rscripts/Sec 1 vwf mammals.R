
setwd("C:/Users/RalphArvin/Desktop/work-s2018/genbank")

####Vwf Cytochrome B####

library(ape)
library("Biostrings")
packageVersion("ape")
#install.packages("taxize")
library("taxize")
library(stringr)
library(data.table)

#Read fasta
setwd("C:/Users/RalphArvin/Desktop/data")
Vwf <- readDNAStringSet("Vwf 1 mammals.fasta")
setwd("C:/Users/RalphArvin/Desktop/work-s2018/genbank")
seq_name = names(Vwf)
sequence = paste(Vwf)

#Put to data frama and convert to data table
dfVwf <- data.frame(seq_name,  sequence)
dfVwf <- as.data.table(dfVwf)

#Split accession number and species name
dfVwf[, c("accession_number", "genus_label", "species_label") := tstrsplit(seq_name, " ")][split1 == "", seq_name := split2]
dfVwf$species_name <- do.call(paste, c(dfVwf[, c("genus_label", "species_label")], sep = " ")) 
dfVwf$genus_label <- NULL
dfVwf$species_label <- NULL
dfVwf <- dfVwf[,c("accession_number", "species_name", "sequence")]

#Filter 1: Remove unverified sequences
dfVwf <- dfVwf[!species_name %like% "[:]"]
dfVwf <- dfVwf[!species_name %like% "[0-9]"]
dfVwf <- dfVwf[order(dfVwf$species_name),]

#Filter 2:
dfVwf[, c("split1", "split2") := tstrsplit(sequence, "^[-N]+")][split1 == "", sequence := split2]
dfVwf[, c("split1","split2") := NULL]
dfVwf[, sequence := tstrsplit(sequence, "[-N]+$")]

#Filter 3: Internal N percent
dfVwf[, internal_gapN := str_count(sequence, c("[N-]"))]
# Remove sequence if the number of Ns or gaps is greater than 1% (0.01) of the total length of the sequence.
dfVwf <- dfVwf[, percentage_gapN := internal_gapN/nchar(sequence)][!percentage_gapN > 0.01]
# Remove these columns as they are no longer needed.
dfVwf[, c("internal_gapN", "percentage_gapN") := NULL]

#Filter 4: Remove short and long sequences
dfVwf <- dfVwf[nchar(gsub("-", "", sequence)) %between% c(500, 1500)]
rm(Vwf)

