
setwd("C:/Users/RalphArvin/Desktop/work-s2018/genbank")

####BRCA Cytochrome B####

library(ape)
library("Biostrings")
packageVersion("ape")
#install.packages("taxize")
library("taxize")
library(stringr)
library(data.table)

#Read fasta
setwd("C:/Users/RalphArvin/Desktop/data")
Brca <- readDNAStringSet("brca1 mammals.fasta")
setwd("C:/Users/RalphArvin/Desktop/work-s2018/genbank")
seq_name = names(Brca)
sequence = paste(Brca)

#Put to data frama and convert to data table
dfBrca <- data.frame(seq_name,  sequence)
dfBrca <- as.data.table(dfBrca)

#Split accession number and species name
dfBrca[, c("accession_number", "genus_label", "species_label") := tstrsplit(seq_name, " ")][split1 == "", seq_name := split2]
dfBrca$species_name <- do.call(paste, c(dfBrca[, c("genus_label", "species_label")], sep = " ")) 
dfBrca$genus_label <- NULL
dfBrca$species_label <- NULL
dfBrca <- dfBrca[,c("accession_number", "species_name", "sequence")]

#Filter 1: Remove unverified sequences
dfBrca <- dfBrca[!species_name %like% "[:]"]
dfBrca <- dfBrca[!species_name %like% "[0-9]"]
dfBrca <- dfBrca[order(dfBrca$species_name),]

#Filter 2:
dfBrca[, c("split1", "split2") := tstrsplit(sequence, "^[-N]+")][split1 == "", sequence := split2]
dfBrca[, c("split1","split2") := NULL]
dfBrca[, sequence := tstrsplit(sequence, "[-N]+$")]

#Filter 3: Internal N percent
dfBrca[, internal_gapN := str_count(sequence, c("[N-]"))]
# Remove sequence if the number of Ns or gaps is greater than 1% (0.01) of the total length of the sequence.
dfBrca <- dfBrca[, percentage_gapN := internal_gapN/nchar(sequence)][!percentage_gapN > 0.01]
# Remove these columns as they are no longer needed.
dfBrca[, c("internal_gapN", "percentage_gapN") := NULL]

#Filter 4: Remove short and long sequences
dfBrca <- dfBrca[nchar(gsub("-", "", sequence)) %between% c(500, 1500)]
rm(Brca)

