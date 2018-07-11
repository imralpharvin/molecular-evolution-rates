
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
setwd("C:/Users/RalphArvin/Desktop/data")
cytB <- readDNAStringSet("16s mammals.fasta")
setwd("C:/Users/RalphArvin/Desktop/work-s2018/genbank")
seq_name = names(cytB)
sequence = paste(cytB)

#Put to data frama and convert to data table
df16s <- data.frame(seq_name,  sequence)
df16s <- as.data.table(df16s)

#Split accession number and species name
df16s[, c("accession_number", "genus_label", "species_label") := tstrsplit(seq_name, " ")][split1 == "", seq_name := split2]
df16s$species_name <- do.call(paste, c(df16s[, c("genus_label", "species_label")], sep = " ")) 
df16s$genus_label <- NULL
df16s$species_label <- NULL
df16s <- df16s[,c("accession_number", "species_name", "sequence")]

#Filter 1: Remove unverified sequences
df16s <- df16s[!species_name %like% "[:]"]
df16s <- df16s[!species_name %like% "[0-9]"]
df16s <- df16s[order(df16s$species_name),]

#Filter 2:
df16s[, c("split1", "split2") := tstrsplit(sequence, "^[-N]+")][split1 == "", sequence := split2]
df16s[, c("split1","split2") := NULL]
df16s[, sequence := tstrsplit(sequence, "[-N]+$")]

#Filter 3: Internal N percent
df16s[, internal_gapN := str_count(sequence, c("[N-]"))]
# Remove sequence if the number of Ns or gaps is greater than 1% (0.01) of the total length of the sequence.
df16s <- df16s[, percentage_gapN := internal_gapN/nchar(sequence)][!percentage_gapN > 0.01]
# Remove these columns as they are no longer needed.
df16s[, c("internal_gapN", "percentage_gapN") := NULL]

#Filter 4: Remove short and long sequences
df16s <- df16s[nchar(gsub("-", "", sequence)) %between% c(500, 1500)]
rm(cytB)

