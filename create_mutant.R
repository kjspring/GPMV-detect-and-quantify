##########################################
# Filename:    mutate.R
# Description: Mutate the LAT-TMD
# Author:      Kevin Spring
##########################################

# This function will take the LAT-TMD sequence and generate all the mutations
# that are nonsynonymous but keeps the hydrophobic amino acids (not hydrophilic).

# Libraries
library(xts)


seqLAT <- "GCCATCCTGGTCCCCTGCGTGCTGGGGCTCCTGCTGCTGCCCATCCTGGCCATGTTGATGGCACTGTGTGTGCACTGCCACAGACTGCCAGGCTCC"

seq <- strsplit(seqLAT, "")

y <- vector()











# first convert all nucleotides to a A=0, T=1, C=2, G=3
for (i in (1:length(seq[[1]]) ) ) {

    if (seq[[1]][i:(i)] == 'A') {
        y[i] <- 0
        }
        
    if (seq[[1]][i] == 'T') {
        y[i] <- 1
        }

    if (seq[[1]][i] == 'C') {
        y[i] <- 2
        }
        
    if (seq[[1]][i] == 'G') {
        y[i] <- 3
        }
    }

#Table of the genetic code

g_code <- data.frame(AA=c(1, 1, 
                          2, 2, 2, 2, 2, 2, 
                          3, 3, 3, 
                          4, 
                          5, 5, 5, 5, 
                          6, 6, 6, 6, 
                          7, 7, 7, 7, 
                          8), 
                     code=c('TTT', 'TTC',
                            'TTA', 'TTG', 'CTT', 'CTC', 'CTA', 'CTG',
                            'ATT', 'ATC', 'ATA',
                            'ATG',
                            'GTT', 'GTC', 'GTA', 'GTG',
                            'GCT', 'GCC', 'GCA', 'GCG',
                            'CTT', 'CCC', 'CCA', 'CCG',
                            'TGG'))


for (i in 1:length(seq[[1]])) {
    


# Search through the code, starting with the last sequence.  Mutate it to another number
# Start at the last sequence and change to a 

