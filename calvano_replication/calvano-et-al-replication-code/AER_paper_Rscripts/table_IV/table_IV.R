# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table 4
# Summary statistics for the asymmetric cost case 
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# Clear memory

rm(list = ls())
gc()

# Set working directory and load packages

if (!require(rstudioapi)) {
    install.packages("rstudioapi")
    library(rstudioapi)
}

current.path = getActiveDocumentContext()$path 
setwd(dirname(current.path))

# Loads data in Rdata format (should be faster)

p = read.csv("A_convResults.txt", header = TRUE, sep = "")
numModels = dim(p)[1]

# Set up the output matrix

tab = matrix(data = NA, nrow = 4, ncol = numModels)
row.names(tab) = c("c2", 
                   "2's Nash market share", 
                   "Average profit gain", 
                   "(Profit1/NashProfit1)/(Profit2/NashProfit2)")

# Compute market shares

for (iModel in 1:numModels) {
    tab[1, iModel] = p$DemPar05[iModel]
    
    tab[2, iModel] = p$NashMktSh2[iModel]/(p$NashMktSh1[iModel]+p$NashMktSh2[iModel])
    
    tab[3, iModel] = p$avgPrGain[iModel]
    
    tab[4, iModel] = (p$avgProf1[iModel]/p$NashProft1[iModel])/(p$avgProf2[iModel]/p$NashProft2[iModel])
}

tab = round(tab, digits = 3)
max.print = getOption('max.print')
options(max.print = nrow(tab)*ncol(tab))
sink('table_IV.txt')
tab
sink()
options(max.print = max.print)
