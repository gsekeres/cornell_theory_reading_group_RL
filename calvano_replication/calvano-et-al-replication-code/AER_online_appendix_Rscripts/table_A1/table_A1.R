# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table A1
# Summary statistics in the memoryless model with delta = 0
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

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

# Define output table

out.table = matrix(data = NA, nrow = 1, ncol = 3)
colnames(out.table) = c("Avg Profit Gain", 
                        "Avg Equilibrium Play", 
                        "Avg Q gap")

# Load data in txt format

conv.res = read.csv(file = "A_convResults.txt", 
                    header = TRUE, sep = "")
out.table[1, 1] = conv.res$avgPrGain[1]

ec = read.csv(file = "A_ec.txt", header = TRUE, sep = "")
out.table[1, 2] = ec$FlagEQOnPath_Len00[1]

qg = read.csv(file = "A_qg.txt", header = TRUE, sep = "")
out.table[1, 3] = qg$QGapTot[1]

round(out.table, digits = 3)

