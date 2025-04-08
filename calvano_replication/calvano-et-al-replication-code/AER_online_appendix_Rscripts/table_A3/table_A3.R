# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table A3
# Summary statistics of gain from the deviation 
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

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

load("det.Rdata")
numGames = 1000

# Only strategies converging to cycles with length 1

CondPreCycle1 = (det$PreShockCycleLength == 1)
sum(CondPreCycle1)/4/15
NCondPreCycle1 = sum(CondPreCycle1)/4/15

dataPreCycle1 = det[CondPreCycle1, ]
dim(dataPreCycle1)
dD = dataPreCycle1[dataPreCycle1$devAg, ]
dN = dataPreCycle1[dataPreCycle1$nondevAg, ]
dim(dD)
dim(dN)

# Create tables

# 3a: Average gain from deviation

tab = matrix(data = NA, nrow = 10, ncol = 16)
cPrices = as.character(Prices)
colnames(tab) = c("freq", cPrices)
rownames(tab) = c(cPrices[6:15])

for (i in 1:10) {
    ii = i+5
    tab[i, 1] = sum(dN$PreShockPriceDevAg == ii)/2/15/NCondPreCycle1
    for (j in 2:(ii+1)) {
        jj = j-1
        x1 = dD$DeviationQ[(dN$PreShockPriceDevAg == ii) & (dD$DevToPrice == jj)]
        x0 = dD$OptStratQ001[(dN$PreShockPriceDevAg == ii) & (dD$DevToPrice == jj)]
        tab[i, j] = mean((x1-x0)/x0)
    }
}

tab = round(tab, digits = 2)
max.print = getOption('max.print')
options(max.print = nrow(tab)*ncol(tab))
sink('table_A3_panel_a.txt')
tab
sink()
options(max.print = max.print)

# 3b: Frequency of negative gains from deviation

tab = matrix(data = NA, nrow = 10, ncol = 16)
colnames(tab) = c("freq", cPrices)
rownames(tab) = c(cPrices[6:15])

for (i in 1:10) {
    ii = i+5
    tab[i, 1] = sum(dD$PreShockPriceDevAg == ii)/2/15/NCondPreCycle1
    for (j in 2:(ii+1)) {
        jj = j-1
        x1 = dD$DeviationQ[(dD$PreShockPriceDevAg == ii) & (dD$DevToPrice == jj)]
        x0 = dD$OptStratQ001[(dD$PreShockPriceDevAg == ii) & (dD$DevToPrice == jj)]
        tab[i, j] = mean(x1 < x0)
    }
}

tab = round(tab, digits = 2)
max.print = getOption('max.print')
options(max.print = nrow(tab)*ncol(tab))
sink('table_A3_panel_b.txt')
tab
sink()
options(max.print = max.print)
