# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table A6
# Summary statistics with 3 agents
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

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

if (!require(data.table)) {
    install.packages("data.table")
    library(data.table)
}

numPrices = 15
numGames = 1000

# Create output table

tab = matrix(data = NA, nrow = 2, ncol = 7)
colnames(tab) = c("alpha", "beta", "Profit Gain", "EqOnPath", "IR", "IC", "Punishment Length")

# 3 agents results: benchmark

convResults = fread(file = "A_convResults.txt", 
          header = TRUE,
          verbose = TRUE)
tab[1, 1] = convResults$alpha1[1]
convResults$beta = convResults$beta3/25000*100000
tab[1, 2] = convResults$beta[1]
tab[1, 3] = convResults$avgPrGain[1]
ec = fread(file = "A_ec.txt", 
          header = TRUE,
          verbose = TRUE)
tab[1, 4] = ec$FlagEQOnPath_Len00[1]

# IR

load("det_1.Rdata")
numAgents = 3
numPrices = 15

CondBR = (det$DevToPrice == det$DevAgStaticBR001)
dataBR = det[CondBR, ]
dim(dataBR)
dN = dataBR[dataBR$nondevAg, ]
dim(dN)

iii = which(names(convResults) == "Ag1Price01")-1
Prices = as.numeric(unlist(convResults[1, (iii+1):(iii+numPrices)]))
x1 = Prices[dN$ShockPrice002]
x0 = Prices[dN$ShockPrice001]
tab[1, 5] = mean((x1-x0)/x0)

# IC

dD = dataBR[dataBR$devAg, ]
x1 = dD$DeviationQ
x0 = dD$OptStratQ001
tab[1, 6] = mean(x1 < x0)

# Punishment Length

x1 = dN$ShockLength
tab[1, 7] = mean(x1)

# 3 agents results: improved

tab[2, 1] = convResults$alpha1[2]
convResults$beta = convResults$beta3/25000*100000
tab[2, 2] = convResults$beta[2]
tab[2, 3] = convResults$avgPrGain[2]
tab[2, 4] = ec$FlagEQOnPath_Len00[2]

# IR

load("det_2.Rdata")

CondBR = (det$DevToPrice == det$DevAgStaticBR001)
dataBR = det[CondBR, ]
dim(dataBR)
dN = dataBR[dataBR$nondevAg, ]
dim(dN)

iii = which(names(convResults) == "Ag1Price01")-1
Prices = as.numeric(unlist(convResults[1, (iii+1):(iii+numPrices)]))
x1 = Prices[dN$ShockPrice002]
x0 = Prices[dN$ShockPrice001]
tab[2, 5] = mean((x1-x0)/x0)

# IC

dD = dataBR[dataBR$devAg, ]
x1 = dD$DeviationQ
x0 = dD$OptStratQ001
tab[2, 6] = mean(x1 < x0)

# Punishment Length

x1 = dN$ShockLength
tab[2, 7] = mean(x1)

# Table

tab = round(tab, digits = 3)
max.print = getOption('max.print')
options(max.print = nrow(tab)*ncol(tab))
sink('table_A6.txt')
tab
sink()
options(max.print = max.print)

