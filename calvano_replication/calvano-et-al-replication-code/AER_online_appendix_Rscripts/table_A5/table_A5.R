# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table A5
# Summary statistics for 2, 3, and 4 agents
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

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

tab = matrix(data = NA, nrow = 3, ncol = 5)
colnames(tab) = c("Profit Gain", "EqOnPath", "IR", "IC", "Punishment Length")
row.names(tab) = c("n = 2", "n = 3", "n = 4")

# 2 agents results

# Profit gain

convResults = fread(file = "A_convResults_2agents.txt", 
          header = TRUE,
          verbose = TRUE)
tab[1, 1] = convResults$avgPrGain[1]

# Equilibrium play on path

ec = fread(file = "A_ec_2agents.txt", 
          header = TRUE,
          verbose = TRUE)
tab[1, 2] = ec$FlagEQOnPath_Len00[1]

# IR

load("det_2agents.Rdata")
numAgents = 2

CondBR = (det$DevToPrice == det$DevAgStaticBR001)
dataBR = det[CondBR, ]
dim(dataBR)
dN = dataBR[dataBR$nondevAg, ]
dim(dN)

iii = which(names(convResults) == "Ag1Price01")-1
Prices = as.numeric(unlist(convResults[1, (iii+1):(iii+numPrices)]))
x1 = Prices[dN$ShockPrice002]
x0 = Prices[dN$ShockPrice001]
tab[1, 3] = mean((x1-x0)/x0)

# IC

dD = dataBR[dataBR$devAg, ]
x1 = dD$DeviationQ
x0 = dD$OptStratQ001
tab[1, 4] = mean(x1 < x0)

# Punishment Length

x1 = dN$ShockLength
tab[1, 5] = mean(x1)

# 3 agents results

convResults = fread(file = "A_convResults_3agents.txt", 
          header = TRUE,
          verbose = TRUE)
tab[2, 1] = convResults$avgPrGain[1]
ec = fread(file = "A_ec_3agents.txt", 
          header = TRUE,
          verbose = TRUE)
tab[2, 2] = ec$FlagEQOnPath_Len00[1]

# IR

load("det_3agents.Rdata")
numAgents = 3

CondBR = (det$DevToPrice == det$DevAgStaticBR001)
dataBR = det[CondBR, ]
dim(dataBR)
dN = dataBR[dataBR$nondevAg, ]
dim(dN)

iii = which(names(convResults) == "Ag1Price01")-1
Prices = as.numeric(unlist(convResults[1, (iii+1):(iii+numPrices)]))
x1 = Prices[dN$ShockPrice002]
x0 = Prices[dN$ShockPrice001]
tab[2, 3] = mean((x1-x0)/x0)

# IC

dD = dataBR[dataBR$devAg, ]
x1 = dD$DeviationQ
x0 = dD$OptStratQ001
tab[2, 4] = mean(x1 < x0)

# Punishment Length

x1 = dN$ShockLength
tab[2, 5] = mean(x1)

# 4 agents results

convResults = fread(file = "A_convResults_4agents.txt", 
          header = TRUE,
          verbose = TRUE)
tab[3, 1] = convResults$avgPrGain[1]
ec = fread(file = "A_ec_4agents.txt", 
          header = TRUE,
          verbose = TRUE)
tab[3, 2] = ec$FlagEQOnPath_Len00[1]

# IR

load("det_4agents.Rdata")
numAgents = 4

CondBR = (det$DevToPrice == det$DevAgStaticBR001)
dataBR = det[CondBR, ]
dim(dataBR)
dN = dataBR[dataBR$nondevAg, ]
dim(dN)

iii = which(names(convResults) == "Ag1Price01")-1
Prices = as.numeric(unlist(convResults[1, (iii+1):(iii+numPrices)]))
x1 = Prices[dN$ShockPrice002]
x0 = Prices[dN$ShockPrice001]
tab[3, 3] = mean((x1-x0)/x0)

# IC

dD = dataBR[dataBR$devAg, ]
x1 = dD$DeviationQ
x0 = dD$OptStratQ001
tab[3, 4] = mean(x1 < x0)

# Punishment Length

x1 = dN$ShockLength
tab[3, 5] = mean(x1)

# Table

tab = round(tab, digits = 3)
max.print = getOption('max.print')
options(max.print = nrow(tab)*ncol(tab))
sink('table_A5.txt')
tab
sink()
options(max.print = max.print)

