# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table A8
# Summary statistics of the stochastic demand model
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
numAgents = 2
numGames = 1000
numModels = 6

# Create output table

tab = matrix(data = NA, nrow = 6, ncol = 6)
colnames(tab) = c("a_0^H", "Profit Gain", "EqOnPath", "IR", "IC", "Punishment Length")

# Load output files

convResults = fread(file = "A_convResults.txt", 
                    header = TRUE,
                    verbose = TRUE)
res = fread(file = "A_res.txt", 
            header = TRUE,
            verbose = TRUE)

# Loop over models

for (iModel in 1:numModels) {
    
    # a_0^H
    
    tab[iModel, 1] = convResults$DemPar03[iModel]
    
    # Profit gain
    
    tab[iModel, 2] = convResults$avgPrGain[iModel]
    
    # Equilibrium play on path

    filename = paste("det_", iModel, ".Rdata", sep = "")
    load(filename)
    
    q = det$flagEQOnPath/det$PreShockCycleLength/numPrices/numAgents/numAgents
    tab[iModel, 3] = sum(q)/numGames                     

    # IR
    
    CondBR = (det$DevToPrice == det$DevAgStaticBR001)
    dataBR = det[CondBR, ]
    dim(dataBR)
    dN = dataBR[dataBR$nondevAg, ]
    dim(dN)
    
    iii = which(names(convResults) == "Ag1Price01")-1
    Prices = as.numeric(unlist(convResults[iModel, (iii+1):(iii+numPrices)]))
    x1 = Prices[dN$ShockPrice002]
    x0 = Prices[dN$ShockPrice001]
    tab[iModel, 4] = mean((x1-x0)/x0)

    # IC
    
    dD = dataBR[dataBR$devAg, ]
    x1 = dD$DeviationQ
    x0 = dD$OptStratQ001
    tab[iModel, 5] = mean(x1 < x0)

    # Punishment Length
    
    x1 = dN$ShockLength
    tab[iModel, 6] = mean(x1)
    
}

# Table

tab = round(tab, digits = 3)
max.print = getOption('max.print')
options(max.print = nrow(tab)*ncol(tab))
sink('table_A8.txt')
tab
sink()
options(max.print = max.print)

