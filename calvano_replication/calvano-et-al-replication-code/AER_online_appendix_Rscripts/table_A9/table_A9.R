# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table A9
# Summary statistics of the variable market structure model
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

numPrices = 16
numGames = 1000

# Create output table

tab = matrix(data = NA, nrow = 2, ncol = 6)
colnames(tab) = c("rho", "Profit Gain", "EqOnPath", "IR", "IC", "Punishment Length")

# Load output files

convResults = fread(file = "A_convResults.txt", 
                    header = TRUE,
                    verbose = TRUE)
res = fread(file = "A_res.txt", 
            header = TRUE,
            verbose = TRUE)

# Loop over models

numModels = 2
for (iModel in 1:2) {
    
    # rho
    
    tab[iModel, 1] = convResults$DemPar11[iModel]
    
    # Profit gain
    
    tab[iModel, 2] = convResults$avgPrGainOut[iModel]
    
    # Equilibrium play on path
    
    filename = paste("det_", iModel, ".Rdata", sep = "")
    load(filename)
    
    det12 = det[det$ShockAgent <= 2 & det$ObsAgent <= 2, ]
    q = det12$flagEQOnPath/det12$PreShockCycleLength/numPrices/2/2
    tab[iModel, 3] = sum(q)/numGames                     

    # IR
    
    CondBR = (det$DevToPrice == det$DevAgStaticBR001) &
        (det$ShockPrice001 != numPrices) &
        (det$ShockPrice002 != numPrices)
    dataBR = det[CondBR, ]
    dim(dataBR)
    dN = dataBR[dataBR$nondevAg, ]
    dim(dN)
    
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

tab = cbind(round(tab[, 1, drop = FALSE], digits = 5), round(tab[, 2:ncol(tab)], digits = 3))
max.print = getOption('max.print')
options(max.print = nrow(tab)*ncol(tab))
sink('table_A9.txt')
tab
sink()
options(max.print = max.print)

