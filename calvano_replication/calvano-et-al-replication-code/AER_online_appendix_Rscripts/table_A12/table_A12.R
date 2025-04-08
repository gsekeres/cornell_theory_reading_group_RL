# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table A12
# Summary statistics with alternative action sets
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

numPricesVec = c(15, 15, 15, 50, 100)
numAgents = 2
numGames = 1000

# Create output table

tab = matrix(data = NA, nrow = 5, ncol = 5)
colnames(tab) = c("Profit Gain", "EqOnPath", "IR", "IC", "Punishment Length")
row.names(tab) = c("k = 15 (benchmark)", 
                   "k = 15, xi = 0.5", 
                   "k = 15, p(min) = 0.99, p(max) = p(Coop)+0.1*(p(Coop)-p(Nash))",
                   "k = 50, xi = 0.1",
                   "k = 100, xi = 0.1")

# Loop over models

numModels = 5
for (iModel in 1:5) {
    
    numPrices = numPricesVec[iModel]
    
    # Load output files
    
    filename = paste("A_convResults_", iModel, ".txt", sep = "")
    convResults = fread(file = filename, 
                        header = TRUE,
                        verbose = TRUE)
    filename = paste("A_res_", iModel, ".txt", sep = "")
    res = fread(file = filename, 
                header = TRUE,
                verbose = TRUE)
    filename = paste("A_ec_", iModel, ".txt", sep = "")
    ec = fread(file = filename, 
               header = TRUE,
               verbose = TRUE)
    
    # Profit gain
    
    tab[iModel, 1] = convResults$avgPrGain[1]
    
    # Equilibrium play on path
    
    tab[iModel, 2] = ec$FlagEQOnPath_Len00[1]

    # IR
    
    filename = paste("det_", iModel, ".Rdata", sep = "")
    load(filename)
    
    CondBR = (det$DevToPrice == det$DevAgStaticBR001)
    dataBR = det[CondBR, ]
    dim(dataBR)
    dN = dataBR[dataBR$nondevAg, ]
    dim(dN)
    
    iii = which(names(convResults) == "Ag1Price01")-1
    Prices = as.numeric(unlist(convResults[1, (iii+1):(iii+numPrices)]))
    x1 = Prices[dN$ShockPrice002]
    x0 = Prices[dN$ShockPrice001]
    tab[iModel, 3] = mean((x1-x0)/x0)

    # IC
    
    dD = dataBR[dataBR$devAg, ]
    x1 = dD$DeviationQ
    x0 = dD$OptStratQ001
    tab[iModel, 4] = mean(x1 < x0)

    # Punishment Length
    
    x1 = dN$ShockLength
    tab[iModel, 5] = mean(x1)
    
}

# Table

tab = round(tab, digits = 3)
max.print = getOption('max.print')
options(max.print = nrow(tab)*ncol(tab))
sink('table_A12.txt')
tab
sink()
options(max.print = max.print)

