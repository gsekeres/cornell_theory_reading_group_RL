# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table A11
# Summary statistics with different Q initializations
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

tab = matrix(data = NA, nrow = 6, ncol = 5)
colnames(tab) = c("Profit Gain", "EqOnPath", "IR", "IC", "Punishment Length")
row.names(tab) = c("Benchmark", "Nash",
                   "Grim Trigger", "Random (0, 10)", "Uniform at Q = 5", 
                   "Uniform at Q = 10, no expl.")

# Load output files

convResults = fread(file = "A_convResults.txt", 
                    header = TRUE,
                    verbose = TRUE)
res = fread(file = "A_res.txt", 
            header = TRUE,
            verbose = TRUE)
ec = fread(file = "A_ec.txt", 
           header = TRUE,
           verbose = TRUE)

# Loop over models

numModels = 6
for (iModel in 1:numModels) {
    
    # Profit gain
    
    tab[iModel, 1] = convResults$avgPrGain[iModel]
    
    # Equilibrium play on path
    
    tab[iModel, 2] = ec$FlagEQOnPath_Len00[iModel]

    # IR
    
    filename = paste("det_", iModel, ".Rdata", sep = "")
    load(filename)
    
    CondBR = (det$DevToPrice == det$DevAgStaticBR001)
    dataBR = det[CondBR, ]
    dim(dataBR)
    dN = dataBR[dataBR$nondevAg, ]
    dim(dN)
    
    iii = which(names(convResults) == "Ag1Price01")-1
    Prices = as.numeric(unlist(convResults[iModel, (iii+1):(iii+numPrices)]))
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
sink('table_A11.txt')
tab
sink()
options(max.print = max.print)

