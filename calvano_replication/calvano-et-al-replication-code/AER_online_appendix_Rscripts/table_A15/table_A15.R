# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table A15
# Summary statistics of the benchmark model with Boltzmann exploration
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

# Load output files

filename = paste("A_convResults.txt", sep = "")
convResults = fread(file = filename, 
                    header = TRUE,
                    verbose = TRUE)
filename = paste("A_res.txt", sep = "")
res = fread(file = filename, 
            header = TRUE,
            verbose = TRUE)
filename = paste("A_ec.txt", sep = "")
ec = fread(file = filename, 
           header = TRUE,
           verbose = TRUE)

# Create output table

tab = matrix(data = NA, nrow = 1, ncol = 7)
colnames(tab) = c("alpha", "beta",  
                  "Profit Gain", "EqOnPath", "IR", "IC", "Punishment Length")

# Loop over models

for (iModel in 1:1) {
    
    # alpha
    
    tab[iModel, 1] = convResults$alpha1[iModel]
    
    # beta
    
    beta = convResults$beta1[iModel]
    tab[iModel, 2] = 1-10^beta
    
    # Profit gain
    
    tab[iModel, 3] = convResults$avgPrGain[iModel]
    
    # Equilibrium play on path
    
    tab[iModel, 4] = ec$FlagEQOnPath_Len00[iModel]

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
    z = (x1-x0)/x0
    z = z[!is.infinite(z)]
    tab[iModel, 5] = mean(z)

    # IC
    
    dD = dataBR[dataBR$devAg, ]
    x1 = dD$DeviationQ
    x0 = dD$OptStratQ001
    tab[iModel, 6] = mean(x1 < x0)

    # Punishment Length
    
    x1 = dN$ShockLength
    tab[iModel, 7] = mean(x1)
    
}

# Table

tab = round(tab, digits = 6)
max.print = getOption('max.print')
options(max.print = nrow(tab)*ncol(tab))
sink('table_A15.txt')
tab
sink()
options(max.print = max.print)

