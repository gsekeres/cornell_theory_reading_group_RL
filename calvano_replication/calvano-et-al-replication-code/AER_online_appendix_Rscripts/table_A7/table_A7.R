# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table A7
# Summary statistics for the demand asymmetry case 
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

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

# Load data 

convResults = read.csv("A_convResults.txt", header = TRUE, sep = "")
numModels = dim(convResults)[1]
numPrices = 15
numAgents = 2
ec = read.csv("A_ec.txt", header = TRUE, sep = "")
res = read.csv("A_res.txt", header = TRUE, sep = "")
ir = read.csv("A_irToBR.txt", header = TRUE, sep = "")

# Set up the output matrix

tab = matrix(data = NA, nrow = numModels, ncol = 7)
colnames(tab) = c("a2", 
                   "2's Nash market share", 
                   "Average profit gain", 
                   "(Profit1/NashProfit1)/(Profit2/NashProfit2)",
                   "Equilibrium play on path",
                   "IR", 
                   "IC")

# Compute market shares

for (iModel in 1:numModels) {
    tab[iModel, 1] = convResults$DemPar03[iModel]
    
    tab[iModel, 2] = convResults$NashMktSh2[iModel]/(convResults$NashMktSh1[iModel]+convResults$NashMktSh2[iModel])
    
    tab[iModel, 3] = convResults$avgPrGain[iModel]
    
    tab[iModel, 4] = (convResults$avgProf1[iModel]/convResults$NashProft1[iModel])/(convResults$avgProf2[iModel]/convResults$NashProft2[iModel])
    
    tab[iModel, 5] = ec$FlagEQOnPath_Len00[iModel]
    
    # IR
    
    filename = paste("A_det_", iModel, ".txt", sep = "")
    det = fread(file = filename, header = TRUE, verbose = TRUE)
    det$devAg = (det$ShockAgent == det$ObsAgent)
    det$nondevAg = !det$devAg
    det$PreShockPriceDevAg = ifelse(det$ShockAgent == 1, det$PreShockPrice1, det$PreShockPrice2)
    det$PreShockPriceNonDevAg = ifelse(det$ShockAgent == 1, det$PreShockPrice2, det$PreShockPrice1)

    CondBR = (det$DevToPrice == det$DevAgStaticBR001)
    dataBR = det[CondBR, ]
    dim(dataBR)
    dN = dataBR[dataBR$nondevAg, ]
    dim(dN)
    
    iii = which(names(convResults) == "Ag1Price01")-1
    Prices = as.numeric(unlist(convResults[iModel, (iii+1):(iii+numPrices)]))
    x1 = Prices[dN$ShockPrice002]
    x0 = Prices[dN$ShockPrice001]
    tab[iModel, 6] = mean((x1-x0)/x0)
    
    # IC
    
    dD = dataBR[dataBR$devAg, ]
    x1 = dD$DeviationQ
    x0 = dD$OptStratQ001
    tab[iModel, 7] = mean(x1 < x0)
    
}

tab = round(tab, digits = 2)
max.print = getOption('max.print')
options(max.print = nrow(tab)*ncol(tab))
sink('table_A7.txt')
tab
sink()
options(max.print = max.print)

