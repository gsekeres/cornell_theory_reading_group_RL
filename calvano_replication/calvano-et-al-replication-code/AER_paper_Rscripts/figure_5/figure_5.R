# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure 5
# Impulse response boxplots
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

# Loads data in Rdata format (should be faster)

load("det.Rdata")

numAgents = 2
numStates = dim(Strategies)[1]/numAgents
numPrices = sqrt(numStates)
numGames = dim(Strategies)[2]
numPeriods = 10

# Only sessions converging to symmetric cycles with length 1
# Normalized prices

CondPreCycle1 = (det$PreShockCycleLength == 1) & (det$PreShockPrice1 == det$PreShockPrice2)
dD = det[CondPreCycle1, ]
dD = dD[dD$devAg, ]
dD = dD[dD$DevToPrice == dD$StaticBRPrice001, ]
round(table(dD$PreShockPriceDevAg)/dim(dD)[1], digits = 3)
dD = dD[dD$PreShockPriceDevAg == as.numeric(names(which.max(table(dD$PreShockPriceDevAg)))), ]
gc()
dD$ShockPrice001S = round(Prices[dD$ShockPrice001]-Prices[dD$PreShockPriceDevAg], digits = 3)
dD$ShockPrice002S = round(Prices[dD$ShockPrice002]-Prices[dD$PreShockPriceDevAg], digits = 3) 
dD$ShockPrice003S = round(Prices[dD$ShockPrice003]-Prices[dD$PreShockPriceDevAg], digits = 3)
dD$ShockPrice004S = round(Prices[dD$ShockPrice004]-Prices[dD$PreShockPriceDevAg], digits = 3)
dD$ShockPrice005S = round(Prices[dD$ShockPrice005]-Prices[dD$PreShockPriceDevAg], digits = 3)
dD$ShockPrice006S = round(Prices[dD$ShockPrice006]-Prices[dD$PreShockPriceDevAg], digits = 3)
dD$ShockPrice007S = round(Prices[dD$ShockPrice007]-Prices[dD$PreShockPriceDevAg], digits = 3)
dD$ShockPrice008S = round(Prices[dD$ShockPrice008]-Prices[dD$PreShockPriceDevAg], digits = 3)
dD$ShockPrice009S = round(Prices[dD$ShockPrice009]-Prices[dD$PreShockPriceDevAg], digits = 3)
dD$ShockPrice010S = round(Prices[dD$ShockPrice010]-Prices[dD$PreShockPriceDevAg], digits = 3)
dD$PreShockPriceDevAgS = 0
dim(dD)

dN = det[CondPreCycle1, ]
dN = dN[dN$nondevAg, ]
dN = dN[dN$DevToPrice == dN$StaticBRPrice001, ]
round(table(dN$PreShockPriceNonDevAg)/dim(dN)[1], digits = 3)
dN = dN[dN$PreShockPriceNonDevAg == as.numeric(names(which.max(table(dN$PreShockPriceNonDevAg)))), ]
gc()
dN$ShockPrice001S = round(Prices[dN$ShockPrice001]-Prices[dN$PreShockPriceNonDevAg], digits = 3)
dN$ShockPrice002S = round(Prices[dN$ShockPrice002]-Prices[dN$PreShockPriceNonDevAg], digits = 3) 
dN$ShockPrice003S = round(Prices[dN$ShockPrice003]-Prices[dN$PreShockPriceNonDevAg], digits = 3)
dN$ShockPrice004S = round(Prices[dN$ShockPrice004]-Prices[dN$PreShockPriceNonDevAg], digits = 3)
dN$ShockPrice005S = round(Prices[dN$ShockPrice005]-Prices[dN$PreShockPriceNonDevAg], digits = 3)
dN$ShockPrice006S = round(Prices[dN$ShockPrice006]-Prices[dN$PreShockPriceNonDevAg], digits = 3)
dN$ShockPrice007S = round(Prices[dN$ShockPrice007]-Prices[dN$PreShockPriceNonDevAg], digits = 3)
dN$ShockPrice008S = round(Prices[dN$ShockPrice008]-Prices[dN$PreShockPriceNonDevAg], digits = 3)
dN$ShockPrice009S = round(Prices[dN$ShockPrice009]-Prices[dN$PreShockPriceNonDevAg], digits = 3)
dN$ShockPrice010S = round(Prices[dN$ShockPrice010]-Prices[dN$PreShockPriceNonDevAg], digits = 3)
dN$PreShockPriceNonDevAgS = 0 
dim(dN)

# Deviating agent: prepare boxplot data

DevAgBP = boxplot(dD$PreShockPriceDevAgS, 
                  dD$ShockPrice001S, dD$ShockPrice002S, dD$ShockPrice003S, dD$ShockPrice004S, dD$ShockPrice005S, 
                  dD$ShockPrice006S, dD$ShockPrice007S, dD$ShockPrice008S, dD$ShockPrice009S, dD$ShockPrice010S)
tmp = as.data.frame(as.matrix(cbind(dD$PreShockPriceDevAgS, 
                                    dD$ShockPrice001S, dD$ShockPrice002S, dD$ShockPrice003S, dD$ShockPrice004S, dD$ShockPrice005S, 
                                    dD$ShockPrice006S, dD$ShockPrice007S, dD$ShockPrice008S, dD$ShockPrice009S, dD$ShockPrice010S)))
DevAgBP$stats[1, ] = apply(tmp, 2, quantile, probs = 0.025)
DevAgBP$stats[2, ] = apply(tmp, 2, quantile, probs = 0.25)
DevAgBP$stats[3, ] = apply(tmp, 2, mean)
DevAgBP$stats[4, ] = apply(tmp, 2, quantile, probs = 0.75)
DevAgBP$stats[5, ] = apply(tmp, 2, quantile, probs = 0.975)
DevAgBP$names = as.character(0:numPeriods)

# Nondeviating agent: prepare boxplot data

NonDevAgBP = boxplot(dN$PreShockPriceNonDevAgS, 
                     dN$ShockPrice001S, dN$ShockPrice002S, dN$ShockPrice003S, dN$ShockPrice004S, dN$ShockPrice005S, 
                     dN$ShockPrice006S, dN$ShockPrice007S, dN$ShockPrice008S, dN$ShockPrice009S, dN$ShockPrice010S)
ylim = c(min(NonDevAgBP$stats), max(NonDevAgBP$stats))
tmp = as.data.frame(as.matrix(cbind(dN$PreShockPriceNonDevAgS, 
                                    dN$ShockPrice001S, dN$ShockPrice002S, dN$ShockPrice003S, dN$ShockPrice004S, dN$ShockPrice005S, 
                                    dN$ShockPrice006S, dN$ShockPrice007S, dN$ShockPrice008S, dN$ShockPrice009S, dN$ShockPrice010S)))
NonDevAgBP$stats[1, ] = apply(tmp, 2, quantile, probs = 0.025)
NonDevAgBP$stats[2, ] = apply(tmp, 2, quantile, probs = 0.25)
NonDevAgBP$stats[3, ] = apply(tmp, 2, mean)
NonDevAgBP$stats[4, ] = apply(tmp, 2, quantile, probs = 0.75)
NonDevAgBP$stats[5, ] = apply(tmp, 2, quantile, probs = 0.975)
NonDevAgBP$names = as.character(0:numPeriods)

# Plots

pdf(file = "figure_5.pdf", width = 8, height = 5, paper = 'special')
par(mfrow = c(1, 2), mgp = c(2.2, 1, 0), mai = c(1.02, 0.75, 0.82, 0.05))
bxp(DevAgBP, boxcol = "black", boxfill = "chocolate1", main = "Deviating agent", 
    xlab = "Time", ylab = "Price change", 
    ylim = ylim, 
    outline = FALSE, cex.axis = 0.9)
bxp(NonDevAgBP, boxcol = "black", boxfill = "chocolate1", main = "Nondeviating agent", 
    xlab = "Time", ylab = "Price change", ylim = ylim, 
    names = as.character(0:numPeriods), outline = FALSE, cex.axis = 0.9)
par(mfrow = c(1, 1), mgp = c(3, 1, 0), mai = c(1.02, 0.82, 0.82, 0.42))
dev.off()

