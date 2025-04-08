# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure A9
# Impulse response fan charts
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

if (!require(ggfan)) {
    install.packages("ggfan")
    library(ggfan)
}
if (!require(ggplot2)) {
    install.packages("ggplot2")
    library(ggplot2)
}
if (!require(gridExtra)) {
    install.packages("gridExtra")
    library(gridExtra)
}
if (!require(colorspace)) {
    install.packages("colorspace")
    library(colorspace)
}

# Load data

load("det.Rdata")

numAgents = 2
numStates = dim(Strategies)[1]/numAgents
numPrices = sqrt(numStates)
numGames = dim(Strategies)[2]
numPeriods = 10

CondPreCycle1 = (det$PreShockCycleLength == 1)
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

# Plots

# Deviating agent

offset = which(names(dD) == "ShockPrice001S")-1
ir = cbind(dD$PreShockPriceDevAgS, dD[, (offset+1):(offset+numPeriods)])
N = dim(ir)[1]
ir2 = data.frame(x = rep(x = 0:numPeriods, times = N),
                 Sim = rep(1:N, each = numPeriods+1), 
                 y = c(t(ir)))
plotDevAg = ggplot(ir2, aes(x = x, y = y)) + 
    geom_fan(intervals = (5:95)/100) + 
    theme_minimal() +  
    scale_fill_distiller(palette = "OrRd") +
    ggtitle("Deviating agent") + 
    scale_x_continuous(name = "Time", 
                       breaks = 0:10, 
                       labels = as.character(0:10)) + 
    scale_y_continuous(name = "Price change") + 
    theme_bw() + 
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5, size = 12, family = "serif"),
          axis.text = element_text(size = 8, family = "serif"),
          axis.title = element_text(size = 10, family = "serif"),
          legend.title = element_text(size = 10, family = "serif"),
          legend.text = element_text(size = 8, family = "serif"))

# Nondeviating agent

offset = which(names(dN) == "ShockPrice001S")-1
ir = cbind(dN$PreShockPriceNonDevAgS, dN[, (offset+1):(offset+numPeriods)])
N = dim(ir)[1]
ir2 = data.frame(x = rep(x = 0:numPeriods, times = N),
                 Sim = rep(1:N, each = numPeriods+1), 
                 y = c(t(ir)))
plotNonDevAg = ggplot(ir2, aes(x = x, y = y)) + 
    geom_fan(intervals = (5:95)/100) + 
    theme_minimal() +  
    scale_fill_distiller(palette = "OrRd") +
    ggtitle("Nondeviating agent") + 
    scale_x_continuous(name = "Time", 
                       breaks = 0:10, 
                       labels = as.character(0:10)) + 
    scale_y_continuous(name = "Price change") + 
    theme_bw() + 
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5, size = 12, family = "serif"),
          axis.text = element_text(size = 8, family = "serif"),
          axis.title = element_text(size = 10, family = "serif"),
          legend.title = element_text(size = 10, family = "serif"),
          legend.text = element_text(size = 8, family = "serif"))

# Save plots to file

pdf(file = "figure_A9.pdf", 
    width = 8, height = 4, paper = 'special')
grid.arrange(plotDevAg, plotNonDevAg)
dev.off()
