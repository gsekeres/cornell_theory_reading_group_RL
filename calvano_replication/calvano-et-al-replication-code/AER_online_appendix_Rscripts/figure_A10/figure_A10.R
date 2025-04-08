# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure A10
# Average best response functions
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

if (!require(igraph)) {
    install.packages("igraph")
    library(igraph)
}
if (!require(lattice)) {
    install.packages("lattice")
    library(lattice)
}
if (!require(colorspace)) {
    install.packages("colorspace")
    library(colorspace)
}
if (!require(gridExtra)) {
    install.packages("gridExtra")
    library(gridExtra)
}

# Loads data in Rdata format (should be faster)

load("det.Rdata")
rm(det, Converged, TimeToConvergence)
gc()

# Defining parameters and computing profits

numAgents = 2
numStates = dim(Strategies)[1]/numAgents
numPrices = sqrt(numStates)
numGames = dim(Strategies)[2]
NashProfit = 0.222927
CoopProfit = 0.33749

# Profit matrix

a0 = 0
a1 = 2
a2 = 2
c1 = 1
c2 = 2
sigma = 0.25
PI = matrix(data = NA, nrow = numPrices, ncol = numPrices)
for (p1 in 1:numPrices) {
    for (p2 in 1:numPrices) {
        q1 = exp((a1-Prices[p1])/sigma)/(exp(a0/sigma)+exp((a1-Prices[p1])/sigma)+exp((a2-Prices[p2])/sigma))
        PI[p1, p2] = q1*(Prices[p1]-c1)
    }
}

# Loop over strategies to identify the 1-period symmetric ones
# converging to (10, 10) (the most frequent)

selStrat = NULL
for (iGame in 1:numGames) {
    if ((CycleLength[iGame] == 1) & (CyclePrices[iGame, 1] == CyclePrices[iGame, 2]) & (CyclePrices[iGame, 1] == 10)) {
        selStrat = c(selStrat, iGame)
    }
}

# Select strategies and compute mean by row

selStrategies = Strategies[, selStrat]
meanStrat = rowMeans(selStrategies)
p1 = rep(x = 1:numPrices, each = numPrices)
p2 = rep(x = 1:numPrices, times = numPrices)
s1 = meanStrat[1:numStates]
s2 = meanStrat[(numStates+1):(2*numStates)]
S = data.frame(s1 = s1, s2 = s2, p1 = p1, p2 = p2)
pdf(file = "figure_A10.pdf", 
    width = 8, height = 4, paper = 'special')
hm1 = levelplot(s1 ~ p1*p2, data = S,
                xlab = list(expression(p[1]), cex = 0.85),
                ylab = list(expression(p[2]), cex = 0.85),
                col.regions = sequential_hcl(n = 100, "OrRd", rev = TRUE), 
                main = "Agent 1",
                par.settings = list(par.main.text = list(cex = 1, font = 1),
                                    layout.heights = list(main.key.padding = -1,
                                                          axis.xlab.padding = 0.1),
                                    layout.widths = list(ylab.axis.padding = -0.5)))
hm2 = levelplot(s2 ~ p1*p2, data = S,
                xlab = list(expression(p[1]), cex = 0.85),
                ylab = list(expression(p[2]), cex = 0.85),
                col.regions = sequential_hcl(n = 100, "OrRd", rev = TRUE), 
                main = "Agent 2",
                par.settings = list(par.main.text = list(cex = 1, font = 1),
                                    layout.heights = list(main.key.padding = -1,
                                                          axis.xlab.padding = 0.1),
                                    layout.widths = list(ylab.axis.padding = -0.5)))
grid.arrange(hm1, hm2, ncol = 2)
dev.off()
