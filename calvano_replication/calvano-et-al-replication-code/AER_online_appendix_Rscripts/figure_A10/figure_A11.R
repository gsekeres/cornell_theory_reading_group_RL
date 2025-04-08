# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure A11
# Distribution of the number of states from which the algos
# do not return to the limit path
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

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

if (!require(plotrix)) {
  install.packages("plotrix")
  library(plotrix)
}

# Loads data in Rdata format

load("det.Rdata")
rm(CyclePrices, CycleProfits, det, Converged, TimeToConvergence)
gc() 

# Analysis of strategies

numAgents = 2
numStates = dim(Strategies)[1]/numAgents
numPrices = sqrt(numStates)
numGames = dim(Strategies)[2]
numPeriods = numStates

# Loop over games

backtocycle = matrix(data = 0, nrow = numStates, ncol = numGames)
for (iGame in 1:numGames) {
  
    strat1 = Strategies[1:numStates, iGame]
    strat2 = Strategies[(numStates+1):(2*numStates), iGame]
    n = CycleLength[iGame]
    S0 = CycleStates[iGame, 1:n]
    
    # Compute data on sessions returning to cycle
    
    for (ip1 in 1:numPrices) {
      for (ip2 in 1:numPrices) {
        s0 = (ip1-1)*numPrices+ip2
        p1.t = ip1
        p2.t = ip2
        S = NULL
        for (t in 1:numPeriods) {
          s.t = (p1.t-1)*numPrices+p2.t
          if (any(S0 == s.t)) {
            backtocycle[s0, iGame] = 1
            break
          } else if (any(S == s.t)) {
            break
          } else {
            S = c(S, s.t)
            p1.t = strat1[s.t]
            p2.t = strat2[s.t]
          }
        }
      }
    }
  
}

# Frequency of return to limit path

z = table(colSums(1-backtocycle))
pdf(file = "figure_A11.pdf", 
    width = 6, height = 4, paper = 'special')
par(mgp = c(2, 1, 0), cex.lab = 0.85)
gap.barplot(z, 
            gap = c(50, 900), 
            xtics = as.numeric(names(z)), 
            xlab = "Number of states", cex.axis = 0.85,
            ytics = c(20, 40, 920),
            ylab = "Number of sessions",
            col = rep(x = "chocolate1", times = length(z)))
par(mgp = c(3, 1, 0), cex.lab = 1)
dev.off()
