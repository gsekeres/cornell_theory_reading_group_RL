# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure A13
# Boxplot of Centrality Concentration Curves
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

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
betweenness = matrix(data = 0, nrow = numStates, ncol = numGames)
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
    
    # Compute data on betweenness towards cycle states
    
    # Create dataframes "nodes" and "edges"
    
    nodes = data.frame(state = seq(from = 1, to = numStates, by = 1))
    edges = data.frame(from = nodes[["state"]])
    edges["to"] = rep(x = 0, times = numStates)
    
    for (p1 in 1:numPrices) {
      for (p2 in 1:numPrices) {
        state.from = (p1-1)*numPrices+p2
        p1.new = strat1[state.from]
        p2.new = strat2[state.from]
        state.to = (p1.new-1)*numPrices+p2.new
        edges[state.from, 2] = state.to
      }
    }
    
    # Set up the network and compute betweenness
    
    ig = graph.edgelist(as.matrix(edges), directed = TRUE)
    
    for (iState in 1:numStates) {
      if (iState %in% S0) next
      path = unlist(shortest_paths(ig, from = iState, to = V(ig)[S0], 
                                   mode = "out", output = "vpath")$vpath)
      betweenness[path, iGame] = betweenness[path, iGame]+1
    }
    
}

# Grafico dettagliato solo per le sessioni in cui si ritorna sempre al path

sel.cond = (colSums(backtocycle) == numStates)
sub.betweenness = betweenness[, sel.cond]
sub.CycleLength = CycleLength[sel.cond]
sub.CycleStates = CycleStates[sel.cond, ]
sub.numGames = dim(sub.betweenness)[2]
fbet = matrix(data = NA, nrow = numStates, ncol = sub.numGames)
rownames(fbet) = as.character(1:numStates)
for (iGame in 1:sub.numGames) {
  z = sub.betweenness[, iGame]
  S0 = sub.CycleStates[iGame, 1:sub.CycleLength[iGame]]
  z = z[-S0]
  z = z[order(z, decreasing = TRUE)]
  z = z/sum(z)
  z = cumsum(z)
  fbet[1:length(z), iGame] = z
}
fbet = t(fbet)
pdf(file = "figure_A13.pdf", 
    width = 6, height = 4, paper = 'special')
par(mgp = c(2, 1, 0), cex.lab = 0.85)
boxplot(fbet[, 1:15], col = "chocolate1", outline = FALSE, 
        xlab = "X most central nodes", 
        ylab = "% of total betweenness", 
        main = "")
par(mgp = c(3, 1, 0), cex.lab = 1)
dev.off()

