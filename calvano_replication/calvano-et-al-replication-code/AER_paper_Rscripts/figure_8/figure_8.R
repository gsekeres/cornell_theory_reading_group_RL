# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure 8
# Graph of limit strategies, session 364
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# Clear memory

rm(list = ls())
gc()

# Set working directory and load packages

if (!require(rstudioapi)) {
        install.packages("rstudioapi")
        library(rstudioapi)
}
if (!require(igraph)) {
  install.packages("igraph")
  library(igraph)
}
if (!require(colorspace)) {
  install.packages("colorspace")
  library(colorspace)
}

current.path = getActiveDocumentContext()$path 
setwd(dirname(current.path))

# Loads data in Rdata format (should be faster)

load("det.Rdata")
rm(Converged, TimeToConvergence)
gc()

# Define setup

numAgents = 2
numStates = dim(Strategies)[1]/numAgents
numPrices = sqrt(numStates)
numGames = dim(Strategies)[2]
NashProfit = 0.222927
CoopProfit = 0.33749

# Compute the profit matrix

a0 = 0
a1 = 2
a2 = 2
c1 = 1
c2 = 1
sigma = 0.25
PI = matrix(data = NA, nrow = numPrices, ncol = numPrices)
for (p1 in 1:numPrices) {
  for (p2 in 1:numPrices) {
    q1 = exp((a1-Prices[p1])/sigma)/(exp(a0/sigma)+exp((a1-Prices[p1])/sigma)+exp((a2-Prices[p2])/sigma))
    PI[p1, p2] = q1*(Prices[p1]-c1)
  }
}

# Load the strategy

iGame = 364
strat1 = Strategies[1:numStates, iGame]
strat2 = Strategies[(numStates+1):(2*numStates), iGame]
n = CycleLength[iGame]
S0 = CycleStates[iGame, 1:n]

# Create the "nodes" data frame

nodes = data.frame(state = seq(from = 1, to = numStates, by = 1))
nodes["label"] = rep(x = "0", times = numStates)
vertex.coord = matrix(data = NA, nrow = numStates, ncol = 2)
for (p1 in 1:numPrices) {
  for (p2 in 1:numPrices) {
    nodes[(p1-1)*numPrices+p2, 2] = paste("(", p1, ",", p2, ")", sep = "")
    vertex.coord[(p1-1)*numPrices+p2, 1] = p1
    vertex.coord[(p1-1)*numPrices+p2, 2] = p2
  }
}

# Create the "edges" data frame

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

# Create the color palette

vertex.col.PI = rep(x = 0, times = numStates)
for (p1 in 1:numPrices) {
  for (p2 in 1:numPrices) {
    vertex.col.PI[(p1-1)*numPrices+p2] = (PI[p1, p2]+PI[p2, p1])/2
    vertex.col.PI[(p1-1)*numPrices+p2] = (vertex.col.PI[(p1-1)*numPrices+p2]-NashProfit)/(CoopProfit-NashProfit)
  }
}

fine = numStates
pal = colorRampPalette(sequential_hcl(n = 100, "OrRd", rev = TRUE))
graphCol = pal(fine)[as.numeric(cut(vertex.col.PI, breaks = fine))]

# Construct the network

ig = graph.edgelist(as.matrix(edges), directed = TRUE)
vertex.shape = rep(x = "circle", times = numStates)
vertex.shape[S0] = "square"

# Compute betweenness w.r.t. limit path

betS0 = rep(x = 0, times = numStates)
for (iState in 1:numStates) {
  if (iState %in% S0) next
  path = unlist(shortest_paths(ig, from = iState, to = V(ig)[S0], mode = "out", output = "vpath")$vpath)
  betS0[path] = betS0[path]+1
}

# Plot

lfr = layout_with_fr(ig)
q = tkplot(ig, layout = lfr, 
           vertex.size = 1+betS0^0.5, vertex.label = NA, 
           vertex.color = graphCol, vertex.shape = vertex.shape, 
           edge.arrow.size = 0.2, edge.arrow.width = 1, edge.color = "gray50")
coords = tk_coords(q)
save(coords, file = "coords_lfr_betS0.Rdata")
tk_off()

load(file = "coords_lfr_betS0.Rdata")
pdf(file = "figure_8.pdf", 
    width = 6, height = 6, paper = 'special')
par(mar = c(0, 0, 0, 0))
plot(ig, layout = coords, 
     vertex.size = 1+betS0^0.5, vertex.label = NA, 
     vertex.color = graphCol, vertex.shape = vertex.shape, 
     edge.arrow.size = 0.2, edge.arrow.width = 1, edge.color = "gray50")
dev.off()

# Check statements after figure 

numAgents = 2
numStates = dim(Strategies)[1]/numAgents
numPrices = sqrt(numStates)
numGames = dim(Strategies)[2]
numPeriods = numStates

backtocycle = matrix(data = NA, nrow = numStates, ncol = numGames)
pathlength = matrix(data = NA, nrow = numStates, ncol = numGames)
betweenness = matrix(data = NA, nrow = numStates, ncol = numGames)

for (iGame in 1:numGames) {
    if ((iGame %% 100) == 0) cat("iGame = ", iGame, "\n")
    strat1 = Strategies[1:numStates, iGame]
    strat2 = Strategies[(numStates+1):(2*numStates), iGame]
    n = CycleLength[iGame]
    S0 = CycleStates[iGame, 1:n]
    
    # Compute data on sessions returning to cycle
    
    backtocyclegame = array(data = 0, dim = numStates)
    pathlengthgame = array(data = 0, dim = numStates)
    betweennessgame = array(data = 0, dim = numStates)
    for (ip1 in 1:numPrices) {
      for (ip2 in 1:numPrices) {
        s0 = (ip1-1)*numPrices+ip2
        p1.t = ip1
        p2.t = ip2
        S = NULL
        for (t in 1:numPeriods) {
          s.t = (p1.t-1)*numPrices+p2.t
          if (any(S0 == s.t)) {
            pathlengthgame[s0] = t
            backtocyclegame[s0] = 1
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
    pathlength[, iGame] = pathlengthgame
    backtocycle[, iGame] = backtocyclegame
    
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
    
    # Build network and compute betweenness
    
    ig = graph.edgelist(as.matrix(edges), directed = TRUE)
    
    for (iState in 1:numStates) {
      if (iState %in% S0) next
      path = unlist(shortest_paths(ig, from = iState, to = V(ig)[S0], 
                                   mode = "out", output = "vpath")$vpath)
      betweennessgame[path] = betweennessgame[path]+1
    }
    betweenness[, iGame] = betweennessgame
    
}

# Frequency of overall return to limit path

cumsum(rev(table(colSums(x = backtocycle))))

# Average length of return path to limit cycle

summary(c(pathlength[backtocycle == 1]))
