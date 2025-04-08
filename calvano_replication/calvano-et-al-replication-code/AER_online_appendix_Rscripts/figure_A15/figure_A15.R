# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure A15
# Average profit gain with Boltzmann exploration: heat map
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

if (!require(lattice)) {
    install.packages("lattice")
    library(lattice)
}
if (!require(colorspace)) {
    install.packages("colorspace")
    library(colorspace)
}

# Load data

ConvResults = read.csv("A_ConvResults.txt", 
                       header = TRUE, sep = "")
ConvResults$alpha = ConvResults$alpha1
ConvResults = ConvResults[ConvResults$alpha >= 0.025, ]
ConvResults$beta = 1-10^(ConvResults$beta3)

pdf(file = "figure_A15.pdf", 
    width = 6, height = 4, paper = 'special')
levelplot(avgPrGain ~ beta*alpha1, data = ConvResults,
          xlab = list(expression(lambda[1]), cex = 0.85),
          ylab = list(expression(alpha), cex = 0.8),
          col.regions = sequential_hcl(n = 101, "OrRd", rev = TRUE),
          xlim = c(max(ConvResults$beta), min(ConvResults$beta)))
dev.off()
