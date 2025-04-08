# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure 7
# Nondeviating agent punishment in period 2
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

p = read.csv("A_irToBR.txt", header = TRUE, sep = "")
p$alpha = p$alpha1
p = p[p$alpha >= 0.025, ]
p$beta = p$beta3/25000*100000

# Plot

punishment = p$AggrNonDevPriceShockPer002/p$AggrNonDevPriceShockPer001
pdf(file = "figure_7.pdf", 
    width = 6, height = 4, paper = 'special')
levelplot(punishment ~ beta*alpha, data = p,
          xlab = list(expression(beta %*% 10^5), cex = 0.85),
          ylab = list(expression(alpha), cex = 0.8),
          col.regions = sequential_hcl(n = 100, "OrRd"),
          par.settings = list(layout.heights = list(axis.xlab.padding = 0.1)))
dev.off()

