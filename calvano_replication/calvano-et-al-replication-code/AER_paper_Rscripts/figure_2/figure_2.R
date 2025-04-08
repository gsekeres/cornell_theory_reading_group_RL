# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure 2
# Fraction of sessions converging to a Nash equilibrium
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

p = read.csv("A_ec.txt", header = TRUE, sep = "")
p$alpha = p$alpha1
p = p[p$alpha >= 0.025, ]
p$beta = p$beta3/25000*100000

# Plot: Equilibrium play on path

pdf(file = "figure_2.pdf", width = 6, height = 4, paper = 'special')
levelplot(FlagEQOnPath_Len00 ~ beta*alpha, data = p,
          xlab = list(expression(beta %*% 10^5), cex = 0.85),
          ylab = list(expression(alpha), cex = 0.8),
          col.regions = sequential_hcl(n = 100, "OrRd", rev = TRUE),
          par.settings = list(layout.heights = list(axis.xlab.padding = 0.1)))
dev.off()


