# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure A3
# Frequency of cycles of period 1 
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

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

p$freq = p$num_Len01/p$num_Len00

pdf(file = "figure_A3.pdf", 
    width = 6, height = 4, paper = 'special')
levelplot(freq ~ beta*alpha, data = p,
          xlab = list(expression(beta %*% 10^5), cex = 0.85),
          ylab = list(expression(alpha), cex = 0.8),
          col.regions = sequential_hcl(n = 101, "OrRd", rev = TRUE),
          par.settings = list(layout.heights = list(axis.xlab.padding = 0.1)))
dev.off()

