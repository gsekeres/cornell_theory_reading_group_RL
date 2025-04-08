# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure A1
# Iterations to convergence
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

p = read.csv("A_res.txt", 
             header = TRUE, sep = "")
p$alpha = p$alpha1
p = p[p$alpha >= 0.025, ]
p$beta = p$beta3/25000*100000

p$avgTTC = p$avgTTC*25000


pdf(file = "figure_A4.1_grid_TTC.pdf", 
    width = 6, height = 4, paper = 'special')
f = function(x) -1/(x/100000000)
z = f(p$avgTTC)
qz = min(z)+(max(z)-min(z))*seq(from = 0, to = 1, length.out = 101)
levelplot(z ~ beta*alpha, data = p,
          xlab = list(expression(beta %*% 10^5), cex = 0.85),
          ylab = list(expression(alpha), cex = 0.8),
          at = qz,
          col.regions = sequential_hcl(n = 101, "OrRd", rev = TRUE),
          colorkey = list(at = qz, 
                          labels = list(at = c(f(500000), 
                                               f(1000000), 
                                               f(2000000)), 
                                        labels = c("0.5", 
                                                   "1", 
                                                   "2"))),
          par.settings = list(layout.heights = list(axis.xlab.padding = 0.1)))
dev.off()






