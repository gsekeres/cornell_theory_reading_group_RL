# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure 3
# Average profit gain as a function of delta
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

# Load data

data = read.csv("A_convResults.txt", header = TRUE, sep = "")
data = data[, c("delta1", "avgPrGain", "sePrGain")]
names(data) = c("delta", "apg", "se.apg")
pdf(file = "figure_3.pdf", width = 6, height = 4, paper = 'special')
par(mgp = c(2.5, 1, 0))
plot(x = data$delta, y = data$apg, 
     ylim = c(min(data$apg), max(data$apg)), xlab = expression(delta), 
     main = "", ylab = expression(Delta),
     type = "l", col = "black", lwd = 2)
par(mgp = c(3, 1, 0))
dev.off()

