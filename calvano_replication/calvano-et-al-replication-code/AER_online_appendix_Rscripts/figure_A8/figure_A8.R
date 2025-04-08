# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure A8
# Average impulse response to a five-period deviation to Nash
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

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

NashPrice = 1.47293
CoopPrice = 1.92498
numPeriods = 15

data = read.csv("A_irToNash.txt", header = TRUE, sep = "")
data = t(data.matrix(data))

p = data.frame(time = 0:numPeriods)
ll = grep("AggrDevPriceShockPer", row.names(data))
ll2 = grep("seAggrDevPriceShockPer", row.names(data))
ll = ll[!(ll %in% ll2)]
ll = ll[1:numPeriods]
row.names(data)[ll]
p$AvgPGDevAg = c(data["AggrPricePre", 1], data[ll, 1])
ll = grep("AggrNonDevPriceShockPer", row.names(data))
ll2 = grep("seAggrNonDevPriceShockPer", row.names(data))
ll = ll[!(ll %in% ll2)]
ll = ll[1:numPeriods]
row.names(data)[ll]
p$AvgPGNonDevAg = c(data["AggrPricePre", 1], data[ll, 1])

# Plot

dev.off()
pdf(file = "figure_A8.pdf", 
    width = 6, height = 4, paper = 'special')
par(mgp = c(2, 1, 0), cex.lab = 0.85)
plot(x = p$time, y = p$AvgPGDevAg, 
     ylim = c(min(c(p$AvgPGDevAg, p$AvgPGNonDevAg, NashPrice)), 
              max(c(p$AvgPGDevAg, p$AvgPGNonDevAg, CoopPrice))), 
     main = "", xlab = "Time", ylab = "Price",
     type = "b", col = "black", pch = 20, lwd = 2, xaxt = "n", cex.axis = 0.85)
axis(1, tick = TRUE, at = c(0, 1, 5, 10, 15), 
     labels = c(0, 1, 5, 10, 15), cex.axis = 0.85)
lines(x = p$time, y = p$AvgPGNonDevAg, type = "b", pch = 17, col = "gray30",
      lty = 2, lwd = 2)
abline(h = NashPrice, lty = 3, lwd = 1, col = "dimgrey")
abline(h = CoopPrice, lty = 4, lwd = 1, col = "dimgrey")
abline(h = data["AggrPricePre", 1], lty = 1, col = "dimgrey", lwd = 1)
op = par(cex = 0.8)
legend(x = 9.6, y = 1.685,
       legend = c("Deviating agent", "Nondeviating agent", "Nash price", "Monopoly price", 
                  "Long run price"), 
       lty = c(1, 2, 3, 4, 1), lwd = c(2, 2, 1, 1, 1), pch = c(20, 17, NA, NA, NA), 
       col = c("black", "gray30", "dimgrey", "dimgrey", "dimgrey"), 
       bty = c("n", "n", "n", "n ", "n"))
par(mgp = c(3, 1, 0), cex.lab = 1)
dev.off()

