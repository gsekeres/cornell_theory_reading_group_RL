# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Figure 10
# Profit gain trajectory, benchmark model
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

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

# Parameters

numPrices = 15
numStates = numPrices^2
numGames = 1000
NashPrice = 1.47293
CoopPrice = 1.92498
NashProfit = 0.222927
CoopProfit = 0.33749

#######################
# Standard price grid #
#######################

a0 = 0
a1 = 2
a2 = 2
c1 = 1
c2 = 1
mu = 0.25
extend1 = 0.1
extend2 = 0.1
Prices = array(data = NA, dim = numPrices)
Prices[1] = NashPrice-extend1*(CoopPrice-NashPrice)
Prices[numPrices] = CoopPrice+extend2*(CoopPrice-NashPrice)
deltaPrices = (Prices[numPrices]-Prices[1])/(numPrices-1)
for (i in 2:(numPrices-1)) {
    Prices[i] = Prices[i-1]+deltaPrices    
}

# Compute the profit matrix

PI = matrix(data = NA, nrow = numPrices, ncol = numPrices)
for (p1 in 1:numPrices) {
    for (p2 in 1:numPrices) {
        q1 = exp((a1-Prices[p1])/mu)/(exp(a0/mu)+exp((a1-Prices[p1])/mu)+exp((a2-Prices[p2])/mu))
        PI[p1, p2] = q1*(Prices[p1]-c1)
    }
}

# Profit gain trajectory with Nash (second price) plus exploration

PInn2 = PI[2, 2]
PIni2 = (mean(PI[2, ])+mean(PI[, 2]))/2
PIii = mean(PI)
PGnn2 = (PInn2-NashProfit)/(CoopProfit-NashProfit)
PGni2 = (PIni2-NashProfit)/(CoopProfit-NashProfit)
PGii = (PIii-NashProfit)/(CoopProfit-NashProfit)

# Compute the average profit gain trajectory

data = read.csv("LTrajectories_1.txt", sep = "")
p = data.frame(iter = data$iter)
p$apg = data$AvgPG

R = dim(p)[1]
beta = 0.1
itersPerYear = 25000
p$ExploreNash2 = 0
p$ExploreNash2[1] = PGii
for (i in 2:R) {
    if ((i %% 100) == 0) cat("i = ", i, " , R = ", R, "\n")
    T0 = data$iter[i-1]
    T1 = data$iter[i]
    meanEps = exp(-beta*T0/itersPerYear)*(1-exp(-beta*(T1-T0+1)/itersPerYear))/(1-exp(-beta/itersPerYear))/(T1-T0+1)
    meanEps2 = exp(-2*beta*T0/itersPerYear)*(1-exp(-2*beta*(T1-T0+1)/itersPerYear))/(1-exp(-2*beta/itersPerYear))/(T1-T0+1)
    p$ExploreNash2[i] = PGnn2*(1-2*meanEps+meanEps2)+
        2*PGni2*(meanEps-meanEps2)+
        PGii*meanEps2
}

# Plot

T = 1500000
v = (p$iter <= T)
pdf(file = "figure_10.pdf", 
    width = 6, height = 4, paper = 'special')
par(mgp = c(2.5, 1, 0))
plot(x = p$iter[v], y = p$apg[v], ylim = c(0, 1), 
     type = "l", lwd = 2, col = "black", lty = 1, 
     xlab = "Time", ylab = expression(Delta))
lines(x = p$iter[v], y = p$ExploreNash2[v], 
      col = "black", lwd = 2, lty = 2)
par(mgp = c(3, 1, 0))
dev.off()
